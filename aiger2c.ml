let input_file = ref ""
let output_file = ref ""

let parse_arguments = 
  let arguments  = 
    let open Arg in
    [ "-o", Set_string output_file, "Output the generated C code to the file" ]
  in 
  Arg.parse arguments (fun f -> input_file := f) "usage: ./aiger2c [-o output.c] input.aig"

module BddHashtbl = Hashtbl.Make
  (struct type t = Cudd.bdd let equal = Cudd.equal let hash = Hashtbl.hash end)

let max_index = ref 2

let bdd_to_index bdd tab =
  let rec add_node queue = match queue with 
    | [] -> ()
    | n :: tl -> 
      if Cudd.value n = -1 && not (BddHashtbl.mem tab n)
      then
	(BddHashtbl.add tab n !max_index; 
	 incr max_index; 
	 add_node (Cudd.t n :: Cudd.e n :: tl) )
      else add_node tl
  in add_node [bdd]; tab

let bdd_to_arrays bdd (tab,i,t,e,c) =
  
  let rec fill_node queue = match queue with 
    | [] -> ()
    | n :: tl -> 
      if Cudd.value n = -1 && i.(BddHashtbl.find tab n) = 0
      then
	(
	  let index = BddHashtbl.find tab n in
	  i.(index) <- Cudd.nodeReadIndex n;
	  t.(index) <- BddHashtbl.find tab (Cudd.t n);
	  e.(index) <- BddHashtbl.find tab (Cudd.e n);
	  c.(index) <- if Cudd.isComplement n then 1 else 0;
	  fill_node (Cudd.t n :: Cudd.e n :: tl) )
      else fill_node tl
  in fill_node [bdd]; tab,i,t,e,c
  

let main =
  let open AigerBdd in

  let aiger = Aiger.read_from_file !input_file in
  Cudd.init 1000;

  let bdd = Circuit.of_aiger aiger in
  let tab = BddHashtbl.create 100 in
  BddHashtbl.add tab (Cudd.bddFalse ()) 0;
  BddHashtbl.add tab (Cudd.bddTrue ()) 1;
 
  let _ = Hashtbl.fold (fun _ -> bdd_to_index) (Circuit.updates bdd) tab in

  (* if, then, else, complement *)
  let i,t,e,c = Array.make (!max_index) 0,Array.make (!max_index) 0,Array.make (!max_index) 0,Array.make (!max_index) 0 in
  
  let tab,i,t,e,c = Hashtbl.fold (fun _ -> bdd_to_arrays) (Circuit.updates bdd) (tab,i,t,e,c) in


  let output_int_array outch array = 
    Printf.fprintf outch "%d" array.(0);
    for a = 1 to !max_index - 1 
    do 
      Printf.fprintf outch ", %d" array.(a)
    done
  in

  let variable_to_lit_map = Hashtbl.create aiger.Aiger.num_inputs in

  for i = 0 to aiger.Aiger.num_inputs - 1
  do 
    let var = (AigerBdd.Variable.of_lit aiger (Aiger.nth_input aiger i)) in
    Hashtbl.add variable_to_lit_map (AigerBdd.Variable.to_int var) i
  done;

  for i = 0 to aiger.Aiger.num_latches - 1
  do 
    Hashtbl.add variable_to_lit_map (AigerBdd.Variable.to_int (AigerBdd.Variable.of_lit aiger (fst (Aiger.nth_latch aiger i)))) (aiger.Aiger.num_inputs + i)
  done;

  let variable_to_output_map = Hashtbl.create (aiger.Aiger.num_outputs + aiger.Aiger.num_latches) in

  let map_inv = Hashtbl.create (aiger.Aiger.num_outputs + aiger.Aiger.num_latches) in
  VariableMap.iter (fun v lit -> 
    Hashtbl.add map_inv lit v) (Circuit.map bdd);

  for i = 0 to aiger.Aiger.num_latches - 1
  do 
    let var = Hashtbl.find map_inv (fst (Aiger.nth_latch aiger i)) in
    Hashtbl.add variable_to_output_map (AigerBdd.Variable.to_int var) i
  done;

  for i = 0 to aiger.Aiger.num_outputs - 1
  do 
    let lit = Aiger.nth_output aiger i in
    let var = try Hashtbl.find map_inv lit with Not_found -> AigerBdd.Variable.of_lit aiger lit
    in
    Hashtbl.add variable_to_output_map (AigerBdd.Variable.to_int var) (aiger.Aiger.num_latches + i)
  done;
  
  Array.iteri (fun index v -> 
    i.(index) <- Hashtbl.find variable_to_lit_map v) i;
  
  let outch = if !output_file = "" then stdout else open_out !output_file in
  Printf.fprintf outch "int index[%d] = {%a};\n" !max_index output_int_array i;
  Printf.fprintf outch "int then_child[%d] = {%a};\n" !max_index output_int_array t;
  Printf.fprintf outch "int else_child[%d] = {%a};\n" !max_index output_int_array e;
  Printf.fprintf outch "int complemented[%d] = {%a};\n" !max_index output_int_array c;
  Printf.fprintf outch "void update(int input[%d], int latch[%d], int output[%d]) {\n" (aiger.Aiger.num_inputs) (aiger.Aiger.num_latches) (aiger.Aiger.num_outputs);
  
  Printf.fprintf outch "  int input_latch[%d];\n" (aiger.Aiger.num_inputs + aiger.Aiger.num_latches);
  Printf.fprintf outch "  int i;\n";
  Printf.fprintf outch "  int complement;\n";

  Printf.fprintf outch "  for (i = 0; i < %d; i++)\n    input_latch[i]=input[i];\n" (aiger.Aiger.num_inputs);
  Printf.fprintf outch "  for (i = 0; i < %d; i++)\n    input_latch[i+%d]=latch[i];\n" (aiger.Aiger.num_latches) (aiger.Aiger.num_inputs) ;


  Hashtbl.iter (fun v up -> 
    let index = Hashtbl.find variable_to_output_map (AigerBdd.Variable.to_int v) in

    if index >= aiger.Aiger.num_latches 
    then 
      (
	Printf.fprintf outch "  output[%d] = %d;\n" (index-aiger.Aiger.num_latches) (BddHashtbl.find tab up);
	Printf.fprintf outch "  complement = 0;\n";

	Printf.fprintf outch "  while(output[%d] > 1){\n" (index-aiger.Aiger.num_latches);
	Printf.fprintf outch "    output[%d]= (input_latch[index[output[%d]]])?then_child[output[%d]]:else_child[output[%d]];\n" (index-aiger.Aiger.num_latches) (index-aiger.Aiger.num_latches) (index-aiger.Aiger.num_latches) (index-aiger.Aiger.num_latches);
	Printf.fprintf outch "    complement = complement ^ complemented[output[%d]];\n  }\n" (index-aiger.Aiger.num_latches);
	Printf.fprintf outch "  output[%d] = complement ^ output[%d];\n" (index-aiger.Aiger.num_latches) (index-aiger.Aiger.num_latches);
      )
    else
      (
	Printf.fprintf outch "  latch[%d] = %d;\n" index (BddHashtbl.find tab up);
	Printf.fprintf outch "  complement = 0;\n";
	Printf.fprintf outch "  while(latch[%d] > 1){\n" index;
	Printf.fprintf outch "    latch[%d]= (input_latch[index[latch[%d]]])?then_child[latch[%d]]:else_child[latch[%d]];\n" index index index index;
	Printf.fprintf outch "    complement = complement ^ complemented[latch[%d]];\n  }\n" index;
	Printf.fprintf outch"  latch[%d] = complement ^ latch[%d];\n" index index;
      )
  ) (Circuit.updates bdd);
  
  
  Printf.fprintf outch "}\n";


  close_out outch
