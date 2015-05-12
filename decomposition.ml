(* ocamlbuild -tag use_ocaml-aiger decomposition.byte *)

let aiger = Aiger.read_from_file Sys.argv.(1)

module StringSet = Set.Make(String)
module Graph = Set.Make(struct type t = string * string let compare = compare end)

let string_set_to_string s = 
  StringSet.fold (fun a accu -> accu^" | "^a) s "{" ^ "}"

let output_graph graph outch = 
  Printf.fprintf outch "strict graph {\n";
  Graph.iter (fun (a,b) -> Printf.fprintf outch "%S -- %S;\n" a b) graph;
  Printf.fprintf outch "}\n"

let cone aiger node = 
  let rec aux node = match Aiger.lit2tag aiger node with
    | Constant x -> StringSet.empty
    | Input x | Latch (x,_) -> StringSet.singleton (Aiger.Symbol.to_string (Aiger.lit2symbol aiger x))
    | And (_,l,r) -> StringSet.union (aux (Aiger.strip l)) (aux (Aiger.strip r))
  in
  aux node

let add_edges graph set = 
  let rec aux = function 
    | [] -> graph
    | hd :: tl -> List.fold_left (fun accu x -> Graph.add (hd,x) accu) (aux tl) tl 
  in aux (StringSet.elements set)


module Decomposition = 
struct 
  type t = Lit of Aiger.lit | Conj of t list | Disj of t list
  let lit i = Lit i
  let conj l =
    let list = 
      List.fold_left 
	(fun accu a -> match a with Conj c -> List.rev_append c accu | _ -> a :: accu
	) [] l
    in match list with [a] -> a | l -> Conj (List.sort compare l)

  let to_string dec = 
    let rec aux = function 
      | Lit i -> string_of_int (Aiger.lit2int i)
      | Conj c -> "("^List.fold_left (fun accu x -> accu ^" && "^aux x) "" c ^")"
      | Disj d -> "("^List.fold_left (fun accu x -> accu ^" || "^aux x) "" d ^")"
    in aux dec


  let disj l =
    let list = 
      List.fold_left 
	(fun accu a -> match a with Disj c -> List.rev_append c accu | _ -> a :: accu
	) [] l
    in match list with [a] -> a | l -> Disj l

      (* { res: Aiger.lit list; pos: Aiger.lit list; neg: Aiger.lit list }
      (Aiger.lit list list) (*list of conjunctions*)*)

  let make res pos neg = (*{res=res;pos=pos;neg=neg}*)
    let res = List.map lit res in
    disj
      (List.rev_append
	 (List.map (fun x -> conj (lit (Aiger.aiger_not x) :: res)) pos)
	 (List.map (fun x -> conj (lit x :: res)) neg))
      

  (* take the disjunction of the i-th and j-th element of the list *)
  let join dec i j = 
    let (_,new_list,old_list) = match dec with 
      | Disj list ->
      List.fold_left
	(fun (index, new_list, old_list) s -> 
	 if index = i || index = j 
	 then (index+1, disj [s;new_list], old_list)
	 else (index+1, new_list, s :: old_list)
	) (1,Disj[],[]) list
      | _ -> failwith "in Decomposition.join the first argument is not a disjunction"
    in Disj (new_list :: old_list)

  let cone aiger dec = 
    let rec aux = function 
      | Lit i -> cone aiger i
      | Disj list 
      | Conj list -> List.fold_left (fun accu d -> StringSet.union accu (aux d)) StringSet.empty list
    in aux dec

  type similarity = { subgame:(t * t); inter_size:int; union_size:int }
		      
  let statistics aiger dec = 
    match dec with 
    | Disj list -> 
       (let rec aux a b = 
	  let cone_a = cone aiger a in 
	  let cone_b = cone aiger b in 
	  let inter = StringSet.inter cone_a cone_b in
	  let union = StringSet.union cone_a cone_b in
	  let inter_size = StringSet.cardinal inter in
	  let union_size = StringSet.cardinal union in
	  Printf.printf "(%2d,%2d) " inter_size union_size;
	  {subgame=(a,b); inter_size=inter_size ;union_size=union_size} 
	in 
	Printf.printf "     ";
	List.iter (fun a -> Printf.printf " ( %s )" (to_string a)) list;
	print_newline ();
	List.fold_left
	  (fun accu a -> 
	   Printf.printf "(%s) " (to_string a);
	   let res = 
	     List.fold_left
	       (fun accu b -> let res = aux a b in if a <> b then res :: accu else accu
	       ) accu list 
	   in
	   print_newline ();
	   res
	  ) [] list
	  
	  
       )
    | _ -> failwith "in Decomposition.statistics the first argument is not a disjunction"


    
  let auto_suggestion stats =
    match stats with 
    | hd :: tl ->
       let alpha = 10 and beta = -4 in
       let diff_val sim = alpha * sim.inter_size + beta * sim.union_size in  
       let a,b = 
	 List.fold_left 
	   (fun (best_arg,best_val) sim -> 
	    let diff_val = diff_val sim in
	    if diff_val > best_val 
	    then (sim.subgame,diff_val) 
	    else (best_arg,best_val)
	   ) (hd.subgame, diff_val hd) tl
       in a,b
    | _ -> failwith "in Decomposition.auto_suggestion: empty statistics"
    
  let auto_join dec sug = 
    match dec with 
    | Disj list -> 
       (let rec aux to_merge rest = function
	  | [] -> (disj to_merge) :: rest
	  | a :: s -> 
	     if a = fst sug || a = snd sug
	     then aux (a :: to_merge) rest s
	     else aux to_merge (a :: rest) s
	in Disj (List.rev (aux [] [] list))
       )
    | _ -> failwith "in Decomposition.auto_join the first argument is not a disjunction"
    

  (* for a disjunction, cones of the corresponding subgames *)
  let cones aiger dec = match dec with 
    | Disj l -> 
       let _ = 
	 List.fold_left
	   (fun index x -> 
	    print_endline (string_of_int index^") "^string_set_to_string (cone aiger x));
	    index + 1
	   ) 1 l 
       in ()
    | _ -> failwith "in Decomposition.cones the argument is not a disjunction"


  let to_aiger aiger dec =
    let output = List.hd aiger.Aiger.outputs in
    let rec make_aux aig a b = 
      let aig,l1 = aux aig a in
      let aig,l2 = aux aig b in
      let aig,v = Aiger.new_var aig in
      let l = Aiger.var2lit v in
      aig,l,l1,l2

    and aux aig = function
      | Lit i -> aig, i
      | Conj [a;b] -> 
	 let aig,l,l1,l2 = make_aux aig a b in
	 Aiger.add_and aig l l1 l2, l 
      | Conj (hd :: tl) -> 
	 aux aig (Conj [hd; Conj tl])
      | Disj [a;b] -> 
	 let aig,l,l1,l2 = make_aux aig a b in
	 Aiger.add_and aig l (Aiger.aiger_not l1) (Aiger.aiger_not l2), Aiger.aiger_not l
      | Disj (hd :: tl) -> 
	 aux aig (Disj [hd; Disj tl])

    in 

    let first_aux aig = function
      | Disj list -> 
	 let aig,new_list =
	   (*List.fold_left 
	     (fun (aig, list) d -> 
	      let aig, l = aux aig d in
	      let aig, v = Aiger.new_var aig in 
	      let new_lit = Aiger.var2lit v in
	      let aig = Aiger.add_and aig new_lit Aiger.aiger_true (Aiger.aiger_not l) in
	      let aig, v = Aiger.new_var aig in 
	      let new_lit2 = Aiger.var2lit v in
	      Aiger.add_and aig new_lit2 Aiger.aiger_true (Aiger.aiger_not new_lit),
	      lit new_lit2 :: list
	     ) (aig,[]) list*) aig,list
	 in aux aig (disj new_list)
    in

    let aig, i = first_aux aiger dec in
    let aig = Aiger.hide aig (Aiger.lit2symbol aiger output) in
    let aig = Aiger.add_output aig i ("err",None) in
    aig

end

let decomp =
  let rec m node = match Aiger.lit2tag aiger node with
    | Constant x -> []
    | Input x | Latch (x,_) -> []
    | And (_,l,r) -> 
       let l = 
	 if Aiger.sign l then [Aiger.strip l]
	 else m l
       in 
       let r =
	 if Aiger.sign r then [Aiger.strip r]
	 else m r
       in 
       l @ r
    and n node = match Aiger.lit2tag aiger node with
    | Constant x -> []
    | Input x | Latch (x,_) -> [x]
    | And (_,l,r) -> 
       let l = if Aiger.sign l then [] else n l in 
       let r = if Aiger.sign r then [] else n r in 
       l @ r
  in 
  let err = List.hd aiger.Aiger.outputs in 
  let n1,m1 =   
    if Aiger.sign err 
    then [], [Aiger.strip err]
    else n err, m err 
  in

  print_endline "possible decomposition:";
  let mm = List.hd m1 in
  let n2,m2 = n mm, m mm in
  let decomposition = Decomposition.make n1 n2 m2 in
    
  let rec loop dec = 
    print_endline (Decomposition.to_string dec);
    Decomposition.cones aiger dec;
    let stats = Decomposition.statistics aiger dec in
    let (x,y),coef = Decomposition.auto_suggestion stats in
    Printf.printf "--> %s %s (%d)\n" (Decomposition.to_string x) (Decomposition.to_string y) coef;
    print_endline "suggested cluster? (quit to stop/ auto for automatic)";
    let l = read_line () in
    if l = "auto" || l = ""
    then loop (Decomposition.auto_join dec (x,y))
    else if l.[0] = 'q' then dec
    else
      let i = int_of_string l in
      let l = read_line () in
      let j = int_of_string l in 
      loop (Decomposition.join dec i j)
  in 
  let dec = loop decomposition in
  print_endline (Decomposition.to_string dec);
  
  let aig = Decomposition.to_aiger aiger dec in 
  print_endline ("writing: "^Sys.argv.(1)^".manual_decomposition.aag");
  let outch = open_out (Sys.argv.(1)^".manual_decomposition.aag") in
  Aiger.write aig outch;
  close_out outch



      
