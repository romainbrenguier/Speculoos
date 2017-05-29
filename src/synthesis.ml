type declaration =
| Input of AigerBdd.symbol 
| Output of AigerBdd.symbol 
| Reg of AigerBdd.symbol
| Wire of AigerBdd.symbol
| DList of declaration list

let decl_input s = Input (AigerBdd.of_aiger_symbol s)
let decl_output s = Output (AigerBdd.of_aiger_symbol s)
let decl_reg s = Reg (AigerBdd.of_aiger_symbol s)
let decl_wire s = Wire (AigerBdd.of_aiger_symbol s)
  

let rec add_declaration types declaration = match declaration with 
  | DList dl ->
    List.fold_left add_declaration types dl
  | Input i -> AigerBdd.SymbolMap.add i (Input i) types
  | Output o -> AigerBdd.SymbolMap.add o (Output o) types
  | Reg r -> AigerBdd.SymbolMap.add r (Reg r) types
  | Wire w -> AigerBdd.SymbolMap.add w (Wire w) types

let of_declaration dl =
  List.fold_left add_declaration AigerBdd.SymbolMap.empty dl

let inputs types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Input i -> i :: accu
      | _ -> accu
    ) types []

let outputs types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Output i -> i :: accu
      | _ -> accu
    ) types []

let registers types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Reg i -> i :: accu
      | _ -> accu
    ) types []

let wires types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Wire i -> i :: accu
      | _ -> accu
    ) types []

let make_vector f name i = 
  let rec aux accu k = 
    if k < 0 then accu
    else aux (f (name,Some k) :: accu) (k-1)
  in aux [] (i-1)

let iter start last f x = 
  let rec aux i accu = 
    if i > last then accu else aux (i+1) (f i accu)
  in aux start x

let var = Integer.var

let input ?(size=1) name =
  DList (make_vector (fun x -> decl_input x) name size),
  var name size
    
let output ?(size=1) name =
  DList (make_vector (fun x -> decl_output x) name size),
  var name size

let reg ?(size=1) name =
  DList (make_vector (fun x -> decl_reg x) name size),
  var name size

let wire ?(size=1) name = 
  DList (make_vector (fun x -> decl_wire x) name size),
  var name size

let inp ?(a=1) = a

exception NonSynthesizable of (Boolean.t * Boolean.t)

let synthesize declarations constr =
  let types = of_declaration declarations in
  let bdd = ExprToBdd.expr_to_bdd constr in
  try
    AigerBdd.bdd_to_aiger (inputs types) (registers types) (outputs types) (wires types) bdd
  with (AigerBdd.Unsatisfiable (aig,bdd)) -> 
    let expr = Boolean.of_bdd bdd (List.rev_append (inputs types) (registers types)) in
    raise (NonSynthesizable (constr,expr))

module SymbolSet = AigerBdd.SymbolSet

let names_in_boolean_expr expr = 
  let rec aux accu = function 
    | Boolean.EVar name -> SymbolSet.add name accu 
    | Boolean.EExists (tl,e) 
    | Boolean.EForall(tl,e) -> (* TODO: Variables in tl should be removed *)
      List.fold_left
	(fun accu -> function 
	| Boolean.EVar name -> SymbolSet.remove name accu
	| _ -> failwith "In Speculog.names_in_boolean_expr: quantification over an expression that is not a variables"
	) (aux accu e) tl
    | Boolean.ENot t -> aux accu t
    | Boolean.EAnd tl | Boolean.EOr tl 
    | Boolean.EList tl -> List.fold_left aux accu tl
    | Boolean.True | Boolean.False -> accu
  in aux SymbolSet.empty expr

let names_in_expr expr = 
  let arr = Integer.to_boolean_array expr in
  Array.fold_left (fun accu i -> 
    SymbolSet.union accu (names_in_boolean_expr i)) SymbolSet.empty arr

let types_of_updates updates = 
  let read,write =
    List.fold_left 
      (fun (read,write) (var,expr) ->
	let to_read = names_in_expr expr in
	let to_write = names_in_expr var in
	SymbolSet.union to_read read, SymbolSet.union to_write write
      ) (SymbolSet.empty,SymbolSet.empty) updates     
  in
  let latches,inputs = 
    SymbolSet.fold 
      (fun s (l,i) -> 
       if SymbolSet.mem s write then SymbolSet.add s l,i else l,SymbolSet.add s i
      ) read (SymbolSet.empty,SymbolSet.empty)
  in
  let outputs = 
    SymbolSet.fold
      (fun s o -> 
       if SymbolSet.mem s read then o else SymbolSet.add s o
      )  write SymbolSet.empty
  in
  inputs, outputs, latches

let functional_synthesis updates = 
  let inputs, outputs, latches = types_of_updates updates in
  let latches_bdds, outputs_bdds = 
    List.fold_left
      (fun (lb, ob) (var, expr) -> 
	let ba_var = Integer.to_boolean_array var in
	let lb,ob,nb = 
	  Array.fold_left 
	    (fun (lb,ob,i) b_var ->
	      match b_var with 
	      | Boolean.EVar s -> 
		if SymbolSet.mem s latches
		then ((s, ExprToBdd.expr_to_bdd (Integer.get expr i))::lb,ob,i+1)
		else if SymbolSet.mem s outputs
		then (lb,(s, ExprToBdd.expr_to_bdd (Integer.get expr i))::ob,i+1)
		else failwith ("In Speculog.functional_synthesis: the variable "^s^" is neither a latch nor an output")
	      | _ -> failwith "In Speculog.functional_synthesis: the expression on the left should be a variable"
	    ) (lb,ob,0) ba_var 
	in lb,ob
      ) ([],[]) updates 
  in
  AigerBdd.bdds_to_aiger (SymbolSet.elements inputs) latches_bdds outputs_bdds 
    
let print_aiger a = Aiger.write a stdout
