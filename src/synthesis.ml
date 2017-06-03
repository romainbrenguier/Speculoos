type declaration_type = Input | Output | Register | Wire

(* TODO: AigerBdd.symbol should have its own module *)
type declaration = { symbol:Symbol.t; typ:declaration_type}

let declare typ s = { symbol=s; typ}
let declare_input = declare Input
let declare_output = declare Output
let declare_register = declare Register
let declare_wire = declare Wire

type declarations =
  {
    inputs: AigerBdd.SymbolSet.t;
    outputs: AigerBdd.SymbolSet.t;
    registers: AigerBdd.SymbolSet.t;
    wires: AigerBdd.SymbolSet.t;
  }

let rec add_declaration declarations decl = match decl.typ with
  | Input ->
     { declarations
       with inputs=AigerBdd.SymbolSet.add decl.symbol declarations.inputs}
  | Output ->
     { declarations
       with outputs=AigerBdd.SymbolSet.add decl.symbol declarations.outputs}
  | Register ->
     { declarations
       with registers=AigerBdd.SymbolSet.add decl.symbol declarations.registers}
  | Wire ->
     { declarations
       with wires=AigerBdd.SymbolSet.add decl.symbol declarations.wires}

let empty_declarations =
  let empty = AigerBdd.SymbolSet.empty in
  { inputs=empty; outputs=empty; registers=empty; wires=empty}

let declarations_of_list dl =
  List.fold_left add_declaration empty_declarations dl

let inputs declarations=
  AigerBdd.SymbolSet.elements declarations.inputs

let outputs declarations=
  AigerBdd.SymbolSet.elements declarations.outputs

let registers declarations=
  AigerBdd.SymbolSet.elements declarations.registers

let wires declarations=
  AigerBdd.SymbolSet.elements declarations.wires

exception NonSynthesizable of (Boolean.t * Boolean.t)

let synthesize declarations constr =
  let decl = declarations_of_list declarations in
  let bdd = ExprToBdd.expr_to_bdd constr in
  try
    AigerBdd.bdd_to_aiger (inputs decl) (registers decl) (outputs decl) (wires decl) bdd
  with (AigerBdd.Unsatisfiable (aig, bdd)) ->
    let input_reg_list =
      AigerBdd.SymbolSet.elements (AigerBdd.SymbolSet.union decl.inputs decl.registers)
    in
    let expr = Boolean.of_bdd bdd input_reg_list in
    raise (NonSynthesizable (constr, expr))

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
                else
		  failwith ("In Speculog.functional_synthesis: the variable "^
			       Symbol.to_string s^" is neither a latch nor an output")
              | _ ->
		 failwith ("In Speculog.functional_synthesis: the expression on the "^
			      "left should be a variable")
            ) (lb,ob,0) ba_var
        in lb,ob
      ) ([],[]) updates
  in
  AigerBdd.bdds_to_aiger (SymbolSet.elements inputs) latches_bdds outputs_bdds
