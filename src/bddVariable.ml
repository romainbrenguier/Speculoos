type t =
  | VSymbol of Symbol.t
  | VNext of Symbol.t
      
let compare a b = match a, b
  | VSymbol c, VSymbol d | VNext c, VNext d ->
     Symbol.compare c d
  | VSymbol _, VNext _ -> 1
  | VNext _, VSymbol _ -> -1

let next = function
  | VSymbol a -> VNext a
  | VNext _ -> failwith "variables is already next"
    
let to_int = function
  | VSymbol a -> 2 * Symbol.id a
  | VNext a -> 2 * Symbol.id a + 1

let of_int v =
  let id = Symbol.id v in
  if v mod 2 = 0
  then VSymbol (v / 2)
  else VNext ((v-1) / 2)

let to_string v = function
  | VSymbol a ->
     Printf.sprintf "Variable(%s, %d)" (Symbol.to_string a) (to_int v)
  | VNext a ->
     Printf.sprintf "VariableNext(%s, %d)" (Symbol.to_string a) (to_int v)
      
let of_string l =
  VSymbol (Symbol.of_string l)

let to_bdd var = Cudd.ithVar (to_int var)

let make_cube var_list = Cudd.make_cube var_list

let of_lit_exn aiger lit = 
  of_string (AigerImperative.lit2string_exn aiger lit)

let rename_configuration bdd variables next_variables =
  Cudd.bddSwapVariables bdd variables next_variables
