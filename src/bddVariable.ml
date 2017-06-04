type t =
  | VSymbol of Symbol.t
  | VNext of Symbol.t

let symbol s = VSymbol s
  
let symbol_next s = VNext s
    
let compare a b = match a, b with
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
  let sym = Symbol.with_id (v / 2) in
  if v mod 2 = 0
  then symbol sym
  else symbol_next sym

let to_string v = match v with
  | VSymbol a ->
     Printf.sprintf "Variable(%s, %d)" (Symbol.to_string a) (to_int v)
  | VNext a ->
     Printf.sprintf "VariableNext(%s, %d)" (Symbol.to_string a) (to_int v)
      
let of_string l =
  symbol (Symbol.of_string l)

let to_bdd var = Cudd.ithVar (to_int var)

let make_cube var_list =
  var_list |> List.map to_int |> Cudd.make_cube

let of_lit_exn aiger lit = 
  of_string (AigerImperative.lit2string_exn aiger lit)

let rename_configuration bdd variables next_variables =
  Cudd.bddSwapVariables
    bdd (Array.map to_int variables) (Array.map to_int next_variables)
