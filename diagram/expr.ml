open Key
open Vector

type t =
| ExprTrue
| ExprFalse
| ExprSymbol of key
| ExprNot of t
| ExprConjunction of t vector
| ExprDisjunction of t vector
| ExprExists of key vector * t
| ExprForall of key vector * t

let rec expr_to_string = function
  | ExprTrue -> "true"
  | ExprFalse -> "false"
  | ExprSymbol s -> s#to_string

  | ExprExists (q, e) ->
     let quantifiers = q#to_string (fun k -> k#to_string) ", " in
     Printf.sprintf "(exists [%s]. %s)" quantifiers (expr_to_string e)

  | ExprForall (q,e) ->
     let quantifiers = q#to_string (fun k -> k#to_string) ", " in
     Printf.sprintf "(forall [%s]. %s)" quantifiers (expr_to_string e)

  | ExprNot a ->
     Printf.sprintf "(not %s)" (expr_to_string a)

  | ExprConjunction v ->
     "(" ^ v#to_string expr_to_string " && " ^ ")"

  | ExprDisjunction v ->
     "(" ^ v#to_string expr_to_string " || " ^ ")"
