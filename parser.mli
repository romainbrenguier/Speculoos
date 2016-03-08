
(* The first list contains the initial condition and the second one the updates *)
val parse : char Stream.t -> (Expression.t * Expression.t) list * (Expression.t * Expression.t) list

val parse_inch : in_channel -> (Expression.t * Expression.t) list * (Expression.t * Expression.t) list
