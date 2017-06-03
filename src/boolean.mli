(** Module to represent Boolean expressions *)
type t = 
  | EVar of Symbol.t
  | EExists of t list * t
  | EForall of t list * t
  | ENot of t
  | EAnd of t list
  | EOr of t list
  | EList of t list
  | True 
  | False

val to_string : t -> string
val var : Symbol.t -> t

val conj : t -> t -> t
val disj : t -> t -> t
val neg : t -> t 
val xor : t -> t -> t
val equals : t -> t -> t
val implies : t -> t -> t
val forall : t list -> t -> t
val exists : t list -> t -> t

val for_each : (int * int) list -> (int -> t) -> t
val of_bdd : Cudd.bdd -> Symbol.t list -> t
val of_list : t list -> t
val add_to_aiger : AigerImperative.t -> t -> AigerImperative.lit
val to_bdd : t -> Cudd.bdd

