type t = 
| ESimple of string
| EVar of string * int
| ESimpleNext of string
| ENext of string * int
| EExists of t list * t
| EForall of t list * t
| ENot of t
| EAnd of t list
| EOr of t list
| EList of t list
| True 
| False

val to_string : t -> string
val var : string -> int -> t
val next_var : string -> int -> t
val simple : string -> t
val next_simple : string -> t

val conj : t -> t -> t
val disj : t -> t -> t
val neg : t -> t 
val xor : t -> t -> t
val equals : t -> t -> t
val implies : t -> t -> t
val forall : t list -> t -> t
val exists : t list -> t -> t

exception AlreadyNext of t
val next : t -> t
    
val for_each : (int * int) list -> (int -> t) -> t
val of_bdd : Cudd.bdd -> AigerBdd.symbol list -> t
val of_list : t list -> t
val add_to_aiger : Aiger.t -> t -> (Aiger.t * Aiger.lit)


