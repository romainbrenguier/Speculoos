(** Variables in BDDs represent either a symbol or the next valuation
    associated to symbol. Their indexes in the BDD are close so that
    substitution is effecient. *)

type t
val symbol : Symbol.t -> t
val symbol_next : Symbol.t -> t
  
val compare : t -> t -> int
val next : t -> t
val to_int : t -> int
val of_int : int -> t
val of_string : string -> t
val to_bdd : t -> Cudd.bdd
val make_cube : t list -> Cudd.cube
val of_lit_exn : AigerImperative.t -> AigerImperative.lit -> t
val rename_configuration : Cudd.bdd -> t array -> t array -> Cudd.bdd

(** For debugging *)
val to_string : t -> string
