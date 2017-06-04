type t =
  | VSymbol of Symbol.t
  | VNext of Symbol.t

val compare : t -> t -> int
val next : t -> t
val to_int : t -> int
val of_int : int -> t
val of_string : string -> t
val to_bdd : t -> Cudd.bdd
val make_cube : Symbol.t list -> Cudd.cube
val of_lit_exn : AigerImperative.t -> AigerImperative.lit -> t
val max_var : unit -> int
val rename_configuration : Cudd.bdd -> t array -> t array -> Cudd.bdd

(** For debugging *)
val to_string : t -> string
