(** Symbols are a representation of strings for which comparison is constant
    time instead of depending on the length of the string. *)
type t

(** Unique identifier *)
val id : t -> int

(** Retrieve symbol from its id *)
val with_id : int -> t

(** String represented by the symbol *)
val to_string : t -> string

(** Symbol associated to the string *)
val of_string : string -> t

(** Comparison *)
val compare : t -> t -> int

(** Create a new unused symbol starting with the given prefix *)
val new_symbol : string -> t
