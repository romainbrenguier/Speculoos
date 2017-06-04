(** This module defines usefull function for going from the aiger representation
 * to the BDD representation and reciproqually *)

(** Initialize CUDD.
    The number of variables to be used is inferred from the
    circuit that is provided. *)
val init : Aiger.t -> unit

module SymbolMap : Map.S with type key = Symbol.t

module SymbolSet : Set.S with type elt = Symbol.t

(** Bdd associated to a symbol *)
val bdd_of_symbol : Symbol.t -> Cudd.bdd

(** Map aiger literals to symbols. *)
val map_of_aiger : Aiger.t -> Aiger.lit SymbolMap.t

(** Display the map as a string. Used for debugging. *)
val map_to_string : Aiger.lit SymbolMap.t -> string

(** Raised by [bdd_to_aiger] if one of the literals in the BDD is not already
    in the given aiger *)
exception UndeclaredLit of Aiger.lit

(** Add to the aiger file, gates to compute the result of the
    evaluation of the BDD. The result contains the new aiger
    file and the literal that points to the result.
    All the variables of the BDD should correspond to literals of the given aiger
    file, or be inside the given SymbolMap.
    otherwise a [UndeclaredLit] exception will be raised.
*)
val add_bdd_to_aiger :
  Aiger.t -> Aiger.lit SymbolMap.t -> Cudd.bdd -> (Aiger.t * Aiger.lit)

exception Unsatisfiable of (Aiger.t * Cudd.bdd)

(** Takes as argument the list of inputs, list of latches and list of outputs
    of the circuit to produce *)
val bdd_to_aiger :
  inputs:Symbol.t list ->
  latches:Symbol.t list ->
  outputs:Symbol.t list -> wires:Symbol.t list -> Cudd.bdd -> Aiger.t

(** the Bdd tell how to update the different latches *)
val bdds_to_aiger :
  inputs:Symbol.t list ->
  latches:(Symbol.t * Cudd.bdd) list ->
  outputs:(Symbol.t * Cudd.bdd) list -> Aiger.t

(** Makes a bdd cube from a list of symbols *)
val make_cube : Symbol.t list -> Cudd.cube

val valuation_of_list : (Symbol.t * bool) list -> bool SymbolMap.t

(** Convert the valuation to a BDD representation *)
val bdd_of_valuation : bool SymbolMap.t -> Cudd.bdd

val bdd_to_valuations : Cudd.bdd -> Symbol.t list -> (bool SymbolMap.t) list

(** Fixpoint of an operation on BDD's *)
val fixpoint : (Cudd.bdd -> Cudd.bdd) -> Cudd.bdd -> Cudd.bdd

(** Reorder the gates of an aiger file so that the index on the left of a gate
    is always greater than those on the right *)
val reorder_aiger : Aiger.t -> Aiger.t
