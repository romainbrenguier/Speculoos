(** Verilog-like declarations *)
type declaration_type = Input | Output | Register | Wire

type declaration = { symbol : Symbol.t; typ : declaration_type}

(** Group of declarations *)
type declarations

(** Adds a declaration to a group *)
val add_declaration : declarations -> declaration -> declarations

(** Group a list of declarations *)
val declarations_of_list : declaration list -> declarations

(** The first component gives the expressions that are non synthesizable and
    raised the exception, the second one gives an expression describing the
    non synthesizable inputs. *)
exception NonSynthesizable of (Boolean.t * Boolean.t)

(** Synthesize from a constraint given as a Boolean expression. *)
val synthesize : declaration list -> Boolean.t -> Aiger.t

(** Synthesize from the definition of how each variable should be updated.
    The expression on the left should be an output or a register.
    Warning: if the expression on the right has size greater than the variable
    on the left, then its result is truncated.
*)
val functional_synthesis : (Integer.t * Integer.t) list -> Aiger.t
