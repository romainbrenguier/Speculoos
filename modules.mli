(** [adder size] generates a module which takes [a] and [b] 
 * as input and put there sum in [out] which as size [size+1]. *)
val adder : int -> Aiger.t

(** generates a module wich takes [a] and [b] as input and 
 * put the bitwize conjunction in [out]. *)
val module_and : int -> Aiger.t

(** generates a module wich takes [a] and [b] as input and 
 * put the bitwize disjunction in [out]. *)
val module_or : int -> Aiger.t

(** generates a module wich takes [a] and [b] as input and 
 * put the bitwize xor in [out]. *)
val module_xor : int -> Aiger.t

(** [module_resize size_in size_out] generates a module that
 * takes an input [a] of size [size_in] and as an output [out]
 * of size [size_out] where all extra bits are put to 0 *)
val module_resize : int -> int -> Aiger.t

(** [module_counter size] generates a register of size [size]
 * called [counter] and
 * counting the number of times the input [target] is true *)
val module_counter : int -> Aiger.t

(** [module_assign size value] generates 
    output of size [size] encoding value [value] *)
val module_assign : int -> int -> Aiger.t

(** [module_equal size value] generates a module that takes an input [a]
    of size [size] and has an output [out] that is true if the input is
    equal to [value] *)
val module_equal : int -> int -> Aiger.t

(** generates a module wich takes [condition], [then] and [else] as input and 
 * puts [then] in [out] if [condition] is [true], [else] otherwise.
 * [then], [else] and [out] have size given by the argument. *)
val module_if : int -> Aiger.t


(** generates a module wich takes [case0],...,[caseN], 
    [then0],...,[thenN] where [N] is given by the argument [nb-1], and 
    puts [thenX] in [out] if [caseX] is [true].
    [thenX], [caseX] and [out] have size give by the second argument [size] *)
val module_switch : int -> int -> Aiger.t

(** generates a module wich takes [input0],...,[inputN], and [select] has input.
    Puts the value of the [i]-th input in the output [out] where [i] is the integer encoded by [select].
    The arguments are  [size_select] the size of the input select, [nb] the number of inputs, and [size] the sizes of the inputs.
*)
val module_multiplexer : int -> int -> int -> Aiger.t

(** Reads an aiger file *)
val module_open : string -> Aiger.t

(** contains parametric modules [adder], [and], [or], [xor], [resize], [counter], [assign], [equal], [if], [switch], [multiplexer] *)
val env : Environment.env





