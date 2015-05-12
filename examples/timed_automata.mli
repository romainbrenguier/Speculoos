(** Clocks. *)
type clock 

(** Clocks are declared by giving them a name and a size (number of bits). *)
val clock : string -> int -> clock

(** Clocks can be converted to an expression. *)
val clock_to_expr : clock -> Expression.t


(** Operators for comparison. *)
type operator = Less | LessEq | Eq | GreaterEq | Greater

(** Given a comparator and two expressions, generates the Boolean expressions 
in which the two expressions are compared. *)
val operator_to_expr : operator -> Expression.t -> Expression.t -> Expression.t

(** Guards are comparison between expressions *)
type guard = Expression.t * operator * Expression.t

(** The second argument is an integer expression giving the time that as been
 elapsed since the last location update. *)
val guard_to_expr : guard -> Expression.t -> Expression.t

(** Expression given by the conjunction of the guards. *)
val guards_to_expr : guard list -> Expression.t -> Expression.t


(** A location is an integer identifier and an invariant. *)
type location = {id : int; invariant : Expression.t }

(** Transitions have a source location, a destination location, a list of guards 
and a list of clocks to be reset to true. *)
type transition = {src : int; dst : int; guards : guard list; resets : clock list }

(** A timed automaton, is given by a list of clocks, of locations, of transitions 
and a maximal constant for the clocks. *)
type t =
  {
    clocks: clock list;
    locations: location list; 
    transitions: transition list;
    maxval : int
  }

(** Conversion from timed automata to AIGER. *)
val to_aiger : t -> Aiger.t

(** Write the generated AIGER to a file. *)
val write_aiger : t -> string -> unit
