
val equal : 'a Expr.t -> 'a Expr.t -> bool Expr.t
val ( === ) : 'a Expr.t -> 'a Expr.t -> bool Expr.t

(*val bitwise_and : 'a Expr.t -> 'a Expr.t -> Expr.t -> Speculog.Expr.T.t
val bitwise_or :  Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t
val bitwise_xor : Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t*)
(*
val bitwise_implication : Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t
*)

(* [add a b c] gives constraints so that c = a + b *)
val add : 'a Expr.t -> 'a Expr.t -> 'a Expr.t -> bool Expr.t

(*
(* [mod a b c] gives constraints so that a mod b = c *)
val modulo : Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t

(* [mult a b c] gives constraints so that a * b = c *)
val mult : Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t

(* [div a b c] gives constraints so that a / b = c *)
val div : Expr.t -> Expr.t -> Expr.t -> Speculog.Expr.T.t

(* [less a b] a is less than b (i.e. a < b)*)
val less : Expr.t -> Expr.t -> Speculog.Expr.T.t
val greater : Expr.t -> Expr.t -> Speculog.Expr.T.t

(* selects the b-th bit of a *)
val bit_selection : Expr.t -> Expr.t -> Speculog.Expr.T.t
*)




(* [ite i t e] Expr.tesses that if [i] is holds then [t] holds and otherwise [e] holds *)
val ite : bool Expr.t -> 'a Expr.t -> 'a Expr.t -> 'a Expr.t

(** Usefull functions to know the size necessary for an interger *)
val exp : int -> int
val log : int -> int
