(*
(** Load an bytecode object file. If this object files modify [current_env] we return its new value. *)
val load_module : string -> Environment.env  
*)

val parse_declarations : Environment.env -> string -> ParametricModule.declaration list

val parse_module : Environment.env -> string -> Aiger.t

val parse : string -> Aiger.t

