val adder : int -> Aiger.t
val product : int -> Aiger.t
val complex_product : int -> Aiger.t
val assign : size:int -> value:int -> output:string -> Aiger.t
val equal : size:int -> value:int -> input:string -> output:string -> Aiger.t
val ite : size:int -> condition:string -> t:string -> e:string -> output:string -> Aiger.t
