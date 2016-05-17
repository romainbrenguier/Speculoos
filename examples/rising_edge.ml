(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/rising_edge.native --*)
open Expression

let x = var "x" Type.bool 
let previous = var "previous" Type.bool 
let _ = compile [var "rising_edge" Type.bool, x $& neg previous; previous $<- x]

