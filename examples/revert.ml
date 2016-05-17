(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/revert.native --*)
open Expression

let x = var "x" (Type.int 8)
let y = var "y" (Type.int 8)
let _ = compile [y, select x [3,0;7,4]]
