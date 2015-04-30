open Speculog
  
open Expr
  
let _ =
  print_aiger
    (let (_decl_1, out) = output "out" 2 in
     let _declarations = _decl_1 :: _declarations in
     let (_decl_1, inp) = input "inp" 2 in
     let _declarations = _decl_1 :: _declarations in
     let (_decl_1, r) = reg "r" 2 in
     let _declarations = _decl_1 :: _declarations
     in
       Cudd.init 10;
       synthesize _declarations
         [ out.(0) == r.(0); out.(1) == (not r.(1)); (next r.(0)) == r.(0);
           (next r.(1)) == (not r.(0)) ])
  

