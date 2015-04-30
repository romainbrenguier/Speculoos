open Speculog
open Expr
open SpeculogSyntax
let main = 
  output cost 2 in
Speculog.print_aiger (Speculog.add_synthesized _declarations (ite (dx = 0 || dx = 2) (cost = 1) (cost = 0)) (Aiger.read_from_file Sys.argv.(1)))
