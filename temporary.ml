open Speculog
open Expression
let main =
  let aiger =
    import_module (Aiger.read_from_file "add_10.aag")
      [ ("a", "a[0][0]"); ("b", "b[0][0]"); ("c", "c[0][0]") ]
      (function
       | [ a; b; c ] ->
           let err = var "err" Type.Bool
           in functional_synthesis [ (err, (neg (equals c (int 0)))) ])
  in Aiger.write aiger stdout
  

