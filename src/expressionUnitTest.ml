open Expression
  
let test () = 
  let union_type = Type.Union ["Int",Type.Int 2;"Bool",Type.Bool] in
  let typ = Type.of_string "{i: Int of int 2 | Bool of bool; a : int 3} [4]" in
  print_endline (Type.to_string typ);
  let e = var "test" typ in
  let f = field (get e (int 3)) "i" in
  let m = match_with union_type f ["Int",(fun i -> int 0);"Bool",neg] in
  print_endline (to_string m)
  
let main = 
  test ()
