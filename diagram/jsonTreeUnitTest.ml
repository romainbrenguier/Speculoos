open Key
open JsonTree

let main =
  let b = new basic_json (String "hello") in
  b#pretty_printf print_base_type 0;
  let a = new array_json [| new basic_json (Number 1.0) |] in
  a#pretty_printf print_base_type 0;
  let c = new class_json [new key "a", a; new key "b", b ] in
  c#pretty_printf print_base_type 0
