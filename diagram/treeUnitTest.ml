open Key
open Tree

let main =
  let t = new tree in
  let v = new tree in
  v#set_value 1;
  Printf.printf "v = %d\n" v#get_value_exn;
  t#add_named_member (new key "x") v;
  let other_v = t#get_named_member_exn (new key "x") in
  print_endline "get v";
  Printf.printf "x = %d\n" other_v#get_value_exn;
  t#set_identifier "expr";
  t#add_indexed_member v;
  t#pretty_printf string_of_int 0
