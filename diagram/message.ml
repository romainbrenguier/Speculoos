
let indent i =
  String.make i ' '

let printf_indent i s =
  print_string (indent i);
  Printf.printf s
