(* A symbol is a integer representing a string *)
type t = int

(* Integers smaller than this value are exactly the ones that are
   used for symbols. This is used to create new, unused, symbols. *)
let last_created_symbol = ref 0

(* We use tables to retrieve the integer associated with a string *)
let symbol_table:(string, t) Hashtbl.t = Hashtbl.create 100

(* We use another one for the other direction *)
let reverse_table:(t, string) Hashtbl.t = Hashtbl.create 100

let id (s:t) = (s:int)

let with_id (s:int) = (s:t)

let to_string s = Hashtbl.find reverse_table s

let of_string s =
  try
    Hashtbl.find symbol_table s
  with Not_found ->
    incr last_created_symbol;
    let new_symbol = !last_created_symbol in
    Hashtbl.add symbol_table s new_symbol;
    Hashtbl.add reverse_table new_symbol s;
    new_symbol

let compare (a:t) (b:t) = a - b

let rec new_symbol prefix =
  if Hashtbl.mem symbol_table prefix
  then
    (* Since the prefix is already present in the table we add
       a number at the end and increment it until we find a string that is
       not already in use. *)
    let i = ref 0 in
    while Hashtbl.mem symbol_table (prefix^"#"^string_of_int !i)
    do
      incr i
    done;
    new_symbol (prefix^"#"^string_of_int !i)
  else
    of_string prefix
