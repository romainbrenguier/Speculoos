(* 
   Module: Key 

   Author: Romain Brenguier
*)

(* Each string is associated a unique id *)
let id_of_string_table = Hashtbl.create 100
  
let max_used_id = ref 0
  
let id_of_string string =
  try
    Hashtbl.find id_of_string_table string
  with Not_found ->
    incr max_used_id;
    Hashtbl.add id_of_string_table string !max_used_id;
    !max_used_id
  
class key (string:string) =
object(self)
  
  val as_string = string

  val id = id_of_string string

  method get_id = id
    
  method compare (b:key) = compare self#get_id b#get_id

  method to_string = as_string

  method equals_string s = id = id_of_string s 
    
end
