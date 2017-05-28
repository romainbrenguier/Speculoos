open Tree

type base_type =
  | Number of float
  | String of string
  | Boolean of bool
  | Null

let print_base_type = function
  | Number f -> string_of_float f
  | String s -> s
  | Boolean true -> "true"
  | Boolean false -> "false"
  | Null -> "null"
      
class json =
object(self)
  inherit [base_type] tree

end

class basic_json basic =
object(self)

  inherit json
    
  initializer
    self#set_identifier "basic";
    self#set_value basic;

end

class json_number f = basic_json (Number f)
  
class json_string s = basic_json (String s)

class json_boolean b = basic_json (Boolean b)
  
class json_null = basic_json Null

class array_json array =
object(self)

  inherit json

  initializer
    self#set_identifier "array";
    for i = 0 to Array.length array - 1
    do
      self#add_indexed_member array.(i)
    done;

end

class class_json list =
object(self)

  inherit json

  initializer
    self#set_identifier "class";
    List.iter
      (fun (field_name, value) ->
	self#add_named_member field_name (value: json)
      ) list

end
