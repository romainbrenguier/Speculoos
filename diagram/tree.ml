open Key
open Vector
open Dictionary

exception NoMemberNamed of string

class ['a] tree =
object(self)

  val mutable identifier = (None : key option)

  val mutable value = (None : 'a option)
    
  val named_members = new dictionary (None : 'a tree option)

  val indexed_members = new vector (None : 'a tree option)

  method get_value = value
    
  method get_value_exn =
    match value with
    | Some v -> v
    | None -> raise (NoMemberNamed "value")

  method set_value v = value <- Some v
    
  method get_identifier = identifier

  method set_identifier id = identifier <- Some (new key id)

  method get_named_member name =
    named_members#get name

  method get_named_member_exn name =
    match named_members#get name with
    | Some v -> v
    | None -> raise (NoMemberNamed name#to_string)

  method get_indexed_member index =
    indexed_members#get index

  method add_named_member name value =
    named_members#add_field name (Some value)
      
  (* raise Not found if it does not already exists *)
  method set_named_member name value =
    named_members#set name (Some value)

  method add_indexed_member value =
    indexed_members#push_back (Some value)
      
  method set_indexed_member index value =
    indexed_members#set index (Some value)

  method pretty_printf base_print indent =
    print_endline "super#printf";
    (match identifier with
    | Some x -> Message.printf_indent indent "identifier : %s\n" x#to_string
    | None -> ());
    (match value with
    | Some x -> Message.printf_indent indent "value : %s\n" (base_print x)
    | None -> ());
    named_members#iter
      (fun a -> function
      | Some b ->
	 Message.printf_indent indent "%s :\n" a#to_string;
	b#pretty_printf base_print (indent+2)
      | None -> ()
      );
    indexed_members#iteri
      (fun i -> function
      | Some b ->
	 Message.printf_indent indent "%d :\n" i;
	b#pretty_printf base_print (indent+2)
      | None -> ()
      );

end
