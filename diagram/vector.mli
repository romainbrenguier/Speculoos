(* Constructor for vector takes a default value as argument *)
class ['a] vector :
  'a ->
object
  (* Dimension of the vector *)
  method size : int
    
  (* Element at the front *)
  method front : 'a

  (* Element at the back *)
  method back : 'a
    
  (* Access the element at the given index *)
  method get : int -> 'a

  (* Remove an element at the front and returns its value *)
  method pop_front : 'a

  (* Remove an element at the back and returns its value *)
  method pop_back : 'a

  (* Add an element at the front *)
  method push_front : 'a -> unit
    
  (* Add an element at the back *)
  method push_back : 'a -> unit

  (* Set the value of the element at the given index *)
  method set : int -> 'a -> unit
    
  (* Applies a function to all elements in order *)
  method iter : ('a -> unit) -> unit

  (* Applies a function to all index and elements in order *)
  method iteri : (int -> 'a -> unit) -> unit
    
end
