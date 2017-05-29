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

  (* Append a vector at the back *)
  method append : 'a vector -> unit

  (* Set the value of the element at the given index *)
  method set : int -> 'a -> unit
    
  (* Applies a function to all elements in order *)
  method iter : ('a -> unit) -> unit

  (* Applies a function to all index and elements in order *)
  method iteri : (int -> 'a -> unit) -> unit

  (* Converts to string using the given function and adding separators *)
  method to_string : ('a -> string) -> string -> string

  (* Tells whether the vector is empty *)
  method empty : bool

  (* Applies a function to elements in succession: f (f (f a1 a2) a3) a4 ... *)
  method reduce : ('a -> 'a -> 'a) -> 'a
    
end

(* Applies a function to each element *)
val map : ('a -> 'b) -> 'a vector -> 'b vector
    
(* Converts to a list applying the given function to each element *)
val map_to_list : ('a -> 'b) -> 'a vector -> 'b list

