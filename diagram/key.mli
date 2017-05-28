(* String like object with constant time comparison *)

class key :
  string ->
object
  method compare : key -> int
  method equals_string : string -> bool
  method to_string : string
end
