var position : Pos of int 7 | Neg of int 7 ;
var input : Left | Right ;
var value : int 2;

position <- 
  match input with
  | Left -> (match position with | Pos i -> if i > 0 then Pos (i - 1) else Neg 1 | Neg i -> Neg (i + 1))
  | Right -> (match position with | Pos i -> Pos (i + 1) | Neg i -> if i > 0 then Neg (i - 1) else Pos 1);
