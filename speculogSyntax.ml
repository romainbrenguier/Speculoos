open Expr

type 'a t = 'a Expr.t

let ( ! ) a = not a
let ( & ) a b = a && b
let ( || ) a b = a || b
let ( ^ ) a b = a ^^ b
let ( = ) a b = a == b
let ( <-> ) a b = equiv a b
let ( --> ) a b = implies a b
let ( + ) a b = a ++ b
let ( - ) a b = a -- b
let ( * ) a b = a ** b
let ( / ) a b = div a b
let ( mod ) a b = modulo a b
let next = next
let ( << ) a b = left_shift a b
let ( >> ) a b = right_shift a b
let ( <= ) a b = less_eq a b
let ( < ) a b = less a b
let ( > ) a b = greater a b
let ( >= ) a b = greater_eq a b

