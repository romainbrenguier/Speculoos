type 'a t = 'a Expr.t

val ( ! ) : 'a t -> 'a t
val ( & ) : 'a t -> 'a t -> 'a t
val ( || ) : 'a t -> 'a t -> 'a t
val ( ^ ) : 'a t -> 'a t -> 'a t
val ( = ) : 'a t -> 'a t -> bool t
val ( <-> ) : 'a t -> 'a t -> 'a t
val ( --> ) : 'a t -> 'a t -> 'a t
val ( + ) : 'a t -> 'a t -> 'a t
val ( - ) : 'a t -> 'a t -> 'a t
val ( * ) : 'a t -> 'a t -> 'a t
val ( / ) : 'a t -> 'a t -> 'a t
val ( mod ): 'a t -> 'a t -> 'a t
val ( << ) : 'a t -> 'a t -> 'a t
val ( >> ) : 'a t -> 'a t -> 'a t
val ( <= ) : 'a t -> 'a t -> bool t
val ( < ) : 'a t -> 'a t -> bool t
val ( > ) : 'a t -> 'a t -> bool t
val ( >= ) : 'a t -> 'a t -> bool t

val next : 'a t -> 'a t -> 'a t

