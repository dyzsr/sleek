type 'a t

val empty : 'a t

val add : 'a * 'a -> 'a t -> 'a t

val exists : 'a * 'a -> 'a t -> bool
