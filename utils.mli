val fixpoint : f:('a -> 'a) -> ?iter:('a -> unit) -> ?stop:('a -> unit) -> 'a -> 'a

val opt_value : 'a option -> 'a
val opt_iter : f:('a -> unit) -> 'a option -> unit
val opt_none : 'a option -> bool
val opt_some : 'a option -> bool
val opt_map : f:('a -> 'b) -> 'a option -> 'b option
val opt_map2 :
  ?a:('a -> 'a) -> ?b:('a -> 'a) -> ab:('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option

val combinations : 'a list list -> 'a list list
