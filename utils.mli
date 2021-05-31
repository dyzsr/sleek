val fixpoint : f:('a -> 'a) -> ?fn_iter:('a -> unit) -> ?fn_stop:('a -> unit) -> 'a -> 'a

val opt_iter : f:('a -> unit) -> 'a option -> unit
val opt_map : f:('a -> 'b) -> 'a option -> 'b option
val opt_map2 :
  ?a:('a -> 'a) -> ?b:('a -> 'a) -> ab:('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
