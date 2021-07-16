module IntSet : Set.S with type elt = int

val next_id : unit -> int

val fixpoint : f:('a -> 'a) -> ?iter:('a -> unit) -> ?stop:('a -> unit) -> 'a -> 'a

(* options *)
val opt_value : 'a option -> 'a
val opt_iter : f:('a -> unit) -> 'a option -> unit
val opt_none : 'a option -> bool
val opt_some : 'a option -> bool
val opt_map : f:('a -> 'b) -> 'a option -> 'b option
val opt_map2 :
  ?a:('a -> 'a) -> ?b:('a -> 'a) -> ab:('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option

(* lists *)
val last : 'a list -> 'a
val combinations : 'a list list -> 'a list list
val zip : 'a list list -> 'a list list

(* roman numbers *)
val roman : int -> string
val case_no : int -> int -> string

module Test : sig
  val test_combinations : unit -> unit
  val test_case_no : unit -> unit
  val test : unit -> unit
end
