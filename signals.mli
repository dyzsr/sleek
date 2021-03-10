(* Type of events *)
type event

val present : string -> event

val waiting : string -> event

(* Type of signals *)
type t

val show : t -> string

val null : t

val is_null : t -> bool

val make : event list -> t

val ( |- ) : t -> t -> bool

val split : t -> t * t

(* Type of the set of signals *)
type set = t list

val empty : set

val from : t -> set

val union : set -> set -> set

val forall : (t -> bool) -> set -> bool

val zip : set -> set -> set
