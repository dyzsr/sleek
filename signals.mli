(* Type of events *)
type event

val show_event : event -> string

val present : string -> event

val is_present : event -> bool

(* Type of signals *)
type t

val show : t -> string

val null : t

val is_null : t -> bool

val make : event list -> t

val ( |- ) : t -> t -> bool

(* Type of the set of signals *)
type set = t list

val empty : set

val from : t -> set

val union : set -> set -> set

val forall : (t -> bool) -> set -> bool

val zip : set -> set -> set
