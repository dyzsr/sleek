type t

val show : t -> string

val null : t

val is_null : t -> bool

val make : string list -> t

val ( |- ) : t -> t -> bool

val subtract : t -> t -> t

type set = t list

val empty : set

val from : t -> set

val union : set -> set -> set

val forall : (t -> bool) -> set -> bool

val zip : set -> set -> set
