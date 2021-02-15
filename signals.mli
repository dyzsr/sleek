type t

val show : t -> string

val empty : t

val make : string list -> t

val ( |- ) : t -> t -> bool
