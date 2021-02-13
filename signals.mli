type t [@@deriving show]

val empty : t

val make : string list -> t

val ( |- ) : t -> t -> bool
