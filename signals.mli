(* Type of events *)
type event

val show_event : event -> string

val compare_event: event -> event -> bool



val present : string -> event

val is_present : event -> bool

(* Type of signals *)
type t

val isSigOne: event -> t -> bool

val show : t -> string

val empty : t

val is_empty : t -> bool

val from : string -> t

val make : event list -> t

val merge : t -> t -> t

val ( |- ) : t -> t -> bool
