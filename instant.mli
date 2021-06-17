(* Type of events *)
type event

val show_event : event -> string

val compare_event : event -> event -> bool

val present : string -> event

val absent : string -> event

val is_present : event -> bool

(* Type of instant *)
type t

val isEventExist : event -> t -> bool

val show : t -> string

val empty : t

val is_empty : t -> bool

val singleton : string -> t

val make : event list -> t

val initUndef : string list -> t

val setAbsent : string -> t -> t option

val setPresent : string -> t -> t option

val controdicts : t -> bool

val add_UndefSigs : string list -> t -> t

val merge : t -> t -> t

val ( |- ) : t -> t -> bool
