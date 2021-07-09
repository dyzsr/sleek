type first
val show_first : first -> string

module Firsts : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val singleton : Instant.t -> t
  val from_list : first list -> t
  val union : t -> t -> t
  val zip : t -> t -> t
  val exists : (first -> bool) -> t -> bool
  val for_all : (first -> bool) -> t -> bool
end

val unify : first -> first -> bool * Ast.pi
val first : Ast.trace -> Firsts.t
val derivative : first -> Ast.trace -> Ast.trace
