type first
val show_first : first -> string

module First_set : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val exists : (first -> bool) -> t -> bool
  val for_all : (first -> bool) -> t -> bool
end

val unify : first -> first -> bool * Ast.pi
val first : Ast.trace -> First_set.t
val derivative : first -> Ast.trace -> Ast.trace
