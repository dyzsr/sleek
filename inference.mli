module Set : sig
  type first
  val show_first : first -> string

  type t
  val empty : t
  val is_empty : t -> bool
  val for_all : (first -> bool) -> t -> bool
end

val first : Proofctx.t -> Ast.trace -> Set.t

val partial_deriv : Proofctx.t -> Set.first -> Ast.trace -> Ast.trace
