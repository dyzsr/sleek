module Set : sig
  type elm
  val show_elm : elm -> string

  type t
  val empty : t
  val is_empty : t -> bool
  val for_all : (elm -> bool) -> t -> bool
end

val is_bot : Ast.trace -> bool

val nullable : Ast.trace -> bool

val first : Proofctx.t -> Ast.trace -> Set.t

val partial_deriv : Proofctx.t -> Set.elm -> Ast.trace -> Ast.trace
