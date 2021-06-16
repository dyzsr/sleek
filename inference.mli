module Set : sig
  type elm
  val show_elm : elm -> string

  type t
  val empty : t
  val is_empty : t -> bool
  val for_all : (elm -> bool) -> t -> bool
end

val is_bot : Ast.instants -> bool

val nullable : Ast.instants -> bool

val first : Proofctx.t -> Ast.instants -> Set.t

val partial_deriv : Proofctx.t -> Set.elm -> Ast.instants -> Ast.instants
