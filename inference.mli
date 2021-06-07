module Set : sig
  include module type of List
  type elm
  val show_elm : elm -> string

  type t = elm list
  val empty : t
  val is_empty : t -> bool
  val from : ?t:Ast.term -> ?p:Ast.term -> Signals.t -> t
  val union : t -> t -> t
  val zip : Proofctx.t -> t -> t -> t
end

val is_bot : Ast.instants -> bool

val nullable : Ast.instants -> bool

val first : Proofctx.t -> Ast.instants -> Set.t

val partial_deriv : Proofctx.t -> Set.elm -> Ast.instants -> Ast.instants
