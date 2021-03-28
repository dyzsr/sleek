module Set : sig
  include module type of List

  type t = (Signals.t * Ast.pi * Ast.term) list

  val empty : t

  val from : Signals.t -> Ast.pi -> Ast.term -> t

  val union : t -> t -> t

  val zip : Proofctx.t -> t -> t -> t
end

val normalize_pi : Ast.pi -> Ast.pi

val normalize_es : Ast.instants -> Ast.instants

val normalize : Ast.effects -> Ast.effects

val nullable : Ast.instants -> bool

val first : Proofctx.t -> Ast.pi -> Ast.instants -> Set.t

val partial_deriv : Proofctx.t -> Signals.t * Ast.pi * Ast.term -> Ast.effects -> Ast.effects
