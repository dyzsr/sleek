module Set : sig
  include module type of List

  type elem = Signals.t * Ast.pi * Ast.term option

  type t = elem list

  val empty : t

  val is_empty : t -> bool

  val from : Signals.t -> Ast.pi -> Ast.term option -> t

  val union : t -> t -> t

  val zip : Proofctx.t -> t -> t -> t
end

val nullable : Ast.instants -> bool

val first : Proofctx.t -> Ast.pi -> Ast.instants -> Set.t

val partial_deriv :
  Proofctx.t -> Proofctx.fn_add_imply -> Set.elem -> Ast.simple_effects -> Ast.simple_effects
