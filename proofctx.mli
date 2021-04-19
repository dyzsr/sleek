type t

val make : unit -> t

val add_entail : Ast.simple_entailment -> t -> t

val exists_entail : Ast.simple_entailment -> t -> bool

val new_term : t -> Ast.term

type fn_add_imply = pre:Ast.pi -> ?post:Ast.pi -> t -> unit

val add_l_imply : fn_add_imply

val add_r_imply : fn_add_imply

val check_implies : t -> bool
