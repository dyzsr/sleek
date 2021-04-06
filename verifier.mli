type history

val show_history : history -> verbose:bool -> string

val show_verification :
  case:Ast.specification -> no:int -> verdict:string -> verbose:bool -> history:history -> string

val verify_entailment : Ast.entailment -> bool * history

val verify_specification : Ast.specification -> bool * string * history
