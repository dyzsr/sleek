open Ast

module Firsts : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val singleton : Instant.t -> t
  val of_list : first list -> t
  val to_list : t -> first list
  val union : t -> t -> t
  val zip : t -> t -> t
  val remove_null : t -> t
  val exists : (first -> bool) -> t -> bool
  val for_all : (first -> bool) -> t -> bool

  module Test : sig
    val test_zip : unit -> unit
    val test : unit -> unit
  end
end

val unify : track -> track -> bool * pi
val first : trace -> Firsts.t
val derivative : first -> trace -> trace
