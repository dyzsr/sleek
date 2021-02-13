type t = string list [@@deriving show]

let empty = []

let make lst = lst

let ( |- ) a b =
  List.fold_left (fun res x -> res && List.exists (( = ) x) a) true b
