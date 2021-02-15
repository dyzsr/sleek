type t = string list

let show = function
  | [] -> "{}"
  | h :: l -> "{" ^ List.fold_right (fun x acc -> x ^ ", " ^ acc) l h ^ "}"
;;

let empty = []

let make lst = lst

let ( |- ) a b =
  List.fold_left (fun res x -> res && List.exists (( = ) x) a) true b
;;

let () = assert ([ "A" ] |- [])

let () = assert ([ "A" ] |- [ "A" ])

let () = assert ([ "A"; "B" ] |- [ "A" ])
