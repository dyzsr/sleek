type t = string list

let show = function
  | [] -> "{}"
  | h :: l -> "{" ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) h l ^ "}"
;;

let null = []

let make lst = List.sort_uniq compare lst

let combine a b = List.sort_uniq compare (a @ b)

let ( |- ) a b =
  b |> List.fold_left (fun res x -> res && a |> List.exists (( = ) x)) true
;;

type set = t list

let empty = []

let from x = [ x ]

let union a b = a @ b

let forall f s =
   List.fold_left (fun acc x -> acc && f x) true s

let zip a b =
  a
  |> List.fold_left
       (fun acc x -> acc @ (b |> List.map (fun y -> combine x y)))
       empty
;;

(* tests *)
let () =
  assert ([] |- []);
  assert ([ "A" ] |- []);
  assert ([ "A" ] |- [ "A" ]);
  assert ([ "A"; "B" ] |- [ "A" ]);
  ()
;;
