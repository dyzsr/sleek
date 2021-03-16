type event = Present of string

let show_event = function
  | Present name -> name
;;

let present name = Present name

let is_present = function
  | Present _ -> true
;;

(* Type of signals *)
type t = event list

let show = function
  | [] -> "{}"
  | l  -> "{" ^ String.concat ", " (List.map show_event l) ^ "}"
;;

(* Null signal *)
let null = []

let is_null = function
  | [] -> true
  | _  -> false
;;

(* Make new signal from name list *)
let make lst = List.sort_uniq compare lst

(* Merge signals `a` and `b` into a new one *)
let merge a b = List.sort_uniq compare (a @ b)

(* Is `b` included in `a`? *)
let ( |- ) a b = b |> List.fold_left (fun res y -> res && a |> List.exists (( = ) y)) true

(* Set of signals *)
type set = t list

(* Empty set *)
let empty = []

(* Make a set from single signal *)
let from x = [ x ]

let union a b = a @ b

let forall f s = List.fold_left (fun acc x -> acc && f x) true s

let zip a b =
  a
  |> List.fold_left (fun acc x -> acc @ (b |> List.map (fun y -> merge x y))) empty
  |> List.sort_uniq compare
;;

(* tests *)
let () =
  assert ([] |- []);
  assert ([ present "A" ] |- []);
  assert ([ present "A" ] |- [ present "A" ]);
  assert ([ present "A"; present "B" ] |- [ present "A" ]);
  assert (
    zip [ make [ present "A" ] ] [ make [ present "B" ] ] = [ make [ present "A"; present "B" ] ]);
  assert (
    zip [ make []; make [ present "A" ] ] [ make []; make [ present "B" ] ]
    = [ make []; make [ present "A" ]; make [ present "A"; present "B" ]; make [ present "B" ] ]);
  ()
;;
