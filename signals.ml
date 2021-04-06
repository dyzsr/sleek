type event = Present of string

let show_event = function
  | Present name -> name


let present name = Present name

let is_present = function
  | Present _ -> true


(* Type of signals *)
type t = event list

let show = function
  | [] -> "{}"
  | l  -> "{" ^ String.concat ", " (List.map show_event l) ^ "}"


(* Empty signal *)
let empty = []

let is_empty = function
  | [] -> true
  | _  -> false


let from name = [ present name ]

(* Make new signal from name list *)
let make lst = List.sort_uniq compare lst

(* Merge signals `a` and `b` into a new one *)
let merge a b = List.sort_uniq compare (a @ b)

(* Is `b` included in `a`? *)
let ( |- ) a b = b |> List.fold_left (fun res y -> res && a |> List.exists (( = ) y)) true

(* tests *)
let () =
  assert ([] |- []);
  assert ([ present "A" ] |- []);
  assert ([ present "A" ] |- [ present "A" ]);
  assert ([ present "A"; present "B" ] |- [ present "A" ])
