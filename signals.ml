(* is the event waiting? *)
let waiting e =
  let n = String.length e in
  if n < 2 then false else e.[n - 1] = '?'
;;

(* is the event present? *)
let present e = not (waiting e)

(* get the present event from a waiting event *)
let get_present e = String.sub e 0 (String.length e - 1)

(* Type of signals *)
type t = string list

let show = function
  | []     -> "{}"
  | h :: l -> "{" ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) h l ^ "}"
;;

(* Null signal *)
let null = []

let is_null s = s = []

(* Make new signal from name list *)
let make lst = List.sort_uniq compare lst

(* Merge signals `a` and `b` into a new one *)
let merge a b =
  let c = List.sort_uniq compare (a @ b) in
  c |> List.filter (fun x -> present x || not (List.exists (( = ) (get_present x)) c))
;;

let all_waiting s = List.for_all waiting s

(* Is `b` included in `a`? *)
let ( |- ) a b =
  b
  |> List.fold_left
       (fun res y ->
         res && a |> List.exists (fun x -> if waiting y then x = y || x = get_present y else x = y))
       true
;;

(* Subtract signal `a` from `b`. Must ensure that `a` |- `b` is true *)
let subtract b a =
  if all_waiting a then
    []
  else
    b
    |> List.filter (fun y ->
           not (a |> List.exists (fun x -> if waiting y then x = get_present y else x = y)))
;;

(* Set of signals *)
type set = t list

(* Empty set *)
let empty = []

(* Make a set from single signal *)
let from x = [ x ]

let union a b = a @ b

let forall f s = List.fold_left (fun acc x -> acc && f x) true s

let zip a b = a |> List.fold_left (fun acc x -> acc @ (b |> List.map (fun y -> merge x y))) empty

(* tests *)
let () =
  assert ([] |- []);
  assert ([ "A" ] |- []);
  assert ([ "A" ] |- [ "A" ]);
  assert ([ "A"; "B" ] |- [ "A" ]);
  ()
;;
