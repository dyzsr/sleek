let fixpoint ~f ?(iter = fun _ -> ()) ?(stop = fun _ -> ()) init =
  let rec loop cur =
    let next = f cur in
    if cur = next then (
      stop cur; cur)
    else (
      iter cur; loop next)
  in
  loop init

let opt_value = Option.get
let opt_iter ~f = Option.iter f
let opt_map ~f = Option.map f
let opt_none = Option.is_none
let opt_some = Option.is_some

let opt_map2 ?(a = fun x -> x) ?(b = fun y -> y) ~ab x y =
  match (x, y) with
  | None, None     -> None
  | Some x, None   -> Some (a x)
  | None, Some y   -> Some (b y)
  | Some x, Some y -> Some (ab x y)

let combinations xss =
  List.fold_left
    (fun acc xs -> xs |> List.map (fun x -> List.map (fun ys -> x :: ys) acc) |> List.flatten)
    [ [] ] xss
