module IntSet = Set.Make (Int)

let id_gen = ref 0

let next_id () =
  let id = !id_gen in
  id_gen := id + 1;
  id

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

let last xs = List.fold_left (fun _ y -> y) (List.hd xs) xs

let combinations xss =
  List.fold_left
    (fun acc xs -> xs |> List.map (fun x -> List.map (fun ys -> x :: ys) acc) |> List.flatten)
    [ [] ] xss

let zip xss =
  let n = List.length (List.hd xss) in
  let init = List.init n (fun _ -> []) in
  List.fold_left (fun accs xs -> List.map2 List.cons xs accs) init xss

let roman n =
  assert (n >= 0);
  let digits = [| ""; "I"; "V"; "X"; "L"; "C"; "D"; "M"; "V"; "X"; "L"; "C"; "D"; "M" |] in
  let idx =
    [|
      [];
      [ 1 ];
      [ 1; 1 ];
      [ 1; 1; 1 ];
      [ 1; 2 ];
      [ 2 ];
      [ 2; 1 ];
      [ 2; 1; 1 ];
      [ 2; 1; 1; 1 ];
      [ 1; 3 ];
    |]
  in
  let rec aux p n =
    match (p, n) with
    | 0, 0 -> "O"
    | _, 0 -> ""
    | p, n ->
        let d = n mod 10 in
        let t = List.fold_left (fun acc x -> acc ^ digits.(p + x)) "" idx.(d) in
        aux (p + 2) (n / 10) ^ t
  in
  aux 0 n

let case_no i j =
  assert (i >= 0 && j >= 0);
  let i = roman i in
  Printf.sprintf "%s-%d" i j

module Test = struct
  let test_combinations () =
    assert (
      combinations [ [ 1; 2 ]; [ 3; 4 ] ]
      |> List.sort Stdlib.compare
      = [ [ 3; 1 ]; [ 3; 2 ]; [ 4; 1 ]; [ 4; 2 ] ]);
    assert (
      combinations [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]
      |> List.sort Stdlib.compare
      = [
          [ 5; 3; 1 ];
          [ 5; 3; 2 ];
          [ 5; 4; 1 ];
          [ 5; 4; 2 ];
          [ 6; 3; 1 ];
          [ 6; 3; 2 ];
          [ 6; 4; 1 ];
          [ 6; 4; 2 ];
        ]);
    ()

  let test_case_no () =
    (* test case_no *)
    assert (case_no 0 0 = "O-0");
    assert (case_no 0 1 = "O-1");
    assert (case_no 1 1 = "I-1");
    assert (case_no 2 1 = "II-1");
    assert (case_no 3 2 = "III-2");
    assert (case_no 4 2 = "IV-2");
    assert (case_no 5 3 = "V-3");
    assert (case_no 6 3 = "VI-3");
    assert (case_no 7 3 = "VII-3");
    assert (case_no 8 3 = "VIII-3");
    assert (case_no 9 3 = "IX-3");
    assert (case_no 10 3 = "X-3");
    assert (case_no 11 3 = "XI-3");
    assert (case_no 12 3 = "XII-3");
    assert (case_no 13 3 = "XIII-3");
    assert (case_no 14 3 = "XIV-3");
    assert (case_no 15 3 = "XV-3");
    assert (case_no 19 3 = "XIX-3");
    assert (case_no 20 3 = "XX-3");
    assert (case_no 40 3 = "XL-3");
    assert (case_no 45 3 = "XLV-3");
    assert (case_no 50 3 = "L-3");
    assert (case_no 60 3 = "LX-3");
    assert (case_no 90 3 = "XC-3");
    assert (case_no 99 3 = "XCIX-3");
    assert (case_no 100 3 = "C-3");
    assert (case_no 200 3 = "CC-3");
    assert (case_no 400 3 = "CD-3");
    assert (case_no 450 3 = "CDL-3");
    assert (case_no 455 3 = "CDLV-3");
    assert (case_no 456 3 = "CDLVI-3");
    assert (case_no 456 3 = "CDLVI-3");
    assert (case_no 495 3 = "CDXCV-3");
    assert (case_no 499 3 = "CDXCIX-3");
    assert (case_no 999 3 = "CMXCIX-3");
    assert (case_no 1000 3 = "M-3");
    assert (case_no 1100 3 = "MC-3");
    assert (case_no 1500 3 = "MD-3");
    assert (case_no 2500 3 = "MMD-3");
    assert (case_no 3999 3 = "MMMCMXCIX-3");
    assert (case_no 4000 3 = "MV-3");
    assert (case_no 9000 3 = "MX-3");
    ()

  let test () =
    print_endline "test_combination";
    test_combinations ();
    print_endline "test_case_no";
    test_case_no ();
    ()
end
