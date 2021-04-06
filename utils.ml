let fixpoint ~f ?(fn_iter = fun _ -> ()) ?(fn_stop = fun _ -> ()) init =
  let rec iter cur =
    let next = f cur in
    if cur = next then (
      fn_stop cur;
      cur)
    else (
      fn_iter cur;
      iter next)
  in
  iter init
