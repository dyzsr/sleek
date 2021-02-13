type 'a set = 'a list

let union (a : 'a set) (b : 'a set) = a @ b

let rec nullable : Ast.effects -> bool = function
  | Bottom -> false
  | Empty -> true
  | Signal _ -> false
  | Sequence (es1, es2) -> nullable es1 && nullable es2
  | Union (es1, es2) -> nullable es1 || nullable es2

let rec first : Ast.effects -> Signals.t set = function
  | Bottom -> [ Signals.empty ]
  | Empty -> [ Signals.empty ]
  | Signal s -> [ s ]
  | Sequence (es1, es2) when nullable es1 -> union (first es1) (first es2)
  | Sequence (_, es) -> first es
  | Union (es1, es2) -> union (first es1) (first es2)

let rec partial_deriv (lhs : Signals.t) : Ast.effects -> Ast.effects = function
  | Bottom -> Bottom
  | Empty -> Bottom
  | Signal j when Signals.(lhs |- j) -> Empty
  | Signal _ -> Bottom
  | Sequence (es1, es2) when nullable es1 ->
      Union (Sequence (partial_deriv lhs es1, es2), partial_deriv lhs es2)
  | Sequence (es1, _) -> partial_deriv lhs es1
  | Union (es1, es2) -> Union (partial_deriv lhs es1, partial_deriv lhs es2)

let normalize : Ast.effects -> Ast.effects = function
  | Union (_, Bottom) -> Bottom
  | Union (Bottom, _) -> Bottom
  | Union (es, es') when es = es' -> es
  | Sequence (Empty, es) -> es
  | Sequence (es, Empty) -> es
  | Sequence (Bottom, _) -> Bottom
  | Sequence (_, Bottom) -> Bottom
  | Union (Union (es1, es2), es3) -> Union (es1, Union (es2, es3))
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  | Sequence (es, Union (es1, es2)) -> Union (Sequence (es, es1), Sequence (es, es2))
  | Sequence (Union (es1, es2), es) -> Union (Sequence (es1, es), Sequence (es2, es))
  | es -> es
