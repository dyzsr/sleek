let _ = Ast.Bottom

let _ = Inference.first

let _ = Verifier.verify_specification

let _ = Proofctx.empty

let test =
  let open Ast in
  let open Signals in
  [ Spec
      (Entailment { lhs = Signal (make [ "A" ]); rhs = Signal (make []) }, true)
  ; Spec
      ( Entailment { lhs = Signal (make [ "A" ]); rhs = Signal (make [ "A" ]) }
      , true )
  ; Spec
      (Entailment { lhs = Signal (make []); rhs = Signal (make [ "A" ]) }, false)
  ; Spec
      ( Entailment
          { lhs = Sequence (Signal (make [ "A" ]), Signal (make [ "B" ]))
          ; rhs = Sequence (Signal (make [ "A" ]), Signal (make [ "B" ]))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = Union (Signal (make [ "A" ]), Signal (make [ "B" ]))
          ; rhs = Signal (make [ "A" ])
          }
      , false )
  ; Spec
      ( Entailment
          { lhs = Signal (make [ "A" ])
          ; rhs = Union (Signal (make [ "A" ]), Signal (make [ "B" ]))
          }
      , true )
  ]

let () =
  List.iter
    (fun case ->
      Ast.show_specification case |> print_endline;
      Verifier.verify_specification case |> print_endline;
      ())
    test
