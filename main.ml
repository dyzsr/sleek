let tests =
  [ (* Single Instant *)
    "True && {A}  |-  True && {A} : true"
  ; "True && {}   |-  True && {A} : false"
  ; "True && {A}  |-  True && {}  : true"
  ; (* Empty, Bottoms *)
    "True && emp  |-  True && {}  : false"
  ; "True && emp  |-  True && {A} : false"
  ; "True && emp  |-  True && emp : true"
  ; "True && emp  |-  True && emp : true"
  ; "True && _|_  |-  True && emp  : true"
  ; "True && _|_  |-  True && {}   : true"
  ; "True && _|_  |-  False && {}  : true"
  ; "True && _|_  |-  False && {}  : true"
  ; "True && _|_  |-  False && _|_ : true"
  ; "False && {}  |-  True && {}   : true"
  ; "True && {}   |-  False && _|_ : false"
  ; "True && {} . _|_   |-  True && _|_ : true"
  ; "True && {} || _|_  |-  True && _|_ : false"
  ; "True && {} // _|_  |-  True && _|_ : true"
  ; "True && {} // _|_  |-  True && _|_ : true"
  ; "True && _|_^*  |-  True && emp : true"
  ; "True && _|_^*  |-  True && _|_ : false"
  ; (* False pure *)
    "True && {}    |-  False && {}  : false"
  ; "False && {}   |-  True && {}   : true"
  ; "False && {}   |-  False && {}  : true"
  ; "False && {}   |-  False && _|_ : true"
  ; "False && emp  |-  False && _|_ : true"
  ; (* Sequence *)
    "True && {A}.{B}  |-  True && {A}.{B} : true"
  ; "True && {A}  |-  True && {A}.{B} : false"
  ; "True && {A}.{B}  |-  True && {B} : false"
  ; "True && {A}.{B}  |-  True && {}.{B} : true"
  ; "True && {A}.{B}  |-  True && {}.{}  : true"
  ; "True && {A}.{B}.{A}.{C}.{A, B}  |-  True && {}.{}.{}.{}.{}    : true"
  ; "True && {A}.{B}.{A}.{C}.{A, B}  |-  True && {A}.{}.{}.{C}.{A} : true"
  ; "True && {A}.{B}.{A}.{C}.{B}  |-  True && {A}.{}.{}.{C}.{A}    : false"
  ; "True && {A}.{B}.{A}.{C}.{B}  |-  True && {A}.{}.{}.{C}.{A}    : false"
  ; (* Union *)
    "True && {A} || {B}  |-  True && {A} : false"
  ; "True && {A}  |-  True && {A} || {B} : true"
  ; "True && {A} || {B}  |-  True && {B} || {A} : true"
  ; "True && ({A} || {C}) || {B}  |-  True && ({C} || {A}) || {B} : true"
  ; "True && {A} || ({C} || {B})  |-  True && {B} || ({A} || {C}) : true"
  ; "True && {A} || {B} || {C} || {D} || {E}  |-  True && {A, B, C, D, E} : false"
  ; "True && {A, B, C, D, E}  |-  True && {A} || {B} || {C} || {D} || {E} : true"
  ; (* Parallel *)
    "True && {A} // {B}  |-  True && {A} // {B} : true"
  ; "True && {A, B}  |-  True && {A} // {B} : true"
  ; "True && {A} // {B}  |-  True && {A, B} : true"
  ; "True && {A} // {B} // {C} // {D} // {E}  |-  True && {A, B, C, D, E} : true"
  ; "True && {A, B, C, D, E}  |-  True && {A} // {B} // {C} // {D} // {E} : true"
  ; (* Sequence, Union *)
    "True && {A}.{B} || {C}.{D}  |-  True && {C}.{D} || {A}.{B} : true"
  ; "True && {A}.({B} || {C}).{D}  |-  True && {}.{}.{} : true"
  ; "True && {A}.{B}.{C} || {C}.{B}.{A}  |-  True && {A}.{B}.{A} : false"
  ; "True && {A}.{B}.{C} || {C}.{B}.{A}  |-  True && {}.{B}.{}   : true"
  ; "True && {A, C}.{B}.{C} || {A, C}.{B}.{A}  |-  True && {A}.{B}.{} : true"
  ; "True && {A, C}.{B}.{C} || {A, C}.{B}.{A}  |-  True && {C}.{B}.{} : true"
  ; "True && {A}.{B}.{A}  |-  True && {A}.{B}.{C} || {C}.{B}.{A}       : false"
  ; "True && {A, C}.{B}.{A, C}  |-  True && {A}.{B}.{C} || {C}.{B}.{A} : true"
  ; (* Sequence, Parallel *)
    "True && {A}.{B} // {C}.{D}  |-  True && {C}.{D} // {A}.{B} : true"
  ; "True && {A}.({B} // {C}).{D}  |-  True && {}.{}.{} : true"
  ; "True && {A}.({B} // {C}).{D}  |-  True && {}.{B, C}.{} : true"
  ; "True && {A}.({B} // {C}).{D}  |-  True && {A}.{B, C}.{D} : true"
  ; "True && {A}.({B} // {C}).{D}  |-  True && {A}.{B}.{D} // {A}.{C}.{D} : true"
  ; "True && {A}.({B} // {C}).{D}  |-  True && ({A}.{B} // {A}.{C}).{D} : true"
  ; "True && {A}.{C}.{E} // {B}.{D}.{F}  |-  True && {A, B}.{C, D}.{E, F} : true"
  ; "True && {A}.{C}.{E} // {B}.{D}  |-  True && {A, B}.{C, D}.{E} : true"
  ; "True && {A}.{C}.{E} // {B, D}  |-  True && {B, D}.{C}.{E} : true"
  ; (* Kleene *)
    "True && {A}^*  |-  True && {A}^* : true"
  ; "True && {A}    |-  True && {A}^* : true"
  ; "True && {A}^*  |-  True && {A}   : false"
  ; "True && {A, B}^*   |-  True && {A}^* : true"
  ; "True && {A, B}^*   |-  True && emp^* : false"
  ; "True && {A, B}^*   |-  True && _|_^* : false"
  ; "False && {A, B}^*  |-  True && _|_^* : true"
  ; (* Kleene, Union, Parallel *)
    "True && ({A} || {B})^*  |-  True && {A}^* : false"
  ; "True && {A}^*  |-  True && ({A} || {B})^* : true"
  ; "True && ({A} || {B})^*  |-  True && ({B} || {A})^* : true"
  ; "True && ({A} || ({B} || {C}))^*  |-  True && ({B} || ({A} || {C}))^* : true"
  ; "True && ({A} || ({B} // {C}))^*  |-  True && {}^* : true"
  ; "True && ({A} || ({A} // {C}))^*  |-  True && {A}^* || ({A} || {C})^* : true"
  ; (* Sequence, Kleene *)
    "True && {A}.{B}^*.{C}.{A, B}  |-  True && {A}.{B}^*.{C}.{A}   : true"
  ; "True && {A}.{B}^*.{C}.{A, B}  |-  True && {A}.{B}^*.{}.{A}^*  : true"
  ; "True && {A}.{B}^*.{C}^*  |-  True && {A}^*.{B}^*.{C}^* : true"
  ; "True && {A}.{B}^*.{C}^*  |-  True && {A}^*.{B, C}^*    : false"
  ; "True && {A}.{B, C}^*  |-  True && {A}^*.{B}^*.{C}^*    : true"
  ; (* Mixed *)
    "True && {A}.{B}^*.{C}^*    |-  True && {A}^*.({B} || {C})^*   : true"
  ; "True && {A}^*.{B}^*.{C}^*  |-  True && ({A} || {B} || {C})^*  : true"
  ; "True && ({A}.{B}.{C})^*    |-  True && ({A} || {B} || {C})^*  : true"
  ; "True && {B}.({A} || ({B} || {C}))^*  |-  True && ({B} || ({A} || {C}))^* : true"
  ; "True && {A, B}.({A} || ({B} // {C}))^*  |-  True && {}^* : true"
  ; "True && {A, C}.({A} || ({A} // {C}))^*  |-  True && {A}^* || ({A} || {C})^* : true"
  ; "True && {A, C}^*.({A} || ({A} // {C}))^*  |-  True && {A}^* || ({A} || {C})^* : true"
  ; "True && ({A}||{D}).{B, E}.({C}//{F})  |-  True && (({A}||{D}).{B, E}.({C}//{F}))^* : true"
  ; "True && (({A}||{D}).{B, E}.({C}//{F}))^*  |-  True && (({A}||{D}).{B, E}.({C}//{F}))^* : true"
  ; "True && (({A}||{D}).{B, E}.({C}//{F}))^*  |-  True && ({A}||{B}||{C}||{D}||{E})^* : true"
  ; (* Alternative Syntax *)
    {| True /\ {A, B}.{A, B}^*  |-  True /\ {A, B}^* : true |}
  ; {| True /\ ({A}.{B})^*      |-  True /\ {A, B}^* : false |}
  ; {| True /\ ({A}.{B})^*      |-  True /\ ({A} || {B})^* : true |}
  ; {| True /\ ({A}.{B})^*      |-  True /\ emp || {A}.{B}.({A} || {B})^* : true |}
  ; {| True /\ ({A}.{B})^*      |-  True /\ bot || {A}.{B}.({A} || {B})^* : false |}
  ; {| True /\ ({A}.{B})^* // {C}    |-  True /\ {A, C}.{B}.({A}.{B})^* : true   |}
  ; {| True /\ ({A}.{B})^* // {C}^*  |-  True /\ ({A, C}.{B, C})^* : true   |}
  ; {| True /\ ({A}.{B})^* // ({C} // {D})^*  |-  True /\ ({A, C}.{B, C})^* : true   |}
  ; {| True /\ {A, B} |-  True /\ {A} // {B} // {C} : false   |}
  ; (* Waiting Signals *)
    "True && A? || {A}  |-  True && {A}  : false"
  ; "True && A? // {A}  |-  True && {A}  : true"
  ; "True && {A}   |-  True && {A} || A? : true"
  ; "True && {A}   |-  True && A?        : true"
  ; "True && A?    |-  True && A?        : true"
  ; "True && A?    |-  True && {A}       : false"
  ; "True && A?.{B} // {A}       |-  True && {A}.{B}     : true"
  ; "True && A?.{B} // {A}.{B}   |-  True && {A}.{B}     : true"
  ; "True && A?.{B} // {A}.{C}   |-  True && {A}.{B, C}  : true"
  ; "True && A?.{B} // A?.{C}    |-  True && A?.{B, C}   : true"
  ; "True && B?.{A} // {X}.{Y}.{B}.{Z}  |-  True && {X}.{Y}.{B}.{A, Z} : true"
  ; "True && A?.B?.{D} // {A}.{B}.{C}   |-  True && {A}.{B}.{C, D} : true"
  ; "True && A?.B?.{D} // {A}.{C}  |-  True && {A}.B?.{C, D}  : false"
  ; "True && A?.B?.{D} // {A}.{C}  |-  True && {A}.{C}.B?.{D} : true"
  ; "True && A?.B?.{D} // {A}.{C}  |-  True && {A}.B?.{D}     : false"
  ; "True && {A}.{C}.B?.{D}  |-  True && {A}.B?.{D}     : false"
  ; "True && {A}.B?.{D}      |-  True && {A}.{C}.B?.{D} : false"
  ; "True && {A}.(B? // {C}).{D}      |-  True && {A}.{C}.B?.{D} : true"
  ; "True && {A} // A? // A?  |-  True && {A} : true"
  ; "True && {A} // {B}.A?  |-  True && {A, B}.A? : true"
  ; "True && (A? // A?).{B} // {C}.{A}  |-  True && {C}.{}.{B} : true"
  ]
;;

let () =
  tests
  |> List.iteri (fun no str ->
         let case = Syntax.parse_specification str in
         let correct, verdict, history = Verifier.verify_specification case in
         Verifier.show_verification ~case ~no ~verdict ~verbose:(not correct) ~history
         |> print_endline;
         assert correct)
;;
