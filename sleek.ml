module Ast = Ast
module Astutils = Astutils
module Checker = Checker
module Colors = Colors
module History = History
module Rewriting = Rewriting
module Context = Context
module Instant = Instant
module Parsing = Parsing
module Utils = Utils
module Verifier = Verifier

include Ast
include Astutils
include Parsing
include Verifier

module Test = struct
  let test_utils = Utils.Test.test
  let test_instant = Instant.Test.test
  let test_firsts = Rewriting.Firsts.Test.test
  let test_context = Context.Test.test
end
