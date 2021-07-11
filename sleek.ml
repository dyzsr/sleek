module Ast = Ast
module Ast_helper = Ast_helper
module Ast_print = Ast_print
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
include Ast_helper
include Ast_print
include Parsing
include Verifier

module Test = struct
  let test_utils = Utils.Test.test
  let test_instant = Instant.Test.test
  let test_firsts = Rewriting.Firsts.Test.test
end
