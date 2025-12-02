open Alcotest

let () =
  Alcotest.run
    "Musi Tests"
    [ ("Lexer", Test_lexer.suite); ("Parser", Test_parser.suite) ]
