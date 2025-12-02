open Basic
open Lex
open Alcotest
open Parse.Parser
open Parse.Node

let test_basic () = Alcotest.(check int) "basic test" 1 1
let suite = [ ("basic", `Quick, test_basic) ]
