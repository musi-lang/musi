open Musi_basic
open Musi_lex
open Musi_parse

let parse src =
  let interner = Interner.create () in
  let file_id, _ = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let tokens, _ = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let stmts, diags = Parser.parse parser in
  (stmts, diags, interner)

let test_literal_int () =
  let stmts, diags, _ = parse "42" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_literal_bool_true () =
  let stmts, diags, _ = parse "true" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_literal_bool_false () =
  let stmts, diags, _ = parse "false" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_ident () =
  let stmts, diags, _ = parse "foo" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_binary_add () =
  let stmts, diags, _ = parse "1 + 2" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_binary_mul () =
  let stmts, diags, _ = parse "3 * 4" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_unary_minus () =
  let stmts, diags, _ = parse "-5" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_binding_const () =
  let stmts, diags, _ = parse "const x := 10" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_binding_var () =
  let stmts, diags, _ = parse "var y := 20" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_if_then () =
  let stmts, diags, _ = parse "if true then { 1 }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_if_then_else () =
  let stmts, diags, _ = parse "if false then { 1 } else { 2 }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_array_empty () =
  let stmts, diags, _ = parse "[]" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_array_items () =
  let stmts, diags, _ = parse "[1, 2, 3]" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_tuple_empty () =
  let stmts, diags, _ = parse "()" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_tuple_items () =
  let stmts, diags, _ = parse "(1, 2)" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_proc_anon () =
  let stmts, diags, _ = parse "proc () { 42 }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_proc_named () =
  let stmts, diags, _ = parse "const add := proc (x: Nat, y: Nat) { x + y }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_record_empty () =
  let stmts, diags, _ = parse "record {}" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_record_fields () =
  let stmts, diags, _ = parse "record { x: Nat, var y: Nat }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_choice_simple () =
  let stmts, diags, _ = parse "choice { case A, case B }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_import_named () =
  let stmts, diags, _ = parse "import { foo } from \"bar\"" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let test_export_named () =
  let stmts, diags, _ = parse "export { foo }" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "stmt count" 1 (List.length stmts)

let () =
  Alcotest.run
    "Parser"
    [
      ( "literals"
      , [
          Alcotest.test_case "int" `Quick test_literal_int
        ; Alcotest.test_case "bool true" `Quick test_literal_bool_true
        ; Alcotest.test_case "bool false" `Quick test_literal_bool_false
        ] )
    ; ("identifiers", [ Alcotest.test_case "simple" `Quick test_ident ])
    ; ( "binary"
      , [
          Alcotest.test_case "add" `Quick test_binary_add
        ; Alcotest.test_case "mul" `Quick test_binary_mul
        ] )
    ; ("unary", [ Alcotest.test_case "minus" `Quick test_unary_minus ])
    ; ( "bindings"
      , [
          Alcotest.test_case "const" `Quick test_binding_const
        ; Alcotest.test_case "var" `Quick test_binding_var
        ] )
    ; ( "if"
      , [
          Alcotest.test_case "then" `Quick test_if_then
        ; Alcotest.test_case "then else" `Quick test_if_then_else
        ] )
    ; ( "arrays"
      , [
          Alcotest.test_case "empty" `Quick test_array_empty
        ; Alcotest.test_case "items" `Quick test_array_items
        ] )
    ; ( "tuples"
      , [
          Alcotest.test_case "empty" `Quick test_tuple_empty
        ; Alcotest.test_case "items" `Quick test_tuple_items
        ] )
    ; ( "proc"
      , [
          Alcotest.test_case "anon" `Quick test_proc_anon
        ; Alcotest.test_case "named" `Quick test_proc_named
        ] )
    ; ( "record"
      , [
          Alcotest.test_case "empty" `Quick test_record_empty
        ; Alcotest.test_case "fields" `Quick test_record_fields
        ] )
    ; ("choice", [ Alcotest.test_case "simple" `Quick test_choice_simple ])
    ; ( "modules"
      , [
          Alcotest.test_case "import named" `Quick test_import_named
        ; Alcotest.test_case "export named" `Quick test_export_named
        ] )
    ]
