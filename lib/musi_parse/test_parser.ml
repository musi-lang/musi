open Musi_basic
open Musi_lex
open Musi_parse
open Alcotest

let parse src =
  let interner = Interner.create () in
  let file_id, source = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let tokens, _ = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let stmts, diags = Parser.parse parser in
  Diagnostic.emit_all Format.std_formatter diags source;
  (stmts, diags, interner)

let test_literal_int () =
  let stmts, diags, _ = parse "42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_literal_bool_true () =
  let stmts, diags, _ = parse "true" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_literal_bool_false () =
  let stmts, diags, _ = parse "false" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_ident () =
  let stmts, diags, _ = parse "foo" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_binary_add () =
  let stmts, diags, _ = parse "1 + 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_binary_mul () =
  let stmts, diags, _ = parse "3 * 4" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_unary_minus () =
  let stmts, diags, _ = parse "-5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_binding_const () =
  let stmts, diags, _ = parse "const x := 10" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_binding_var () =
  let stmts, diags, _ = parse "var y := 20" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_if_then () =
  let stmts, diags, _ = parse "if true then 1" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_if_then_else () =
  let stmts, diags, _ = parse "if false then 1 else 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_array_empty () =
  let stmts, diags, _ = parse "[]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_array_items () =
  let stmts, diags, _ = parse "[1, 2, 3]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_tuple_empty () =
  let stmts, diags, _ = parse "()" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_tuple_items () =
  let stmts, diags, _ = parse "(1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_proc_anon () =
  let stmts, diags, _ = parse "proc () { 42 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_proc_named () =
  let stmts, diags, _ = parse "const add := proc (x: Nat, y: Nat) { x + y }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_record_empty () =
  let stmts, diags, _ = parse "record {}" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_record_fields () =
  let stmts, diags, _ = parse "record { x: Nat, var y: Nat }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_choice_simple () =
  let stmts, diags, _ = parse "choice { case A, case B }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_import_named () =
  let stmts, diags, _ = parse "import { foo } from \"bar\"" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let test_export_named () =
  let stmts, diags, _ = parse "export { foo }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "stmt count" 1 (List.length stmts)

let () =
  run
    "Parser"
    [
      ( "literals"
      , [
          test_case "int" `Quick test_literal_int
        ; test_case "bool true" `Quick test_literal_bool_true
        ; test_case "bool false" `Quick test_literal_bool_false
        ] )
    ; ("identifiers", [ test_case "simple" `Quick test_ident ])
    ; ( "binary"
      , [
          test_case "add" `Quick test_binary_add
        ; test_case "mul" `Quick test_binary_mul
        ] )
    ; ("unary", [ test_case "minus" `Quick test_unary_minus ])
    ; ( "bindings"
      , [
          test_case "const" `Quick test_binding_const
        ; test_case "var" `Quick test_binding_var
        ] )
    ; ( "if"
      , [
          test_case "then" `Quick test_if_then
        ; test_case "then else" `Quick test_if_then_else
        ] )
    ; ( "arrays"
      , [
          test_case "empty" `Quick test_array_empty
        ; test_case "items" `Quick test_array_items
        ] )
    ; ( "tuples"
      , [
          test_case "empty" `Quick test_tuple_empty
        ; test_case "items" `Quick test_tuple_items
        ] )
    ; ( "proc"
      , [
          test_case "anon" `Quick test_proc_anon
        ; test_case "named" `Quick test_proc_named
        ] )
    ; ( "record"
      , [
          test_case "empty" `Quick test_record_empty
        ; test_case "fields" `Quick test_record_fields
        ] )
    ; ("choice", [ test_case "simple" `Quick test_choice_simple ])
    ; ( "modules"
      , [
          test_case "import named" `Quick test_import_named
        ; test_case "export named" `Quick test_export_named
        ] )
    ]
