open Musi_basic
open Musi_lex
open Musi_parse

let test_pass path () =
  let source = Test_helpers.read_file path in
  let interner = Interner.create () in
  let file_id, src = Source.add_file Source.empty path source in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let _, parse_diags = Parser.parse parser in
  let diags = Diagnostic.merge [ lex_diags; parse_diags ] in
  Diagnostic.emit_all Format.std_formatter diags src;
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags)

let test_fail path () =
  let source = Test_helpers.read_file path in
  let interner = Interner.create () in
  let file_id, src = Source.add_file Source.empty path source in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let _, parse_diags = Parser.parse parser in
  let diags = Diagnostic.merge [ lex_diags; parse_diags ] in
  Diagnostic.emit_all Format.std_formatter diags src;
  Alcotest.(check bool) "has errors" true (Diagnostic.has_errors diags)

let () =
  let pass_files = Test_helpers.collect_files "test/parser/pass" in
  let fail_files = Test_helpers.collect_files "test/parser/fail" in
  let tests =
    Test_helpers.make_test_cases pass_files fail_files test_pass test_fail
  in
  Alcotest.run "Parser Integration" tests
