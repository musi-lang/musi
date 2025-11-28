open Basic
open Lex

let make_test_state source =
  let interner = Interner.create () in
  let file_id, _ = Source.add_file Source.empty "test.ms" source in
  Lexer.make_state source file_id interner

let lex_single source =
  let state = make_test_state source in
  let tokens, diags = Lexer.tokenize source 0 state.interner in
  (tokens, diags)

let has_error_containing diags substring =
  Diagnostic.has_message_containing diags substring

let test_string_literals () =
  let _, diags = lex_single "\"hello\"" in
  Alcotest.(check bool)
    "valid string has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "\"hello" in
  Alcotest.(check bool)
    "unterminated string error"
    true
    (has_error_containing diags "unterminated string");
  let _, diags = lex_single "\"hello\\q\"" in
  Alcotest.(check bool)
    "invalid escape error"
    true
    (has_error_containing diags "invalid escape");
  let _, diags = lex_single "\"hello\\" in
  Alcotest.(check bool)
    "unterminated escape error"
    true
    (has_error_containing diags "unterminated string")

let test_rune_literals () =
  let _, diags = lex_single "'a'" in
  Alcotest.(check bool)
    "valid rune has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "''" in
  Alcotest.(check bool)
    "empty rune error"
    true
    (has_error_containing diags "empty rune");
  let _, diags = lex_single "'abc'" in
  Alcotest.(check bool)
    "multi-character rune error"
    true
    (has_error_containing diags "multiple characters");
  let _, diags = lex_single "'a" in
  Alcotest.(check bool)
    "unterminated rune error"
    true
    (has_error_containing diags "unterminated rune")

let test_number_literals () =
  let _, diags = lex_single "42" in
  Alcotest.(check bool)
    "valid decimal has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "0xFF" in
  Alcotest.(check bool)
    "valid hex has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "0b1010" in
  Alcotest.(check bool)
    "valid binary has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "0xGHI" in
  Alcotest.(check bool)
    "invalid hex digit error"
    true
    (has_error_containing diags "invalid hex digit");
  let _, diags = lex_single "0b123" in
  Alcotest.(check bool)
    "invalid binary digit error"
    true
    (has_error_containing diags "invalid binary digit");
  let _, diags = lex_single "3.14.159" in
  Alcotest.(check bool)
    "multiple decimal points error"
    true
    (has_error_containing diags "multiple decimal points");
  let _, diags = lex_single "0x" in
  Alcotest.(check bool)
    "incomplete hex error"
    true
    (has_error_containing diags "incomplete hex");
  let _, diags = lex_single "0b" in
  Alcotest.(check bool)
    "incomplete binary error"
    true
    (has_error_containing diags "incomplete binary");
  let _, diags = lex_single "0123" in
  Alcotest.(check bool)
    "leading zeros error"
    true
    (has_error_containing diags "leading zeros")

let test_comments () =
  let _, diags = lex_single "// comment" in
  Alcotest.(check bool)
    "valid line comment has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "/* comment */" in
  Alcotest.(check bool)
    "valid block comment has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "/* comment" in
  Alcotest.(check bool)
    "unterminated block comment error"
    true
    (has_error_containing diags "unterminated block comment");
  let _, diags = lex_single "/* outer /* inner */ */" in
  Alcotest.(check bool)
    "nested block comment error"
    true
    (has_error_containing diags "nested block comments")

let test_identifiers () =
  let _, diags = lex_single "hello_world" in
  Alcotest.(check bool)
    "valid identifier has no errors"
    false
    (Diagnostic.has_errors diags);
  let _, diags = lex_single "hellö" in
  Alcotest.(check bool)
    "non-ASCII identifier error"
    true
    (has_error_containing diags "non-ASCII character")

let test_control_characters () =
  let _, diags = lex_single "\x01" in
  Alcotest.(check bool)
    "control character error"
    true
    (has_error_containing diags "control character");
  let _, diags = lex_single "\x00" in
  Alcotest.(check bool)
    "null byte error"
    true
    (has_error_containing diags "control character")

let test_unexpected_characters () =
  let _, diags = lex_single "§" in
  Alcotest.(check bool)
    "unexpected character error"
    true
    (has_error_containing diags "unexpected character")

let () =
  let open Alcotest in
  run
    "Lexer"
    [
      ( "string_literals"
      , [ test_case "string literal errors" `Quick test_string_literals ] )
    ; ( "rune_literals"
      , [ test_case "rune literal errors" `Quick test_rune_literals ] )
    ; ( "number_literals"
      , [ test_case "number literal errors" `Quick test_number_literals ] )
    ; ("comments", [ test_case "comment errors" `Quick test_comments ])
    ; ("identifiers", [ test_case "identifier errors" `Quick test_identifiers ])
    ; ( "control_characters"
      , [ test_case "control character errors" `Quick test_control_characters ]
      )
    ; ( "unexpected_characters"
      , [
          test_case
            "unexpected character errors"
            `Quick
            test_unexpected_characters
        ] )
    ]
