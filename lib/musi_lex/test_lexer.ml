open Musi_basic
open Musi_lex
open Alcotest

let lex src =
  let interner = Interner.create () in
  let file_id, _ = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let toks, diags = Lexer.lex_all lexer in
  (toks, diags, interner)

let test_keyword_if () =
  let tokens, diags, _ = lex "if" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_keyword_while () =
  let tokens, diags, _ = lex "while" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_ident () =
  let tokens, diags, interner = lex "foo" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.Ident n; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "ident value" "foo" (Interner.lookup interner n)
  | _ -> fail "expected Ident token"

let test_underscore () =
  let tokens, diags, _ = lex "_" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_number_decimal () =
  let tokens, diags, _ = lex "42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNumber s; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "number value" "42" s
  | _ -> fail "expected LitNumber token"

let test_number_hex () =
  let tokens, diags, _ = lex "0xFF" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNumber s; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "number value" "0xFF" s
  | _ -> fail "expected LitNumber token"

let test_number_binary () =
  let tokens, diags, _ = lex "0b1010" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNumber s; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "number value" "0b1010" s
  | _ -> fail "expected LitNumber token"

let test_number_float () =
  let tokens, diags, _ = lex "3.14" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNumber s; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "number value" "3.14" s
  | _ -> fail "expected LitNumber token"

let test_str_simple () =
  let tokens, diags, interner = lex {|"hello"|} in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitString n; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "text value" "hello" (Interner.lookup interner n)
  | _ -> fail "expected LitString token"

let test_str_escape () =
  let tokens, diags, interner = lex {|"\n"|} in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitString n; _ }; { kind = Token.Eof; _ } ] ->
    (check string) "text value" "\n" (Interner.lookup interner n)
  | _ -> fail "expected LitString token"

let test_rune () =
  let tokens, diags, _ = lex "'a'" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitRune c; _ }; { kind = Token.Eof; _ } ] ->
    (check int) "rune value" (Char.code 'a') c
  | _ -> fail "expected LitRune token"

let test_operator_assign () =
  let tokens, diags, _ = lex ":=" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_operator_arrow () =
  let tokens, diags, _ = lex "->" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_line_comment () =
  let tokens, diags, _ = lex "// test" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_block_comment () =
  let tokens, diags, _ = lex "/* test */" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "token count" 2 (List.length tokens)

let test_invalid_escape () =
  let _, diags, _ = lex {|"\x"|} in
  (check bool) "has errors" true (Diagnostic.has_errors diags)

let test_unterminated_string () =
  let _, diags, _ = lex {|"hello|} in
  (check bool) "has errors" true (Diagnostic.has_errors diags)

let test_unexpected_char () =
  let _, diags, _ = lex "`" in
  (check bool) "has errors" true (Diagnostic.has_errors diags)

let () =
  run
    "Lexer"
    [
      ( "keywords"
      , [
          test_case "if" `Quick test_keyword_if
        ; test_case "while" `Quick test_keyword_while
        ] )
    ; ( "identifiers"
      , [
          test_case "simple" `Quick test_ident
        ; test_case "underscore" `Quick test_underscore
        ] )
    ; ( "numbers"
      , [
          test_case "decimal" `Quick test_number_decimal
        ; test_case "hex" `Quick test_number_hex
        ; test_case "binary" `Quick test_number_binary
        ; test_case "float" `Quick test_number_float
        ] )
    ; ( "strings"
      , [
          test_case "simple" `Quick test_str_simple
        ; test_case "escape" `Quick test_str_escape
        ] )
    ; ("runes", [ test_case "simple" `Quick test_rune ])
    ; ( "operators"
      , [
          test_case "assign" `Quick test_operator_assign
        ; test_case "arrow" `Quick test_operator_arrow
        ] )
    ; ( "comments"
      , [
          test_case "line" `Quick test_line_comment
        ; test_case "block" `Quick test_block_comment
        ] )
    ; ( "errors"
      , [
          test_case "invalid escape" `Quick test_invalid_escape
        ; test_case "unterminated string" `Quick test_unterminated_string
        ; test_case "unexpected char" `Quick test_unexpected_char
        ] )
    ]
