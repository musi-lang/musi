open Musi_basic
open Musi_lex

let lex src =
  let interner = Interner.create () in
  let file_id, _ = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let toks, diags = Lexer.lex_all lexer in
  (toks, diags, interner)

let test_keyword_if () =
  let tokens, diags, _ = lex "if" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_keyword_while () =
  let tokens, diags, _ = lex "while" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_ident () =
  let tokens, diags, interner = lex "foo" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.Ident n; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "ident value" "foo" (Interner.lookup interner n)
  | _ -> Alcotest.fail "expected Ident token"

let test_underscore () =
  let tokens, diags, _ = lex "_" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_number_decimal () =
  let tokens, diags, _ = lex "42" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNum s; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "number value" "42" s
  | _ -> Alcotest.fail "expected LitNum token"

let test_number_hex () =
  let tokens, diags, _ = lex "0xFF" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNum s; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "number value" "0xFF" s
  | _ -> Alcotest.fail "expected LitNum token"

let test_number_binary () =
  let tokens, diags, _ = lex "0b1010" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNum s; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "number value" "0b1010" s
  | _ -> Alcotest.fail "expected LitNum token"

let test_number_float () =
  let tokens, diags, _ = lex "3.14" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitNum s; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "number value" "3.14" s
  | _ -> Alcotest.fail "expected LitNum token"

let test_string_simple () =
  let tokens, diags, interner = lex {|"hello"|} in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitStr n; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "text value" "hello" (Interner.lookup interner n)
  | _ -> Alcotest.fail "expected LitStr token"

let test_string_escape () =
  let tokens, diags, interner = lex {|"\n"|} in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitStr n; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check string) "text value" "\n" (Interner.lookup interner n)
  | _ -> Alcotest.fail "expected LitStr token"

let test_rune () =
  let tokens, diags, _ = lex "'a'" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  match tokens with
  | [ { Token.kind = Token.LitRune c; _ }; { kind = Token.Eof; _ } ] ->
    Alcotest.(check int) "rune value" (Char.code 'a') c
  | _ -> Alcotest.fail "expected LitRune token"

let test_operator_assign () =
  let tokens, diags, _ = lex ":=" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_operator_arrow () =
  let tokens, diags, _ = lex "->" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_line_comment () =
  let tokens, diags, _ = lex "// test" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_block_comment () =
  let tokens, diags, _ = lex "/* test */" in
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags);
  Alcotest.(check int) "token count" 2 (List.length tokens)

let test_invalid_escape () =
  let _, diags, _ = lex {|"\x"|} in
  Alcotest.(check bool) "has errors" true (Diagnostic.has_errors diags)

let test_unterminated_string () =
  let _, diags, _ = lex {|"hello|} in
  Alcotest.(check bool) "has errors" true (Diagnostic.has_errors diags)

let test_unexpected_char () =
  let _, diags, _ = lex "`" in
  Alcotest.(check bool) "has errors" true (Diagnostic.has_errors diags)

let () =
  Alcotest.run
    "Lexer"
    [
      ( "keywords"
      , [
          Alcotest.test_case "if" `Quick test_keyword_if
        ; Alcotest.test_case "while" `Quick test_keyword_while
        ] )
    ; ( "identifiers"
      , [
          Alcotest.test_case "simple" `Quick test_ident
        ; Alcotest.test_case "underscore" `Quick test_underscore
        ] )
    ; ( "numbers"
      , [
          Alcotest.test_case "decimal" `Quick test_number_decimal
        ; Alcotest.test_case "hex" `Quick test_number_hex
        ; Alcotest.test_case "binary" `Quick test_number_binary
        ; Alcotest.test_case "float" `Quick test_number_float
        ] )
    ; ( "strings"
      , [
          Alcotest.test_case "simple" `Quick test_string_simple
        ; Alcotest.test_case "escape" `Quick test_string_escape
        ] )
    ; ("runes", [ Alcotest.test_case "simple" `Quick test_rune ])
    ; ( "operators"
      , [
          Alcotest.test_case "assign" `Quick test_operator_assign
        ; Alcotest.test_case "arrow" `Quick test_operator_arrow
        ] )
    ; ( "comments"
      , [
          Alcotest.test_case "line" `Quick test_line_comment
        ; Alcotest.test_case "block" `Quick test_block_comment
        ] )
    ; ( "errors"
      , [
          Alcotest.test_case "invalid escape" `Quick test_invalid_escape
        ; Alcotest.test_case
            "unterminated string"
            `Quick
            test_unterminated_string
        ; Alcotest.test_case "unexpected char" `Quick test_unexpected_char
        ] )
    ]
