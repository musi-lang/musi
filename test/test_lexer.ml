open Basic
open Lex
open Alcotest

let make_test_state source =
  let interner = Interner.create () in
  let file_id = 42 in
  let state = Lexer.mk_state source file_id interner in
  (state, interner)

let check_no_errors diags =
  (check bool) "no errors" false (Diagnostic.has_errors diags)

let get_interned_string interner name = Interner.lookup interner name

let check_string_value interner name expected =
  let actual = get_interned_string interner name in
  (check string) "string value" expected actual

let check_identifier_value interner name expected =
  let actual = get_interned_string interner name in
  (check string) "identifier value" expected actual

let test_char_preds () =
  (check bool) "is_alpha lowercase" true (Lexer.is_alpha 'a');
  (check bool) "is_alpha uppercase" true (Lexer.is_alpha 'Z');
  (check bool) "is_alpha non-alpha" false (Lexer.is_alpha '5');
  (check bool) "is_digit true" true (Lexer.is_digit '7');
  (check bool) "is_digit false" false (Lexer.is_digit 'a');
  (check bool) "is_whitespace space" true (Lexer.is_whitespace ' ');
  (check bool) "is_whitespace tab" true (Lexer.is_whitespace '\t');
  (check bool) "is_whitespace newline" false (Lexer.is_whitespace '\n');
  (check bool) "is_xdigit digit" true (Lexer.is_xdigit '5');
  (check bool) "is_xdigit hex" true (Lexer.is_xdigit 'A');
  (check bool) "is_xdigit invalid" false (Lexer.is_xdigit 'G');
  (check bool) "is_bdigit 0" true (Lexer.is_bdigit '0');
  (check bool) "is_bdigit 1" true (Lexer.is_bdigit '1');
  (check bool) "is_bdigit 2" false (Lexer.is_bdigit '2');
  (check bool) "is_odigit 7" true (Lexer.is_odigit '7');
  (check bool) "is_odigit 8" false (Lexer.is_odigit '8');
  (check bool) "is_ident_start letter" true (Lexer.is_ident_start 'a');
  (check bool) "is_ident_start underscore" true (Lexer.is_ident_start '_');
  (check bool) "is_ident_start digit" false (Lexer.is_ident_start '5');
  (check bool) "is_ident_cont letter" true (Lexer.is_ident_cont 'b');
  (check bool) "is_ident_cont digit" true (Lexer.is_ident_cont '9');
  (check bool) "is_ident_cont underscore" true (Lexer.is_ident_cont '_')

let test_state_functions () =
  let state, _ = make_test_state "hello" in
  (check (option char)) "peek_opt first" (Some 'h') (Lexer.peek_opt state);
  let state2 = Lexer.advance state in
  (check int) "advance position" 1 state2.pos;
  (check (option char))
    "peek_opt after advance"
    (Some 'e')
    (Lexer.peek_opt state2)

let test_numbers_decimal () =
  let state, _ = make_test_state "42" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  (check string) "decimal simple" "42" content;
  (check int) "decimal position" 2 new_state.pos

let test_numbers_hex () =
  let state, _ = make_test_state "0xFF" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  (check string) "hex uppercase" "0xFF" content;
  (check int) "hex position" 4 new_state.pos

let test_numbers_binary () =
  let state, _ = make_test_state "0b1010" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  (check string) "binary" "0b1010" content;
  (check int) "binary position" 6 new_state.pos

let test_numbers_octal () =
  let state, _ = make_test_state "0o777" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  (check string) "octal" "0o777" content;
  (check int) "octal position" 5 new_state.pos

let test_numbers_float () =
  let state, _ = make_test_state "3.14159" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  (check string) "float" "3.14159" content;
  (check int) "float position" 7 new_state.pos

let test_numbers_errors_incomplete_hex () =
  let state, _ = make_test_state "0x" in
  let new_state, _content, _span = Lexer.scan_number state in
  (check bool)
    "hex incomplete has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_strings_basic () =
  let state, interner = make_test_state "\"hello\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "hello";
  (check int) "string basic position" 7 new_state.pos

let test_strings_empty () =
  let state, interner = make_test_state "\"\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "";
  (check int) "string empty position" 2 new_state.pos

let test_strings_escape_newline () =
  let state, interner = make_test_state "\"hello\\nworld\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "hello\nworld"

let test_strings_unterminated () =
  let state, _ = make_test_state "\"hello" in
  let new_state, _content, _span = Lexer.scan_string state in
  (check bool)
    "unterminated has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_basic () =
  let state, _ = make_test_state "'a'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  check_no_errors new_state.diags;
  (check char) "rune basic" 'a' char_val;
  (check int) "rune basic position" 3 new_state.pos

let test_runes_escape_newline () =
  let state, _ = make_test_state "'\\n'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  let has_errors = Diagnostic.has_errors new_state.diags in
  if has_errors then
    Printf.printf "Errors found in rune escape: %b\n" has_errors;
  (check bool) "rune escape has no errors" false has_errors;
  if not has_errors then (check char) "rune escape newline" '\n' char_val

let test_runes_empty () =
  let state, _ = make_test_state "''" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  (check bool)
    "empty rune has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_multichar () =
  let state, _ = make_test_state "'ab'" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  (check bool)
    "multichar rune has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_invalid_escape () =
  let state, _ = make_test_state "'\\q'" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  (check bool)
    "invalid escape has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_templates_basic () =
  let state, interner = make_test_state "$\"hello world\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello world"
  | _ -> fail "Expected template token"

let test_templates_escape () =
  let state, interner = make_test_state "$\"hello\\nworld\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello\nworld"
  | _ -> fail "Expected template token"

let test_idents_basic () =
  let state, interner = make_test_state "hello_world" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  check_identifier_value interner name "hello_world";
  (check int) "identifier basic position" 11 new_state.pos

let test_idents_1letter () =
  let state, interner = make_test_state "x" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  check_identifier_value interner name "x";
  (check int) "identifier single position" 1 new_state.pos

let test_keywords_val () =
  let state, interner = make_test_state "val" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  let token =
    Token.lookup_keyword interner (get_interned_string interner name)
  in
  assert (token = Token.KwVal)

let test_keywords_var () =
  let state, interner = make_test_state "var" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  let token =
    Token.lookup_keyword interner (get_interned_string interner name)
  in
  assert (token = Token.KwVar)

let test_keywords_fn () =
  let state, interner = make_test_state "fn" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  let token =
    Token.lookup_keyword interner (get_interned_string interner name)
  in
  assert (token = Token.KwFn)

let test_symbols_assignment () =
  let state, _ = make_test_state "<-" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.LtMinus);
  (check int) "assignment position" 2 new_state.pos

let test_symbols_equals () =
  let state, _ = make_test_state "=" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.Eq)

let test_symbols_plus () =
  let state, _ = make_test_state "+" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.Plus)

let test_symbols_dollar () =
  let state, _ = make_test_state "$" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.Dollar)

let test_dollar_standalone () =
  let state, _ = make_test_state "$x" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  assert (token = Token.Dollar);
  (check int) "dollar standalone position" 1 new_state.pos

let test_parentheses () =
  let state, _ = make_test_state "(" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.LParen);
  let state2, _ = make_test_state ")" in
  let new_state2, token2, _span2 = Lexer.scan_symbol state2 in
  check_no_errors new_state2.diags;
  assert (token2 = Token.RParen)

let test_comments_line () =
  let state, _ = make_test_state "// comment" in
  let new_state, content, _span = Lexer.scan_line_comment state in
  check_no_errors new_state.diags;
  (check string) "line comment content" " comment" content;
  (check int) "line comment position" 10 new_state.pos

let test_comments_block () =
  let state, _ = make_test_state "/* comment */" in
  let new_state, content = Lexer.scan_block_comment state in
  check_no_errors new_state.Lexer.diags;
  (check string) "block comment content" " comment " content;
  (check int) "block comment position" 13 new_state.Lexer.pos

let test_whitespace_space () =
  let state, _ = make_test_state "   " in
  let new_state, _, _span = Lexer.scan_whitespace state in
  (check int) "whitespace position" 3 new_state.pos

let test_unicode_escape_valid_small () =
  let state, interner = make_test_state "\"\\u{00A9}\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "\169"

let test_unicode_escape_valid_big () =
  let state, interner = make_test_state "\"\\U{E001}\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "\001"

let test_unicode_escape_empty () =
  let state, _ = make_test_state "\"\\u{}\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  (check bool)
    "unicode empty escape has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_missing_brace () =
  let state, _ = make_test_state "\"\\u00A9\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  (check bool)
    "unicode missing brace has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_invalid_hex () =
  let state, _ = make_test_state "\"\\u{GGGG}\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  (check bool)
    "unicode invalid hex has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_exceed_small_limit () =
  let state, _ = make_test_state "\"\\u{110000}\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  (check bool)
    "unicode exceeds small limit has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_incomplete () =
  let state, _ = make_test_state "\"\\u{" in
  let new_state, _name, _span = Lexer.scan_string state in
  (check bool)
    "unicode incomplete has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_in_rune () =
  let state, _ = make_test_state "'\\u{41}'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  check_no_errors new_state.diags;
  (check char) "unicode rune" 'A' char_val

let test_template_extra_closing_brace () =
  let state, _ = make_test_state "$\"hello}world\"" in
  let new_state, _token, _span = Lexer.scan_template_or_dollar state in
  (check bool)
    "template extra closing brace has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_template_unclosed_opening_brace () =
  let state, _ = make_test_state "$\"hello{world\"" in
  let new_state, _token, _span = Lexer.scan_template_or_dollar state in
  (check bool)
    "template unclosed opening brace has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_template_nested_braces () =
  let state, interner = make_test_state "$\"hello {user{name} } end\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name ->
    check_string_value interner name "hello {user{name} } end"
  | _ -> fail "Expected template token"

let test_template_empty_braces () =
  let state, interner = make_test_state "$\"hello{}world\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello{}world"
  | _ -> fail "Expected template token"

let test_utf8_valid_2byte () =
  let state, interner = make_test_state "\"\xc2\xa9\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "\u{00A9}"

let test_utf8_valid_3byte () =
  let state, interner = make_test_state "\"\xe2\x98\x83\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "\u{2603}"

let test_utf8_valid_4byte () =
  let state, interner = make_test_state "\"\xf0\x9f\x98\x80\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "\u{1F600}"

let test_utf8_invalid_continuation () =
  let interner = Interner.create () in
  let _tokens, diags = Lexer.tokenize "\xc2\x41" 42 interner in
  (check bool)
    "utf8 invalid continuation has errors"
    true
    (Diagnostic.has_errors diags)

let test_utf8_incomplete_sequence () =
  let interner = Interner.create () in
  let _tokens, diags = Lexer.tokenize "\xc2" 42 interner in
  (check bool)
    "utf8 incomplete sequence has errors"
    true
    (Diagnostic.has_errors diags)

let test_utf8_invalid_start_byte () =
  let interner = Interner.create () in
  let _tokens, diags = Lexer.tokenize "\xfc" 42 interner in
  (check bool)
    "utf8 invalid start byte has errors"
    true
    (Diagnostic.has_errors diags)

let test_cases =
  [
    test_case "char_preds" `Quick test_char_preds
  ; test_case "state_functions" `Quick test_state_functions
  ; test_case "numbers_decimal" `Quick test_numbers_decimal
  ; test_case "numbers_hex" `Quick test_numbers_hex
  ; test_case "numbers_binary" `Quick test_numbers_binary
  ; test_case "numbers_octal" `Quick test_numbers_octal
  ; test_case "numbers_float" `Quick test_numbers_float
  ; test_case
      "numbers_errors_incomplete_hex"
      `Quick
      test_numbers_errors_incomplete_hex
  ; test_case "strings_basic" `Quick test_strings_basic
  ; test_case "strings_empty" `Quick test_strings_empty
  ; test_case "strings_escape_newline" `Quick test_strings_escape_newline
  ; test_case "strings_unterminated" `Quick test_strings_unterminated
  ; test_case "runes_basic" `Quick test_runes_basic
  ; test_case "runes_escape_newline" `Quick test_runes_escape_newline
  ; test_case "runes_empty" `Quick test_runes_empty
  ; test_case "runes_multichar" `Quick test_runes_multichar
  ; test_case "runes_invalid_escape" `Quick test_runes_invalid_escape
  ; test_case
      "unicode_escape_valid_small"
      `Quick
      test_unicode_escape_valid_small
  ; test_case "unicode_escape_valid_big" `Quick test_unicode_escape_valid_big
  ; test_case "unicode_escape_empty" `Quick test_unicode_escape_empty
  ; test_case
      "unicode_escape_missing_brace"
      `Quick
      test_unicode_escape_missing_brace
  ; test_case
      "unicode_escape_invalid_hex"
      `Quick
      test_unicode_escape_invalid_hex
  ; test_case
      "unicode_escape_exceed_small_limit"
      `Quick
      test_unicode_escape_exceed_small_limit
  ; test_case "unicode_escape_incomplete" `Quick test_unicode_escape_incomplete
  ; test_case "unicode_escape_in_rune" `Quick test_unicode_escape_in_rune
  ; test_case
      "template_extra_closing_brace"
      `Quick
      test_template_extra_closing_brace
  ; test_case
      "template_unclosed_opening_brace"
      `Quick
      test_template_unclosed_opening_brace
  ; test_case "template_nested_braces" `Quick test_template_nested_braces
  ; test_case "template_empty_braces" `Quick test_template_empty_braces
  ; test_case "utf8_valid_2byte" `Quick test_utf8_valid_2byte
  ; test_case "utf8_valid_3byte" `Quick test_utf8_valid_3byte
  ; test_case "utf8_valid_4byte" `Quick test_utf8_valid_4byte
  ; test_case "utf8_invalid_continuation" `Quick test_utf8_invalid_continuation
  ; test_case "utf8_incomplete_sequence" `Quick test_utf8_incomplete_sequence
  ; test_case "utf8_invalid_start_byte" `Quick test_utf8_invalid_start_byte
  ; test_case "idents_basic" `Quick test_idents_basic
  ; test_case "idents_1letter" `Quick test_idents_1letter
  ; test_case "keywords_val" `Quick test_keywords_val
  ; test_case "keywords_var" `Quick test_keywords_var
  ; test_case "keywords_fn" `Quick test_keywords_fn
  ; test_case "symbols_assignment" `Quick test_symbols_assignment
  ; test_case "symbols_equals" `Quick test_symbols_equals
  ; test_case "symbols_plus" `Quick test_symbols_plus
  ; test_case "symbols_dollar" `Quick test_symbols_dollar
  ; test_case "templates_basic" `Quick test_templates_basic
  ; test_case "templates_escape" `Quick test_templates_escape
  ; test_case "dollar_standalone" `Quick test_dollar_standalone
  ; test_case "parentheses" `Quick test_parentheses
  ; test_case "comments_line" `Quick test_comments_line
  ; test_case "comments_block" `Quick test_comments_block
  ; test_case "whitespace_space" `Quick test_whitespace_space
  ]

let suite = [ ("lexer", test_cases) ]
let () = run "lexer" suite
