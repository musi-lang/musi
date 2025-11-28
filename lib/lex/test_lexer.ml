open Basic
open Lex

let make_test_state source =
  let interner = Interner.create () in
  let file_id = 42 in
  let state = Lexer.make_state source file_id interner in
  (state, interner)

let check_no_errors diags =
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags)

let get_interned_string interner name = Interner.lookup interner name

let check_string_value interner name expected =
  let actual = get_interned_string interner name in
  Alcotest.(check string) "string value" expected actual

let check_identifier_value interner name expected =
  let actual = get_interned_string interner name in
  Alcotest.(check string) "identifier value" expected actual

let test_character_predicates () =
  Alcotest.(check bool) "is_alpha lowercase" true (Lexer.is_alpha 'a');
  Alcotest.(check bool) "is_alpha uppercase" true (Lexer.is_alpha 'Z');
  Alcotest.(check bool) "is_alpha non-alpha" false (Lexer.is_alpha '5');
  Alcotest.(check bool) "is_digit true" true (Lexer.is_digit '7');
  Alcotest.(check bool) "is_digit false" false (Lexer.is_digit 'a');
  Alcotest.(check bool) "is_whitespace space" true (Lexer.is_whitespace ' ');
  Alcotest.(check bool) "is_whitespace tab" true (Lexer.is_whitespace '\t');
  Alcotest.(check bool) "is_whitespace newline" false (Lexer.is_whitespace '\n');
  Alcotest.(check bool) "is_xdigit digit" true (Lexer.is_xdigit '5');
  Alcotest.(check bool) "is_xdigit hex" true (Lexer.is_xdigit 'A');
  Alcotest.(check bool) "is_xdigit invalid" false (Lexer.is_xdigit 'G');
  Alcotest.(check bool) "is_bdigit 0" true (Lexer.is_bdigit '0');
  Alcotest.(check bool) "is_bdigit 1" true (Lexer.is_bdigit '1');
  Alcotest.(check bool) "is_bdigit 2" false (Lexer.is_bdigit '2');
  Alcotest.(check bool) "is_odigit 7" true (Lexer.is_odigit '7');
  Alcotest.(check bool) "is_odigit 8" false (Lexer.is_odigit '8');
  Alcotest.(check bool) "is_ident_start letter" true (Lexer.is_ident_start 'a');
  Alcotest.(check bool)
    "is_ident_start underscore"
    true
    (Lexer.is_ident_start '_');
  Alcotest.(check bool) "is_ident_start digit" false (Lexer.is_ident_start '5');
  Alcotest.(check bool) "is_ident_cont letter" true (Lexer.is_ident_cont 'b');
  Alcotest.(check bool) "is_ident_cont digit" true (Lexer.is_ident_cont '9');
  Alcotest.(check bool)
    "is_ident_cont underscore"
    true
    (Lexer.is_ident_cont '_');
  Alcotest.(check bool)
    "is_template_delim open"
    true
    (Lexer.is_template_delim '{');
  Alcotest.(check bool)
    "is_template_delim close"
    true
    (Lexer.is_template_delim '}');
  Alcotest.(check bool)
    "is_template_delim other"
    false
    (Lexer.is_template_delim '[')

let test_state_functions () =
  let state, _ = make_test_state "hello" in
  Alcotest.(check (option char))
    "peek_char first"
    (Some 'h')
    (Lexer.peek_char state);
  let state2 = Lexer.advance state in
  Alcotest.(check int) "advance position" 1 state2.pos;
  Alcotest.(check (option char))
    "peek_char after advance"
    (Some 'e')
    (Lexer.peek_char state2)

let test_numbers_decimal () =
  let state, _ = make_test_state "42" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "decimal simple" "42" content;
  Alcotest.(check int) "decimal position" 2 new_state.pos

let test_numbers_hex () =
  let state, _ = make_test_state "0xFF" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "hex uppercase" "0xFF" content;
  Alcotest.(check int) "hex position" 4 new_state.pos

let test_numbers_binary () =
  let state, _ = make_test_state "0b1010" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "binary" "0b1010" content;
  Alcotest.(check int) "binary position" 6 new_state.pos

let test_numbers_octal () =
  let state, _ = make_test_state "0o777" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "octal" "0o777" content;
  Alcotest.(check int) "octal position" 5 new_state.pos

let test_numbers_float () =
  let state, _ = make_test_state "3.14159" in
  let new_state, content, _span = Lexer.scan_number state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "float" "3.14159" content;
  Alcotest.(check int) "float position" 7 new_state.pos

let test_numbers_errors_incomplete_hex () =
  let state, _ = make_test_state "0x" in
  let new_state, _content, _span = Lexer.scan_number state in
  Alcotest.(check bool)
    "hex incomplete has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_strings_basic () =
  let state, interner = make_test_state "\"hello\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "hello";
  Alcotest.(check int) "string basic position" 7 new_state.pos

let test_strings_empty () =
  let state, interner = make_test_state "\"\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "";
  Alcotest.(check int) "string empty position" 2 new_state.pos

let test_strings_escape_newline () =
  let state, interner = make_test_state "\"hello\\nworld\"" in
  let new_state, name, _span = Lexer.scan_string state in
  check_no_errors new_state.diags;
  check_string_value interner name "hello\nworld"

let test_strings_unterminated () =
  let state, _ = make_test_state "\"hello" in
  let new_state, _content, _span = Lexer.scan_string state in
  Alcotest.(check bool)
    "unterminated has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_basic () =
  let state, _ = make_test_state "'a'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  check_no_errors new_state.diags;
  Alcotest.(check char) "rune basic" 'a' char_val;
  Alcotest.(check int) "rune basic position" 3 new_state.pos

let test_runes_escape_newline () =
  let state, _ = make_test_state "'\\n'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  let has_errors = Diagnostic.has_errors new_state.diags in
  if has_errors then
    Printf.printf "Errors found in rune escape: %b\n" has_errors;
  Alcotest.(check bool) "rune escape has no errors" false has_errors;
  if not has_errors then
    Alcotest.(check char) "rune escape newline" '\n' char_val

let test_runes_empty () =
  let state, _ = make_test_state "''" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  Alcotest.(check bool)
    "empty rune has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_multichar () =
  let state, _ = make_test_state "'ab'" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  Alcotest.(check bool)
    "multichar rune has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_runes_invalid_escape () =
  let state, _ = make_test_state "'\\q'" in
  let new_state, _char_val, _span = Lexer.scan_rune state in
  Alcotest.(check bool)
    "invalid escape has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_templates_basic () =
  let state, interner = make_test_state "$\"hello world\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello world"
  | _ -> Alcotest.fail "Expected template token"

let test_templates_escape () =
  let state, interner = make_test_state "$\"hello\\nworld\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello\nworld"
  | _ -> Alcotest.fail "Expected template token"

let test_identifiers_basic () =
  let state, interner = make_test_state "hello_world" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  check_identifier_value interner name "hello_world";
  Alcotest.(check int) "identifier basic position" 11 new_state.pos

let test_identifiers_single_letter () =
  let state, interner = make_test_state "x" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  check_identifier_value interner name "x";
  Alcotest.(check int) "identifier single position" 1 new_state.pos

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

let test_keywords_def () =
  let state, interner = make_test_state "def" in
  let new_state, name, _span = Lexer.scan_ident state in
  check_no_errors new_state.diags;
  let token =
    Token.lookup_keyword interner (get_interned_string interner name)
  in
  assert (token = Token.KwDef)

let test_symbols_assignment () =
  let state, _ = make_test_state "<-" in
  let new_state, token, _span = Lexer.scan_symbol state in
  check_no_errors new_state.diags;
  assert (token = Token.LtMinus);
  Alcotest.(check int) "assignment position" 2 new_state.pos

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
  Alcotest.(check int) "dollar standalone position" 1 new_state.pos

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
  Alcotest.(check string) "line comment content" " comment" content;
  Alcotest.(check int) "line comment position" 10 new_state.pos

let test_comments_block () =
  let state, _ = make_test_state "/* comment */" in
  let new_state, content, _span = Lexer.scan_block_comment state in
  check_no_errors new_state.diags;
  Alcotest.(check string) "block comment content" " comment " content;
  Alcotest.(check int) "block comment position" 13 new_state.pos

let test_whitespace_space () =
  let state, _ = make_test_state "   " in
  let new_state, _span = Lexer.scan_whitespace state in
  Alcotest.(check int) "whitespace position" 3 new_state.pos

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
  Alcotest.(check bool)
    "unicode empty escape has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_missing_brace () =
  let state, _ = make_test_state "\"\\u00A9\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  Alcotest.(check bool)
    "unicode missing brace has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_invalid_hex () =
  let state, _ = make_test_state "\"\\u{GGGG}\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  Alcotest.(check bool)
    "unicode invalid hex has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_exceed_small_limit () =
  let state, _ = make_test_state "\"\\u{110000}\"" in
  let new_state, _name, _span = Lexer.scan_string state in
  Alcotest.(check bool)
    "unicode exceeds small limit has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_incomplete () =
  let state, _ = make_test_state "\"\\u{" in
  let new_state, _name, _span = Lexer.scan_string state in
  Alcotest.(check bool)
    "unicode incomplete has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_unicode_escape_in_rune () =
  let state, _ = make_test_state "'\\u{41}'" in
  let new_state, char_val, _span = Lexer.scan_rune state in
  check_no_errors new_state.diags;
  Alcotest.(check char) "unicode rune" 'A' char_val

let test_template_extra_closing_brace () =
  let state, _ = make_test_state "$\"hello}world\"" in
  let new_state, _token, _span = Lexer.scan_template_or_dollar state in
  Alcotest.(check bool)
    "template extra closing brace has errors"
    true
    (Diagnostic.has_errors new_state.diags)

let test_template_unclosed_opening_brace () =
  let state, _ = make_test_state "$\"hello{world\"" in
  let new_state, _token, _span = Lexer.scan_template_or_dollar state in
  Alcotest.(check bool)
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
  | _ -> Alcotest.fail "Expected template token"

let test_template_empty_braces () =
  let state, interner = make_test_state "$\"hello{}world\"" in
  let new_state, token, _span = Lexer.scan_template_or_dollar state in
  check_no_errors new_state.diags;
  match token with
  | Token.LitTemplate name -> check_string_value interner name "hello{}world"
  | _ -> Alcotest.fail "Expected template token"

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
  Alcotest.(check bool)
    "utf8 invalid continuation has errors"
    true
    (Diagnostic.has_errors diags)

let test_utf8_incomplete_sequence () =
  let interner = Interner.create () in
  let _tokens, diags = Lexer.tokenize "\xc2" 42 interner in
  Alcotest.(check bool)
    "utf8 incomplete sequence has errors"
    true
    (Diagnostic.has_errors diags)

let test_utf8_invalid_start_byte () =
  let interner = Interner.create () in
  let _tokens, diags = Lexer.tokenize "\xfc" 42 interner in
  Alcotest.(check bool)
    "utf8 invalid start byte has errors"
    true
    (Diagnostic.has_errors diags)

let () =
  let open Alcotest in
  run
    "Lexer Unit Tests"
    [
      ( "character_predicates"
      , [ test_case "character predicates" `Quick test_character_predicates ] )
    ; ( "state_functions"
      , [ test_case "state functions" `Quick test_state_functions ] )
    ; ( "numbers"
      , [
          test_case "decimal" `Quick test_numbers_decimal
        ; test_case "hex" `Quick test_numbers_hex
        ; test_case "binary" `Quick test_numbers_binary
        ; test_case "octal" `Quick test_numbers_octal
        ; test_case "float" `Quick test_numbers_float
        ; test_case
            "incomplete hex error"
            `Quick
            test_numbers_errors_incomplete_hex
        ] )
    ; ( "strings"
      , [
          test_case "basic" `Quick test_strings_basic
        ; test_case "empty" `Quick test_strings_empty
        ; test_case "escape newline" `Quick test_strings_escape_newline
        ; test_case "unterminated" `Quick test_strings_unterminated
        ] )
    ; ( "runes"
      , [
          test_case "basic" `Quick test_runes_basic
        ; test_case "escape newline" `Quick test_runes_escape_newline
        ; test_case "empty" `Quick test_runes_empty
        ; test_case "multichar" `Quick test_runes_multichar
        ; test_case "invalid escape" `Quick test_runes_invalid_escape
        ] )
    ; ( "unicode_escapes"
      , [
          test_case "valid small unicode" `Quick test_unicode_escape_valid_small
        ; test_case "valid big unicode" `Quick test_unicode_escape_valid_big
        ; test_case "empty unicode escape" `Quick test_unicode_escape_empty
        ; test_case "missing brace" `Quick test_unicode_escape_missing_brace
        ; test_case "invalid hex digits" `Quick test_unicode_escape_invalid_hex
        ; test_case
            "exceed small limit"
            `Quick
            test_unicode_escape_exceed_small_limit
        ; test_case "incomplete escape" `Quick test_unicode_escape_incomplete
        ; test_case "unicode in rune" `Quick test_unicode_escape_in_rune
        ] )
    ; ( "template_validation"
      , [
          test_case
            "extra closing brace"
            `Quick
            test_template_extra_closing_brace
        ; test_case
            "unclosed opening brace"
            `Quick
            test_template_unclosed_opening_brace
        ; test_case "nested braces" `Quick test_template_nested_braces
        ; test_case "empty braces" `Quick test_template_empty_braces
        ] )
    ; ( "utf8_validation"
      , [
          test_case "valid 2-byte" `Quick test_utf8_valid_2byte
        ; test_case "valid 3-byte" `Quick test_utf8_valid_3byte
        ; test_case "valid 4-byte" `Quick test_utf8_valid_4byte
        ; test_case "invalid continuation" `Quick test_utf8_invalid_continuation
        ; test_case "incomplete sequence" `Quick test_utf8_incomplete_sequence
        ; test_case "invalid start byte" `Quick test_utf8_invalid_start_byte
        ] )
    ; ( "identifiers"
      , [
          test_case "basic" `Quick test_identifiers_basic
        ; test_case "single letter" `Quick test_identifiers_single_letter
        ] )
    ; ( "keywords"
      , [
          test_case "val" `Quick test_keywords_val
        ; test_case "var" `Quick test_keywords_var
        ; test_case "def" `Quick test_keywords_def
        ] )
    ; ( "operators"
      , [
          test_case "assignment" `Quick test_symbols_assignment
        ; test_case "equals" `Quick test_symbols_equals
        ; test_case "plus" `Quick test_symbols_plus
        ; test_case "dollar" `Quick test_symbols_dollar
        ] )
    ; ( "templates_and_dollar"
      , [
          test_case "basic" `Quick test_templates_basic
        ; test_case "escape newline" `Quick test_templates_escape
        ; test_case "dollar standalone" `Quick test_dollar_standalone
        ] )
    ; ("punctuation", [ test_case "parentheses" `Quick test_parentheses ])
    ; ( "comments"
      , [
          test_case "line" `Quick test_comments_line
        ; test_case "block" `Quick test_comments_block
        ] )
    ; ("whitespace", [ test_case "space" `Quick test_whitespace_space ])
    ]
