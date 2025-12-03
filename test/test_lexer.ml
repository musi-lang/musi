open Basic
open Lex
open Alcotest

let make_test_state source =
  let interner = Interner.create () in
  let file_id = 42 in
  Lexer.mk_state source file_id interner

let check_no_errors diags =
  (check bool) "no errors" false (Diagnostic.has_errors diags)

let get_interned_string interner name = Interner.lookup interner name

let check_interned_string interner name expected =
  let actual = get_interned_string interner name in
  (check string) "interned string" expected actual

let test_is_letter () =
  [
    test_case "is_letter: lowercase a" `Quick (fun () ->
      (check bool) "is_letter 'a'" true (Lexer.is_letter 'a'))
  ; test_case "is_letter: uppercase Z" `Quick (fun () ->
      (check bool) "is_letter 'Z'" true (Lexer.is_letter 'Z'))
  ; test_case "is_letter: digit 5 invalid" `Quick (fun () ->
      (check bool) "is_letter '5'" false (Lexer.is_letter '5'))
  ]

let test_is_digit () =
  [
    test_case "is_digit: numeric digit 7" `Quick (fun () ->
      (check bool) "is_digit '7'" true (Lexer.is_digit '7'))
  ; test_case "is_digit: letter a invalid" `Quick (fun () ->
      (check bool) "is_digit 'a'" false (Lexer.is_digit 'a'))
  ]

let test_is_space () =
  [
    test_case "is_space: space character" `Quick (fun () ->
      (check bool) "is_space ' '" true (Lexer.is_space ' '))
  ; test_case "is_space: tab character" `Quick (fun () ->
      (check bool) "is_space '\\t'" true (Lexer.is_space '\t'))
  ; test_case "is_space: newline invalid" `Quick (fun () ->
      (check bool) "is_space '\\n'" false (Lexer.is_space '\n'))
  ]

let test_is_xdigit () =
  [
    test_case "is_xdigit: numeric digit 5" `Quick (fun () ->
      (check bool) "is_xdigit '5'" true (Lexer.is_xdigit '5'))
  ; test_case "is_xdigit: hex lowercase a" `Quick (fun () ->
      (check bool) "is_xdigit 'a'" true (Lexer.is_xdigit 'a'))
  ; test_case "is_xdigit: hex uppercase F" `Quick (fun () ->
      (check bool) "is_xdigit 'F'" true (Lexer.is_xdigit 'F'))
  ; test_case "is_xdigit: letter G invalid" `Quick (fun () ->
      (check bool) "is_xdigit 'G'" false (Lexer.is_xdigit 'G'))
  ]

let test_is_ident_start () =
  [
    test_case "is_ident_start: letter b valid" `Quick (fun () ->
      (check bool) "is_ident_start 'b'" true (Lexer.is_ident_start 'b'))
  ; test_case "is_ident_start: underscore valid" `Quick (fun () ->
      (check bool) "is_ident_start '_'" true (Lexer.is_ident_start '_'))
  ; test_case "is_ident_start: digit 9 invalid" `Quick (fun () ->
      (check bool) "is_ident_start '9'" false (Lexer.is_ident_start '9'))
  ]

let test_is_ident_cont () =
  [
    test_case "is_ident_cont: letter c valid" `Quick (fun () ->
      (check bool) "is_ident_cont 'c'" true (Lexer.is_ident_cont 'c'))
  ; test_case "is_ident_cont: digit 8 valid" `Quick (fun () ->
      (check bool) "is_ident_cont '8'" true (Lexer.is_ident_cont '8'))
  ; test_case "is_ident_cont: underscore valid" `Quick (fun () ->
      (check bool) "is_ident_cont '_'" true (Lexer.is_ident_cont '_'))
  ]

let test_state_functions () =
  [
    test_case "state: peek then advance" `Quick (fun () ->
      let state = make_test_state "hello" in
      (check char) "peek first" 'h' (Lexer.peek state);
      Lexer.advance state;
      (check int) "advance position" 1 state.pos;
      (check char) "peek after advance" 'e' (Lexer.peek state))
  ; test_case "state: peek_n multiple chars" `Quick (fun () ->
      let state = make_test_state "hello" in
      (check char) "peek_n 0" 'h' (Lexer.peek_n state 0);
      (check char) "peek_n 1" 'e' (Lexer.peek_n state 1);
      (check char) "peek_n 2" 'l' (Lexer.peek_n state 2))
  ; test_case "state: peek beyond string bounds" `Quick (fun () ->
      let state = make_test_state "hi" in
      (check char) "peek_n 1" 'i' (Lexer.peek_n state 1);
      (check char) "peek_n 2" '\000' (Lexer.peek_n state 2);
      (check char) "peek_n 10" '\000' (Lexer.peek_n state 10))
  ; test_case "state: advance_n multiple chars" `Quick (fun () ->
      let state = make_test_state "hello" in
      Lexer.advance_n state 3;
      (check int) "advance_n position" 3 state.pos;
      (check char) "peek after advance_n" 'l' (Lexer.peek state))
  ]

let test_lookup_escape () =
  [
    test_case "escape: newline \\n" `Quick (fun () ->
      (check char) "lookup_escape 'n'" '\n' (Lexer.lookup_escape 'n'))
  ; test_case "escape: tab \\t" `Quick (fun () ->
      (check char) "lookup_escape 't'" '\t' (Lexer.lookup_escape 't'))
  ; test_case "escape: carriage return \\r" `Quick (fun () ->
      (check char) "lookup_escape 'r'" '\r' (Lexer.lookup_escape 'r'))
  ; test_case "escape: backslash \\\\" `Quick (fun () ->
      (check char) "lookup_escape '\\\\'" '\\' (Lexer.lookup_escape '\\'))
  ; test_case "escape: double quote \\\"" `Quick (fun () ->
      (check char) "lookup_escape '\"'" '\"' (Lexer.lookup_escape '\"'))
  ; test_case "escape: single quote \\'" `Quick (fun () ->
      (check char) "lookup_escape '\\''" '\'' (Lexer.lookup_escape '\''))
  ; test_case "escape: null \\0" `Quick (fun () ->
      (check char) "lookup_escape '0'" '\000' (Lexer.lookup_escape '0'))
  ; test_case "escape: passthrough unknown" `Quick (fun () ->
      (check char) "lookup_escape 'x'" 'x' (Lexer.lookup_escape 'x'))
  ]

let test_tokenize () =
  [
    test_case "tokenize: empty input" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "" 42 interner in
      check_no_errors diags;
      (check int) "token count" 1 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _) ] -> ()
      | _ -> Alcotest.fail "Expected EOF token")
  ; test_case "tokenize: number literal 42" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "42" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitNumber "42", _) ] -> ()
      | _ -> Alcotest.fail "Expected number and EOF")
  ; test_case "tokenize: identifier x" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "x" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.Ident name, _) ] ->
        check_interned_string interner name "x"
      | _ -> Alcotest.fail "Expected identifier and EOF")
  ; test_case "tokenize: string \"hello\"" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "\"hello\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "hello"
      | _ -> Alcotest.fail "Expected string and EOF")
  ; test_case "tokenize: rune 'a'" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "'a'" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitRune 'a', _) ] -> ()
      | _ -> Alcotest.fail "Expected rune and EOF")
  ; test_case "tokenize: template $\"hello\"" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "$\"hello\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitTemplate name, _) ] ->
        check_interned_string interner name "hello"
      | _ -> Alcotest.fail "Expected template and EOF")
  ; test_case "tokenize: unicode escape in string" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "\"\\u{41}\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "A"
      | _ -> Alcotest.fail "Expected unicode string and EOF")
  ; test_case "tokenize: unicode escape in rune" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "'\\u{42}'" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitRune 'B', _) ] -> ()
      | _ -> Alcotest.fail "Expected unicode rune and EOF")
  ; test_case "tokenize: mixed escape sequences" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "\"\\n\\t\\u{43}\\0\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "\n\tC\000"
      | _ -> Alcotest.fail "Expected mixed escapes string and EOF")
  ; test_case "tokenize: hex escape in string" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "\"\\x{41}\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "A"
      | _ -> Alcotest.fail "Expected hex string and EOF")
  ; test_case "tokenize: hex escape in rune" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "'\\x{42}'" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitRune 'B', _) ] -> ()
      | _ -> Alcotest.fail "Expected hex rune and EOF")
  ; test_case "tokenize: all escape types combined" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags =
        Lexer.tokenize "\"\\n\\u{41}\\x{42}\\t\"" 42 interner
      in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "\nAB\t"
      | _ -> Alcotest.fail "Expected combined escapes string and EOF")
  ; test_case "tokenize: keyword recognition" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "val var fn if else" 42 interner in
      check_no_errors diags;
      (check int) "token count" 6 (List.length tokens);
      let expected_tokens =
        [
          Token.EOF
        ; Token.KwVal
        ; Token.KwVar
        ; Token.KwFn
        ; Token.KwIf
        ; Token.KwElse
        ]
      in
      let actual_tokens = List.map fst tokens in
      List.iter2
        (fun expected actual ->
          (check bool) "keyword match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "tokenize: arithmetic operators" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "+-*/" 42 interner in
      check_no_errors diags;
      (check int) "token count" 5 (List.length tokens);
      let expected_tokens =
        [ Token.EOF; Token.Plus; Token.Minus; Token.Star; Token.Slash ]
      in
      let actual_tokens = List.map fst tokens in
      List.iter2
        (fun expected actual ->
          (check bool) "operator match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "tokenize: delimiters" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "()[]{},:;" 42 interner in
      check_no_errors diags;
      (check int) "token count" 10 (List.length tokens);
      let expected_tokens =
        [
          Token.EOF
        ; Token.LParen
        ; Token.RParen
        ; Token.LBrack
        ; Token.RBrack
        ; Token.LBrace
        ; Token.RBrace
        ; Token.Comma
        ; Token.Colon
        ; Token.Semi
        ]
      in
      let actual_tokens = List.map fst tokens in
      List.iter2
        (fun expected actual ->
          (check bool) "delimiter match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "tokenize: thousand separators in decimal" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "1_000_000" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitNumber "1_000_000", _) ] -> ()
      | _ -> Alcotest.fail "Expected thousand separator number and EOF")
  ; test_case "tokenize: hexadecimal numbers" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens1, diags1 = Lexer.tokenize "0xFF" 42 interner in
      let tokens2, diags2 = Lexer.tokenize "0x1A_2B" 42 interner in
      check_no_errors diags1;
      check_no_errors diags2;
      (check int) "hex token count" 2 (List.length tokens1);
      (check int) "hex with separator token count" 2 (List.length tokens2);
      match (tokens1, tokens2) with
      | ( [ (Token.EOF, _); (Token.LitNumber "0xFF", _) ]
        , [ (Token.EOF, _); (Token.LitNumber "0x1A_2B", _) ] ) ->
        ()
      | _ -> Alcotest.fail "Expected hex numbers and EOF")
  ; test_case "tokenize: octal numbers" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens1, diags1 = Lexer.tokenize "0o755" 42 interner in
      let tokens2, diags2 = Lexer.tokenize "0O7_5_5" 42 interner in
      check_no_errors diags1;
      check_no_errors diags2;
      (check int) "octal token count" 2 (List.length tokens1);
      (check int) "octal with separator token count" 2 (List.length tokens2);
      match (tokens1, tokens2) with
      | ( [ (Token.EOF, _); (Token.LitNumber "0o755", _) ]
        , [ (Token.EOF, _); (Token.LitNumber "0O7_5_5", _) ] ) ->
        ()
      | _ -> Alcotest.fail "Expected octal numbers and EOF")
  ; test_case "tokenize: binary numbers" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens1, diags1 = Lexer.tokenize "0b1010" 42 interner in
      let tokens2, diags2 = Lexer.tokenize "0B1_0_1_0" 42 interner in
      check_no_errors diags1;
      check_no_errors diags2;
      (check int) "binary token count" 2 (List.length tokens1);
      (check int) "binary with separator token count" 2 (List.length tokens2);
      match (tokens1, tokens2) with
      | ( [ (Token.EOF, _); (Token.LitNumber "0b1010", _) ]
        , [ (Token.EOF, _); (Token.LitNumber "0B1_0_1_0", _) ] ) ->
        ()
      | _ -> Alcotest.fail "Expected binary numbers and EOF")
  ; test_case "tokenize: decimal with fraction and separators" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "1_234.567_890" 42 interner in
      check_no_errors diags;
      (check int) "decimal fraction token count" 2 (List.length tokens);
      match tokens with
      | [ (Token.EOF, _); (Token.LitNumber "1_234.567_890", _) ] -> ()
      | _ -> Alcotest.fail "Expected decimal fraction with separators and EOF")
  ; test_case "tokenize: complex expression" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags =
        Lexer.tokenize "result := func(x, y) + 42" 42 interner
      in
      check_no_errors diags;
      (check int) "complex token count" 11 (List.length tokens);
      let expected_tokens =
        [
          Token.EOF
        ; Token.Ident (Interner.empty_name interner)
        ; Token.ColonEq
        ; Token.Ident (Interner.empty_name interner)
        ; Token.LParen
        ; Token.Ident (Interner.empty_name interner)
        ; Token.Comma
        ; Token.Ident (Interner.empty_name interner)
        ; Token.RParen
        ; Token.Plus
        ; Token.LitNumber "42"
        ]
      in
      let actual_tokens = List.map fst tokens in
      List.iter2
        (fun expected actual ->
          match (expected, actual) with
          | Token.Ident _, Token.Ident _ -> ()
          | Token.LitNumber _, Token.LitNumber _ -> ()
          | e, a -> (check bool) "token type match" true (e = a))
        expected_tokens
        actual_tokens)
  ]

let test_cases =
  test_is_letter () @ test_is_digit () @ test_is_space () @ test_is_xdigit ()
  @ test_is_ident_start () @ test_is_ident_cont () @ test_state_functions ()
  @ test_lookup_escape () @ test_tokenize ()

let suite = [ ("lexer", test_cases) ]
let () = run "lexer" suite
