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
    test_case "lowercase letter" `Quick (fun () ->
      (check bool) "is_letter 'a'" true (Lexer.is_letter 'a'))
  ; test_case "uppercase letter" `Quick (fun () ->
      (check bool) "is_letter 'Z'" true (Lexer.is_letter 'Z'))
  ; test_case "non-letter" `Quick (fun () ->
      (check bool) "is_letter '5'" false (Lexer.is_letter '5'))
  ]

let test_is_digit () =
  [
    test_case "digit" `Quick (fun () ->
      (check bool) "is_digit '7'" true (Lexer.is_digit '7'))
  ; test_case "non-digit" `Quick (fun () ->
      (check bool) "is_digit 'a'" false (Lexer.is_digit 'a'))
  ]

let test_is_space () =
  [
    test_case "space" `Quick (fun () ->
      (check bool) "is_space ' '" true (Lexer.is_space ' '))
  ; test_case "tab" `Quick (fun () ->
      (check bool) "is_space '\\t'" true (Lexer.is_space '\t'))
  ; test_case "newline" `Quick (fun () ->
      (check bool) "is_space '\\n'" false (Lexer.is_space '\n'))
  ]

let test_is_xdigit () =
  [
    test_case "digit" `Quick (fun () ->
      (check bool) "is_xdigit '5'" true (Lexer.is_xdigit '5'))
  ; test_case "hex lowercase" `Quick (fun () ->
      (check bool) "is_xdigit 'a'" true (Lexer.is_xdigit 'a'))
  ; test_case "hex uppercase" `Quick (fun () ->
      (check bool) "is_xdigit 'F'" true (Lexer.is_xdigit 'F'))
  ; test_case "invalid hex" `Quick (fun () ->
      (check bool) "is_xdigit 'G'" false (Lexer.is_xdigit 'G'))
  ]

let test_is_ident_start () =
  [
    test_case "letter" `Quick (fun () ->
      (check bool) "is_ident_start 'b'" true (Lexer.is_ident_start 'b'))
  ; test_case "underscore" `Quick (fun () ->
      (check bool) "is_ident_start '_'" true (Lexer.is_ident_start '_'))
  ; test_case "digit" `Quick (fun () ->
      (check bool) "is_ident_start '9'" false (Lexer.is_ident_start '9'))
  ]

let test_is_ident_cont () =
  [
    test_case "letter" `Quick (fun () ->
      (check bool) "is_ident_cont 'c'" true (Lexer.is_ident_cont 'c'))
  ; test_case "digit" `Quick (fun () ->
      (check bool) "is_ident_cont '8'" true (Lexer.is_ident_cont '8'))
  ; test_case "underscore" `Quick (fun () ->
      (check bool) "is_ident_cont '_'" true (Lexer.is_ident_cont '_'))
  ]

let test_state_functions () =
  [
    test_case "peek and advance" `Quick (fun () ->
      let state = make_test_state "hello" in
      (check char) "peek first" 'h' (Lexer.peek state);
      Lexer.adv state;
      (check int) "adv position" 1 state.pos;
      (check char) "peek after advance" 'e' (Lexer.peek state))
  ; test_case "peek_n" `Quick (fun () ->
      let state = make_test_state "hello" in
      (check char) "peek_n 0" 'h' (Lexer.peek_n state 0);
      (check char) "peek_n 1" 'e' (Lexer.peek_n state 1);
      (check char) "peek_n 2" 'l' (Lexer.peek_n state 2))
  ; test_case "peek beyond bounds" `Quick (fun () ->
      let state = make_test_state "hi" in
      (check char) "peek_n 1" 'i' (Lexer.peek_n state 1);
      (check char) "peek_n 2" '\000' (Lexer.peek_n state 2);
      (check char) "peek_n 10" '\000' (Lexer.peek_n state 10))
  ; test_case "adv_n" `Quick (fun () ->
      let state = make_test_state "hello" in
      Lexer.adv_n state 3;
      (check int) "adv_n position" 3 state.pos;
      (check char) "peek after adv_n" 'l' (Lexer.peek state))
  ]

let test_lookup_escape () =
  [
    test_case "newline" `Quick (fun () ->
      (check char) "lookup_escape 'n'" '\n' (Lexer.lookup_escape 'n'))
  ; test_case "tab" `Quick (fun () ->
      (check char) "lookup_escape 't'" '\t' (Lexer.lookup_escape 't'))
  ; test_case "return" `Quick (fun () ->
      (check char) "lookup_escape 'r'" '\r' (Lexer.lookup_escape 'r'))
  ; test_case "backslash" `Quick (fun () ->
      (check char) "lookup_escape '\\\\'" '\\' (Lexer.lookup_escape '\\'))
  ; test_case "double quote" `Quick (fun () ->
      (check char) "lookup_escape '\"'" '\"' (Lexer.lookup_escape '\"'))
  ; test_case "single quote" `Quick (fun () ->
      (check char) "lookup_escape '\\''" '\'' (Lexer.lookup_escape '\''))
  ; test_case "null" `Quick (fun () ->
      (check char) "lookup_escape '0'" '\000' (Lexer.lookup_escape '0'))
  ; test_case "passthrough" `Quick (fun () ->
      (check char) "lookup_escape 'x'" 'x' (Lexer.lookup_escape 'x'))
  ]

let test_tokenize () =
  [
    test_case "empty input" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "" 42 interner in
      check_no_errors diags;
      (check int) "token count" 1 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _) ] -> ()
      | _ -> Alcotest.fail "Expected EOF token")
  ; test_case "single number" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "42" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _); (Token.LitNumber "42", _) ] -> ()
      | _ -> Alcotest.fail "Expected number and EOF")
  ; test_case "single identifier" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "x" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _); (Token.Ident name, _) ] ->
        check_interned_string interner name "x"
      | _ -> Alcotest.fail "Expected identifier and EOF")
  ; test_case "string literal" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "\"hello\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _); (Token.LitString name, _) ] ->
        check_interned_string interner name "hello"
      | _ -> Alcotest.fail "Expected string and EOF")
  ; test_case "rune literal" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "'a'" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _); (Token.LitRune 'a', _) ] -> ()
      | _ -> Alcotest.fail "Expected rune and EOF")
  ; test_case "template string" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "$\"hello\"" 42 interner in
      check_no_errors diags;
      (check int) "token count" 2 (List.length tokens);
      match List.rev tokens with
      | [ (Token.EOF, _); (Token.LitTemplate name, _) ] ->
        check_interned_string interner name "hello"
      | _ -> Alcotest.fail "Expected template and EOF")
  ; test_case "keyword recognition" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "val var fn if else" 42 interner in
      check_no_errors diags;
      (check int) "token count" 6 (List.length tokens);
      let expected_tokens =
        [
          Token.KwVal
        ; Token.KwVar
        ; Token.KwFn
        ; Token.KwIf
        ; Token.KwElse
        ; Token.EOF
        ]
      in
      let actual_tokens = List.map fst (List.rev tokens) in
      List.iter2
        (fun expected actual ->
          (check bool) "keyword match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "arithmetic operators" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "+-*/" 42 interner in
      check_no_errors diags;
      (check int) "token count" 5 (List.length tokens);
      let expected_tokens =
        [ Token.Plus; Token.Minus; Token.Star; Token.Slash; Token.EOF ]
      in
      let actual_tokens = List.map fst (List.rev tokens) in
      List.iter2
        (fun expected actual ->
          (check bool) "operator match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "delimiters" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags = Lexer.tokenize "()[]{},:;" 42 interner in
      check_no_errors diags;
      (check int) "token count" 8 (List.length tokens);
      let expected_tokens =
        [
          Token.LParen
        ; Token.RParen
        ; Token.LBrack
        ; Token.RBrack
        ; Token.LBrace
        ; Token.RBrace
        ; Token.Comma
        ; Token.Semi
        ; Token.Colon
        ; Token.EOF
        ]
      in
      let actual_tokens = List.map fst (List.rev tokens) in
      List.iter2
        (fun expected actual ->
          (check bool) "delimiter match" true (expected = actual))
        expected_tokens
        actual_tokens)
  ; test_case "complex expression" `Quick (fun () ->
      let interner = Interner.create () in
      let tokens, diags =
        Lexer.tokenize "result := func(x, y) + 42" 42 interner
      in
      check_no_errors diags;
      (check int) "complex token count" 9 (List.length tokens);
      let expected_tokens =
        [
          Token.Ident (Interner.empty_name interner)
        ; Token.ColonEq
        ; Token.Ident (Interner.empty_name interner)
        ; Token.LParen
        ; Token.Ident (Interner.empty_name interner)
        ; Token.Comma
        ; Token.Ident (Interner.empty_name interner)
        ; Token.RParen
        ; Token.Plus
        ; Token.LitNumber "42"
        ; Token.EOF
        ]
      in
      let actual_tokens = List.map fst (List.rev tokens) in
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
