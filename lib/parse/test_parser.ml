open Basic
open Lex
open Alcotest
open Parse.Parser
open Parse.Node

let make_test_state tokens =
  let interner = Interner.create () in
  let state = mk_state tokens interner in
  (state, interner)

let check_no_errors diags =
  (check bool) "no errors" false (Diagnostic.has_errors diags)

let test_parser_state_creation () =
  let tokens = [ (Token.KwVal, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  (check int) "initial position" 0 state.pos;
  (check int) "tokens length" 1 (Array.length state.tokens);
  let curr_token, _ = peek state in
  (check bool) "current token is val" true (curr_token = Token.KwVal);
  check_no_errors state.diags

let test_parser_advance () =
  let tokens = [ (Token.KwVal, Span.dummy); (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let initial_token, _ = peek state in
  (check bool) "initial token" true (initial_token = Token.KwVal);
  let state_after = advance state in
  let next_token, _ = peek state_after in
  (check bool)
    "advanced token"
    true
    (match next_token with Token.Ident _ -> true | _ -> false);
  (check int) "position after advance" 1 state_after.pos

let test_parser_skip_trivia () =
  let tokens =
    [
      (Token.Whitespace, Span.dummy)
    ; (Token.Newline, Span.dummy)
    ; (Token.Comment " test", Span.dummy)
    ; (Token.KwVal, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after = skip_trivia state in
  let curr_token, _ = peek state_after in
  (check bool) "skipped to val" true (curr_token = Token.KwVal)

let test_parser_expect () =
  let tokens = [ (Token.KwVal, Span.dummy); (Token.Semi, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, _ = expect state Token.KwVal in
  (check bool)
    "expect success"
    true
    (match fst (peek state_after) with Token.Semi -> true | _ -> false)

let test_parser_expect_error () =
  let tokens = [ (Token.KwVar, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, _ = expect state Token.KwVal in
  (check bool)
    "expect error has diags"
    true
    (Diagnostic.has_errors state_after.diags)

let test_parse_pattern_binding () =
  let tokens = [ (Token.KwVal, Span.dummy); (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed pat binding"
    true
    (match pattern.kind with
    | PatBind { mutable_ = false; name = _ } -> true
    | _ -> false)

let test_parse_pattern_constructor () =
  let tokens =
    [
      (Token.Ident 0, Span.dummy)
    ; (Token.LParen, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.RParen, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed pat constructor"
    true
    (match pattern.kind with PatCtor (_, [ _ ]) -> true | _ -> false)

let test_parse_pattern_literal () =
  let tokens = [ (Token.LitNumber "42", Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed pat literal"
    true
    (match pattern.kind with PatLiteral (LitInt "42") -> true | _ -> false)

let test_parse_type_identifier () =
  let tokens = [ (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, typ = parse_typ_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed type identifier"
    true
    (match typ.kind with TypExprIdent _ -> true | _ -> false)

let test_parse_type_function () =
  let tokens =
    [
      (Token.KwDef, Span.dummy)
    ; (Token.LParen, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.RParen, Span.dummy)
    ; (Token.MinusGt, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, typ = parse_typ_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed function type"
    true
    (match typ.kind with TypExprFunc ([ _ ], Some _) -> true | _ -> false)

let test_parse_type_record () =
  let tokens =
    [
      (Token.LBrace, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.Colon, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.Comma, Span.dummy)
    ; (Token.Ident 2, Span.dummy)
    ; (Token.Colon, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, typ = parse_typ_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed record type"
    true
    (match typ.kind with TypExprRecord [ _; _ ] -> true | _ -> false)

let test_parse_statement_val_binding () =
  let tokens =
    [
      (Token.KwVal, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.ColonEq, Span.dummy)
    ; (Token.LitNumber "42", Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed val binding"
    true
    (match stmt.kind with
    | StmtBinding { mutable_ = false; name = _; typ = None; value = _ } -> true
    | _ -> false)

let test_parse_statement_var_binding () =
  let tokens =
    [
      (Token.KwVar, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.Colon, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.ColonEq, Span.dummy)
    ; (Token.LitNumber "10", Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed var binding"
    true
    (match stmt.kind with
    | StmtBinding { mutable_ = true; name = _; typ = Some _; value = _ } -> true
    | _ -> false)

let test_parse_statement_assignment () =
  let tokens =
    [
      (Token.Ident 0, Span.dummy)
    ; (Token.LtMinus, Span.dummy)
    ; (Token.LitNumber "20", Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed assignment"
    true
    (match stmt.kind with StmtAssign (_, _) -> true | _ -> false)

let test_parse_pattern_wild () =
  let tokens = [ (Token.Minus, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed wildcard"
    true
    (match pattern.kind with PatWild -> true | _ -> false)

let test_parse_statement_missing_semicolon () =
  let tokens =
    [
      (Token.KwVal, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.ColonEq, Span.dummy)
    ; (Token.LitNumber "1", Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, _ = parse_stmt state in
  (check bool)
    "missing semicolon has errors"
    true
    (Diagnostic.has_errors state_after.diags)

let test_parse_type_missing_closing_brace () =
  let tokens =
    [
      (Token.LBrace, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.Colon, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, _ = parse_typ_expr state in
  (check bool)
    "missing brace has errors"
    true
    (Diagnostic.has_errors state_after.diags)

let () =
  let open Alcotest in
  run
    "Parser Unit Tests"
    [
      ( "parser_state"
      , [
          test_case "state creation" `Quick test_parser_state_creation
        ; test_case "advance" `Quick test_parser_advance
        ; test_case "skip trivia" `Quick test_parser_skip_trivia
        ] )
    ; ( "parser_expect"
      , [
          test_case "expect success" `Quick test_parser_expect
        ; test_case "expect error" `Quick test_parser_expect_error
        ] )
    ; ( "pattern_parsing"
      , [
          test_case "val binding" `Quick test_parse_pattern_binding
        ; test_case "constructor" `Quick test_parse_pattern_constructor
        ; test_case "literal" `Quick test_parse_pattern_literal
        ; test_case "wildcard" `Quick test_parse_pattern_wild
        ] )
    ; ( "type_parsing"
      , [
          test_case "identifier" `Quick test_parse_type_identifier
        ; test_case "function" `Quick test_parse_type_function
        ; test_case "record" `Quick test_parse_type_record
        ; test_case
            "missing closing brace"
            `Quick
            test_parse_type_missing_closing_brace
        ] )
    ; ( "statement_parsing"
      , [
          test_case "val binding" `Quick test_parse_statement_val_binding
        ; test_case "var binding" `Quick test_parse_statement_var_binding
        ; test_case "assignment" `Quick test_parse_statement_assignment
        ; test_case
            "missing semicolon"
            `Quick
            test_parse_statement_missing_semicolon
        ] )
    ]
