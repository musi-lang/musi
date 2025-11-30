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

let test_parser_state_create () =
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

let test_parse_pat_binding () =
  let tokens = [ (Token.KwVal, Span.dummy); (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed pat binding"
    true
    (match pattern.kind with
    | PatBinding { mutable_ = false; name = _ } -> true
    | _ -> false)

let test_parse_pat_ctor () =
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

let test_parse_pat_literal () =
  let tokens = [ (Token.LitNumber "42", Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed pat literal"
    true
    (match pattern.kind with PatLiteral (LitInt "42") -> true | _ -> false)

let test_parse_typ_expr_ident () =
  let tokens = [ (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, typ = parse_typ_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed type identifier"
    true
    (match typ.kind with TypExprIdent _ -> true | _ -> false)

let test_parse_typ_expr_func () =
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

let test_parse_typ_expr_record () =
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

let test_parse_stmt_val_binding () =
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

let test_parse_stmt_var_binding () =
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

let test_parse_stmt_assign () =
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

let test_parse_pat_wild () =
  let tokens = [ (Token.Underscore, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, pattern = parse_pat state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed wildcard"
    true
    (match pattern.kind with PatWild -> true | _ -> false)

let test_parse_stmt_missing_semi () =
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

let test_parse_typ_expr_missing_closing_brace () =
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

let test_parse_expr_literal_int () =
  let tokens = [ (Token.LitNumber "123", Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed int literal"
    true
    (match expr.kind with ExprLiteral (LitInt "123") -> true | _ -> false)

let test_parse_expr_ident () =
  let tokens = [ (Token.Ident 0, Span.dummy) ] in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed identifier"
    true
    (match expr.kind with ExprIdent _ -> true | _ -> false)

let test_parse_expr_tuple () =
  let tokens =
    [
      (Token.LParen, Span.dummy)
    ; (Token.LitNumber "1", Span.dummy)
    ; (Token.Comma, Span.dummy)
    ; (Token.LitNumber "2", Span.dummy)
    ; (Token.RParen, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed tuple"
    true
    (match expr.kind with ExprTuple (_, [ _ ]) -> true | _ -> false)

let test_parse_expr_block () =
  let tokens =
    [
      (Token.LBrace, Span.dummy)
    ; (Token.LitNumber "42", Span.dummy)
    ; (Token.Semi, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed block"
    true
    (match expr.kind with ExprBlock _ -> true | _ -> false)

let test_parse_expr_unary () =
  let tokens =
    [ (Token.Minus, Span.dummy); (Token.LitNumber "5", Span.dummy) ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed unary"
    true
    (match expr.kind with ExprUnary (_, _) -> true | _ -> false)

let test_parse_expr_binary () =
  let tokens =
    [
      (Token.LitNumber "1", Span.dummy)
    ; (Token.Plus, Span.dummy)
    ; (Token.LitNumber "2", Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed binary"
    true
    (match expr.kind with ExprBinary (_, _, _) -> true | _ -> false)

let test_parse_expr_call () =
  let tokens =
    [
      (Token.Ident 0, Span.dummy)
    ; (Token.LParen, Span.dummy)
    ; (Token.LitNumber "1", Span.dummy)
    ; (Token.RParen, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed call"
    true
    (match expr.kind with ExprCall (_, [ _ ]) -> true | _ -> false)

let test_parse_expr_field () =
  let tokens =
    [
      (Token.Ident 0, Span.dummy)
    ; (Token.Dot, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed field access"
    true
    (match expr.kind with ExprField (_, _) -> true | _ -> false)

let test_parse_expr_index () =
  let tokens =
    [
      (Token.Ident 0, Span.dummy)
    ; (Token.LBrack, Span.dummy)
    ; (Token.LitNumber "0", Span.dummy)
    ; (Token.RBrack, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed index"
    true
    (match expr.kind with ExprIndex (_, _) -> true | _ -> false)

let test_parse_expr_if () =
  let tokens =
    [
      (Token.KwIf, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed if"
    true
    (match expr.kind with ExprIf ([ _ ], _, None) -> true | _ -> false)

let test_parse_expr_if_with_guard () =
  let tokens =
    [
      (Token.KwIf, Span.dummy)
    ; (Token.KwCase, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.ColonEq, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed if with case"
    true
    (match expr.kind with
    | ExprIf ([ CondCase _ ], _, None) -> true
    | _ -> false)

let test_parse_expr_match () =
  let tokens =
    [
      (Token.KwMatch, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.KwCase, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.MinusGt, Span.dummy)
    ; (Token.LitNumber "1", Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed match"
    true
    (match expr.kind with ExprMatch (_, [ _ ]) -> true | _ -> false)

let test_parse_expr_while () =
  let tokens =
    [
      (Token.KwWhile, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed while"
    true
    (match expr.kind with ExprWhile (_, None, _) -> true | _ -> false)

let test_parse_expr_for () =
  let tokens =
    [
      (Token.KwFor, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.KwIn, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, expr = parse_expr state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed for"
    true
    (match expr.kind with
    | ExprFor
        { binding = ForIdent _; range = _; step = None; guard = None; body = _ }
      ->
      true
    | _ -> false)

let test_parse_stmt_import_all () =
  let tokens =
    [
      (Token.KwImport, Span.dummy)
    ; (Token.Star, Span.dummy)
    ; (Token.KwAs, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.KwFrom, Span.dummy)
    ; (Token.LitString 1, Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed import * as"
    true
    (match stmt.kind with StmtImport (ImportAll _, _) -> true | _ -> false)

let test_parse_stmt_import_named () =
  let tokens =
    [
      (Token.KwImport, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.Comma, Span.dummy)
    ; (Token.Ident 1, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ; (Token.KwFrom, Span.dummy)
    ; (Token.LitString 2, Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed import { items }"
    true
    (match stmt.kind with
    | StmtImport (ImportNamed [ _; _ ], _) -> true
    | _ -> false)

let test_parse_stmt_export_all () =
  let tokens =
    [
      (Token.KwExport, Span.dummy)
    ; (Token.Star, Span.dummy)
    ; (Token.KwAs, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed export * as"
    true
    (match stmt.kind with StmtExport (ExportAll _, None) -> true | _ -> false)

let test_parse_stmt_export_named () =
  let tokens =
    [
      (Token.KwExport, Span.dummy)
    ; (Token.LBrace, Span.dummy)
    ; (Token.Ident 0, Span.dummy)
    ; (Token.RBrace, Span.dummy)
    ; (Token.KwFrom, Span.dummy)
    ; (Token.LitString 1, Span.dummy)
    ; (Token.Semi, Span.dummy)
    ]
  in
  let state, _ = make_test_state tokens in
  let state_after, stmt = parse_stmt state in
  check_no_errors state_after.diags;
  (check bool)
    "parsed export { items } from"
    true
    (match stmt.kind with
    | StmtExport (ExportNamed [ _ ], Some _) -> true
    | _ -> false)

let () =
  let open Alcotest in
  run
    "Parser Unit Tests"
    [
      ( "parser_state"
      , [
          test_case "state creation" `Quick test_parser_state_create
        ; test_case "advance" `Quick test_parser_advance
        ; test_case "skip trivia" `Quick test_parser_skip_trivia
        ] )
    ; ( "parser_expect"
      , [
          test_case "expect success" `Quick test_parser_expect
        ; test_case "expect failure" `Quick test_parser_expect_error
        ] )
    ; ( "pattern_parsing"
      , [
          test_case "val binding" `Quick test_parse_pat_binding
        ; test_case "constructor" `Quick test_parse_pat_ctor
        ; test_case "literal" `Quick test_parse_pat_literal
        ; test_case "wildcard" `Quick test_parse_pat_wild
        ] )
    ; ( "type_expression_parsing"
      , [
          test_case "identifier" `Quick test_parse_typ_expr_ident
        ; test_case "function" `Quick test_parse_typ_expr_func
        ; test_case "record" `Quick test_parse_typ_expr_record
        ; test_case
            "missing closing brace"
            `Quick
            test_parse_typ_expr_missing_closing_brace
        ] )
    ; ( "expression_parsing"
      , [
          test_case "literal int" `Quick test_parse_expr_literal_int
        ; test_case "identifier" `Quick test_parse_expr_ident
        ; test_case "tuple" `Quick test_parse_expr_tuple
        ; test_case "block" `Quick test_parse_expr_block
        ; test_case "unary" `Quick test_parse_expr_unary
        ; test_case "binary" `Quick test_parse_expr_binary
        ; test_case "call" `Quick test_parse_expr_call
        ; test_case "field access" `Quick test_parse_expr_field
        ; test_case "index" `Quick test_parse_expr_index
        ; test_case "if" `Quick test_parse_expr_if
        ; test_case "if with case" `Quick test_parse_expr_if_with_guard
        ; test_case "match" `Quick test_parse_expr_match
        ; test_case "while" `Quick test_parse_expr_while
        ; test_case "for" `Quick test_parse_expr_for
        ] )
    ; ( "statement_parsing"
      , [
          test_case "val binding" `Quick test_parse_stmt_val_binding
        ; test_case "var binding" `Quick test_parse_stmt_var_binding
        ; test_case "assignment" `Quick test_parse_stmt_assign
        ; test_case "missing semicolon" `Quick test_parse_stmt_missing_semi
        ; test_case "import * as" `Quick test_parse_stmt_import_all
        ; test_case "import { items }" `Quick test_parse_stmt_import_named
        ; test_case "export * as" `Quick test_parse_stmt_export_all
        ; test_case "export { items } from" `Quick test_parse_stmt_export_named
        ] )
    ]
