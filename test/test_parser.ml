open Basic
open Lex
open Parse
open Alcotest

let make_test_state source =
  let interner = Interner.create () in
  let file_id = 42 in
  let tokens, _ = Lexer.tokenize source file_id interner in
  let state = Parser.mk_state source file_id tokens interner in
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

let test_parser_state () =
  let state, _ = make_test_state "42" in
  (check int) "initial position" 0 state.pos;
  let token = Parser.peek state in
  (check string)
    "peek token"
    "NUMBER(42)"
    (match token with Some (t, _) -> Token.to_string t | None -> "EOF")

let test_expr_lit_number () =
  let state, _ = make_test_state "42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprLit (Node.LitNumber s) -> (check string) "number literal" "42" s
  | _ -> fail "expected number literal"

let test_expr_lit_string () =
  let state, interner = make_test_state "\"hello\"" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprLit (Node.LitString name) ->
    check_string_value interner name "hello"
  | _ -> fail "expected string literal"

let test_expr_lit_rune () =
  let state, _ = make_test_state "'a'" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprLit (Node.LitRune c) -> (check char) "rune literal" 'a' c
  | _ -> fail "expected rune literal"

let test_expr_ident () =
  let state, interner = make_test_state "x" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprIdent name -> check_identifier_value interner name "x"
  | _ -> fail "expected identifier"

let test_expr_template () =
  let state, interner = make_test_state "$\"hello world\"" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprTemplate name -> check_string_value interner name "hello world"
  | _ -> fail "expected template"

let test_expr_tuple () =
  let state, _ = make_test_state "(1, 2, 3)" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprTuple (_, rest) -> (check int) "tuple length" 2 (List.length rest)
  | _ -> fail "expected tuple"

let test_expr_block () =
  let state, _ = make_test_state "{ val x := 42; }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprBlock _ -> ()
  | _ -> fail "expected block"

let test_expr_if () =
  let state, _ = make_test_state "if true { 1 }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprIf _ -> ()
  | _ -> fail "expected if expression"

let test_expr_if_else () =
  let state, _ = make_test_state "if true { 1 } else { 2 }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprIf { else_block = Some _; _ } -> ()
  | _ -> fail "expected if-else expression"

let test_expr_match () =
  let state, _ = make_test_state "match x { case 1 => 2 }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprMatch _ -> ()
  | _ -> fail "expected match expression"

let test_expr_for () =
  let state, _ = make_test_state "for i in 0..10 { i }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprFor _ -> ()
  | _ -> fail "expected for expression"

let test_expr_while () =
  let state, _ = make_test_state "while true { break }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprWhile _ -> ()
  | _ -> fail "expected while expression"

let test_expr_defer () =
  let state, _ = make_test_state "defer cleanup()" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprDefer _ -> ()
  | _ -> fail "expected defer expression"

let test_expr_break () =
  let state, _ = make_test_state "break" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprBreak None -> ()
  | _ -> fail "expected break expression"

let test_expr_break_value () =
  let state, _ = make_test_state "break 42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprBreak (Some _) -> ()
  | _ -> fail "expected break with value"

(* Test cycle expressions *)
let test_expr_cycle () =
  let state, _ = make_test_state "cycle" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprCycle -> ()
  | _ -> fail "expected cycle expression"

let test_expr_unsafe () =
  let state, _ = make_test_state "unsafe { dangerous() }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprUnsafe _ -> ()
  | _ -> fail "expected unsafe expression"

let test_expr_assign () =
  let state, interner = make_test_state "x <- 42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprAssign { target; value = _ } ->
    check_identifier_value interner target "x"
  | _ -> fail "expected assignment expression"

let test_expr_binary_add () =
  let state, _ = make_test_state "1 + 2" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprBinary { op = Token.Plus; left = _; right = _ } -> ()
  | _ -> fail "expected binary addition"

let test_expr_binary_mul () =
  let state, _ = make_test_state "3 * 4" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprBinary { op = Token.Star; left = _; right = _ } -> ()
  | _ -> fail "expected binary multiplication"

let test_expr_unary_neg () =
  let state, _ = make_test_state "-42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprUnary { op = Token.Minus; operand = _ } -> ()
  | _ -> fail "expected unary negation"

let test_expr_unary_not () =
  let state, _ = make_test_state "not true" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprUnary { op = Token.KwNot; operand = _ } -> ()
  | _ -> fail "expected unary not"

let test_expr_call () =
  let state, _ = make_test_state "f(1, 2, 3)" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprCall { args; _ } ->
    (check int) "call args count" 3 (List.length args)
  | _ -> fail "expected call expression"

let test_expr_field () =
  let state, interner = make_test_state "obj.field" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprField { field; optional = false; _ } ->
    check_identifier_value interner field "field"
  | _ -> fail "expected field access"

let test_expr_index () =
  let state, _ = make_test_state "arr[0]" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprIndex { optional = false; _ } -> ()
  | _ -> fail "expected index expression"

let test_expr_record_lit () =
  let state, _ = make_test_state "{ .x := 1, .y := 2 }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprRecordLit { name = None; fields } ->
    (check int) "record fields count" 2 (List.length fields)
  | _ -> fail "expected record literal"

let test_expr_record_lit_named () =
  let state, interner = make_test_state "Point { .x := 1, .y := 2 }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprRecordLit { name = Some name; fields } ->
    check_identifier_value interner name "Point";
    (check int) "record fields count" 2 (List.length fields)
  | _ -> fail "expected named record literal"

let test_expr_fn () =
  let state, _ = make_test_state "fn () -> 42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprFn { name = None; params = []; ret_typ = None; _ } -> ()
  | _ -> fail "expected function expression"

let test_expr_fn_named () =
  let state, interner = make_test_state "fn my_func() -> 42" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprFn { name = Some name; params = []; _ } ->
    check_identifier_value interner name "my_func"
  | _ -> fail "expected named function expression"

let test_expr_record () =
  let state, _ = make_test_state "record { x : Int32; }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprRecord { typ_params = []; trait_bound = None; fields; body = [] }
    ->
    (check int) "record fields count" 1 (List.length fields)
  | _ -> fail "expected record expression"

let test_expr_choice () =
  let state, _ = make_test_state "choice { case Some(Int32); case None; }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprChoice { typ_params = []; cases; body = [] } ->
    (check int) "choice cases count" 2 (List.length cases)
  | _ -> fail "expected choice expression"

let test_expr_trait () =
  let state, _ = make_test_state "trait { fn method(); }" in
  let state, expr = Parser.parse_expr state in
  check_no_errors state.diags;
  match expr.Node.kind with
  | Node.ExprTrait { typ_params = []; trait_bound = None; items } ->
    (check int) "trait items count" 1 (List.length items)
  | _ -> fail "expected trait expression"

let test_pat_bind () =
  let state, interner = make_test_state "val x" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatBind { mutable_ = false; name } ->
    check_identifier_value interner name "x"
  | _ -> fail "expected pattern binding"

let test_pat_bind_var () =
  let state, interner = make_test_state "var x" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatBind { mutable_ = true; name } ->
    check_identifier_value interner name "x"
  | _ -> fail "expected mutable pattern binding"

let test_pat_lit_number () =
  let state, _ = make_test_state "42" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatLit (Node.LitNumber s) ->
    (check string) "pattern number literal" "42" s
  | _ -> fail "expected pattern number literal"

let test_pat_lit_string () =
  let state, interner = make_test_state "\"hello\"" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatLit (Node.LitString name) ->
    check_string_value interner name "hello"
  | _ -> fail "expected pattern string literal"

let test_pat_wild () =
  let state, _ = make_test_state "_" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatWild -> ()
  | _ -> fail "expected wildcard pattern"

let test_pat_ident () =
  let state, interner = make_test_state "x" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatIdent name -> check_identifier_value interner name "x"
  | _ -> fail "expected identifier pattern"

let test_pat_record () =
  let state, interner = make_test_state "Point { .x, .y }" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatRecord { name; fields } ->
    check_identifier_value interner name "Point";
    (check int) "record pattern fields count" 2 (List.length fields)
  | _ -> fail "expected record pattern"

let test_pat_ctor () =
  let state, interner = make_test_state "Some(42)" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatCtor { name; args } ->
    check_identifier_value interner name "Some";
    (check int) "constructor pattern args count" 1 (List.length args)
  | _ -> fail "expected constructor pattern"

let test_pat_tuple () =
  let state, _ = make_test_state "(1, 2)" in
  let state, pat = Parser.parse_pat state in
  check_no_errors state.diags;
  match pat.Node.kind with
  | Node.PatTuple (_, rest) ->
    (check int) "tuple pattern length" 1 (List.length rest)
  | _ -> fail "expected tuple pattern"

let test_typ_ptr () =
  let state, _ = make_test_state "^Int32" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypPtr _ -> ()
  | _ -> fail "expected pointer type"

let test_typ_arr () =
  let state, _ = make_test_state "[10]Int32" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypArr { size = Some _; elem = _ } -> ()
  | _ -> fail "expected array type with size"

let test_typ_slice () =
  let state, _ = make_test_state "[]Int32" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypArr { size = None; elem = _ } -> ()
  | _ -> fail "expected slice type"

let test_typ_ident () =
  let state, interner = make_test_state "Int32" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypIdent name -> check_identifier_value interner name "Int32"
  | _ -> fail "expected identifier type"

let test_typ_app () =
  let state, interner = make_test_state "List<Int32>" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypApp { base; args } ->
    check_identifier_value interner base "List";
    (check int) "type args count" 1 (List.length args)
  | _ -> fail "expected type application"

let test_typ_tuple () =
  let state, _ = make_test_state "(Int32, String)" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypTuple (_, rest) ->
    (check int) "tuple type length" 1 (List.length rest)
  | _ -> fail "expected tuple type"

let test_typ_fn () =
  let state, _ = make_test_state "fn(Int32) -> String" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypFn { params; ret = Some _ } ->
    (check int) "function params count" 1 (List.length params)
  | _ -> fail "expected function type"

let test_typ_record () =
  let state, _ = make_test_state "{ x : Int32; y : String }" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypRecord fields ->
    (check int) "record type fields count" 2 (List.length fields)
  | _ -> fail "expected record type"

let test_typ_optional () =
  let state, _ = make_test_state "Int32?" in
  let state, typ = Parser.parse_typ state in
  check_no_errors state.diags;
  match typ.Node.kind with
  | Node.TypOptional _ -> ()
  | _ -> fail "expected optional type"

let test_stmt_import_named () =
  let state, _ = make_test_state "import { x, y } from \"module\";" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtImport { clause = Node.ImportNamed names; source = _ } ->
    (check int) "import names count" 2 (List.length names)
  | _ -> fail "expected named import"

let test_stmt_import_all () =
  let state, interner = make_test_state "import * as module from \"module\";" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtImport { clause = Node.ImportAll name; source = _ } ->
    check_identifier_value interner name "module"
  | _ -> fail "expected import all"

let test_stmt_export_named () =
  let state, _ = make_test_state "export { x, y } from \"module\";" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtExport { clause = Node.ExportNamed names; source = Some _ } ->
    (check int) "export names count" 2 (List.length names)
  | _ -> fail "expected named export"

let test_stmt_export_all () =
  let state, interner = make_test_state "export * as module from \"module\";" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtExport { clause = Node.ExportAll name; source = Some _ } ->
    check_identifier_value interner name "module"
  | _ -> fail "expected export all"

let test_stmt_bind_val () =
  let state, interner = make_test_state "val x := 42;" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtBind { mutable_ = false; name; typ = None; value = _ } ->
    check_identifier_value interner name "x"
  | _ -> fail "expected val binding"

let test_stmt_bind_var () =
  let state, interner = make_test_state "var x := 42;" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtBind { mutable_ = true; name; typ = None; value = _ } ->
    check_identifier_value interner name "x"
  | _ -> fail "expected var binding"

let test_stmt_bind_typed () =
  let state, interner = make_test_state "val x : Int32 := 42;" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtBind { mutable_ = false; name; typ = Some _; value = _ } ->
    check_identifier_value interner name "x"
  | _ -> fail "expected typed binding"

let test_stmt_extern () =
  let state, _ =
    make_test_state "extern unsafe { fn malloc(size : Nat64) -> ^Unit; }"
  in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtExtern { abi = None; decls } ->
    (check int) "extern decls count" 1 (List.length decls)
  | _ -> fail "expected extern statement"

let test_stmt_extern_abi () =
  let state, _ =
    make_test_state "extern \"C\" unsafe { fn malloc(size : Nat64) -> ^Unit; }"
  in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtExtern { abi = Some "C"; decls } ->
    (check int) "extern decls count" 1 (List.length decls)
  | _ -> fail "expected extern with ABI"

let test_stmt_expr () =
  let state, _ = make_test_state "func();" in
  let state, stmt = Parser.parse_stmt state in
  check_no_errors state.diags;
  match stmt.Node.kind with
  | Node.StmtExpr _ -> ()
  | _ -> fail "expected expression statement"

let test_program_empty () =
  let _, interner = make_test_state "" in
  let tokens, _ = Lexer.tokenize "" 42 interner in
  let prog, diags = Parser.parse "" 42 interner tokens in
  check_no_errors diags;
  (check int) "empty program length" 0 (List.length prog)

let test_program_single_stmt () =
  let _, interner = make_test_state "val x := 42;" in
  let tokens, _ = Lexer.tokenize "val x := 42;" 42 interner in
  let prog, diags = Parser.parse "val x := 42;" 42 interner tokens in
  check_no_errors diags;
  (check int) "single stmt program length" 1 (List.length prog)

let test_program_multi_stmt () =
  let _, interner = make_test_state "val x := 42; val y := 24;" in
  let tokens, _ = Lexer.tokenize "val x := 42; val y := 24;" 42 interner in
  let prog, diags =
    Parser.parse "val x := 42; val y := 24;" 42 interner tokens
  in
  check_no_errors diags;
  (check int) "multi stmt program length" 2 (List.length prog)

let test_cases =
  [
    test_case "parser_state" `Quick test_parser_state
  ; test_case "expr_lit_number" `Quick test_expr_lit_number
  ; test_case "expr_lit_string" `Quick test_expr_lit_string
  ; test_case "expr_lit_rune" `Quick test_expr_lit_rune
  ; test_case "expr_ident" `Quick test_expr_ident
  ; test_case "expr_template" `Quick test_expr_template
  ; test_case "expr_tuple" `Quick test_expr_tuple
  ; test_case "expr_block" `Quick test_expr_block
  ; test_case "expr_if" `Quick test_expr_if
  ; test_case "expr_if_else" `Quick test_expr_if_else
  ; test_case "expr_match" `Quick test_expr_match
  ; test_case "expr_for" `Quick test_expr_for
  ; test_case "expr_while" `Quick test_expr_while
  ; test_case "expr_defer" `Quick test_expr_defer
  ; test_case "expr_break" `Quick test_expr_break
  ; test_case "expr_break_value" `Quick test_expr_break_value
  ; test_case "expr_cycle" `Quick test_expr_cycle
  ; test_case "expr_unsafe" `Quick test_expr_unsafe
  ; test_case "expr_assign" `Quick test_expr_assign
  ; test_case "expr_binary_add" `Quick test_expr_binary_add
  ; test_case "expr_binary_mul" `Quick test_expr_binary_mul
  ; test_case "expr_unary_neg" `Quick test_expr_unary_neg
  ; test_case "expr_unary_not" `Quick test_expr_unary_not
  ; test_case "expr_call" `Quick test_expr_call
  ; test_case "expr_field" `Quick test_expr_field
  ; test_case "expr_index" `Quick test_expr_index
  ; test_case "expr_record_lit" `Quick test_expr_record_lit
  ; test_case "expr_record_lit_named" `Quick test_expr_record_lit_named
  ; test_case "expr_fn" `Quick test_expr_fn
  ; test_case "expr_fn_named" `Quick test_expr_fn_named
  ; test_case "expr_record" `Quick test_expr_record
  ; test_case "expr_choice" `Quick test_expr_choice
  ; test_case "expr_trait" `Quick test_expr_trait
  ; test_case "pat_bind" `Quick test_pat_bind
  ; test_case "pat_bind_var" `Quick test_pat_bind_var
  ; test_case "pat_lit_number" `Quick test_pat_lit_number
  ; test_case "pat_lit_string" `Quick test_pat_lit_string
  ; test_case "pat_wild" `Quick test_pat_wild
  ; test_case "pat_ident" `Quick test_pat_ident
  ; test_case "pat_record" `Quick test_pat_record
  ; test_case "pat_ctor" `Quick test_pat_ctor
  ; test_case "pat_tuple" `Quick test_pat_tuple
  ; test_case "typ_ptr" `Quick test_typ_ptr
  ; test_case "typ_arr" `Quick test_typ_arr
  ; test_case "typ_slice" `Quick test_typ_slice
  ; test_case "typ_ident" `Quick test_typ_ident
  ; test_case "typ_app" `Quick test_typ_app
  ; test_case "typ_tuple" `Quick test_typ_tuple
  ; test_case "typ_fn" `Quick test_typ_fn
  ; test_case "typ_record" `Quick test_typ_record
  ; test_case "typ_optional" `Quick test_typ_optional
  ; test_case "stmt_import_named" `Quick test_stmt_import_named
  ; test_case "stmt_import_all" `Quick test_stmt_import_all
  ; test_case "stmt_export_named" `Quick test_stmt_export_named
  ; test_case "stmt_export_all" `Quick test_stmt_export_all
  ; test_case "stmt_bind_val" `Quick test_stmt_bind_val
  ; test_case "stmt_bind_var" `Quick test_stmt_bind_var
  ; test_case "stmt_bind_typed" `Quick test_stmt_bind_typed
  ; test_case "stmt_extern" `Quick test_stmt_extern
  ; test_case "stmt_extern_abi" `Quick test_stmt_extern_abi
  ; test_case "stmt_expr" `Quick test_stmt_expr
  ; test_case "program_empty" `Quick test_program_empty
  ; test_case "program_single_stmt" `Quick test_program_single_stmt
  ; test_case "program_multi_stmt" `Quick test_program_multi_stmt
  ]

let suite = [ ("parser", test_cases) ]
let () = run "parser" suite
