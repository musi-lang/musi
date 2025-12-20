open Basic
open Ast
open Nodes
open Parse
open Alcotest
open Lex

let rec eq_lit l1 l2 =
  match (l1, l2) with
  | LitInt i1, LitInt i2 -> i1 = i2
  | LitFloat f1, LitFloat f2 -> f1 = f2
  | LitString s1, LitString s2 -> s1 = s2
  | LitRune c1, LitRune c2 -> c1 = c2
  | LitBool b1, LitBool b2 -> b1 = b2
  | _ -> false

and eq_mod m1 m2 =
  m1.is_export = m2.is_export
  && m1.is_extern = m2.is_extern
  && m1.is_unsafe = m2.is_unsafe

and eq_ty t1 t2 =
  match (t1.kind, t2.kind) with
  | TyIdent i1, TyIdent i2 -> i1 = i2
  | TyApp (i1, as1), TyApp (i2, as2) -> i1 = i2 && List.for_all2 eq_ty as1 as2
  | TyArray (sz1, e1), TyArray (sz2, e2) -> sz1 = sz2 && eq_ty e1 e2
  | TyOptional e1, TyOptional e2 -> eq_ty e1 e2
  | TyPtr e1, TyPtr e2 -> eq_ty e1 e2
  | TyFn (p1, r1), TyFn (p2, r2) -> eq_ty p1 p2 && eq_ty r1 r2
  | TyTuple es1, TyTuple es2 -> List.for_all2 eq_ty es1 es2
  | TyError, TyError -> true
  | _ -> false

and eq_fn_sig s1 s2 =
  s1.fn_name = s2.fn_name && List.for_all2 eq_param s1.fn_params s2.fn_params

and eq_param p1 p2 =
  p1.param_name = p2.param_name && Option.equal eq_ty p1.param_ty p2.param_ty

and eq_expr e1 e2 =
  match (e1.kind, e2.kind) with
  | ExprLit l1, ExprLit l2 -> eq_lit l1 l2
  | ExprIdent i1, ExprIdent i2 -> i1 = i2
  | ExprBinary (l1, o1, r1), ExprBinary (l2, o2, r2) ->
    eq_expr l1 l2 && o1 = o2 && eq_expr r1 r2
  | ExprFn (_, m1, s1, b1), ExprFn (_, m2, s2, b2) ->
    eq_mod m1 m2 && eq_fn_sig s1 s2 && eq_expr b1 b2
  | ExprBind (m1, _, i1, _, init1, _), ExprBind (m2, _, i2, _, init2, _) ->
    eq_mod m1 m2 && i1 = i2 && eq_expr init1 init2
  | ExprExtern (a1, u1, s1), ExprExtern (a2, u2, s2) ->
    a1 = a2 && u1 = u2 && List.for_all2 eq_fn_sig s1 s2
  | ExprBlock (ss1, e1), ExprBlock (ss2, e2) ->
    List.for_all2 eq_stmt ss1 ss2 && Option.equal eq_expr e1 e2
  | ExprIf (c1, t1, f1), ExprIf (c2, t2, f2) ->
    eq_expr c1 c2 && eq_expr t1 t2 && eq_expr f1 f2
  | ExprCall (f1, as1), ExprCall (f2, as2) ->
    eq_expr f1 f2 && List.for_all2 eq_expr as1 as2
  | ExprError, ExprError -> true
  | _ -> false

and eq_stmt s1 s2 =
  match (s1.kind, s2.kind) with StmtExpr e1, StmtExpr e2 -> eq_expr e1 e2

let assert_stmt input expected =
  let source = Source.create "test.ms" input in
  let interner = Interner.create () in
  let lexer = Lexer.create ~interner:(Some interner) source 0 in
  match Lexer.try_tokenize lexer with
  | Ok tokens ->
    let p = Parser.create (List.to_seq tokens) source 0 interner in
    let actual = Parser.parse_stmt p in
    if Parser.has_errors p then (
      Reporter.emit_all Format.err_formatter (Parser.diag p) [ (0, source) ];
      failf "parse error: %s" input);
    if not (eq_stmt actual expected) then failf "mismatch: %s" input
  | Error _ -> fail "lexing failed"

let test_literals () =
  let mk_stmt_lit l =
    make_stmt (StmtExpr (make_expr (ExprLit l) Span.dummy)) Span.dummy
  in
  assert_stmt "42;" (mk_stmt_lit (LitInt "42"));
  assert_stmt "true;" (mk_stmt_lit (LitBool true))

let test_binary () =
  let expected =
    make_stmt
      (StmtExpr
         (make_expr
            (ExprBinary
               ( make_expr (ExprLit (LitInt "1")) Span.dummy
               , Token.Plus
               , make_expr (ExprLit (LitInt "2")) Span.dummy ))
            Span.dummy))
      Span.dummy
  in
  assert_stmt "1 + 2;" expected

let test_bind () =
  let m = { is_export = false; is_extern = (None, false); is_unsafe = false } in
  let expected =
    make_stmt
      (StmtExpr
         (make_expr
            (ExprBind
               ( m
               , false
               , "x"
               , None
               , make_expr (ExprLit (LitInt "10")) Span.dummy
               , make_expr (ExprLit (LitInt "0")) Span.dummy ))
            Span.dummy))
      Span.dummy
  in
  assert_stmt "val x := 10;" expected

let test_extern_fn () =
  let m = { is_export = false; is_extern = (None, true); is_unsafe = false } in
  let s =
    {
      fn_name = Some "foo"
    ; fn_ty_params = []
    ; fn_params = []
    ; fn_ret_ty = None
    }
  in
  let expected =
    make_stmt
      (StmtExpr
         (make_expr
            (ExprFn ([], m, s, make_expr (ExprBlock ([], None)) Span.dummy))
            Span.dummy))
      Span.dummy
  in
  assert_stmt "extern fn foo() {};" expected

let test_extern_c_block () =
  let s =
    {
      fn_name = Some "foo"
    ; fn_ty_params = []
    ; fn_params = []
    ; fn_ret_ty = None
    }
  in
  let expected =
    make_stmt
      (StmtExpr (make_expr (ExprExtern (Some "C", true, [ s ])) Span.dummy))
      Span.dummy
  in
  assert_stmt "extern \"C\" unsafe { fn foo(); };" expected

let test_suite =
  [
    ("literals", [ test_case "literals" `Quick test_literals ])
  ; ("binary", [ test_case "binary" `Quick test_binary ])
  ; ("bind", [ test_case "bind" `Quick test_bind ])
  ; ( "extern"
    , [
        test_case "fn" `Quick test_extern_fn
      ; test_case "block" `Quick test_extern_c_block
      ] )
  ]

let () = run "parser_tests" test_suite
