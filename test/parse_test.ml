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

and eq_attr_arg a1 a2 =
  match (a1, a2) with
  | AttrArgNamed (id1, id2, l1), AttrArgNamed (id3, id4, l2) ->
    id1 = id3 && id2 = id4 && eq_lit l1 l2
  | AttrArgPos (id1, l1), AttrArgPos (id2, l2) -> id1 = id2 && eq_lit l1 l2
  | _ -> false

and eq_attr a1 a2 =
  a1.attr_name = a2.attr_name
  && List.for_all2 eq_attr_arg a1.attr_args a2.attr_args

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

and eq_pat p1 p2 =
  match (p1.kind, p2.kind) with
  | PatIdent i1, PatIdent i2 -> i1 = i2
  | PatWild, PatWild -> true
  | PatLit l1, PatLit l2 -> eq_lit l1 l2
  | PatLitTuple ps1, PatLitTuple ps2 -> List.for_all2 eq_pat ps1 ps2
  | PatLitArray ps1, PatLitArray ps2 -> List.for_all2 eq_pat ps1 ps2
  | PatLitRecord (i1, fs1), PatLitRecord (i2, fs2) ->
    i1 = i2
    && List.for_all2
         (fun (f1 : pat_field) (f2 : pat_field) ->
           f1.field_name = f2.field_name)
         fs1
         fs2
  | PatVariant (i1, ts1, p1), PatVariant (i2, ts2, p2) ->
    i1 = i2 && List.for_all2 eq_ty ts1 ts2 && Option.equal eq_pat p1 p2
  | PatCons (l1, r1), PatCons (l2, r2) -> eq_pat l1 l2 && eq_pat r1 r2
  | PatError, PatError -> true
  | _ -> false

and eq_expr e1 e2 =
  match (e1.kind, e2.kind) with
  | ExprLit l1, ExprLit l2 -> eq_lit l1 l2
  | ExprLitTuple es1, ExprLitTuple es2 -> List.for_all2 eq_expr es1 es2
  | ExprLitArray es1, ExprLitArray es2 -> List.for_all2 eq_expr es1 es2
  | ExprLitRecord (i1, fs1, b1), ExprLitRecord (i2, fs2, b2) ->
    i1 = i2
    && List.for_all2 eq_record_field fs1 fs2
    && Option.equal eq_expr b1 b2
  | ExprIdent i1, ExprIdent i2 -> i1 = i2
  | ExprBlock (ss1, e1), ExprBlock (ss2, e2) ->
    List.for_all2 eq_stmt ss1 ss2 && Option.equal eq_expr e1 e2
  | ExprIf (c1, t1, f1), ExprIf (c2, t2, f2) ->
    eq_expr c1 c2 && eq_expr t1 t2 && eq_expr f1 f2
  | ExprWhile (c1, b1), ExprWhile (c2, b2) -> eq_expr c1 c2 && eq_expr b1 b2
  | ExprFor (id1, it1, b1), ExprFor (id2, it2, b2) ->
    id1 = id2 && eq_expr it1 it2 && eq_expr b1 b2
  | ExprMatch (e1, cs1), ExprMatch (e2, cs2) ->
    eq_expr e1 e2 && List.for_all2 eq_match_case cs1 cs2
  | ExprReturn e1, ExprReturn e2 -> Option.equal eq_expr e1 e2
  | ExprBreak e1, ExprBreak e2 -> Option.equal eq_expr e1 e2
  | ExprDefer e1, ExprDefer e2 -> eq_expr e1 e2
  | ExprUnsafe e1, ExprUnsafe e2 -> eq_expr e1 e2
  | ExprImport s1, ExprImport s2 -> s1 = s2
  | ExprExtern (a1, u1, s1), ExprExtern (a2, u2, s2) ->
    a1 = a2 && u1 = u2 && List.for_all2 eq_fn_sig s1 s2
  | ( ExprBind (m1, mut1, i1, t1, init1, n1)
    , ExprBind (m2, mut2, i2, t2, init2, n2) ) ->
    eq_mod m1 m2 && mut1 = mut2 && i1 = i2 && Option.equal eq_ty t1 t2
    && eq_expr init1 init2 && eq_expr n1 n2
  | ExprFn (as1, m1, s1, b1), ExprFn (as2, m2, s2, b2) ->
    List.for_all2 eq_attr as1 as2
    && eq_mod m1 m2 && eq_fn_sig s1 s2 && eq_expr b1 b2
  | ExprRecord (as1, m1, id1, tps1, fs1), ExprRecord (as2, m2, id2, tps2, fs2)
    ->
    List.for_all2 eq_attr as1 as2
    && eq_mod m1 m2 && id1 = id2 && tps1 = tps2
    && List.for_all2 eq_record_field fs1 fs2
  | ExprSum (as1, m1, id1, tps1, cs1), ExprSum (as2, m2, id2, tps2, cs2) ->
    List.for_all2 eq_attr as1 as2
    && eq_mod m1 m2 && id1 = id2 && tps1 = tps2
    && List.for_all2 eq_sum_case cs1 cs2
  | ExprCall (f1, as1), ExprCall (f2, as2) ->
    eq_expr f1 f2 && List.for_all2 eq_expr as1 as2
  | ExprIndex (e1, i1), ExprIndex (e2, i2) -> eq_expr e1 e2 && eq_expr i1 i2
  | ExprField (e1, i1), ExprField (e2, i2) -> eq_expr e1 e2 && i1 = i2
  | ExprUnaryPrefix (o1, e1), ExprUnaryPrefix (o2, e2) ->
    o1 = o2 && eq_expr e1 e2
  | ExprUnaryPostfix (e1, o1), ExprUnaryPostfix (e2, o2) ->
    eq_expr e1 e2 && o1 = o2
  | ExprBinary (l1, o1, r1), ExprBinary (l2, o2, r2) ->
    eq_expr l1 l2 && o1 = o2 && eq_expr r1 r2
  | ExprRange (l1, o1, r1), ExprRange (l2, o2, r2) ->
    eq_expr l1 l2 && o1 = o2 && Option.equal eq_expr r1 r2
  | ExprAssign (l1, r1), ExprAssign (l2, r2) -> eq_expr l1 l2 && eq_expr r1 r2
  | ExprError, ExprError -> true
  | _ -> false

and eq_stmt s1 s2 =
  match (s1.kind, s2.kind) with StmtExpr e1, StmtExpr e2 -> eq_expr e1 e2

and eq_match_case c1 c2 =
  eq_pat c1.case_pat c2.case_pat && eq_expr c1.case_expr c2.case_expr

and eq_fn_sig s1 s2 =
  s1.fn_name = s2.fn_name
  && s1.fn_ty_params = s2.fn_ty_params
  && List.for_all2 eq_param s1.fn_params s2.fn_params
  && Option.equal eq_ty s1.fn_ret_ty s2.fn_ret_ty

and eq_param p1 p2 =
  p1.param_mutable = p2.param_mutable
  && p1.param_name = p2.param_name
  && Option.equal eq_ty p1.param_ty p2.param_ty
  && Option.equal eq_expr p1.param_default p2.param_default

and eq_record_field f1 f2 =
  f1.field_mutable = f2.field_mutable
  && f1.field_name = f2.field_name
  && Option.equal eq_ty f1.field_ty f2.field_ty
  && Option.equal eq_expr f1.field_default f2.field_default

and eq_sum_case c1 c2 =
  c1.case_name = c2.case_name
  && List.for_all2 eq_ty c1.case_tys c2.case_tys
  && List.for_all2 eq_param c1.case_params c2.case_params

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

let mk_stmt_expr e = make_stmt (StmtExpr e) Span.dummy
let mk_expr e = make_expr e Span.dummy
let mk_lit l = mk_expr (ExprLit l)
let mk_ident i = mk_expr (ExprIdent i)
let mk_ty_ident i = make_ty (TyIdent i) Span.dummy
let mk_mod = { is_export = false; is_extern = (None, false); is_unsafe = false }

let test_expr_lit () =
  assert_stmt "1024;" (mk_stmt_expr (mk_lit (LitInt "1024")));
  assert_stmt "2.71828;" (mk_stmt_expr (mk_lit (LitFloat "2.71828")));
  assert_stmt "false;" (mk_stmt_expr (mk_lit (LitBool false)));
  assert_stmt "\"musi-lang\";" (mk_stmt_expr (mk_lit (LitString "musi-lang")))

let test_expr_binary () =
  assert_stmt
    "base + offset;"
    (mk_stmt_expr
       (mk_expr (ExprBinary (mk_ident "base", Token.Plus, mk_ident "offset"))));
  let expected_prec =
    mk_stmt_expr
      (mk_expr
         (ExprBinary
            ( mk_lit (LitInt "1")
            , Token.Plus
            , mk_expr
                (ExprBinary
                   (mk_lit (LitInt "2"), Token.Star, mk_lit (LitInt "3"))) )))
  in
  assert_stmt "1 + 2 * 3;" expected_prec

let test_expr_unary () =
  assert_stmt
    "-delta;"
    (mk_stmt_expr (mk_expr (ExprUnaryPrefix (Token.Minus, mk_ident "delta"))));
  assert_stmt
    "not is_valid;"
    (mk_stmt_expr
       (mk_expr (ExprUnaryPrefix (Token.KwNot, mk_ident "is_valid"))))

let test_expr_if () =
  let cond = mk_ident "is_ready" in
  let t_expr = mk_expr (ExprBlock ([], Some (mk_ident "start"))) in
  let f_expr = mk_expr (ExprBlock ([], Some (mk_ident "wait"))) in
  assert_stmt
    "if is_ready {start} else {wait};"
    (mk_stmt_expr (mk_expr (ExprIf (cond, t_expr, f_expr))))

let test_expr_bind () =
  let expected =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            ( mk_mod
            , false
            , "timeout"
            , None
            , mk_lit (LitInt "5000")
            , mk_expr (ExprLit (LitInt "0")) )))
  in
  assert_stmt "val timeout := 5000;" expected;
  let expected_mut =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            ( mk_mod
            , true
            , "count"
            , Some (mk_ty_ident "Int")
            , mk_lit (LitInt "0")
            , mk_expr (ExprLit (LitInt "0")) )))
  in
  assert_stmt "var count: Int := 0;" expected_mut

let test_expr_fn () =
  let body = mk_expr (ExprBlock ([], None)) in
  let p1 =
    {
      param_mutable = false
    ; param_name = "name"
    ; param_ty = Some (mk_ty_ident "String")
    ; param_default = None
    }
  in
  let p2 =
    {
      param_mutable = true
    ; param_name = "age"
    ; param_ty = Some (mk_ty_ident "Int")
    ; param_default = None
    }
  in
  let sig_greet =
    {
      fn_name = Some "greet"
    ; fn_ty_params = []
    ; fn_params = [ p1; p2 ]
    ; fn_ret_ty = Some (mk_ty_ident "Unit")
    }
  in
  assert_stmt
    "fn greet(name: String, var age: Int): Unit {};"
    (mk_stmt_expr (mk_expr (ExprFn ([], mk_mod, sig_greet, body))));
  let sig_anon = { sig_greet with fn_name = None } in
  let fn_anon = mk_expr (ExprFn ([], mk_mod, sig_anon, body)) in
  assert_stmt
    "val greet_fn := fn (name: String, var age: Int): Unit {};"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             ( mk_mod
             , false
             , "greet_fn"
             , None
             , fn_anon
             , mk_expr (ExprLit (LitInt "0")) ))));
  assert_stmt
    "val greet_named := fn greet(name: String, var age: Int): Unit {};"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             ( mk_mod
             , false
             , "greet_named"
             , None
             , mk_expr (ExprFn ([], mk_mod, sig_greet, body))
             , mk_expr (ExprLit (LitInt "0")) ))));
  let m_ext = { mk_mod with is_extern = (None, true) } in
  let m_ext_unsafe = { m_ext with is_unsafe = true } in
  assert_stmt
    "extern fn print_line() {};"
    (mk_stmt_expr
       (mk_expr
          (ExprFn
             ( []
             , m_ext
             , {
                 fn_name = Some "print_line"
               ; fn_ty_params = []
               ; fn_params = []
               ; fn_ret_ty = None
               }
             , body ))));
  assert_stmt
    "extern unsafe fn raw_access() {};"
    (mk_stmt_expr
       (mk_expr
          (ExprFn
             ( []
             , m_ext_unsafe
             , {
                 fn_name = Some "raw_access"
               ; fn_ty_params = []
               ; fn_params = []
               ; fn_ret_ty = None
               }
             , body ))));
  let sig_ms =
    { fn_name = None; fn_ty_params = []; fn_params = []; fn_ret_ty = None }
  in
  let fn_ext = mk_expr (ExprFn ([], m_ext, sig_ms, body)) in
  assert_stmt
    "val log := extern fn () {};"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             (mk_mod, false, "log", None, fn_ext, mk_expr (ExprLit (LitInt "0"))))));
  let fn_ext_unsafe = mk_expr (ExprFn ([], m_ext_unsafe, sig_ms, body)) in
  assert_stmt
    "val write_raw := extern unsafe fn () {};"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             ( mk_mod
             , false
             , "write_raw"
             , None
             , fn_ext_unsafe
             , mk_expr (ExprLit (LitInt "0")) ))))

let test_expr_record () =
  let rf_email =
    {
      field_mutable = false
    ; field_name = "email"
    ; field_ty = Some (mk_ty_ident "String")
    ; field_default = None
    }
  in
  let rf_id =
    {
      field_mutable = false
    ; field_name = "id"
    ; field_ty = Some (mk_ty_ident "Int")
    ; field_default = None
    }
  in
  let rec_user =
    mk_expr (ExprRecord ([], mk_mod, Some "User", [], [ rf_email; rf_id ]))
  in
  assert_stmt "record User { email: String; id: Int };" (mk_stmt_expr rec_user);
  let rec_anon =
    mk_expr (ExprRecord ([], mk_mod, None, [], [ rf_email; rf_id ]))
  in
  let bind_anon =
    mk_expr
      (ExprBind
         (mk_mod, false, "User", None, rec_anon, mk_expr (ExprLit (LitInt "0"))))
  in
  assert_stmt
    "val User := record { email: String; id: Int };"
    (mk_stmt_expr bind_anon);
  let bind_named =
    mk_expr
      (ExprBind
         (mk_mod, false, "User", None, rec_user, mk_expr (ExprLit (LitInt "0"))))
  in
  assert_stmt
    "val User := record User { email: String; id: Int };"
    (mk_stmt_expr bind_named)

let test_expr_sum () =
  let case_none = { case_name = "None"; case_tys = []; case_params = [] } in
  let case_some =
    { case_name = "Some"; case_tys = [ mk_ty_ident "T" ]; case_params = [] }
  in
  let sum_option =
    mk_expr
      (ExprSum ([], mk_mod, Some "Option", [ "T" ], [ case_none; case_some ]))
  in
  assert_stmt
    "sum Option<T> { case None, case Some(T) };"
    (mk_stmt_expr sum_option);
  let sum_anon =
    mk_expr (ExprSum ([], mk_mod, None, [ "T" ], [ case_none; case_some ]))
  in
  assert_stmt
    "val Option := sum<T> { case None, case Some(T) };"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             ( mk_mod
             , false
             , "Option"
             , None
             , sum_anon
             , mk_expr (ExprLit (LitInt "0")) ))));
  assert_stmt
    "val Option := sum Option<T> { case None, case Some(T) };"
    (mk_stmt_expr
       (mk_expr
          (ExprBind
             ( mk_mod
             , false
             , "Option"
             , None
             , sum_option
             , mk_expr (ExprLit (LitInt "0")) ))))

let test_expr_match () =
  let target = mk_ident "result" in
  let pat_ok =
    make_pat
      (PatVariant ("Ok", [], Some (make_pat (PatIdent "val") Span.dummy)))
      Span.dummy
  in
  let case_ok = { case_pat = pat_ok; case_expr = mk_ident "val" } in
  assert_stmt
    "match result { case Ok(`val`) => `val` };"
    (mk_stmt_expr (mk_expr (ExprMatch (target, [ case_ok ]))))

let test_suite =
  [
    ("lit", [ test_case "lit" `Quick test_expr_lit ])
  ; ("binary", [ test_case "binary" `Quick test_expr_binary ])
  ; ("unary", [ test_case "unary" `Quick test_expr_unary ])
  ; ("if", [ test_case "if" `Quick test_expr_if ])
  ; ("bind", [ test_case "bind" `Quick test_expr_bind ])
  ; ("fn", [ test_case "fn" `Quick test_expr_fn ])
  ; ("record", [ test_case "record" `Quick test_expr_record ])
  ; ("sum", [ test_case "sum" `Quick test_expr_sum ])
  ; ("match", [ test_case "match" `Quick test_expr_match ])
  ]

let () = run "parser_tests" test_suite
