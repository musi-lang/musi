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

and eq_attr a1 a2 =
  a1.attr_name = a2.attr_name
  && List.length a1.attr_args = List.length a2.attr_args

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
  | PatVariant (i1, ts1, ps1), PatVariant (i2, ts2, ps2) ->
    i1 = i2 && List.for_all2 eq_ty ts1 ts2 && List.for_all2 eq_pat ps1 ps2
  | PatCons (l1, r1), PatCons (l2, r2) -> eq_pat l1 l2 && eq_pat r1 r2
  | PatError, PatError -> true
  | _ -> false

and eq_cond c1 c2 =
  match (c1, c2) with
  | CondExpr e1, CondExpr e2 -> eq_expr e1 e2
  | CondPat (p1, e1), CondPat (p2, e2) -> eq_pat p1 p2 && eq_expr e1 e2
  | _ -> false

and eq_cond_list cs1 cs2 = List.for_all2 eq_cond cs1 cs2

and eq_expr e1 e2 =
  match (e1.kind, e2.kind) with
  | ExprLit l1, ExprLit l2 -> eq_lit l1 l2
  | ExprTemplate (ps1, t1), ExprTemplate (ps2, t2) ->
    t1 = t2
    && List.for_all2 (fun (s1, e1) (s2, e2) -> s1 = s2 && eq_expr e1 e2) ps1 ps2
  | ExprLitTuple es1, ExprLitTuple es2 -> List.for_all2 eq_expr es1 es2
  | ExprLitArray es1, ExprLitArray es2 -> List.for_all2 eq_expr es1 es2
  | ExprLitRecord (i1, fs1, b1), ExprLitRecord (i2, fs2, b2) ->
    i1 = i2
    && List.for_all2 eq_record_field fs1 fs2
    && Option.equal eq_expr b1 b2
  | ExprIdent i1, ExprIdent i2 -> i1 = i2
  | ExprBlock (ss1, e1), ExprBlock (ss2, e2) ->
    List.for_all2 eq_stmt ss1 ss2 && Option.equal eq_expr e1 e2
  | ExprIf (c1, t1, e1), ExprIf (c2, t2, e2) ->
    eq_cond_list c1 c2 && eq_expr t1 t2 && Option.equal eq_expr e1 e2
  | ExprWhile (c1, g1, b1), ExprWhile (c2, g2, b2) ->
    eq_cond c1 c2 && Option.equal eq_expr g1 g2 && eq_expr b1 b2
  | ExprFor (ic1, p1, it1, g1, b1), ExprFor (ic2, p2, it2, g2, b2) ->
    ic1 = ic2 && eq_pat p1 p2 && eq_expr it1 it2 && Option.equal eq_expr g1 g2
    && eq_expr b1 b2
  | ExprMatch (e1, cs1), ExprMatch (e2, cs2) ->
    eq_expr e1 e2 && List.for_all2 eq_match_case cs1 cs2
  | ExprReturn e1, ExprReturn e2 -> Option.equal eq_expr e1 e2
  | ExprBreak e1, ExprBreak e2 -> Option.equal eq_expr e1 e2
  | ExprDefer e1, ExprDefer e2 -> eq_expr e1 e2
  | ExprUnsafe e1, ExprUnsafe e2 -> eq_expr e1 e2
  | ExprImport s1, ExprImport s2 -> s1 = s2
  | ExprExtern (a1, u1, s1), ExprExtern (a2, u2, s2) ->
    a1 = a2 && u1 = u2 && List.for_all2 eq_fn_sig s1 s2
  | ExprBind (m1, mr1, p1, ty1, i1, n1), ExprBind (m2, mr2, p2, ty2, i2, n2) ->
    eq_mod m1 m2 && mr1 = mr2 && eq_pat p1 p2 && Option.equal eq_ty ty1 ty2
    && eq_expr i1 i2 && eq_expr n1 n2
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

let test_lit_basic () =
  assert_stmt "1024;" (mk_stmt_expr (mk_lit (LitInt "1024")));
  assert_stmt "3.14159;" (mk_stmt_expr (mk_lit (LitFloat "3.14159")));
  assert_stmt "true;" (mk_stmt_expr (mk_lit (LitBool true)));
  assert_stmt "false;" (mk_stmt_expr (mk_lit (LitBool false)));
  assert_stmt
    "\"hello world\";"
    (mk_stmt_expr (mk_lit (LitString "hello world")));
  assert_stmt "'a';" (mk_stmt_expr (mk_lit (LitRune 'a')))

let test_lit_numbers () =
  assert_stmt "0xDEADBEEF;" (mk_stmt_expr (mk_lit (LitInt "0xDEADBEEF")));
  assert_stmt "0o755;" (mk_stmt_expr (mk_lit (LitInt "0o755")));
  assert_stmt "0b1010;" (mk_stmt_expr (mk_lit (LitInt "0b1010")))

let verify_ty input expected =
  let wrapped = Printf.sprintf "var x: %s := 0;" input in
  let expected_stmt =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            ( mk_mod
            , true
            , make_pat (PatIdent "x") Span.dummy
            , Some expected
            , mk_lit (LitInt "0")
            , mk_expr (ExprLit (LitInt "0")) )))
  in
  assert_stmt wrapped expected_stmt

let test_ty_basic () =
  verify_ty "Int" (mk_ty_ident "Int");
  verify_ty
    "List<Int>"
    (make_ty (TyApp ("List", [ mk_ty_ident "Int" ])) Span.dummy)

let test_ty_ptr_opt () =
  verify_ty "?Int" (make_ty (TyOptional (mk_ty_ident "Int")) Span.dummy);
  verify_ty "^Int" (make_ty (TyPtr (mk_ty_ident "Int")) Span.dummy)

let test_ty_struct () =
  verify_ty "[]Int" (make_ty (TyArray (None, mk_ty_ident "Int")) Span.dummy);
  verify_ty
    "[10]Int"
    (make_ty (TyArray (Some 10, mk_ty_ident "Int")) Span.dummy);
  verify_ty
    "(Int, String)"
    (make_ty (TyTuple [ mk_ty_ident "Int"; mk_ty_ident "String" ]) Span.dummy)

let test_ty_fn () =
  verify_ty
    "Int -> String"
    (make_ty (TyFn (mk_ty_ident "Int", mk_ty_ident "String")) Span.dummy);
  verify_ty
    "Int -> String -> Bool"
    (make_ty
       (TyFn
          ( mk_ty_ident "Int"
          , make_ty (TyFn (mk_ty_ident "String", mk_ty_ident "Bool")) Span.dummy
          ))
       Span.dummy)

let verify_pat input expected =
  let wrapped = Printf.sprintf "match x { case %s => 0 };" input in
  let expected_stmt =
    mk_stmt_expr
      (mk_expr
         (ExprMatch
            ( mk_ident "x"
            , [
                {
                  case_pat = expected
                ; case_guard = None
                ; case_expr = mk_lit (LitInt "0")
                }
              ] )))
  in
  assert_stmt wrapped expected_stmt

let test_pat_basic () =
  verify_pat "_" (make_pat PatWild Span.dummy);
  verify_pat "x" (make_pat (PatIdent "x") Span.dummy);
  verify_pat "42" (make_pat (PatLit (LitInt "42")) Span.dummy)

let test_pat_struct () =
  verify_pat
    "(x, y)"
    (make_pat
       (PatLitTuple
          [
            make_pat (PatIdent "x") Span.dummy
          ; make_pat (PatIdent "y") Span.dummy
          ])
       Span.dummy);
  verify_pat
    "[x, y]"
    (make_pat
       (PatLitArray
          [
            make_pat (PatIdent "x") Span.dummy
          ; make_pat (PatIdent "y") Span.dummy
          ])
       Span.dummy);
  verify_pat
    "Point.{x, y}"
    (make_pat
       (PatLitRecord ("Point", [ { field_name = "x" }; { field_name = "y" } ]))
       Span.dummy)

let test_pat_variant () =
  verify_pat
    "Some(x)"
    (make_pat
       (PatVariant ("Some", [], [ make_pat (PatIdent "x") Span.dummy ]))
       Span.dummy);
  verify_pat "None" (make_pat (PatIdent "None") Span.dummy)

let test_pat_cons () =
  verify_pat
    "head :: tail"
    (make_pat
       (PatCons
          ( make_pat (PatIdent "head") Span.dummy
          , make_pat (PatIdent "tail") Span.dummy ))
       Span.dummy)

let test_expr_binary () =
  assert_stmt
    "1 + 2 * 3;"
    (mk_stmt_expr
       (mk_expr
          (ExprBinary
             ( mk_lit (LitInt "1")
             , Token.Plus
             , mk_expr
                 (ExprBinary
                    (mk_lit (LitInt "2"), Token.Star, mk_lit (LitInt "3"))) ))));
  assert_stmt
    "a |> b |> c;"
    (mk_stmt_expr
       (mk_expr
          (ExprBinary
             ( mk_expr (ExprBinary (mk_ident "a", Token.BarGt, mk_ident "b"))
             , Token.BarGt
             , mk_ident "c" ))));
  assert_stmt
    "a ?? b ?? c;"
    (mk_stmt_expr
       (mk_expr
          (ExprBinary
             ( mk_expr
                 (ExprBinary (mk_ident "a", Token.QuestionQuestion, mk_ident "b"))
             , Token.QuestionQuestion
             , mk_ident "c" ))))

let test_expr_range () =
  assert_stmt
    "0..10;"
    (mk_stmt_expr
       (mk_expr
          (ExprRange
             (mk_lit (LitInt "0"), Token.DotDot, Some (mk_lit (LitInt "10"))))));
  assert_stmt
    "0..;"
    (mk_stmt_expr
       (mk_expr (ExprRange (mk_lit (LitInt "0"), Token.DotDot, None))))

let test_expr_postfix () =
  assert_stmt
    "a.^;"
    (mk_stmt_expr (mk_expr (ExprUnaryPostfix (mk_ident "a", Token.DotCaret))));
  assert_stmt
    "a?;"
    (mk_stmt_expr (mk_expr (ExprUnaryPostfix (mk_ident "a", Token.Question))));
  assert_stmt
    "f(x, y);"
    (mk_stmt_expr
       (mk_expr (ExprCall (mk_ident "f", [ mk_ident "x"; mk_ident "y" ]))));
  assert_stmt
    "a[i];"
    (mk_stmt_expr (mk_expr (ExprIndex (mk_ident "a", mk_ident "i"))));
  assert_stmt "a.b;" (mk_stmt_expr (mk_expr (ExprField (mk_ident "a", "b"))))

let test_expr_assign () =
  assert_stmt
    "x <- 1;"
    (mk_stmt_expr (mk_expr (ExprAssign (mk_ident "x", mk_lit (LitInt "1")))))

let test_expr_stmt_bind () =
  let expected_bind =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            ( mk_mod
            , false
            , make_pat (PatIdent "x") Span.dummy
            , None
            , mk_lit (LitInt "1")
            , mk_lit (LitInt "0") )))
  in
  assert_stmt "val x := 1;" expected_bind

let test_expr_record_update () =
  let rf =
    {
      field_mutable = false
    ; field_name = "x"
    ; field_ty = None
    ; field_default = Some (mk_lit (LitInt "1"))
    }
  in
  assert_stmt
    ".{ orig with x := 1 };"
    (mk_stmt_expr
       (mk_expr (ExprLitRecord (None, [ rf ], Some (mk_ident "orig")))))

let test_expr_control () =
  assert_stmt
    "return 1;"
    (mk_stmt_expr (mk_expr (ExprReturn (Some (mk_lit (LitInt "1"))))));
  assert_stmt "break;" (mk_stmt_expr (mk_expr (ExprBreak None)));
  assert_stmt
    "defer cleanup();"
    (mk_stmt_expr
       (mk_expr (ExprDefer (mk_expr (ExprCall (mk_ident "cleanup", []))))))

let test_suite =
  [
    ("lit_basic", [ test_case "lit_basic" `Quick test_lit_basic ])
  ; ("lit_nums", [ test_case "lit_nums" `Quick test_lit_numbers ])
  ; ("ty_basic", [ test_case "ty_basic" `Quick test_ty_basic ])
  ; ("ty_ptr_opt", [ test_case "ty_ptr_opt" `Quick test_ty_ptr_opt ])
  ; ("ty_struct", [ test_case "ty_struct" `Quick test_ty_struct ])
  ; ("ty_fn", [ test_case "ty_fn" `Quick test_ty_fn ])
  ; ("pat_basic", [ test_case "pat_basic" `Quick test_pat_basic ])
  ; ("pat_struct", [ test_case "pat_struct" `Quick test_pat_struct ])
  ; ("pat_variant", [ test_case "pat_variant" `Quick test_pat_variant ])
  ; ("pat_cons", [ test_case "pat_cons" `Quick test_pat_cons ])
  ; ("expr_binary", [ test_case "expr_binary" `Quick test_expr_binary ])
  ; ("expr_range", [ test_case "expr_range" `Quick test_expr_range ])
  ; ("expr_postfix", [ test_case "expr_postfix" `Quick test_expr_postfix ])
  ; ("expr_assign", [ test_case "expr_assign" `Quick test_expr_assign ])
  ; ("expr_bind", [ test_case "expr_bind" `Quick test_expr_stmt_bind ])
  ; ("expr_record", [ test_case "expr_record" `Quick test_expr_record_update ])
  ; ("expr_control", [ test_case "expr_control" `Quick test_expr_control ])
  ]

let () = run "parser_tests" test_suite
