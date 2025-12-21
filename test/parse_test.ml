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

and eq_attr (a1 : attr) (a2 : attr) =
  a1.name = a2.name
  && Option.equal
       (fun (d1 : _ delimited) (d2 : _ delimited) ->
         List.length d1.value.elems = List.length d2.value.elems)
       a1.args
       a2.args

and eq_ty t1 t2 =
  match (t1.kind, t2.kind) with
  | TyIdent i1, TyIdent i2 -> i1 = i2
  | TyApp { name = n1; args = a1 }, TyApp { name = n2; args = a2 } ->
    n1 = n2 && List.for_all2 eq_ty a1.value.elems a2.value.elems
  | TyArray { ty = t1; len = l1; _ }, TyArray { ty = t2; len = l2; _ } ->
    l1 = l2 && eq_ty t1 t2
  | TyOptional (_, t1), TyOptional (_, t2) -> eq_ty t1 t2
  | TyPtr (_, t1), TyPtr (_, t2) -> eq_ty t1 t2
  | TyFn (p1, _, r1), TyFn (p2, _, r2) -> eq_ty p1 p2 && eq_ty r1 r2
  | TyTuple d1, TyTuple d2 -> List.for_all2 eq_ty d1.value.elems d2.value.elems
  | TyError, TyError -> true
  | _ -> false

and eq_pat p1 p2 =
  match (p1.kind, p2.kind) with
  | PatIdent i1, PatIdent i2 -> i1 = i2
  | PatWild _, PatWild _ -> true
  | PatLit l1, PatLit l2 -> eq_lit l1.kind l2.kind
  | PatLitTuple d1, PatLitTuple d2 ->
    List.for_all2 eq_pat d1.value.elems d2.value.elems
  | PatLitArray d1, PatLitArray d2 ->
    List.for_all2 eq_pat d1.value.elems d2.value.elems
  | ( PatLitRecord { name = n1; fields = f1; _ }
    , PatLitRecord { name = n2; fields = f2; _ } ) ->
    n1 = n2 && List.for_all2 eq_pat_field f1.value.elems f2.value.elems
  | ( PatVariant { name = n1; ty_args = t1; args = a1 }
    , PatVariant { name = n2; ty_args = t2; args = a2 } ) ->
    n1 = n2
    && Option.equal
         (fun (d1 : _ delimited) (d2 : _ delimited) ->
           List.for_all2 eq_ty d1.value.elems d2.value.elems)
         t1
         t2
    && Option.equal
         (fun (d1 : _ delimited) (d2 : _ delimited) ->
           List.for_all2 eq_pat d1.value.elems d2.value.elems)
         a1
         a2
  | PatCons (l1, _, r1), PatCons (l2, _, r2) -> eq_pat l1 l2 && eq_pat r1 r2
  | PatOr d1, PatOr d2 -> List.for_all2 eq_pat d1.kind.elems d2.kind.elems
  | PatError, PatError -> true
  | _ -> false

and eq_pat_field (f1 : pat_field) (f2 : pat_field) = f1.name = f2.name

and eq_expr e1 e2 =
  match (e1.kind, e2.kind) with
  | ExprLit l1, ExprLit l2 -> eq_lit l1.kind l2.kind
  | ExprTemplate (ps1, t1), ExprTemplate (ps2, t2) ->
    t1 = t2
    && List.for_all2
         (fun (s1, ex1) (s2, ex2) -> s1 = s2 && eq_expr ex1 ex2)
         ps1
         ps2
  | ExprLitTuple d1, ExprLitTuple d2 ->
    List.for_all2 eq_expr d1.value.elems d2.value.elems
  | ExprLitArray d1, ExprLitArray d2 ->
    List.for_all2 eq_expr d1.value.elems d2.value.elems
  | ( ExprLitRecord { name = n1; fields = f1; _ }
    , ExprLitRecord { name = n2; fields = f2; _ } ) ->
    n1 = n2
    && Option.equal
         (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_expr p1.value p2.value)
         f1.value.with_expr
         f2.value.with_expr
    && List.for_all2 eq_record_field f1.value.fields.elems f2.value.fields.elems
  | ExprIdent i1, ExprIdent i2 -> i1 = i2
  | ExprBlock d1, ExprBlock d2 ->
    List.for_all2 eq_stmt d1.value.stmts d2.value.stmts
    && Option.equal eq_expr d1.value.result_expr d2.value.result_expr
  | ExprIf i1, ExprIf i2 ->
    eq_expr i1.cond i2.cond
    && eq_expr i1.then_branch i2.then_branch
    && List.for_all2
         (fun (b1 : else_if_branch) (b2 : else_if_branch) ->
           eq_expr b1.cond b2.cond && eq_expr b1.branch b2.branch)
         i1.else_if_branches
         i2.else_if_branches
    && Option.equal
         (fun (_, ex1) (_, ex2) -> eq_expr ex1 ex2)
         i1.else_branch
         i2.else_branch
  | ExprWhile w1, ExprWhile w2 ->
    eq_expr w1.cond w2.cond && eq_expr w1.body w2.body
  | ExprFor f1, ExprFor f2 ->
    eq_pat f1.pat f2.pat
    && eq_expr f1.target f2.target
    && eq_expr f1.body f2.body
  | ExprMatch m1, ExprMatch m2 ->
    eq_expr m1.target m2.target
    && List.for_all2 eq_match_case m1.cases.value.elems m2.cases.value.elems
  | ExprReturn (_, ex1), ExprReturn (_, ex2) -> Option.equal eq_expr ex1 ex2
  | ExprBreak (_, ex1), ExprBreak (_, ex2) -> Option.equal eq_expr ex1 ex2
  | ExprDefer (_, ex1), ExprDefer (_, ex2) -> eq_expr ex1 ex2
  | ExprUnsafe (_, ex1), ExprUnsafe (_, ex2) -> eq_expr ex1 ex2
  | ExprImport (_, s1), ExprImport (_, s2) -> s1 = s2
  | ExprExtern e1, ExprExtern e2 ->
    Option.equal
      (fun (p1 : _ preceded) (p2 : _ preceded) -> p1.value = p2.value)
      e1.abi
      e2.abi
    && List.for_all2 eq_fn_sig_extern e1.sigs.value.elems e2.sigs.value.elems
  | ExprBind b1, ExprBind b2 ->
    eq_pat b1.pat b2.pat
    && Option.equal
         (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_ty p1.value p2.value)
         b1.ty_annot
         b2.ty_annot
    && eq_expr b1.init.value b2.init.value
  | ExprFn f1, ExprFn f2 ->
    List.for_all2 eq_attr f1.attrs f2.attrs
    && eq_fn_sig f1.sig_ f2.sig_ && eq_expr f1.body f2.body
  | ExprRecord r1, ExprRecord r2 ->
    List.for_all2 eq_attr r1.attrs r2.attrs
    && r1.name = r2.name
    && Option.equal
         (fun (d1 : _ delimited) (d2 : _ delimited) ->
           d1.value.elems = d2.value.elems)
         r1.ty_params
         r2.ty_params
    && List.for_all2
         eq_record_field_def
         r1.fields.value.elems
         r2.fields.value.elems
  | ExprSum s1, ExprSum s2 ->
    List.for_all2 eq_attr s1.attrs s2.attrs
    && s1.name = s2.name
    && Option.equal
         (fun (d1 : _ delimited) (d2 : _ delimited) ->
           d1.value.elems = d2.value.elems)
         s1.ty_params
         s2.ty_params
    && List.for_all2 eq_sum_case s1.cases.value.elems s2.cases.value.elems
  | ExprAlias a1, ExprAlias a2 ->
    List.for_all2 eq_attr a1.attrs a2.attrs
    && a1.name = a2.name
    && Option.equal
         (fun (d1 : _ delimited) (d2 : _ delimited) ->
           d1.value.elems = d2.value.elems)
         a1.ty_params
         a2.ty_params
    && eq_ty a1.init.value a2.init.value
  | ExprCall c1, ExprCall c2 ->
    eq_expr c1.callee c2.callee
    && List.for_all2 eq_expr c1.args.value.elems c2.args.value.elems
  | ExprIndex i1, ExprIndex i2 ->
    eq_expr i1.target i2.target && eq_expr i1.index.value i2.index.value
  | ExprField f1, ExprField f2 ->
    eq_expr f1.target f2.target && f1.field = f2.field
  | ExprUnaryPrefix (_, ex1), ExprUnaryPrefix (_, ex2) -> eq_expr ex1 ex2
  | ExprUnaryPostfix (ex1, _), ExprUnaryPostfix (ex2, _) -> eq_expr ex1 ex2
  | ExprBinary b1, ExprBinary b2 ->
    eq_expr b1.left b2.left && b1.op.kind = b2.op.kind
    && eq_expr b1.right b2.right
  | ExprRange r1, ExprRange r2 ->
    eq_expr r1.left r2.left && Option.equal eq_expr r1.right r2.right
  | ExprAssign a1, ExprAssign a2 ->
    eq_expr a1.left a2.left && eq_expr a1.right a2.right
  | ExprError, ExprError -> true
  | _ -> false

and eq_stmt s1 s2 =
  match (s1.kind, s2.kind) with
  | StmtExpr e1, StmtExpr e2 -> eq_expr e1.value e2.value
  | StmtError, StmtError -> true
  | _ -> false

and eq_match_case (c1 : match_case) (c2 : match_case) =
  eq_pat c1.pat c2.pat
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_expr p1.value p2.value)
       c1.guard
       c2.guard
  && eq_expr c1.expr c2.expr

and eq_fn_sig (s1 : fn_sig) (s2 : fn_sig) =
  s1.name = s2.name
  && Option.equal
       (fun (d1 : _ delimited) (d2 : _ delimited) ->
         d1.value.elems = d2.value.elems)
       s1.ty_params
       s2.ty_params
  && List.for_all2 eq_param s1.params.value.elems s2.params.value.elems
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_ty p1.value p2.value)
       s1.ret_ty
       s2.ret_ty

and eq_fn_sig_extern (s1 : fn_sig_extern) (s2 : fn_sig_extern) =
  eq_fn_sig s1.sig_ s2.sig_

and eq_param (p1 : param) (p2 : param) =
  p1.name = p2.name
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_ty p1.value p2.value)
       p1.ty_annot
       p2.ty_annot
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_expr p1.value p2.value)
       p1.init
       p2.init

and eq_record_field (f1 : record_field) (f2 : record_field) =
  f1.name = f2.name
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_ty p1.value p2.value)
       f1.ty_annot
       f2.ty_annot
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_expr p1.value p2.value)
       f1.init
       f2.init

and eq_record_field_def (f1 : record_field_def) (f2 : record_field_def) =
  f1.name = f2.name
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_ty p1.value p2.value)
       f1.ty_annot
       f2.ty_annot
  && Option.equal
       (fun (p1 : _ preceded) (p2 : _ preceded) -> eq_expr p1.value p2.value)
       f1.init
       f2.init

and eq_sum_case (c1 : sum_case) (c2 : sum_case) =
  c1.name = c2.name
  && Option.equal
       (fun (d1 : _ delimited) (d2 : _ delimited) ->
         List.for_all2 eq_ty d1.value.elems d2.value.elems)
       c1.ty_args
       c2.ty_args
  && Option.equal
       (fun (d1 : _ delimited) (d2 : _ delimited) ->
         List.for_all2 eq_sum_case_arg d1.value.elems d2.value.elems)
       c1.args
       c2.args

and eq_sum_case_arg a1 a2 =
  match (a1, a2) with
  | SumCaseArgTy t1, SumCaseArgTy t2 -> eq_ty t1 t2
  | SumCaseArgParam p1, SumCaseArgParam p2 -> eq_param p1 p2
  | _ -> false

let assert_stmt input expected_fn =
  let source = Source.create "test.ms" input in
  let interner = Interner.create () in
  let lexer = Lexer.create ~interner:(Some interner) source 0 in
  match Lexer.try_tokenize lexer with
  | Ok tokens ->
    let p = Parser.create (List.to_seq tokens) source 0 interner in
    let actual = Parser.parse_stmt p in
    let expected = expected_fn interner in
    if Parser.has_errors p then (
      Reporter.emit_all Format.err_formatter (Parser.diag p) [ (0, source) ];
      failf "parse error: %s" input);
    if not (eq_stmt actual expected) then failf "mismatch: %s" input
  | Error _ -> fail "lexing failed"

let s_dummy = Span.dummy
let mk_sp kind = { kind; span = s_dummy }
let mk_ident interner name = mk_sp (Interner.intern interner name)

let mk_stmt_expr e =
  make_stmt (StmtExpr { value = e; trailer = mk_sp Token.Semicolon }) s_dummy

let mk_expr e = make_expr e s_dummy
let mk_lit l = mk_expr (ExprLit (mk_sp l))
let mk_ident_expr interner name = mk_expr (ExprIdent (mk_ident interner name))

let mk_ty_ident interner name =
  make_ty (TyIdent (mk_ident interner name)) s_dummy

let mk_mod = { is_export = None; is_extern = None; is_unsafe = None }
let mk_preceded _ k v = { leader = mk_sp k; value = v }
let mk_delimited l v r = { ldelim = mk_sp l; value = v; rdelim = mk_sp r }

let mk_sep_delim l elems seps r =
  mk_delimited l { elems; seps = List.map mk_sp seps } r

let test_lit_basic () =
  assert_stmt "1024;" (fun i ->
    mk_stmt_expr (mk_lit (LitInt (Interner.intern i "1024"))));
  assert_stmt "3.14159;" (fun i ->
    mk_stmt_expr (mk_lit (LitFloat (Interner.intern i "3.14159"))));
  assert_stmt "true;" (fun _ -> mk_stmt_expr (mk_lit (LitBool true)));
  assert_stmt "false;" (fun _ -> mk_stmt_expr (mk_lit (LitBool false)));
  assert_stmt "\"hello world\";" (fun i ->
    mk_stmt_expr (mk_lit (LitString (Interner.intern i "hello world"))));
  assert_stmt "'a';" (fun _ -> mk_stmt_expr (mk_lit (LitRune 'a')))

let test_lit_numbers () =
  assert_stmt "0xDEADBEEF;" (fun i ->
    mk_stmt_expr (mk_lit (LitInt (Interner.intern i "0xDEADBEEF"))));
  assert_stmt "0o755;" (fun i ->
    mk_stmt_expr (mk_lit (LitInt (Interner.intern i "0o755"))));
  assert_stmt "0b1010;" (fun i ->
    mk_stmt_expr (mk_lit (LitInt (Interner.intern i "0b1010"))))

let verify_ty input expected_ty_fn =
  let wrapped = Printf.sprintf "var x: %s := 0;" input in
  let expected_stmt interner =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            {
              modifier = mk_mod
            ; kind_kw = mk_sp Token.KwVar
            ; pat = make_pat (PatIdent (mk_ident interner "x")) s_dummy
            ; ty_annot =
                Some
                  (mk_preceded interner Token.Colon (expected_ty_fn interner))
            ; init =
                mk_preceded
                  interner
                  Token.ColonEq
                  (mk_lit (LitInt (Interner.intern interner "0")))
            }))
  in
  assert_stmt wrapped expected_stmt

let test_ty_basic () =
  verify_ty "Int" (fun i -> mk_ty_ident i "Int");
  verify_ty "List<Int>" (fun i ->
    make_ty
      (TyApp
         {
           name = mk_ident i "List"
         ; args = mk_sep_delim Token.Lt [ mk_ty_ident i "Int" ] [] Token.Gt
         })
      s_dummy)

let test_ty_ptr_opt () =
  verify_ty "?Int" (fun i ->
    make_ty (TyOptional (mk_sp Token.Question, mk_ty_ident i "Int")) s_dummy);
  verify_ty "^Int" (fun i ->
    make_ty (TyPtr (mk_sp Token.Caret, mk_ty_ident i "Int")) s_dummy)

let test_ty_struct () =
  verify_ty "[]Int" (fun i ->
    make_ty
      (TyArray
         {
           ldelim = mk_sp Token.LBrack
         ; len = None
         ; rdelim = mk_sp Token.RBrack
         ; ty = mk_ty_ident i "Int"
         })
      s_dummy);
  verify_ty "[10]Int" (fun i ->
    make_ty
      (TyArray
         {
           ldelim = mk_sp Token.LBrack
         ; len = Some 10
         ; rdelim = mk_sp Token.RBrack
         ; ty = mk_ty_ident i "Int"
         })
      s_dummy);
  verify_ty "(Int, String)" (fun i ->
    make_ty
      (TyTuple
         (mk_sep_delim
            Token.LParen
            [ mk_ty_ident i "Int"; mk_ty_ident i "String" ]
            [ Token.Comma ]
            Token.RParen))
      s_dummy)

let test_ty_fn () =
  verify_ty "Int -> String" (fun i ->
    make_ty
      (TyFn (mk_ty_ident i "Int", mk_sp Token.MinusGt, mk_ty_ident i "String"))
      s_dummy);
  verify_ty "Int -> String -> Bool" (fun i ->
    make_ty
      (TyFn
         ( mk_ty_ident i "Int"
         , mk_sp Token.MinusGt
         , make_ty
             (TyFn
                ( mk_ty_ident i "String"
                , mk_sp Token.MinusGt
                , mk_ty_ident i "Bool" ))
             s_dummy ))
      s_dummy)

let verify_pat input expected_pat_fn =
  let wrapped = Printf.sprintf "match x { case %s => 0 };" input in
  let expected_stmt i =
    mk_stmt_expr
      (mk_expr
         (ExprMatch
            {
              match_kw = mk_sp Token.KwMatch
            ; target = mk_ident_expr i "x"
            ; cases =
                mk_sep_delim
                  Token.LBrace
                  [
                    {
                      case_kw = mk_sp Token.KwCase
                    ; pat = expected_pat_fn i
                    ; guard = None
                    ; arrow = mk_sp Token.EqGt
                    ; expr = mk_lit (LitInt (Interner.intern i "0"))
                    }
                  ]
                  []
                  Token.RBrace
            }))
  in
  assert_stmt wrapped expected_stmt

let test_pat_basic () =
  verify_pat "_" (fun _ -> make_pat (PatWild (mk_sp Token.Underscore)) s_dummy);
  verify_pat "x" (fun i -> make_pat (PatIdent (mk_ident i "x")) s_dummy);
  verify_pat "42" (fun i ->
    make_pat (PatLit (mk_sp (LitInt (Interner.intern i "42")))) s_dummy)

let test_pat_struct () =
  verify_pat "(x, y)" (fun i ->
    make_pat
      (PatLitTuple
         (mk_sep_delim
            Token.LParen
            [
              make_pat (PatIdent (mk_ident i "x")) s_dummy
            ; make_pat (PatIdent (mk_ident i "y")) s_dummy
            ]
            [ Token.Comma ]
            Token.RParen))
      s_dummy);
  verify_pat "[x, y]" (fun i ->
    make_pat
      (PatLitArray
         (mk_sep_delim
            Token.LBrack
            [
              make_pat (PatIdent (mk_ident i "x")) s_dummy
            ; make_pat (PatIdent (mk_ident i "y")) s_dummy
            ]
            [ Token.Comma ]
            Token.RBrack))
      s_dummy);
  verify_pat "Point.{x, y}" (fun i ->
    make_pat
      (PatLitRecord
         {
           name = Some (mk_ident i "Point")
         ; dot = mk_sp Token.Dot
         ; fields =
             mk_sep_delim
               Token.LBrace
               [ { name = mk_ident i "x" }; { name = mk_ident i "y" } ]
               [ Token.Comma ]
               Token.RBrace
         })
      s_dummy)

let test_pat_variant () =
  verify_pat "Some(x)" (fun i ->
    make_pat
      (PatVariant
         {
           name = mk_ident i "Some"
         ; ty_args = None
         ; args =
             Some
               (mk_sep_delim
                  Token.LParen
                  [ make_pat (PatIdent (mk_ident i "x")) s_dummy ]
                  []
                  Token.RParen)
         })
      s_dummy);
  verify_pat "None" (fun i -> make_pat (PatIdent (mk_ident i "None")) s_dummy)

let test_pat_cons () =
  verify_pat "head :: tail" (fun i ->
    make_pat
      (PatCons
         ( make_pat (PatIdent (mk_ident i "head")) s_dummy
         , mk_sp Token.ColonColon
         , make_pat (PatIdent (mk_ident i "tail")) s_dummy ))
      s_dummy)

let test_expr_binary () =
  assert_stmt "1 + 2 * 3;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprBinary
            {
              left = mk_lit (LitInt (Interner.intern i "1"))
            ; op = mk_sp Token.Plus
            ; right =
                mk_expr
                  (ExprBinary
                     {
                       left = mk_lit (LitInt (Interner.intern i "2"))
                     ; op = mk_sp Token.Star
                     ; right = mk_lit (LitInt (Interner.intern i "3"))
                     })
            })));
  assert_stmt "a |> b |> c;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprBinary
            {
              left =
                mk_expr
                  (ExprBinary
                     {
                       left = mk_ident_expr i "a"
                     ; op = mk_sp Token.BarGt
                     ; right = mk_ident_expr i "b"
                     })
            ; op = mk_sp Token.BarGt
            ; right = mk_ident_expr i "c"
            })));
  assert_stmt "a ?? b ?? c;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprBinary
            {
              left =
                mk_expr
                  (ExprBinary
                     {
                       left = mk_ident_expr i "a"
                     ; op = mk_sp Token.QuestionQuestion
                     ; right = mk_ident_expr i "b"
                     })
            ; op = mk_sp Token.QuestionQuestion
            ; right = mk_ident_expr i "c"
            })))

let test_expr_range () =
  assert_stmt "0..10;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprRange
            {
              left = mk_lit (LitInt (Interner.intern i "0"))
            ; op = mk_sp Token.DotDot
            ; right = Some (mk_lit (LitInt (Interner.intern i "10")))
            })));
  assert_stmt "0..;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprRange
            {
              left = mk_lit (LitInt (Interner.intern i "0"))
            ; op = mk_sp Token.DotDot
            ; right = None
            })))

let test_expr_postfix () =
  assert_stmt "a.^;" (fun i ->
    mk_stmt_expr
      (mk_expr (ExprUnaryPostfix (mk_ident_expr i "a", mk_sp Token.DotCaret))));
  assert_stmt "a?;" (fun i ->
    mk_stmt_expr
      (mk_expr (ExprUnaryPostfix (mk_ident_expr i "a", mk_sp Token.Question))));
  assert_stmt "f(x, y);" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprCall
            {
              callee = mk_ident_expr i "f"
            ; args =
                mk_sep_delim
                  Token.LParen
                  [ mk_ident_expr i "x"; mk_ident_expr i "y" ]
                  [ Token.Comma ]
                  Token.RParen
            })));
  assert_stmt "a[i];" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprIndex
            {
              target = mk_ident_expr i "a"
            ; index =
                mk_delimited Token.LBrack (mk_ident_expr i "i") Token.RBrack
            })));
  assert_stmt "a.b;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprField
            {
              target = mk_ident_expr i "a"
            ; dot = mk_sp Token.Dot
            ; field = mk_ident i "b"
            })))

let test_expr_assign () =
  assert_stmt "x <- 1;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprAssign
            {
              left = mk_ident_expr i "x"
            ; op = mk_sp Token.LtMinus
            ; right = mk_lit (LitInt (Interner.intern i "1"))
            })))

let test_expr_stmt_bind () =
  let expected_bind i =
    mk_stmt_expr
      (mk_expr
         (ExprBind
            {
              modifier = mk_mod
            ; kind_kw = mk_sp Token.KwVal
            ; pat = make_pat (PatIdent (mk_ident i "x")) s_dummy
            ; ty_annot = None
            ; init =
                mk_preceded
                  i
                  Token.ColonEq
                  (mk_lit (LitInt (Interner.intern i "1")))
            }))
  in
  assert_stmt "val x := 1;" expected_bind

let test_expr_record_update () =
  assert_stmt ".{ orig with x := 1 };" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprLitRecord
            {
              name = None
            ; dot = mk_sp Token.Dot
            ; fields =
                mk_delimited
                  Token.LBrace
                  {
                    with_expr =
                      Some (mk_preceded i Token.KwWith (mk_ident_expr i "orig"))
                  ; fields =
                      {
                        elems =
                          [
                            {
                              name = mk_ident i "x"
                            ; ty_annot = None
                            ; init =
                                Some
                                  (mk_preceded
                                     i
                                     Token.ColonEq
                                     (mk_lit (LitInt (Interner.intern i "1"))))
                            }
                          ]
                      ; seps = []
                      }
                  }
                  Token.RBrace
            })))

let test_expr_control () =
  assert_stmt "return 1;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprReturn
            ( mk_sp Token.KwReturn
            , Some (mk_lit (LitInt (Interner.intern i "1"))) ))));
  assert_stmt "break;" (fun _ ->
    mk_stmt_expr (mk_expr (ExprBreak (mk_sp Token.KwBreak, None))));
  assert_stmt "defer cleanup();" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprDefer
            ( mk_sp Token.KwDefer
            , mk_expr
                (ExprCall
                   {
                     callee = mk_ident_expr i "cleanup"
                   ; args = mk_sep_delim Token.LParen [] [] Token.RParen
                   }) ))))

let test_expr_alias () =
  assert_stmt "alias MyInt := Int;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprAlias
            {
              attrs = []
            ; modifier = mk_mod
            ; alias_kw = mk_sp Token.KwAlias
            ; name = mk_ident i "MyInt"
            ; ty_params = None
            ; init = mk_preceded i Token.ColonEq (mk_ty_ident i "Int")
            })));
  assert_stmt "alias List<T> := []T;" (fun i ->
    mk_stmt_expr
      (mk_expr
         (ExprAlias
            {
              attrs = []
            ; modifier = mk_mod
            ; alias_kw = mk_sp Token.KwAlias
            ; name = mk_ident i "List"
            ; ty_params =
                Some (mk_sep_delim Token.Lt [ mk_ident i "T" ] [] Token.Gt)
            ; init =
                mk_preceded
                  i
                  Token.ColonEq
                  (make_ty
                     (TyArray
                        {
                          ldelim = mk_sp Token.LBrack
                        ; len = None
                        ; rdelim = mk_sp Token.RBrack
                        ; ty = mk_ty_ident i "T"
                        })
                     s_dummy)
            })))

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
  ; ("expr_alias", [ test_case "expr_alias" `Quick test_expr_alias ])
  ]

let () = run "parser_tests" test_suite
