(* ========================================
   CHECKER STATE
   ======================================== *)

type t = {
    interner : Interner.t
  ; symbols : Symbol.t
  ; diags : Diagnostic.bag ref
  ; mutable ty_var_counter : int
  ; mutable level : int
}

let create interner symbols =
  {
    interner
  ; symbols
  ; diags = ref Diagnostic.empty_bag
  ; ty_var_counter = 0
  ; level = 0
  }

(* ========================================
   DIAGNOSTICS
   ======================================== *)

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

let error_with_fixit t msg span replacement =
  let diag = Diagnostic.error msg span in
  let diag = Diagnostic.with_fixit diag { Diagnostic.span; replacement } in
  t.diags := Diagnostic.add !(t.diags) diag

let warning t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.warning msg span)

let note t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.note msg span)

(* ========================================
   TYPE VARIABLE GENERATION
   ======================================== *)

let fresh_ty_var t =
  let id = t.ty_var_counter in
  t.ty_var_counter <- t.ty_var_counter + 1;
  Ty.TyVar (ref (Ty.Unbound (id, t.level)))

(* ========================================
   UNIFICATION
   ======================================== *)

let rec unify t ty1 ty2 span =
  let ty1 = Ty.repr ty1 in
  let ty2 = Ty.repr ty2 in
  match (ty1, ty2) with
  | Ty.TyVar v1, Ty.TyVar v2 when v1 == v2 -> ()
  | Ty.TyVar ({ contents = Ty.Unbound (id, _) } as v), ty
  | ty, Ty.TyVar ({ contents = Ty.Unbound (id, _) } as v) ->
    if Ty.occurs_check id ty then error t "cannot construct infinite type" span
    else v := Ty.Link ty
  | Ty.TyUnit, Ty.TyUnit -> ()
  | Ty.TyBool, Ty.TyBool -> ()
  | Ty.TyInt, Ty.TyInt -> ()
  | Ty.TyNat, Ty.TyNat -> ()
  | _ ->
    let msg =
      Printf.sprintf
        "expected type '%s', found type '%s'"
        (Ty.show ty2)
        (Ty.show ty1)
    in
    error t msg span

(* ========================================
   LET-POLYMORPHISM
   ======================================== *)

let rec generalize level ty =
  match Ty.repr ty with
  | Ty.TyVar ({ contents = Ty.Unbound (id, var_level) } as v)
    when var_level > level ->
    v := Ty.Unbound (id, -1)
  | _ -> ()

let rec instantiate t ty =
  match Ty.repr ty with
  | Ty.TyVar { contents = Ty.Unbound (_, -1) } -> fresh_ty_var t
  | ty -> ty

(* ========================================
   BIDIRECTIONAL TYPE CHECKING
   ======================================== *)

let check_decorator t (dec : Node.decorator) modifiers =
  let dec_name = Interner.lookup t.interner dec.name in
  match dec_name with
  | "link" -> (
    if not (fst modifiers.Node.is_extern) then
      error t "'@link' decorator only valid on 'extern' procedures" dec.span
    else if List.length dec.args <> 1 then
      error t "'@link' requires exactly one text argument" dec.span
    else
      match List.hd dec.args with
      | { Node.kind = Node.ExprLitText _; _ } -> ()
      | _ -> error t "'@link' argument must be text literal" dec.span)
  | _ -> error t (Printf.sprintf "unknown decorator '@%s'" dec_name) dec.span

let check_decorators t modifiers =
  List.iter
    (fun dec -> check_decorator t dec modifiers)
    modifiers.Node.decorators

let rec check_stmt t stmt =
  match stmt.Node.kind with
  | Node.StmtExpr expr ->
    (match expr.Node.kind with
    | Node.ExprBinding { modifiers; _ } | Node.ExprProc { modifiers; _ } ->
      check_decorators t modifiers
    | _ -> ());
    ignore (infer_expr t expr)
  | _ -> ()

and infer_expr_ident t name =
  match Symbol.lookup t.symbols name with
  | Some sym -> (
    match sym.Symbol.ty with
    | Some ty -> instantiate t ty
    | None -> fresh_ty_var t)
  | None -> fresh_ty_var t

and infer_expr_binding t is_mutable pat ty init span =
  let init_ty = infer_expr t init in
  let expected_ty =
    match ty with
    | Some node_ty -> Ty.from_node_ty t.interner node_ty
    | None -> init_ty
  in
  unify t init_ty expected_ty span;
  if not is_mutable then generalize t.level expected_ty;
  check_pat t pat expected_ty;
  Ty.TyUnit

and infer_expr_proc t params ret_ty body =
  t.level <- t.level + 1;
  let param_tys =
    List.map
      (fun p ->
        match p.Node.ty with
        | Some node_ty -> Ty.from_node_ty t.interner node_ty
        | None -> fresh_ty_var t)
      params
  in
  List.iter2
    (fun (param : Node.param) param_ty ->
      match Symbol.lookup t.symbols param.name with
      | Some sym -> sym.Symbol.ty <- Some param_ty
      | None -> ())
    params
    param_tys;
  let ret_ty_expected =
    match ret_ty with
    | Some node_ty -> Ty.from_node_ty t.interner node_ty
    | None -> fresh_ty_var t
  in
  (match body with Some b -> check_expr t b ret_ty_expected | None -> ());
  t.level <- t.level - 1;
  Ty.TyUnit

and infer_expr_if t pat then_branch else_branch span =
  let cond_ty = infer_expr t pat in
  unify t cond_ty Ty.TyBool span;
  let then_ty = infer_expr t then_branch in
  match else_branch with
  | Some e ->
    let else_ty = infer_expr t e in
    unify t then_ty else_ty span;
    then_ty
  | None -> Ty.TyUnit

and infer_expr_call t callee args =
  let callee_ty = infer_expr t callee in
  let arg_tys = List.map (infer_expr t) args.Node.items in
  let arg_count = List.length arg_tys in
  (match callee.Node.kind with
  | Node.ExprIdent name -> (
    match Symbol.lookup t.symbols name with
    | Some sym -> (
      match sym.Symbol.kind with
      | Symbol.SymProc { param_count } when param_count <> arg_count ->
        let msg =
          Printf.sprintf
            "procedure expects %d argument%s, found %d"
            param_count
            (if param_count = 1 then "" else "s")
            arg_count
        in
        let replacement =
          String.make param_count '_'
          |> String.to_seq |> List.of_seq
          |> List.map (fun _ -> "_")
          |> String.concat ", "
        in
        error_with_fixit t msg callee.Node.span replacement;
        note t "procedure defined here" sym.Symbol.span
      | _ -> ())
    | None -> ())
  | _ -> ());
  ignore callee_ty;
  fresh_ty_var t

and infer_expr_assign t target value span =
  (match target.Node.kind with
  | Node.ExprIdent name -> (
    match Symbol.lookup t.symbols name with
    | Some sym -> (
      match sym.Symbol.kind with
      | Symbol.SymVar { mutable_ = false } ->
        error_with_fixit t "cannot assign to immutable binding" span "var";
        note t "binding declared here" sym.Symbol.span
      | _ -> ())
    | None -> ())
  | _ -> ());
  let target_ty = infer_expr t target in
  let value_ty = infer_expr t value in
  unify t target_ty value_ty span;
  Ty.TyUnit

and infer_expr_binary t left right span =
  let left_ty = infer_expr t left in
  let right_ty = infer_expr t right in
  unify t left_ty right_ty span;
  left_ty

and infer_expr_while t pat body span =
  let cond_ty = infer_expr t pat in
  unify t cond_ty Ty.TyBool span;
  ignore (infer_expr t body);
  Ty.TyUnit

and infer_expr_block t stmts expr =
  let stmt_ty = ref None in
  List.iter
    (fun stmt ->
      match stmt.Node.kind with
      | Node.StmtExpr { Node.kind = Node.ExprReturn _; _ } as e ->
        stmt_ty := Some (infer_expr t (Node.make e stmt.Node.span));
        check_stmt t stmt
      | _ -> check_stmt t stmt)
    stmts;
  match (!stmt_ty, expr) with
  | Some ty, _ -> ty
  | None, Some e -> infer_expr t e
  | None, None -> Ty.TyUnit

and infer_expr_return t expr_opt =
  match expr_opt with Some e -> infer_expr t e | None -> Ty.TyUnit

and infer_expr t expr =
  match expr.Node.kind with
  | Node.ExprIdent name -> infer_expr_ident t name
  | Node.ExprLitNumeric _ -> Ty.TyInt
  | Node.ExprLitBool _ -> Ty.TyBool
  | Node.ExprLitText _ -> fresh_ty_var t
  | Node.ExprBinding { is_mutable; pat; ty; init; _ } ->
    infer_expr_binding t is_mutable pat ty init expr.Node.span
  | Node.ExprProc { params; ret_ty; body; _ } ->
    infer_expr_proc t params ret_ty body
  | Node.ExprCall { callee; args; _ } -> infer_expr_call t callee args
  | Node.ExprAssign { target; value } ->
    infer_expr_assign t target value expr.Node.span
  | Node.ExprBinary { left; right; _ } ->
    infer_expr_binary t left right expr.Node.span
  | Node.ExprUnary { operand; _ } -> infer_expr t operand
  | Node.ExprIf { pat; then_branch; else_branch } ->
    infer_expr_if t pat then_branch else_branch expr.Node.span
  | Node.ExprWhile { pat; body } -> infer_expr_while t pat body expr.Node.span
  | Node.ExprBlock { stmts; expr } -> infer_expr_block t stmts expr
  | Node.ExprReturn expr_opt -> infer_expr_return t expr_opt
  | _ -> fresh_ty_var t

and check_expr t expr expected_ty =
  let inferred_ty = infer_expr t expr in
  unify t inferred_ty expected_ty expr.Node.span

and check_pat t pat expected_ty =
  match pat.Node.kind with
  | Node.ExprIdent name | Node.PatBinding name -> (
    match Symbol.lookup t.symbols name with
    | Some sym -> sym.Symbol.ty <- Some expected_ty
    | None -> ())
  | _ -> ()

let check t program =
  List.iter (check_stmt t) program;
  !(t.diags)
