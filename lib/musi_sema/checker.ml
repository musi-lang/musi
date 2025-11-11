open Musi_basic
open Musi_parse

type context = { in_proc : bool; in_loop : bool; in_async : bool }

let empty_context = { in_proc = false; in_loop = false; in_async = false }

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let safe_unify diags t1 t2 span =
  try Types.unify t1 t2 with Failure msg -> error diags msg span

let rec infer_expr ctx table interner diags expr =
  match expr.Node.ekind with
  | Node.ExprLiteral kind -> infer_expr_literal kind
  | Node.ExprIdent name -> infer_expr_ident table interner diags name expr.span
  | Node.ExprBinary (_, e1, e2) ->
    infer_expr_binary ctx table interner diags e1 e2
  | Node.ExprUnary (_, e) -> infer_expr ctx table interner diags e
  | Node.ExprCall (callee, args, _) ->
    infer_expr_call ctx table interner diags callee args expr.span
  | Node.ExprField (base, field, _) ->
    infer_expr_field ctx table interner diags base field expr.span
  | Node.ExprIndex (base, idx, _) ->
    infer_expr_index ctx table interner diags base idx expr.span
  | Node.ExprRange (e1, e2, _) ->
    infer_expr_range ctx table interner diags e1 e2
  | Node.ExprTuple exprs -> infer_expr_tuple ctx table interner diags exprs
  | Node.ExprArray exprs -> infer_expr_array ctx table interner diags exprs
  | Node.ExprBlock exprs -> infer_expr_block ctx table interner diags exprs
  | Node.ExprIf (cond, then_br, else_opt) ->
    infer_expr_if ctx table interner diags cond then_br else_opt
  | Node.ExprWhile (cond, body) ->
    infer_expr_while ctx table interner diags cond body
  | Node.ExprDo (body, cond_opt) ->
    infer_expr_do ctx table interner diags body cond_opt
  | Node.ExprFor (pat, iter, body) ->
    infer_expr_for ctx table interner diags pat iter body expr.span
  | Node.ExprReturn e_opt ->
    infer_expr_return ctx table interner diags e_opt expr.span
  | Node.ExprBreak e_opt ->
    infer_expr_break ctx table interner diags e_opt expr.span
  | Node.ExprContinue -> infer_expr_continue ctx diags expr.span
  | Node.ExprYield e_opt ->
    infer_expr_yield ctx table interner diags e_opt expr.span
  | Node.ExprBinding (_, _, pat, ty_opt, init, _) ->
    infer_expr_binding ctx table interner diags pat ty_opt init
  | _ ->
    error
      diags
      (Printf.sprintf
         "unimplemented '%s' in type inference"
         (Node.string_of_expr expr))
      expr.span;
    Types.TyError

and infer_expr_literal kind =
  match kind with
  | Node.LitInt text ->
    if String.length text > 0 && text.[0] = '-' then Types.TyInt
    else Types.TyNat
  | Node.LitBin _ -> Types.TyNat
  | Node.LitStr _ -> Types.TyText
  | Node.LitRune _ -> Types.TyRune
  | Node.LitBool _ -> Types.TyBool
  | Node.LitRecord _ -> Types.TyError (* TODO: infer record types *)

and infer_expr_ident table interner diags name span =
  match Symbol.lookup table name with
  | Some sym -> !(sym.ty)
  | None ->
    let name_str = Interner.lookup interner name in
    error diags (Printf.sprintf "undefined identifier '%s'" name_str) span;
    Types.TyError

and infer_expr_binary ctx table interner diags e1 e2 =
  let t1 = infer_expr ctx table interner diags e1 in
  let t2 = infer_expr ctx table interner diags e2 in
  safe_unify diags t1 t2 e2.span;
  t1

and infer_expr_call ctx table interner diags callee args span =
  let callee_ty = infer_expr ctx table interner diags callee in
  match Types.repr callee_ty with
  | Types.TyProc (param_tys, ret_ty) ->
    if List.length args <> List.length param_tys then
      error
        diags
        (Printf.sprintf
           "expected %d argument(s), found %d"
           (List.length param_tys)
           (List.length args))
        span;
    List.iter2
      (fun arg param_ty ->
        let arg_ty = infer_expr ctx table interner diags arg in
        safe_unify diags arg_ty param_ty arg.span)
      args
      param_tys;
    ret_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected procedure type in call expression" span;
    Types.TyError

and infer_expr_tuple ctx table interner diags exprs =
  let tys = List.map (infer_expr ctx table interner diags) exprs in
  Types.TyTuple tys

and infer_expr_array ctx table interner diags exprs =
  match exprs with
  | [] -> Types.TyArray (Types.fresh_var ())
  | e :: rest ->
    let elem_ty = infer_expr ctx table interner diags e in
    List.iter
      (fun e ->
        let ty = infer_expr ctx table interner diags e in
        safe_unify diags elem_ty ty e.span)
      rest;
    Types.TyArray elem_ty

and infer_expr_field ctx table interner diags base field span =
  let base_ty = infer_expr ctx table interner diags base in
  match Types.repr base_ty with
  | Types.TyRecord fields -> (
    match List.assoc_opt field fields with
    | Some ty -> ty
    | None ->
      let field_str = Interner.lookup interner field in
      error
        diags
        (Printf.sprintf "field '%s' not found in record" field_str)
        span;
      Types.TyError)
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected record type in field access" span;
    Types.TyError

and infer_expr_index ctx table interner diags base idx span =
  let base_ty = infer_expr ctx table interner diags base in
  let idx_ty = infer_expr ctx table interner diags idx in
  safe_unify diags idx_ty Types.TyNat idx.span;
  match Types.repr base_ty with
  | Types.TyArray elem_ty -> elem_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected array type in index expression" span;
    Types.TyError

and infer_expr_range ctx table interner diags e1 e2 =
  let t1 = infer_expr ctx table interner diags e1 in
  let t2 = infer_expr ctx table interner diags e2 in
  safe_unify diags t1 t2 e2.span;
  Types.TyUnit

and infer_expr_if ctx table interner diags cond then_br else_opt =
  let cond_ty = infer_expr ctx table interner diags cond in
  safe_unify diags cond_ty Types.TyBool cond.span;
  let then_ty = infer_expr ctx table interner diags then_br in
  match else_opt with
  | Some else_br ->
    let else_ty = infer_expr ctx table interner diags else_br in
    safe_unify diags then_ty else_ty else_br.span;
    then_ty
  | None ->
    safe_unify diags then_ty Types.TyUnit then_br.span;
    Types.TyUnit

and infer_expr_while ctx table interner diags cond body =
  let loop_ctx = { ctx with in_loop = true } in
  let cond_ty = infer_expr loop_ctx table interner diags cond in
  safe_unify diags cond_ty Types.TyBool cond.span;
  ignore (infer_expr loop_ctx table interner diags body);
  Types.TyUnit

and infer_expr_do ctx table interner diags body cond_opt =
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags body);
  Option.iter
    (fun cond ->
      let cond_ty = infer_expr loop_ctx table interner diags cond in
      safe_unify diags cond_ty Types.TyBool cond.span)
    cond_opt;
  Types.TyUnit

and infer_expr_for ctx table interner diags _pat iter body _span =
  let loop_ctx = { ctx with in_loop = true } in
  let iter_ty = infer_expr loop_ctx table interner diags iter in
  (match Types.repr iter_ty with
  | Types.TyArray _elem_ty -> ()
  | Types.TyError -> ()
  | _ -> error diags "expected array or range in for-loop iterator" iter.span);
  ignore (infer_expr loop_ctx table interner diags body);
  Types.TyUnit

and infer_expr_return ctx table interner diags e_opt span =
  if not ctx.in_proc then error diags "'return' outside of procedure body" span;
  Option.iter (fun e -> ignore (infer_expr ctx table interner diags e)) e_opt;
  Types.TyUnit

and infer_expr_break ctx table interner diags e_opt span =
  if not ctx.in_loop then error diags "'break' outside of loop body" span;
  Option.iter (fun e -> ignore (infer_expr ctx table interner diags e)) e_opt;
  Types.TyUnit

and infer_expr_continue ctx diags span =
  if not ctx.in_loop then error diags "'continue' outside of loop body" span;
  Types.TyUnit

and infer_expr_yield ctx table interner diags e_opt span =
  if not ctx.in_async then
    error diags "'yield' outside of asynchronous procedure" span;
  Option.iter (fun e -> ignore (infer_expr ctx table interner diags e)) e_opt;
  Types.TyUnit

and infer_expr_block ctx table interner diags exprs =
  match List.rev exprs with
  | [] -> Types.TyUnit
  | last :: rest ->
    List.iter
      (fun e -> ignore (infer_expr ctx table interner diags e))
      (List.rev rest);
    infer_expr ctx table interner diags last

and infer_expr_binding ctx table interner diags pat ty_opt init =
  let init_ty = infer_expr ctx table interner diags init in
  (match ty_opt with
  | Some ty_node ->
    let expected_ty = Types.from_node ty_node in
    safe_unify diags init_ty expected_ty init.span
  | None -> ());
  (match pat.Node.pkind with
  | Node.PatIdent name | Node.PatBinding name -> (
    match Symbol.lookup table name with
    | Some sym -> sym.ty := init_ty
    | None -> ())
  | _ -> ());
  Types.TyUnit

let _check_expr ctx table interner diags expr expected_ty =
  let actual_ty = infer_expr ctx table interner diags expr in
  safe_unify diags actual_ty expected_ty expr.span

let check_stmt table interner diags stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) ->
    ignore (infer_expr empty_context table interner diags expr)
  | Node.StmtImport _ | Node.StmtExport _ | Node.StmtError -> ()

let check nodes table interner diags =
  List.iter (check_stmt table interner diags) nodes

let check_all nodes interner diags =
  let table = Resolver.resolve nodes interner diags in
  check nodes table interner diags
