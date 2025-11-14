open Musi_basic
open Musi_parse

type context = { in_proc : bool; in_loop : bool; in_async : bool }

let empty_context = { in_proc = false; in_loop = false; in_async = false }

(* HELPERS *)

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let warn diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.warning msg span)

let safe_unify diags t1 t2 span =
  try Types.unify t1 t2 with Failure msg -> error diags msg span

(* === TYPE INFERENCE === *)

let rec infer_expr ctx table interner diags expr =
  match expr.Node.ekind with
  | Node.ExprLiteral kind -> infer_expr_literal ctx table interner diags kind
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
  | Node.ExprAssign (lhs, rhs) ->
    infer_expr_assign ctx table interner diags lhs rhs expr.span
  | Node.ExprProc (_, _, params, _, body_opt, mods) ->
    infer_expr_proc table interner diags params body_opt mods expr.span
  | Node.ExprAwait e -> infer_expr_await ctx table interner diags e expr.span
  | Node.ExprRecord (_, fields, _) ->
    let lookup name =
      Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
    in
    Types.TyRecord
      (List.map
         (fun (f : Node.field) -> (f.fname, Types.from_node lookup f.fty))
         fields)
  | Node.ExprChoice _ -> Types.TyError
  | _ ->
    error
      diags
      (Printf.sprintf
         "unimplemented '%s' in type inference"
         (Node.string_of_expr expr))
      expr.span;
    Types.TyError

and infer_expr_literal ctx table interner diags kind =
  match kind with
  | Node.LitInt text ->
    Types.TyNamed
      (Interner.intern
         interner
         (if String.length text > 0 && text.[0] = '-' then "Int" else "Nat"))
  | Node.LitBin _ -> Types.TyNamed (Interner.intern interner "Nat")
  | Node.LitStr _ -> Types.TyNamed (Interner.intern interner "Str")
  | Node.LitRune _ -> Types.TyNamed (Interner.intern interner "Rune")
  | Node.LitBool _ -> Types.TyNamed (Interner.intern interner "Bool")
  | Node.LitRecord fields ->
    Types.TyRecord
      (List.map
         (fun (name, expr) -> (name, infer_expr ctx table interner diags expr))
         fields)

and infer_expr_ident table interner diags name span =
  match Symbol.lookup table name with
  | Some sym -> !(sym.ty)
  | None ->
    error
      diags
      (Printf.sprintf
         "undefined identifier '%s'"
         (Interner.lookup interner name))
      span;
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
  Types.TyTuple (List.map (infer_expr ctx table interner diags) exprs)

and infer_expr_array ctx table interner diags exprs =
  match exprs with
  | [] -> Types.TyArray (Types.fresh_var ())
  | e :: rest ->
    let elem_ty = infer_expr ctx table interner diags e in
    List.iter
      (fun e ->
        safe_unify diags elem_ty (infer_expr ctx table interner diags e) e.span)
      rest;
    Types.TyArray elem_ty

and infer_expr_field ctx table interner diags base field span =
  match Types.repr (infer_expr ctx table interner diags base) with
  | Types.TyRecord fields -> (
    match List.assoc_opt field fields with
    | Some ty -> ty
    | None ->
      error
        diags
        (Printf.sprintf
           "field '%s' not found in record"
           (Interner.lookup interner field))
        span;
      Types.TyError)
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected record type in field access" span;
    Types.TyError

and infer_expr_index ctx table interner diags base idx span =
  safe_unify
    diags
    (infer_expr ctx table interner diags idx)
    (Types.TyNamed (Interner.intern interner "Nat"))
    idx.span;
  match Types.repr (infer_expr ctx table interner diags base) with
  | Types.TyArray elem_ty -> elem_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected array type in index expression" span;
    Types.TyError

and infer_expr_range ctx table interner diags e1 e2 =
  let t1 = infer_expr ctx table interner diags e1 in
  safe_unify diags t1 (infer_expr ctx table interner diags e2) e2.span;
  Types.TyUnit

and infer_expr_if ctx table interner diags cond then_br else_opt =
  safe_unify
    diags
    (infer_expr ctx table interner diags cond)
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  let then_ty = infer_expr ctx table interner diags then_br in
  match else_opt with
  | Some else_br ->
    safe_unify
      diags
      then_ty
      (infer_expr ctx table interner diags else_br)
      else_br.span;
    then_ty
  | None ->
    safe_unify diags then_ty Types.TyUnit then_br.span;
    Types.TyUnit

and infer_expr_while ctx table interner diags cond body =
  let loop_ctx = { ctx with in_loop = true } in
  safe_unify
    diags
    (infer_expr loop_ctx table interner diags cond)
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  ignore (infer_expr loop_ctx table interner diags body);
  Types.TyUnit

and infer_expr_do ctx table interner diags body cond_opt =
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags body);
  Option.iter
    (fun cond ->
      safe_unify
        diags
        (infer_expr loop_ctx table interner diags cond)
        (Types.TyNamed (Interner.intern interner "Bool"))
        cond.span)
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
    let lookup name =
      Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
    in
    let expected_ty = Types.from_node lookup ty_node in
    safe_unify diags init_ty expected_ty init.span
  | None -> ());
  (match pat.Node.pkind with
  | Node.PatIdent name | Node.PatBinding name -> (
    match Symbol.lookup table name with
    | Some sym -> sym.ty := init_ty
    | None -> ())
  | _ -> ());
  Types.TyUnit

and infer_expr_proc table interner diags _params body_opt mods _span =
  let proc_ctx =
    { in_proc = true; in_loop = false; in_async = mods.Node.is_async }
  in
  Option.iter
    (fun body -> ignore (infer_expr proc_ctx table interner diags body))
    body_opt;
  Types.TyError

and infer_expr_await ctx table interner diags e span =
  if not ctx.in_async then
    error diags "'await' outside of asynchronous procedure" span;
  infer_expr ctx table interner diags e

and infer_expr_assign ctx table interner diags lhs rhs span =
  let lhs_ty = infer_expr ctx table interner diags lhs in
  let rhs_ty = infer_expr ctx table interner diags rhs in
  safe_unify diags lhs_ty rhs_ty rhs.span;
  (match lhs.Node.ekind with
  | Node.ExprIdent name -> (
    match Symbol.lookup table name with
    | Some sym -> (
      (if not sym.is_mutable then
         let name_str = Interner.lookup interner name in
         error
           diags
           (Printf.sprintf "cannot assign to immutable binding '%s'" name_str)
           span);
      match rhs.Node.ekind with
      | Node.ExprIdent rhs_name when rhs_name = name ->
        let name_str = Interner.lookup interner name in
        warn
          diags
          (Printf.sprintf "assignment to '%s' has no effect" name_str)
          span
      | _ -> ())
    | None -> ())
  | _ -> ());
  Types.TyUnit

(* === TYPE CHECKING === *)

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
