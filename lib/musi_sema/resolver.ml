open Musi_basic
open Musi_parse

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let note diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.note msg span)

let collect_expr_binding table interner diags is_mutable name ty_opt pat_span =
  match Symbol.lookup table name with
  | Some existing ->
    let name_str = Interner.lookup interner name in
    error diags (Printf.sprintf "redefinition of '%s'" name_str) pat_span;
    note
      diags
      (Printf.sprintf "previous definition of '%s' here" name_str)
      existing.span
  | None ->
    let ty =
      match ty_opt with
      | Some t -> Types.from_node t
      | None -> Types.fresh_var ()
    in
    let sym = { Symbol.name; ty = ref ty; is_mutable; span = pat_span } in
    ignore (Symbol.bind table name sym)

let rec collect_2exprs table interner diags e1 e2 =
  collect_expr table interner diags e1;
  collect_expr table interner diags e2

and collect_scoped table f =
  let _ = Symbol.enter_scope table in
  f ();
  let _ = Symbol.exit_scope table in
  ()

and collect_expr_proc table interner diags params body_opt span =
  collect_scoped table (fun () ->
    List.iter
      (fun (p : Node.param) ->
        collect_expr_binding table interner diags false p.pname p.pty span)
      params;
    Option.iter (collect_expr table interner diags) body_opt)

and collect_expr_block table interner diags exprs =
  collect_scoped table (fun () ->
    List.iter (collect_expr table interner diags) exprs)

and collect_match_case table interner diags case_body =
  collect_scoped table (fun () -> collect_expr table interner diags case_body)

and validate_binding_mods diags mods span =
  if mods.Node.is_unsafe then
    error diags "'unsafe' modifier not allowed on bindings" span;
  if mods.is_async then
    error diags "'async' modifier not allowed on bindings" span;
  if mods.is_exported then
    error diags "'export' modifier not allowed on binding expressions" span

and validate_expr_mods diags mods kind span =
  if mods.Node.is_exported then
    error
      diags
      (Printf.sprintf "'export' modifier not allowed on '%s' expressions" kind)
      span

and collect_expr table interner diags expr =
  match expr.Node.ekind with
  | Node.ExprBinding (is_mutable, _, pat, ty_opt, init, mods) ->
    validate_binding_mods diags mods expr.span;
    collect_pat table interner diags is_mutable pat ty_opt pat.span;
    collect_expr table interner diags init
  | Node.ExprProc (_, _, params, _, body_opt, mods) ->
    validate_expr_mods diags mods "proc" expr.span;
    collect_expr_proc table interner diags params body_opt expr.span
  | Node.ExprRecord (_, _, mods) ->
    validate_expr_mods diags mods "record" expr.span
  | Node.ExprChoice (_, _, mods) ->
    validate_expr_mods diags mods "choice" expr.span
  | Node.ExprBlock exprs -> collect_expr_block table interner diags exprs
  | Node.ExprIf (cond, then_br, else_opt) ->
    collect_2exprs table interner diags cond then_br;
    Option.iter (collect_expr table interner diags) else_opt
  | Node.ExprMatch (scrutinee, cases) ->
    collect_expr table interner diags scrutinee;
    List.iter
      (fun (c : Node.case) -> collect_match_case table interner diags c.body)
      cases
  | Node.ExprWhile (cond, body) -> collect_2exprs table interner diags cond body
  | Node.ExprDo (body, cond_opt) ->
    collect_expr table interner diags body;
    Option.iter (collect_expr table interner diags) cond_opt
  | Node.ExprFor (pat, iter, body) ->
    collect_scoped table (fun () ->
      collect_pat table interner diags false pat None expr.span;
      collect_2exprs table interner diags iter body)
  | Node.ExprBinary (_, e1, e2) -> collect_2exprs table interner diags e1 e2
  | Node.ExprUnary (_, e) -> collect_expr table interner diags e
  | Node.ExprCall (callee, args, _) ->
    collect_expr table interner diags callee;
    List.iter (collect_expr table interner diags) args
  | Node.ExprField (e, _, _) -> collect_expr table interner diags e
  | Node.ExprIndex (e, idx, _) -> collect_2exprs table interner diags e idx
  | Node.ExprTuple exprs | Node.ExprArray exprs ->
    List.iter (collect_expr table interner diags) exprs
  | Node.ExprRange (e1, e2, _) -> collect_2exprs table interner diags e1 e2
  | Node.ExprAssign (lhs, rhs) -> collect_2exprs table interner diags lhs rhs
  | Node.ExprReturn e_opt | Node.ExprBreak e_opt | Node.ExprYield e_opt ->
    Option.iter (collect_expr table interner diags) e_opt
  | Node.ExprAwait e
  | Node.ExprTry e
  | Node.ExprDefer e
  | Node.ExprUnwrap e
  | Node.ExprCast (e, _)
  | Node.ExprTest (e, _)
  | Node.ExprAsync e
  | Node.ExprUnsafe e ->
    collect_expr table interner diags e
  | Node.ExprIdent _ | Node.ExprLiteral _ | Node.ExprContinue | Node.ExprError
    ->
    ()

and collect_pats table interner diags is_mutable pats ty_opt span =
  List.iter
    (fun p -> collect_pat table interner diags is_mutable p ty_opt span)
    pats

and collect_pat table interner diags is_mutable pat ty_opt span =
  match pat.Node.pkind with
  | Node.PatIdent name | Node.PatBinding name ->
    collect_expr_binding table interner diags is_mutable name ty_opt span
  | Node.PatTuple pats | Node.PatArray (pats, _) ->
    collect_pats table interner diags is_mutable pats ty_opt span
  | Node.PatRecord fields ->
    collect_pats
      table
      interner
      diags
      is_mutable
      (List.map snd fields)
      ty_opt
      span
  | Node.PatWild | Node.PatChoice _ | Node.PatLiteral _ | Node.PatRest _
  | Node.PatError ->
    ()

let collect_stmt table interner diags stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) -> collect_expr table interner diags expr
  | Node.StmtImport _ | Node.StmtExport _ | Node.StmtError -> ()

let resolve nodes interner diags =
  let table = Symbol.empty_table () in
  List.iter (collect_stmt table interner diags) nodes;
  table
