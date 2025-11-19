open Musi_basic
open Musi_parse

let empty_context = { Inferrer.in_fn = false; in_loop = false }

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let warn diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.warning msg span)

let check_stmt table interner diags mono_state stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) ->
    ignore
      (Inferrer.infer_expr empty_context table interner diags mono_state expr)
  | Node.StmtImport (_, _, _) | Node.StmtExport _ | Node.StmtError -> ()

let check_program ?(base_path = Sys.getcwd ()) nodes interner diags =
  let table = Resolver.resolve ~base_path nodes interner diags in
  let mono_state = Mono.create interner in
  List.iter (check_stmt table interner diags mono_state) nodes;
  table

let infer_expr_with_context ctx table interner diags mono_state expr =
  Inferrer.infer_expr ctx table interner diags mono_state expr

let check_expr_with_context ctx table interner diags mono_state expected expr =
  Checker.check_expr ctx table interner diags mono_state expected expr

let infer_expr table interner diags mono_state expr =
  infer_expr_with_context empty_context table interner diags mono_state expr

let check_expr table interner diags mono_state expected expr =
  check_expr_with_context
    empty_context
    table
    interner
    diags
    mono_state
    expected
    expr
