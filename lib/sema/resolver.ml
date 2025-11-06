(* ========================================
   RESOLVER STATE
   ======================================== *)

type t = {
    interner : Interner.t
  ; symbols : Symbol.t
  ; diags : Diagnostic.bag ref
}

let create interner =
  let symbols = Symbol.create () in
  Symbol.push_scope symbols;
  { interner; symbols; diags = ref Diagnostic.empty_bag }

let symbols t = t.symbols

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
   HELPERS
   ======================================== *)

let rec resolve_exprs t exprs = List.iter (resolve_expr t) exprs
and resolve_opt t = function Some e -> resolve_expr t e | None -> ()

and add_sym_var t name mutable_ span =
  let sym =
    {
      Symbol.name
    ; kind = Symbol.SymVar { mutable_ }
    ; span
    ; ty = None
    ; used = false
    }
  in
  Symbol.add t.symbols sym

(* ========================================
   NAME RESOLUTION
   ======================================== *)

and resolve_stmt t stmt =
  match stmt.Node.kind with
  | Node.StmtExpr expr -> resolve_expr t expr
  | _ -> ()

and resolve_expr t expr =
  match expr.Node.kind with
  | Node.ExprIdent name -> (
    match Symbol.lookup t.symbols name with
    | Some _ -> ()
    | None ->
      let name_str = Interner.lookup t.interner name in
      error
        t
        (Printf.sprintf "cannot find '%s' in this scope" name_str)
        expr.Node.span)
  | Node.ExprBinding { is_mutable; pat; init; _ } ->
    resolve_expr t init;
    resolve_pat t pat is_mutable
  | Node.ExprProc { params; body; _ } ->
    Symbol.push_scope t.symbols;
    List.iter (resolve_param t) params;
    resolve_opt t body;
    check_unused t;
    Symbol.pop_scope t.symbols
  | Node.ExprCall { callee; args } ->
    resolve_expr t callee;
    resolve_exprs t args.Node.items
  | Node.ExprBinary { left; right; _ } -> resolve_exprs t [ left; right ]
  | Node.ExprUnary { operand; _ } -> resolve_expr t operand
  | Node.ExprAssign { target; value } -> resolve_exprs t [ target; value ]
  | Node.ExprField { target; _ } -> resolve_expr t target
  | Node.ExprIndex { target; index } -> resolve_exprs t [ target; index ]
  | Node.ExprIf { pat; then_branch; else_branch } ->
    resolve_expr t pat;
    resolve_expr t then_branch;
    resolve_opt t else_branch
  | Node.ExprWhile { pat; body } -> resolve_exprs t [ pat; body ]
  | Node.ExprFor { pat; iter; body } -> resolve_exprs t [ pat; iter; body ]
  | Node.ExprBlock { stmts; expr } ->
    Symbol.push_scope t.symbols;
    List.iter (resolve_stmt t) stmts;
    resolve_opt t expr;
    check_unused t;
    Symbol.pop_scope t.symbols
  | Node.ExprBlockUnsafe { stmts; expr } ->
    Symbol.push_scope t.symbols;
    List.iter (resolve_stmt t) stmts;
    resolve_opt t expr;
    check_unused t;
    Symbol.pop_scope t.symbols
  | Node.ExprReturn expr_opt -> resolve_opt t expr_opt
  | Node.ExprMatch { scrutinee; cases } ->
    resolve_expr t scrutinee;
    List.iter (resolve_case t) cases
  | Node.ExprBreak expr_opt -> resolve_opt t expr_opt
  | Node.ExprArray items -> resolve_exprs t items.Node.items
  | Node.ExprTuple items -> resolve_exprs t items.Node.items
  | Node.ExprLitRecord { fields; _ } ->
    List.iter (fun f -> resolve_expr t f.Node.value) fields
  | _ -> ()

and resolve_pat t pat is_mutable =
  match pat.Node.kind with
  | Node.ExprIdent name -> add_sym_var t name is_mutable pat.Node.span
  | Node.PatBinding name -> add_sym_var t name is_mutable pat.Node.span
  | Node.PatTuple items ->
    List.iter (fun p -> resolve_pat t p is_mutable) items.Node.items
  | Node.PatExpr e -> resolve_expr t e
  | _ -> ()

and resolve_param t param =
  add_sym_var t param.Node.name param.Node.is_mutable Span.dummy

and resolve_case t case =
  Symbol.push_scope t.symbols;
  resolve_pat t case.Node.pattern false;
  resolve_opt t case.Node.guard;
  resolve_expr t case.Node.body;
  check_unused t;
  Symbol.pop_scope t.symbols

and check_unused t =
  let syms = Symbol.current_scope_symbols t.symbols in
  List.iter
    (fun sym ->
      if not sym.Symbol.used then
        let name_str = Interner.lookup t.interner sym.Symbol.name in
        match sym.Symbol.kind with
        | Symbol.SymVar _ ->
          warning
            t
            (Printf.sprintf "unused binding '%s'" name_str)
            sym.Symbol.span
        | Symbol.SymProc { param_count = _ } ->
          warning
            t
            (Printf.sprintf "unused parameter '%s'" name_str)
            sym.Symbol.span)
    syms

let resolve t program =
  List.iter (resolve_stmt t) program;
  if not (Diagnostic.has_errors !(t.diags)) then check_unused t;
  !(t.diags)
