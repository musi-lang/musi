open Musi_basic
open Musi_lex
open Musi_parse

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let note diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.note msg span)

(* === VALIDATION === *)

let validate_binding_mods diags mods span =
  if mods.Node.is_exported then
    error diags "'export' modifier not allowed on binding expressions" span;
  if mods.Node.is_extern then
    error diags "'extern' modifier not allowed on binding expressions" span

let validate_expr_mods diags mods kind span =
  if mods.Node.is_exported then
    error
      diags
      (Printf.sprintf "'export' modifier not allowed on '%s' expressions" kind)
      span;
  if mods.Node.is_extern && kind <> "fn" then
    error
      diags
      (Printf.sprintf "'extern' modifier not allowed on '%s' expressions" kind)
      span

(* === SYMBOL COLLECTION === *)

let bind_symbol table interner diags is_mutable name ty_opt span =
  match Symbol.lookup table name with
  | Some existing ->
    let name_str = Interner.lookup interner name in
    error diags (Printf.sprintf "redefinition of '%s'" name_str) span;
    note
      diags
      (Printf.sprintf "previous definition of '%s' here" name_str)
      existing.span
  | None ->
    let ty =
      match ty_opt with
      | Some t ->
        let lookup name =
          Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
        in
        Types.from_node lookup t
      | None -> Types.fresh_var ()
    in
    let sym =
      { Symbol.name; ty = ref ty; is_mutable; is_exported = false; span }
    in
    ignore (Symbol.bind table name sym)

let scoped table f =
  ignore (Symbol.enter_scope table);
  f ();
  ignore (Symbol.exit_scope table)

let rec collect_expr table interner diags expr =
  match expr.Node.ekind with
  | Node.ExprBinding (is_mutable, _, pat, ty_opt, init, mods) ->
    validate_binding_mods diags mods expr.span;
    collect_pat table interner diags is_mutable pat ty_opt pat.span;
    collect_expr table interner diags init
  | Node.ExprFn (_, params, _, body_opt, mods) ->
    validate_expr_mods diags mods "fn" expr.span;
    scoped table (fun () ->
      List.iter
        (fun (p : Node.param) ->
          bind_symbol table interner diags false p.pname p.pty expr.span)
        params;
      Option.iter (collect_expr table interner diags) body_opt)
  | Node.ExprRecord (_, _, mods) ->
    validate_expr_mods diags mods "record" expr.span
  | Node.ExprChoice (_, _, mods) ->
    validate_expr_mods diags mods "choice" expr.span
  | Node.ExprBlock exprs ->
    scoped table (fun () -> List.iter (collect_expr table interner diags) exprs)
  | Node.ExprIf (cond, then_br, else_opt) ->
    collect_expr table interner diags cond;
    collect_expr table interner diags then_br;
    Option.iter (collect_expr table interner diags) else_opt
  | Node.ExprMatch (scrutinee, cases) ->
    collect_expr table interner diags scrutinee;
    List.iter
      (fun (c : Node.case) ->
        scoped table (fun () -> collect_expr table interner diags c.body))
      cases
  | Node.ExprWhile (cond, body) | Node.ExprBinary (_, cond, body) ->
    collect_expr table interner diags cond;
    collect_expr table interner diags body
  | Node.ExprDo (body, cond_opt) ->
    collect_expr table interner diags body;
    Option.iter (collect_expr table interner diags) cond_opt
  | Node.ExprFor (pat, iter, body) ->
    scoped table (fun () ->
      collect_pat table interner diags false pat None expr.span;
      collect_expr table interner diags iter;
      collect_expr table interner diags body)
  | Node.ExprUnary (_, e)
  | Node.ExprField (e, _, _)
  | Node.ExprTry e
  | Node.ExprDefer e
  | Node.ExprUnwrap e
  | Node.ExprCast (e, _)
  | Node.ExprTest (e, _) ->
    collect_expr table interner diags e
  | Node.ExprCall (callee, args, _) ->
    collect_expr table interner diags callee;
    List.iter (collect_expr table interner diags) args
  | Node.ExprIndex (e, idx, _)
  | Node.ExprRange (e, idx, _)
  | Node.ExprAssign (e, idx) ->
    collect_expr table interner diags e;
    collect_expr table interner diags idx
  | Node.ExprTuple exprs | Node.ExprArray exprs ->
    List.iter (collect_expr table interner diags) exprs
  | Node.ExprReturn e_opt | Node.ExprBreak e_opt ->
    Option.iter (collect_expr table interner diags) e_opt
  | Node.ExprIdent _ | Node.ExprLiteral _ | Node.ExprContinue | Node.ExprError
    ->
    ()

and collect_pat table interner diags is_mutable pat ty_opt span =
  match pat.Node.pkind with
  | Node.PatIdent (name, _) | Node.PatBinding name ->
    bind_symbol table interner diags is_mutable name ty_opt span
  | Node.PatTuple pats | Node.PatArray (pats, _) ->
    List.iter
      (fun p -> collect_pat table interner diags is_mutable p ty_opt span)
      pats
  | Node.PatRecord fields ->
    List.iter
      (fun (_, p) -> collect_pat table interner diags is_mutable p ty_opt span)
      fields
  | Node.PatWild | Node.PatChoice _ | Node.PatLiteral _ | Node.PatRest _
  | Node.PatError ->
    ()

(* === MODULE SYSTEM === *)

let mark_export table interner diags (name, name_span) =
  match Symbol.lookup table name with
  | Some sym -> sym.is_exported <- true
  | None ->
    error
      diags
      (Printf.sprintf
         "cannot export undefined symbol '%s'"
         (Interner.lookup interner name))
      name_span

let collect_stmt table interner diags stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) -> collect_expr table interner diags expr
  | Node.StmtImport (_, _, _) | Node.StmtExport _ | Node.StmtError -> ()

let parse_module file_id base_path module_path interner diags =
  let full_path = Filename.concat base_path (module_path ^ ".ms") in
  try
    let source = In_channel.with_open_text full_path In_channel.input_all in
    let lexer = Lexer.make file_id source interner in
    let tokens, lex_diags = Lexer.lex_all lexer in
    diags := Diagnostic.merge [ !diags; lex_diags ];
    let parser = Parser.make tokens interner in
    let nodes, parse_diags = Parser.parse parser in
    diags := Diagnostic.merge [ !diags; parse_diags ];
    nodes
  with Sys_error _ ->
    error diags (Printf.sprintf "module '%s' not found" module_path) Span.dummy;
    []

let collect_bindings table interner diags nodes =
  List.iter
    (fun stmt ->
      match stmt.Node.skind with
      | Node.StmtImport _ | Node.StmtExport _ -> ()
      | _ -> collect_stmt table interner diags stmt)
    nodes

let mark_exports table interner diags nodes =
  List.iter
    (fun stmt ->
      match stmt.Node.skind with
      | Node.StmtExport (Node.ExportNamed names, _) ->
        List.iter (mark_export table interner diags) names
      | Node.StmtExport (Node.ExportNamespace _, _) ->
        error diags "namespace exports not yet implemented" Span.dummy
      | _ -> ())
    nodes

let rec build_module_graph cache visited file_id_counter base_path module_path
  interner diags =
  let full_path = Filename.concat base_path (module_path ^ ".ms") in
  if List.mem full_path !visited then
    error
      diags
      (Printf.sprintf "circular import '%s' detected" module_path)
      Span.dummy
  else if Hashtbl.mem cache full_path then ()
  else (
    visited := full_path :: !visited;
    let nodes =
      parse_module !file_id_counter base_path module_path interner diags
    in
    incr file_id_counter;
    let table = Symbol.prelude (Symbol.empty_table ()) interner in
    collect_bindings table interner diags nodes;
    mark_exports table interner diags nodes;
    Hashtbl.add cache full_path (table, nodes);
    List.iter
      (fun stmt ->
        match stmt.Node.skind with
        | Node.StmtImport (_, module_name, _) ->
          let dep_path = Interner.lookup interner module_name in
          build_module_graph
            cache
            visited
            file_id_counter
            base_path
            dep_path
            interner
            diags
        | _ -> ())
      nodes)

let link_imports cache base_path table interner diags nodes =
  List.iter
    (fun stmt ->
      match stmt.Node.skind with
      | Node.StmtImport (spec, module_name, _) -> (
        let module_path = Interner.lookup interner module_name in
        let full_path = Filename.concat base_path (module_path ^ ".ms") in
        let module_table, _ = Hashtbl.find cache full_path in
        match spec with
        | Node.ImportNamed names ->
          List.iter
            (fun (name, name_span) ->
              match Symbol.lookup module_table name with
              | Some sym when sym.is_exported ->
                ignore (Symbol.bind table name { sym with is_exported = false })
              | Some _ ->
                error
                  diags
                  (Printf.sprintf
                     "'%s' not exported from module '%s'"
                     (Interner.lookup interner name)
                     module_path)
                  name_span
              | None ->
                error
                  diags
                  (Printf.sprintf
                     "'%s' not found in module '%s'"
                     (Interner.lookup interner name)
                     module_path)
                  name_span)
            names
        | Node.ImportNamespace _ ->
          error diags "namespace imports not yet implemented" Span.dummy)
      | _ -> ())
    nodes

let resolve ?(base_path = Sys.getcwd ()) nodes interner diags =
  let cache = Hashtbl.create 16 in
  let visited = ref [] in
  let file_id_counter = ref 1 in
  let table = Symbol.prelude (Symbol.empty_table ()) interner in
  (* PHASE 1: build module graph and collect symbols *)
  collect_bindings table interner diags nodes;
  mark_exports table interner diags nodes;
  List.iter
    (fun stmt ->
      match stmt.Node.skind with
      | Node.StmtImport (_, module_name, _) ->
        let module_path = Interner.lookup interner module_name in
        build_module_graph
          cache
          visited
          file_id_counter
          base_path
          module_path
          interner
          diags
      | _ -> ())
    nodes;
  (* PHASE 2: link imports *)
  link_imports cache base_path table interner diags nodes;
  table
