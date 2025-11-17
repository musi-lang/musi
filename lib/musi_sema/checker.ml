open Musi_basic
open Musi_parse

type context = { in_fn : bool; in_loop : bool }

let empty_context = { in_fn = false; in_loop = false }

(* === HELPERS === *)

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let warn diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.warning msg span)

let safe_unify interner diags t1 t2 span =
  try Types.unify (Types.repr t1) (Types.repr t2) with
  | Failure msg when String.contains msg '\'' ->
    let t1_str = Types.show_with interner (Types.repr t1) in
    let t2_str = Types.show_with interner (Types.repr t2) in
    let fixed_msg =
      Printf.sprintf "expected type '%s', found type '%s'" t1_str t2_str
    in
    error diags fixed_msg span
  | Failure msg -> error diags msg span

(* === TYPE INFERENCE === *)

let rec infer_expr ctx table interner diags mono_state ?expected expr =
  match expr.Node.ekind with
  | Node.ExprLiteral kind ->
    infer_expr_literal ctx table interner diags mono_state ?expected kind
  | Node.ExprIdent name ->
    infer_expr_ident table interner diags mono_state name expr.span
  | Node.ExprBinary (_, e1, e2) ->
    infer_expr_binary ctx table interner diags mono_state e1 e2
  | Node.ExprUnary (_, e) -> infer_expr ctx table interner diags mono_state e
  | Node.ExprCall (callee, args, _) ->
    infer_expr_call
      ctx
      table
      interner
      diags
      mono_state
      ?expected
      callee
      args
      expr.span
  | Node.ExprField (base, field, _) ->
    infer_expr_field ctx table interner diags mono_state base field expr.span
  | Node.ExprIndex (base, idx, _) ->
    infer_expr_index ctx table interner diags mono_state base idx expr.span
  | Node.ExprRange (e1, e2, _) ->
    infer_expr_range ctx table interner diags mono_state e1 e2
  | Node.ExprTuple exprs ->
    infer_expr_tuple ctx table interner diags mono_state exprs
  | Node.ExprArray exprs ->
    infer_expr_array ctx table interner diags mono_state exprs
  | Node.ExprBlock exprs ->
    infer_expr_block ctx table interner diags mono_state exprs
  | Node.ExprIf (cond, then_br, else_opt) ->
    infer_expr_if ctx table interner diags mono_state cond then_br else_opt
  | Node.ExprWhile (cond, body) ->
    infer_expr_while ctx table interner diags mono_state cond body
  | Node.ExprDo (body, cond_opt) ->
    infer_expr_do ctx table interner diags mono_state body cond_opt
  | Node.ExprFor (pat, iter, body) ->
    infer_expr_for ctx table interner diags mono_state pat iter body expr.span
  | Node.ExprReturn e_opt ->
    infer_expr_return ctx table interner diags mono_state e_opt expr.span
  | Node.ExprBreak e_opt ->
    infer_expr_break ctx table interner diags mono_state e_opt expr.span
  | Node.ExprContinue -> infer_expr_continue ctx diags expr.span
  | Node.ExprBinding (_, ty_params, pat, ty_opt, init, _) ->
    infer_expr_binding
      ctx
      table
      interner
      diags
      mono_state
      ty_params
      pat
      ty_opt
      init
  | Node.ExprAssign (lhs, rhs) ->
    infer_expr_assign ctx table interner diags mono_state lhs rhs expr.span
  | Node.ExprFn (ty_params, params, _, body_opt, mods) ->
    infer_expr_fn
      table
      interner
      diags
      mono_state
      ty_params
      params
      body_opt
      mods
      expr.span
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

and infer_expr_literal ctx table interner diags mono_state ?expected kind =
  match kind with
  | Node.LitInt text -> (
    match expected with
    | Some (Types.TyNamed name) when Interner.lookup interner name = "Int" ->
      Types.TyNamed (Interner.intern interner "Int")
    | Some (Types.TyNamed name) when Interner.lookup interner name = "Nat" ->
      Types.TyNamed (Interner.intern interner "Nat")
    | _ ->
      Types.TyNamed
        (Interner.intern
           interner
           (if String.length text > 0 && text.[0] = '-' then "Int" else "Nat")))
  | Node.LitBin _ -> Types.TyNamed (Interner.intern interner "Nat")
  | Node.LitStr _ -> Types.TyNamed (Interner.intern interner "Str")
  | Node.LitRune _ -> Types.TyNamed (Interner.intern interner "Rune")
  | Node.LitBool _ -> Types.TyNamed (Interner.intern interner "Bool")
  | Node.LitRecord fields -> (
    match expected with
    | Some expected_ty -> (
      match Types.repr expected_ty with
      | Types.TyRecord expected_fields ->
        List.iter
          (fun (name, expr) ->
            match List.assoc_opt name expected_fields with
            | Some expected_field_ty ->
              let field_ty =
                infer_expr
                  ctx
                  table
                  interner
                  diags
                  mono_state
                  ~expected:expected_field_ty
                  expr
              in
              safe_unify
                interner
                diags
                field_ty
                expected_field_ty
                expr.Node.span
            | None -> ())
          fields;
        expected_ty
      | _ -> expected_ty)
    | None ->
      let field_tys =
        List.map
          (fun (name, expr) ->
            (name, infer_expr ctx table interner diags mono_state expr))
          fields
      in
      Types.TyRecord field_tys)

and infer_expr_ident table interner diags mono_state name span =
  match Symbol.lookup table name with
  | Some sym -> (
    let ty = !(sym.ty) in
    match ty with
    | Types.TyScheme (_, Types.TyRecord _) -> ty
    | _ ->
      let instiated = Types.instiate ty in
      let _ = Mono.add_inst mono_state name [] in
      instiated)
  | None ->
    error
      diags
      (Printf.sprintf
         "undefined identifier '%s'"
         (Interner.lookup interner name))
      span;
    Types.TyError

and infer_expr_binary ctx table interner diags mono_state e1 e2 =
  let t1 = infer_expr ctx table interner diags mono_state e1 in
  let t2 = infer_expr ctx table interner diags mono_state e2 in
  safe_unify interner diags t1 t2 e2.span;
  t1

and infer_expr_call ctx table interner diags mono_state ?expected callee args
  span =
  let callee_ty = infer_expr ctx table interner diags mono_state callee in
  let arg_tys =
    List.map (infer_expr ctx table interner diags mono_state) args
  in
  match (callee.Node.ekind, args) with
  | ( Node.ExprIdent name
    , [
        ({ Node.ekind = Node.ExprLiteral (Node.LitRecord _); _ } as record_arg)
      ] ) -> (
    match Symbol.lookup table name with
    | Some _sym ->
      let record_ty =
        match expected with
        | Some expected_ty ->
          let lookup name =
            Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
          in
          Mono.manage_generic_insting
            mono_state
            name
            []
            lookup
            ~expected:expected_ty
            ()
        | None ->
          let lookup name =
            Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
          in
          Mono.manage_generic_insting mono_state name [] lookup ()
      in
      let _ =
        infer_expr
          ctx
          table
          interner
          diags
          mono_state
          ~expected:record_ty
          record_arg
      in
      let _ = Mono.add_inst mono_state name arg_tys in
      record_ty
    | None ->
      error
        diags
        (Printf.sprintf
           "undefined record literal '%s'"
           (Interner.lookup interner name))
        span;
      Types.TyError)
  | Node.ExprIdent name, _ -> (
    let _ = Mono.add_inst mono_state name arg_tys in
    match Types.repr callee_ty with
    | Types.TyFn (param_tys, ret_ty) ->
      if List.length args <> List.length param_tys then
        error
          diags
          (Printf.sprintf
             "expected %d argument(s), found %d"
             (List.length param_tys)
             (List.length args))
          span;
      List.iter2
        (fun param_ty arg_ty -> safe_unify interner diags param_ty arg_ty span)
        param_tys
        arg_tys;
      ret_ty
    | Types.TyError -> Types.TyError
    | _ ->
      error diags "expected function type in call expression" span;
      Types.TyError)
  | _ -> (
    match Types.repr callee_ty with
    | Types.TyFn (param_tys, ret_ty) ->
      if List.length args <> List.length param_tys then
        error
          diags
          (Printf.sprintf
             "expected %d argument(s), found %d"
             (List.length param_tys)
             (List.length args))
          span;
      List.iter2
        (fun param_ty arg_ty -> safe_unify interner diags param_ty arg_ty span)
        param_tys
        arg_tys;
      ret_ty
    | Types.TyError -> Types.TyError
    | _ ->
      error diags "expected function type in call expression" span;
      Types.TyError)

and infer_expr_tuple ctx table interner diags mono_state exprs =
  Types.TyTuple
    (List.map (infer_expr ctx table interner diags mono_state) exprs)

and infer_expr_array ctx table interner diags mono_state ?expected exprs =
  match exprs with
  | [] -> (
    match expected with
    | Some (Types.TyArray elem_ty) -> Types.TyArray elem_ty
    | _ -> Types.TyArray (Types.fresh_var ()))
  | e :: rest ->
    let expected_elem =
      match expected with
      | Some (Types.TyArray elem_ty) -> Some elem_ty
      | _ -> None
    in
    let elem_ty =
      infer_expr ctx table interner diags mono_state ?expected:expected_elem e
    in
    List.iter
      (fun e ->
        safe_unify
          interner
          diags
          elem_ty
          (infer_expr
             ctx
             table
             interner
             diags
             mono_state
             ?expected:expected_elem
             e)
          e.span)
      rest;
    Types.TyArray elem_ty

and infer_expr_field ctx table interner diags mono_state base field span =
  match Types.repr (infer_expr ctx table interner diags mono_state base) with
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

and infer_expr_index ctx table interner diags mono_state base idx span =
  safe_unify
    interner
    diags
    (infer_expr ctx table interner diags mono_state idx)
    (Types.TyNamed (Interner.intern interner "Nat"))
    idx.span;
  match Types.repr (infer_expr ctx table interner diags mono_state base) with
  | Types.TyArray elem_ty -> elem_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected array type in index expression" span;
    Types.TyError

and infer_expr_range ctx table interner diags mono_state e1 e2 =
  let t1 = infer_expr ctx table interner diags mono_state e1 in
  safe_unify
    interner
    diags
    t1
    (infer_expr ctx table interner diags mono_state e2)
    e2.span;
  Types.TyUnit

and infer_expr_if ctx table interner diags mono_state cond then_br else_opt =
  safe_unify
    interner
    diags
    (infer_expr ctx table interner diags mono_state cond)
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  let then_ty = infer_expr ctx table interner diags mono_state then_br in
  match else_opt with
  | Some else_br ->
    safe_unify
      interner
      diags
      then_ty
      (infer_expr ctx table interner diags mono_state else_br)
      else_br.span;
    then_ty
  | None ->
    safe_unify interner diags then_ty Types.TyUnit then_br.span;
    Types.TyUnit

and infer_expr_while ctx table interner diags mono_state cond body =
  let loop_ctx = { ctx with in_loop = true } in
  safe_unify
    interner
    diags
    (infer_expr loop_ctx table interner diags mono_state cond)
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  Types.TyUnit

and infer_expr_do ctx table interner diags mono_state body cond_opt =
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  Option.iter
    (fun cond ->
      safe_unify
        interner
        diags
        (infer_expr loop_ctx table interner diags mono_state cond)
        (Types.TyNamed (Interner.intern interner "Bool"))
        cond.span)
    cond_opt;
  Types.TyUnit

and infer_expr_for ctx table interner diags mono_state _pat iter body _span =
  let loop_ctx = { ctx with in_loop = true } in
  let iter_ty = infer_expr loop_ctx table interner diags mono_state iter in
  (match Types.repr iter_ty with
  | Types.TyArray _elem_ty -> ()
  | Types.TyError -> ()
  | _ -> error diags "expected array or range in for-loop iterator" iter.span);
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  Types.TyUnit

and infer_expr_return ctx table interner diags mono_state e_opt span =
  if not ctx.in_fn then error diags "'return' outside of function body" span;
  Option.iter
    (fun e -> ignore (infer_expr ctx table interner diags mono_state e))
    e_opt;
  Types.TyUnit

and infer_expr_break ctx table interner diags mono_state e_opt span =
  if not ctx.in_loop then error diags "'break' outside of loop body" span;
  Option.iter
    (fun e -> ignore (infer_expr ctx table interner diags mono_state e))
    e_opt;
  Types.TyUnit

and infer_expr_continue ctx diags span =
  if not ctx.in_loop then error diags "'continue' outside of loop body" span;
  Types.TyUnit

and infer_expr_block ctx table interner diags mono_state exprs =
  match List.rev exprs with
  | [] -> Types.TyUnit
  | last :: rest ->
    List.iter
      (fun e -> ignore (infer_expr ctx table interner diags mono_state e))
      (List.rev rest);
    infer_expr ctx table interner diags mono_state last

and infer_expr_binding ctx table interner diags mono_state ty_params pat ty_opt
  init =
  let expected_ty_opt =
    match ty_opt with
    | Some ty_node ->
      let lookup name =
        Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
      in
      Some (Mono.infer_ty mono_state lookup ty_node)
    | None -> None
  in
  let init_ty =
    infer_expr
      ctx
      table
      interner
      diags
      mono_state
      ?expected:expected_ty_opt
      init
  in
  (match expected_ty_opt with
  | Some expected_ty -> safe_unify interner diags init_ty expected_ty init.span
  | None -> ());
  (match pat.Node.pkind with
  | Node.PatIdent (name, _) | Node.PatBinding name -> (
    match Symbol.lookup table name with
    | Some sym ->
      if ty_params <> [] then (
        let init_ty_with_params =
          infer_expr ctx table interner diags mono_state init
        in

        let rec collect_vars acc = function
          | Types.TyVar v -> if List.mem v acc then acc else v :: acc
          | Types.TyRecord fields ->
            List.fold_left (fun acc (_, t) -> collect_vars acc t) acc fields
          | Types.TyArray t -> collect_vars acc t
          | Types.TyTuple ts -> List.fold_left collect_vars acc ts
          | Types.TyFn (params, ret) ->
            List.fold_left collect_vars (collect_vars acc ret) params
          | _ -> acc
        in
        let ty_var_refs = collect_vars [] init_ty_with_params in
        let scheme = Types.TyScheme (ty_var_refs, init_ty_with_params) in
        sym.ty := scheme;
        let _ = Mono.add_inst mono_state name [] in
        ())
      else sym.ty := init_ty
    | None -> ())
  | _ -> ());
  Types.TyUnit

and infer_expr_fn table interner diags mono_state ty_params params body_opt
  _mods _span =
  let fn_ctx = { in_fn = true; in_loop = false } in
  let scoped_table = Symbol.enter_scope table in
  let ty_var_refs =
    List.map
      (fun _ ->
        let id = !Types.var_idx in
        incr Types.var_idx;
        ref (Types.Unbound id))
      ty_params
  in
  let ty_vars = List.map (fun r -> Types.TyVar r) ty_var_refs in
  List.iter2
    (fun tp tv ->
      let tp_sym =
        {
          Symbol.name = tp
        ; ty = ref tv
        ; is_mutable = false
        ; is_exported = false
        ; span = Span.dummy
        }
      in
      ignore (Symbol.bind scoped_table tp tp_sym))
    ty_params
    ty_vars;
  let param_tys =
    List.map
      (fun (p : Node.param) ->
        let param_ty =
          match p.pty with
          | Some ty_node ->
            let lookup name =
              Option.map
                (fun s -> !(s.Symbol.ty))
                (Symbol.lookup scoped_table name)
            in
            Mono.infer_ty mono_state lookup ty_node
          | None -> Types.fresh_var ()
        in
        let param_sym =
          {
            Symbol.name = p.pname
          ; ty = ref param_ty
          ; is_mutable = p.is_mutable
          ; is_exported = false
          ; span = Span.dummy
          }
        in
        ignore (Symbol.bind scoped_table p.pname param_sym);
        param_ty)
      params
  in
  let ret_ty =
    match body_opt with
    | Some body -> infer_expr fn_ctx scoped_table interner diags mono_state body
    | None -> Types.TyUnit
  in
  ignore (Symbol.exit_scope scoped_table);
  let fn_ty = Types.TyFn (param_tys, ret_ty) in
  if ty_params <> [] then
    let scheme = Types.TyScheme (ty_var_refs, fn_ty) in

    scheme
  else fn_ty

and infer_expr_assign ctx table interner diags mono_state lhs rhs span =
  let lhs_ty = infer_expr ctx table interner diags mono_state lhs in
  let rhs_ty = infer_expr ctx table interner diags mono_state rhs in
  safe_unify interner diags lhs_ty rhs_ty rhs.span;
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

let check_stmt table interner diags mono_state stmt =
  match stmt.Node.skind with
  | Node.StmtExpr (expr, _) ->
    ignore (infer_expr empty_context table interner diags mono_state expr)
  | Node.StmtImport _ | Node.StmtExport _ | Node.StmtError -> ()

let check nodes table interner diags mono_state =
  List.iter (check_stmt table interner diags mono_state) nodes

let check_all nodes interner diags =
  let table = Resolver.resolve nodes interner diags in
  let mono_state = Mono.create interner in
  check nodes table interner diags mono_state
