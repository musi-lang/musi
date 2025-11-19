open Musi_basic
open Musi_parse

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let warn diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.warning msg span)

let safe_unify interner diags t1 t2 span =
  try Unifier.unify (Types.repr t1) (Types.repr t2) with
  | Failure msg when String.contains msg '\'' ->
    let t1_str = Types.show_with interner (Types.repr t1) in
    let t2_str = Types.show_with interner (Types.repr t2) in
    let fixed_msg =
      Printf.sprintf "expected type '%s', found type '%s'" t1_str t2_str
    in
    error diags fixed_msg span
  | Failure msg -> error diags msg span

let handle_ty_params table ty_params =
  List.iter
    (fun tp ->
      match Symbol.lookup table tp with
      | Some sym -> ignore (Types.instiate !(sym.ty))
      | None -> ())
    ty_params

let bool_type interner = Types.TyNamed (Interner.intern interner "Bool")

let rec check_pat table interner diags pat expected_ty =
  match pat.Node.pkind with
  | Node.PatIdent (name, _) | Node.PatBinding name -> (
    match Symbol.lookup table name with
    | Some sym -> safe_unify interner diags !(sym.ty) expected_ty pat.span
    | None -> ())
  | _ -> ()

and check_expr ctx table interner diags mono_state expected expr =
  match expr.Node.ekind with
  | Node.ExprLiteral kind ->
    check_expr_literal
      ctx
      table
      interner
      diags
      mono_state
      expected
      kind
      expr.span
  | Node.ExprIdent name ->
    check_expr_ident table interner diags mono_state expected name expr.span
  | Node.ExprBinary (_, e1, e2) ->
    check_expr_binary
      ctx
      table
      interner
      diags
      mono_state
      expected
      e1
      e2
      expr.span
  | Node.ExprUnary (_, e) ->
    check_expr ctx table interner diags mono_state expected e
  | Node.ExprCall (callee, args, _) ->
    check_expr_call
      ctx
      table
      interner
      diags
      mono_state
      expected
      callee
      args
      expr.span
  | Node.ExprField (base, field, _) ->
    check_expr_field
      ctx
      table
      interner
      diags
      mono_state
      expected
      base
      field
      expr.span
  | Node.ExprIndex (base, idx, _) ->
    check_expr_index
      ctx
      table
      interner
      diags
      mono_state
      expected
      base
      idx
      expr.span
  | Node.ExprTuple exprs ->
    check_expr_tuple
      ctx
      table
      interner
      diags
      mono_state
      expected
      exprs
      expr.span
  | Node.ExprArray exprs ->
    check_expr_array
      ctx
      table
      interner
      diags
      mono_state
      expected
      exprs
      expr.span
  | Node.ExprBlock exprs ->
    check_expr_block ctx table interner diags mono_state expected exprs
  | Node.ExprIf (cond, then_br, else_opt) ->
    check_expr_if
      ctx
      table
      interner
      diags
      mono_state
      expected
      cond
      then_br
      else_opt
  | Node.ExprWhile (cond, body) ->
    check_expr_while
      ctx
      table
      interner
      diags
      mono_state
      expected
      cond
      body
      expr.span
  | Node.ExprDo (body, cond_opt) ->
    check_expr_do
      ctx
      table
      interner
      diags
      mono_state
      expected
      body
      cond_opt
      expr.span
  | Node.ExprFor (pat, iter, body) ->
    check_expr_for
      ctx
      table
      interner
      diags
      mono_state
      expected
      pat
      iter
      body
      expr.span
  | Node.ExprReturn e_opt ->
    check_expr_return
      ctx
      table
      interner
      diags
      mono_state
      expected
      e_opt
      expr.span
  | Node.ExprBreak e_opt ->
    check_expr_break
      ctx
      table
      interner
      diags
      mono_state
      expected
      e_opt
      expr.span
  | Node.ExprContinue ->
    check_expr_continue ctx table interner diags mono_state expected expr.span
  | Node.ExprBinding (_, ty_params, pat, ty_opt, init, _) ->
    check_expr_binding
      ctx
      table
      interner
      diags
      mono_state
      expected
      ty_params
      pat
      ty_opt
      init
      expr.span
  | Node.ExprAssign (lhs, rhs) ->
    check_expr_assign
      ctx
      table
      interner
      diags
      mono_state
      expected
      lhs
      rhs
      expr.span
  | Node.ExprFn (ty_params, params, _, body_opt, mods) ->
    check_expr_fn
      table
      interner
      diags
      mono_state
      expected
      ty_params
      params
      body_opt
      mods
      expr.span
  | _ ->
    error
      diags
      (Printf.sprintf
         "unimplemented '%s' in type checking"
         (Node.string_of_expr expr))
      expr.span

and check_expr_literal ctx table interner diags mono_state expected kind span =
  let inferred =
    match kind with
    | Node.LitInt text -> (
      match expected with
      | Types.TyNamed name when Interner.lookup interner name = "Int" ->
        Types.TyNamed (Interner.intern interner "Int")
      | Types.TyNamed name when Interner.lookup interner name = "Nat" ->
        Types.TyNamed (Interner.intern interner "Nat")
      | _ ->
        Types.TyNamed
          (Interner.intern
             interner
             (if String.length text > 0 && text.[0] = '-' then "Int" else "Nat"))
      )
    | Node.LitBin _ -> Types.TyNamed (Interner.intern interner "Nat")
    | Node.LitStr _ -> Types.TyNamed (Interner.intern interner "Str")
    | Node.LitRune _ -> Types.TyNamed (Interner.intern interner "Rune")
    | Node.LitBool _ -> Types.TyNamed (Interner.intern interner "Bool")
    | Node.LitRecord fields -> (
      match Types.repr expected with
      | Types.TyRecord expected_fields ->
        List.iter
          (fun (name, expr) ->
            match List.assoc_opt name expected_fields with
            | Some expected_field_ty ->
              check_expr
                ctx
                table
                interner
                diags
                mono_state
                expected_field_ty
                expr
            | None ->
              error
                diags
                (Printf.sprintf
                   "unexpected field '%s'"
                   (Interner.lookup interner name))
                expr.Node.span)
          fields;
        expected
      | _ ->
        let field_tys =
          List.map
            (fun (name, expr) ->
              ( name
              , Inferrer.infer_expr ctx table interner diags mono_state expr ))
            fields
        in
        Types.TyRecord field_tys)
  in
  safe_unify interner diags inferred expected span

and check_expr_ident table interner diags mono_state expected name span =
  match Symbol.lookup table name with
  | Some sym ->
    let ty = !(sym.ty) in
    let instiated = Types.instiate ty in
    let _ = Mono.add_inst mono_state name [] in
    safe_unify interner diags instiated expected span
  | None ->
    error
      diags
      (Printf.sprintf
         "undefined identifier '%s'"
         (Interner.lookup interner name))
      span

and check_expr_binary ctx table interner diags mono_state expected e1 e2 span =
  let t1 = Inferrer.infer_expr ctx table interner diags mono_state e1 in
  let t2 = Inferrer.infer_expr ctx table interner diags mono_state e2 in
  safe_unify interner diags t1 t2 span;
  safe_unify interner diags t1 expected span

and check_expr_call ctx table interner diags mono_state expected callee args
  span =
  let callee_ty =
    Inferrer.infer_expr ctx table interner diags mono_state callee
  in
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
      (fun param_ty arg ->
        check_expr ctx table interner diags mono_state param_ty arg)
      param_tys
      args;
    safe_unify interner diags ret_ty expected span
  | Types.TyError -> ()
  | _ -> error diags "expected function type in call expression" span

and check_expr_field ctx table interner diags mono_state expected base field
  span =
  let base_ty = Inferrer.infer_expr ctx table interner diags mono_state base in
  match Types.repr base_ty with
  | Types.TyRecord fields -> (
    match List.assoc_opt field fields with
    | Some field_ty -> safe_unify interner diags field_ty expected span
    | None ->
      error
        diags
        (Printf.sprintf "field '%s' not found" (Interner.lookup interner field))
        span)
  | Types.TyError -> ()
  | _ -> error diags "expected record type for field access" span

and check_expr_index ctx table interner diags mono_state expected base idx span
    =
  let base_ty = Inferrer.infer_expr ctx table interner diags mono_state base in
  check_expr
    ctx
    table
    interner
    diags
    mono_state
    (Types.TyNamed (Interner.intern interner "Nat"))
    idx;
  match Types.repr base_ty with
  | Types.TyArray elem_ty -> safe_unify interner diags elem_ty expected span
  | Types.TyError -> ()
  | _ -> error diags "expected array type for index access" span

and check_expr_tuple ctx table interner diags mono_state expected exprs span =
  match Types.repr expected with
  | Types.TyTuple expected_tys ->
    if List.length exprs <> List.length expected_tys then
      error
        diags
        (Printf.sprintf
           "expected %d elements, found %d"
           (List.length expected_tys)
           (List.length exprs))
        span;
    List.iter2
      (fun expected_ty expr ->
        check_expr ctx table interner diags mono_state expected_ty expr)
      expected_tys
      exprs
  | _ -> error diags "expected tuple type" span

and check_expr_array ctx table interner diags mono_state expected exprs span =
  match Types.repr expected with
  | Types.TyArray elem_ty ->
    List.iter (check_expr ctx table interner diags mono_state elem_ty) exprs
  | _ -> error diags "expected array type" span

and check_expr_block ctx table interner diags mono_state expected exprs =
  match List.rev exprs with
  | [] -> safe_unify interner diags Types.TyUnit expected Span.dummy
  | last :: rest ->
    List.iter
      (fun expr ->
        ignore (Inferrer.infer_expr ctx table interner diags mono_state expr))
      (List.rev rest);
    check_expr ctx table interner diags mono_state expected last

and check_expr_if ctx table interner diags mono_state expected cond then_br
  else_opt =
  check_expr ctx table interner diags mono_state (bool_type interner) cond;
  check_expr ctx table interner diags mono_state expected then_br;
  match else_opt with
  | Some else_br ->
    check_expr ctx table interner diags mono_state expected else_br
  | None -> safe_unify interner diags Types.TyUnit expected Span.dummy

and check_expr_while ctx table interner diags mono_state expected cond body span
    =
  check_expr ctx table interner diags mono_state (bool_type interner) cond;
  let loop_ctx = { ctx with Inferrer.in_loop = true } in
  ignore (Inferrer.infer_expr loop_ctx table interner diags mono_state body);
  safe_unify interner diags Types.TyUnit expected span

and check_expr_do ctx table interner diags mono_state expected body cond_opt
  span =
  let loop_ctx = { ctx with Inferrer.in_loop = true } in
  ignore (Inferrer.infer_expr loop_ctx table interner diags mono_state body);
  (match cond_opt with
  | Some cond ->
    check_expr
      loop_ctx
      table
      interner
      diags
      mono_state
      (bool_type interner)
      cond
  | None -> ());
  safe_unify interner diags Types.TyUnit expected span

and check_expr_for ctx table interner diags mono_state expected pat iter body
  span =
  let iter_ty = Inferrer.infer_expr ctx table interner diags mono_state iter in
  let elem_ty =
    match Types.repr iter_ty with
    | Types.TyArray elem_ty -> elem_ty
    | Types.TyError -> Types.TyError
    | _ ->
      error diags "expected array type in 'for' loop" span;
      Types.TyError
  in
  check_pat table interner diags pat elem_ty;
  let loop_ctx = { ctx with Inferrer.in_loop = true } in
  ignore (Inferrer.infer_expr loop_ctx table interner diags mono_state body);
  safe_unify interner diags Types.TyUnit expected span

and check_expr_return ctx table interner diags mono_state expected e_opt span =
  if not ctx.in_fn then error diags "'return' outside of function" span;
  (match e_opt with
  | Some e -> ignore (Inferrer.infer_expr ctx table interner diags mono_state e)
  | None -> ());
  safe_unify interner diags Types.TyUnit expected span

and check_expr_break ctx table interner diags mono_state expected e_opt span =
  if not ctx.in_loop then error diags "'break' outside of loop" span;
  (match e_opt with
  | Some e -> ignore (Inferrer.infer_expr ctx table interner diags mono_state e)
  | None -> ());
  safe_unify interner diags Types.TyUnit expected span

and check_expr_continue ctx _ interner diags _ expected span =
  if not ctx.Inferrer.in_loop then error diags "'continue' outside of loop" span;
  safe_unify interner diags Types.TyUnit expected span

and check_expr_binding ctx table interner diags mono_state expected ty_params
  pat ty_opt init span =
  handle_ty_params table ty_params;
  let init_ty = Inferrer.infer_expr ctx table interner diags mono_state init in
  (match ty_opt with
  | Some expected_ty ->
    let lookup name =
      Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
    in
    let expected_init = Types.from_node lookup expected_ty in
    safe_unify interner diags init_ty expected_init init.span
  | None -> ());
  check_pat table interner diags pat init_ty;
  safe_unify interner diags init_ty expected span

and check_expr_assign ctx table interner diags mono_state expected lhs rhs span
    =
  let lhs_ty = Inferrer.infer_expr ctx table interner diags mono_state lhs in
  check_expr ctx table interner diags mono_state lhs_ty rhs;
  safe_unify interner diags Types.TyUnit expected span

and check_expr_fn table interner diags mono_state expected ty_params params
  body_opt mods span =
  handle_ty_params table ty_params;
  if mods.Node.is_extern && body_opt <> None then
    error diags "external functions cannot have bodies" span;
  match Types.repr expected with
  | Types.TyFn (expected_param_tys, expected_ret_ty) -> (
    if List.length params <> List.length expected_param_tys then
      error
        diags
        (Printf.sprintf
           "expected %d parameter(s), found %d"
           (List.length expected_param_tys)
           (List.length params))
        span;
    match body_opt with
    | Some body ->
      let fn_ctx = { Inferrer.in_fn = true; in_loop = false } in
      check_expr fn_ctx table interner diags mono_state expected_ret_ty body
    | None -> ())
  | _ -> error diags "expected function type" span
