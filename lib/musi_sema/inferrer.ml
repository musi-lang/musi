open Musi_basic
open Musi_parse

type context = { in_fn : bool; in_loop : bool }

let empty_context = { in_fn = false; in_loop = false }

let error diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.error msg span)

let warn diags msg span =
  diags := Diagnostic.add !diags (Diagnostic.warning msg span)

let rec subtype_unify interner t1 t2 =
  let t1_repr = Types.repr t1 in
  let t2_repr = Types.repr t2 in
  match (t1_repr, t2_repr) with
  | Types.TyNamed n1, Types.TyNamed n2 ->
    let name1 = Interner.lookup interner n1 in
    let name2 = Interner.lookup interner n2 in
    n1 = n2
    || (name1 = "Nat" && name2 = "Int")
    || (name1 = "Int" && name2 = "Nat")
  | Types.TyArray t1, Types.TyArray t2 -> subtype_unify interner t1 t2
  | Types.TyRecord fs1, Types.TyRecord fs2 ->
    List.length fs1 = List.length fs2
    && List.for_all2
         (fun (n1, t1) (n2, t2) -> n1 = n2 && subtype_unify interner t1 t2)
         fs1
         fs2
  | _ -> false

let safe_unify interner diags t1 t2 span =
  if subtype_unify interner t1 t2 then ()
  else
    try Unifier.unify (Types.repr t1) (Types.repr t2) with
    | Failure msg when String.contains msg '\'' ->
      let t1_str = Types.show_with interner (Types.repr t1) in
      let t2_str = Types.show_with interner (Types.repr t2) in
      let fixed_msg =
        Printf.sprintf "expected type '%s', found type '%s'" t1_str t2_str
      in
      error diags fixed_msg span
    | Failure msg -> error diags msg span

let rec infer_expr ctx table interner diags mono_state expr =
  match expr.Node.ekind with
  | Node.ExprLiteral kind ->
    infer_expr_literal ctx table interner diags mono_state kind
  | Node.ExprIdent name ->
    infer_expr_ident table interner diags mono_state name expr.span
  | Node.ExprBinary (_, e1, e2) ->
    infer_expr_binary ctx table interner diags mono_state e1 e2
  | Node.ExprUnary (_, e) -> infer_expr ctx table interner diags mono_state e
  | Node.ExprCall (callee, args, _) ->
    infer_expr_call ctx table interner diags mono_state callee args expr.span
  | Node.ExprField (base, field, _) ->
    infer_expr_field ctx table interner diags mono_state base field expr.span
  | Node.ExprIndex (base, idx, _) ->
    infer_expr_index ctx table interner diags mono_state base idx expr.span
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
  | Node.ExprRecord (ty_args, fields, _) ->
    infer_expr_record
      ctx
      table
      interner
      diags
      mono_state
      ty_args
      fields
      expr.span
  | _ ->
    error
      diags
      (Printf.sprintf
         "unimplemented '%s' in type inference"
         (Node.string_of_expr expr))
      expr.span;
    Types.TyError

and infer_expr_literal ctx table interner diags mono_state kind =
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
    let field_tys =
      List.map
        (fun (name, expr) ->
          (name, infer_expr ctx table interner diags mono_state expr))
        fields
    in
    Types.TyRecord field_tys

and infer_expr_ident table interner diags mono_state name span =
  match Symbol.lookup table name with
  | Some sym ->
    let ty = !(sym.ty) in
    let instiated = Types.instiate ty in
    let _ = Mono.add_inst mono_state name [] in
    instiated
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

and infer_expr_call ctx table interner diags mono_state callee args span =
  let callee_ty = infer_expr ctx table interner diags mono_state callee in
  let arg_tys =
    List.map (infer_expr ctx table interner diags mono_state) args
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
      (fun param_ty arg_ty -> safe_unify interner diags param_ty arg_ty span)
      param_tys
      arg_tys;
    ret_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected function type in call expression" span;
    Types.TyError

and infer_expr_field ctx table interner diags mono_state base field span =
  let base_ty = infer_expr ctx table interner diags mono_state base in
  match Types.repr base_ty with
  | Types.TyRecord fields -> (
    match List.assoc_opt field fields with
    | Some field_ty -> field_ty
    | None ->
      error
        diags
        (Printf.sprintf "field '%s' not found" (Interner.lookup interner field))
        span;
      Types.TyError)
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected record type for field access" span;
    Types.TyError

and infer_expr_index ctx table interner diags mono_state base idx span =
  let base_ty = infer_expr ctx table interner diags mono_state base in
  let idx_ty = infer_expr ctx table interner diags mono_state idx in
  match Types.repr base_ty with
  | Types.TyArray elem_ty ->
    safe_unify
      interner
      diags
      idx_ty
      (Types.TyNamed (Interner.intern interner "Nat"))
      span;
    elem_ty
  | Types.TyError -> Types.TyError
  | _ ->
    error diags "expected array type for index access" span;
    Types.TyError

and infer_expr_tuple ctx table interner diags mono_state exprs =
  let elem_tys =
    List.map (infer_expr ctx table interner diags mono_state) exprs
  in
  Types.TyTuple elem_tys

and infer_expr_array ctx table interner diags mono_state exprs =
  match exprs with
  | [] -> Types.TyArray (Types.fresh_var ())
  | first :: rest ->
    let first_ty = infer_expr ctx table interner diags mono_state first in
    List.iter
      (fun expr ->
        let expr_ty = infer_expr ctx table interner diags mono_state expr in
        safe_unify interner diags first_ty expr_ty expr.span)
      rest;
    Types.TyArray first_ty

and infer_expr_block ctx table interner diags mono_state exprs =
  match List.rev exprs with
  | [] -> Types.TyUnit
  | last :: rest ->
    List.iter
      (fun expr -> ignore (infer_expr ctx table interner diags mono_state expr))
      (List.rev rest);
    infer_expr ctx table interner diags mono_state last

and infer_expr_if ctx table interner diags mono_state cond then_br else_opt =
  let cond_ty = infer_expr ctx table interner diags mono_state cond in
  safe_unify
    interner
    diags
    cond_ty
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  let then_ty = infer_expr ctx table interner diags mono_state then_br in
  match else_opt with
  | Some else_br ->
    let else_ty = infer_expr ctx table interner diags mono_state else_br in
    safe_unify interner diags then_ty else_ty else_br.span;
    then_ty
  | None -> Types.TyUnit

and infer_expr_while ctx table interner diags mono_state cond body =
  let cond_ty = infer_expr ctx table interner diags mono_state cond in
  safe_unify
    interner
    diags
    cond_ty
    (Types.TyNamed (Interner.intern interner "Bool"))
    cond.span;
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  Types.TyUnit

and infer_expr_do ctx table interner diags mono_state body cond_opt =
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  (match cond_opt with
  | Some cond ->
    let cond_ty = infer_expr loop_ctx table interner diags mono_state cond in
    safe_unify
      interner
      diags
      cond_ty
      (Types.TyNamed (Interner.intern interner "Bool"))
      cond.span
  | None -> ());
  Types.TyUnit

and infer_expr_for ctx table interner diags mono_state pat iter body span =
  let iter_ty = infer_expr ctx table interner diags mono_state iter in
  let elem_ty =
    match Types.repr iter_ty with
    | Types.TyArray elem_ty -> elem_ty
    | Types.TyError -> Types.TyError
    | _ ->
      error diags "expected array type in 'for' loop" span;
      Types.TyError
  in
  let rec check_pattern pat expected_ty =
    match pat.Node.pkind with
    | Node.PatIdent (name, _) | Node.PatBinding name -> (
      match Symbol.lookup table name with
      | Some sym -> safe_unify interner diags !(sym.ty) expected_ty pat.span
      | None -> ())
    | Node.PatTuple pats -> (
      match Types.repr expected_ty with
      | Types.TyTuple elem_tys when List.length pats = List.length elem_tys ->
        List.iter2 check_pattern pats elem_tys
      | _ -> error diags "pattern type mismatch" pat.span)
    | Node.PatArray (pats, _) -> (
      match Types.repr expected_ty with
      | Types.TyArray elem_ty ->
        List.iter (fun p -> check_pattern p elem_ty) pats
      | _ -> error diags "pattern type mismatch" pat.span)
    | Node.PatRecord fields -> (
      match Types.repr expected_ty with
      | Types.TyRecord field_tys ->
        List.iter
          (fun (name, p) ->
            match List.assoc_opt name field_tys with
            | Some field_ty -> check_pattern p field_ty
            | None ->
              error
                diags
                (Printf.sprintf
                   "field '%s' not found"
                   (Interner.lookup interner name))
                pat.span)
          fields
      | _ -> error diags "pattern type mismatch" pat.span)
    | Node.PatWild | Node.PatLiteral _ | Node.PatRest _ | Node.PatChoice _
    | Node.PatError ->
      ()
  in
  check_pattern pat elem_ty;
  let loop_ctx = { ctx with in_loop = true } in
  ignore (infer_expr loop_ctx table interner diags mono_state body);
  Types.TyUnit

and infer_expr_return ctx table interner diags mono_state e_opt span =
  if not ctx.in_fn then error diags "'return' outside of function" span;
  (match e_opt with
  | Some e -> ignore (infer_expr ctx table interner diags mono_state e)
  | None -> ());
  Types.TyUnit

and infer_expr_break ctx table interner diags mono_state e_opt span =
  if not ctx.in_loop then error diags "'break' outside of loop" span;
  (match e_opt with
  | Some e -> ignore (infer_expr ctx table interner diags mono_state e)
  | None -> ());
  Types.TyUnit

and infer_expr_continue ctx diags span =
  if not ctx.in_loop then error diags "'continue' outside of loop" span;
  Types.TyUnit

and infer_expr_binding ctx table interner diags mono_state ty_params pat ty_opt
  init =
  let ty_vars =
    List.map (fun _ -> ref (Types.Unbound !Types.var_idx)) ty_params
  in
  List.iter (fun _ -> incr Types.var_idx) ty_params;
  List.iter2
    (fun tp var_ref ->
      match Symbol.lookup table tp with
      | Some sym -> sym.ty := Types.TyVar var_ref
      | None -> ())
    ty_params
    ty_vars;
  let init_ty = infer_expr ctx table interner diags mono_state init in
  let final_ty =
    if ty_params <> [] then Types.TyScheme (ty_vars, init_ty) else init_ty
  in
  (match ty_opt with
  | Some expected_ty ->
    let lookup name =
      Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
    in
    let expected = Types.from_node lookup expected_ty in
    safe_unify interner diags final_ty expected init.span
  | None -> ());
  let rec bind_pattern pat bind_ty =
    match pat.Node.pkind with
    | Node.PatIdent (name, _) | Node.PatBinding name -> (
      match Symbol.lookup table name with
      | Some sym -> sym.ty := bind_ty
      | None -> ())
    | Node.PatTuple pats -> (
      match Types.repr bind_ty with
      | Types.TyTuple elem_tys when List.length pats = List.length elem_tys ->
        List.iter2 bind_pattern pats elem_tys
      | _ -> ())
    | _ -> ()
  in
  bind_pattern pat final_ty;
  final_ty

and infer_expr_assign ctx table interner diags mono_state lhs rhs span =
  (match lhs.Node.ekind with
  | Node.ExprIdent name -> (
    match Symbol.lookup table name with
    | Some sym when not sym.is_mutable ->
      error
        diags
        (Printf.sprintf
           "cannot assign to immutable binding '%s'"
           (Interner.lookup interner name))
        span
    | _ -> ())
  | _ -> ());
  let lhs_ty = infer_expr ctx table interner diags mono_state lhs in
  let rhs_ty = infer_expr ctx table interner diags mono_state rhs in
  safe_unify interner diags lhs_ty rhs_ty span;
  Types.TyUnit

and infer_expr_fn table interner diags mono_state ty_params params body_opt mods
  span =
  List.iter
    (fun tp ->
      match Symbol.lookup table tp with
      | Some sym -> ignore (Types.instiate !(sym.ty))
      | None -> ())
    ty_params;
  if mods.Node.is_extern && body_opt <> None then
    error diags "external functions cannot have bodies" span;
  let param_tys =
    List.map
      (fun (p : Node.param) ->
        match p.pty with
        | Some ty ->
          let lookup name =
            Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name)
          in
          Types.from_node lookup ty
        | None -> Types.fresh_var ())
      params
  in
  let ret_ty =
    match body_opt with
    | Some body ->
      let fn_ctx = { in_fn = true; in_loop = false } in
      ignore (Symbol.enter_scope table);
      List.iter2
        (fun (p : Node.param) param_ty ->
          let param_sym =
            {
              Symbol.name = p.pname
            ; ty = ref param_ty
            ; is_mutable = false
            ; is_exported = false
            ; span
            }
          in
          ignore (Symbol.bind table p.pname param_sym))
        params
        param_tys;
      let body_ty = infer_expr fn_ctx table interner diags mono_state body in
      ignore (Symbol.exit_scope table);
      body_ty
    | None -> Types.TyUnit
  in
  Types.TyFn (param_tys, ret_ty)

and infer_expr_record _ table _ _ _ _ fields _ =
  let field_tys =
    List.map
      (fun (f : Node.field) ->
        let field_ty =
          Types.from_node
            (fun name ->
              Option.map (fun s -> !(s.Symbol.ty)) (Symbol.lookup table name))
            f.fty
        in
        (f.fname, field_ty))
      fields
  in
  Types.TyRecord field_tys
