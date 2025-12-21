open Nodes

type 'ctx visitor = {
    visit_expr : 'ctx visitor -> 'ctx -> expr -> unit
  ; visit_pat : 'ctx visitor -> 'ctx -> pat -> unit
  ; visit_ty : 'ctx visitor -> 'ctx -> ty -> unit
  ; visit_stmt : 'ctx visitor -> 'ctx -> stmt -> unit
  ; visit_ident : 'ctx visitor -> 'ctx -> ident -> unit
  ; visit_lit : 'ctx visitor -> 'ctx -> lit -> unit
}

let visit_opt visit v ctx opt = Option.iter (visit v ctx) opt
let visit_list visit v ctx list = List.iter (visit v ctx) list
let visit_delimited visit v ctx (d : _ delimited) = visit v ctx d.value

let visit_list_delimited visit v ctx (d : _ delimited) =
  List.iter (visit v ctx) d.value.elems

let visit_seq2 visit v ctx a b =
  visit v ctx a;
  visit v ctx b

let rec traverse_expr (v : 'ctx visitor) (ctx : 'ctx) (expr : expr) =
  match expr.kind with
  | ExprError -> ()
  | ExprLit lit -> v.visit_lit v ctx lit.kind
  | ExprTemplate (parts, _tail) ->
    List.iter (fun (_s, e) -> v.visit_expr v ctx e) parts
  | ExprIdent ident -> v.visit_ident v ctx ident
  | ExprLitTuple exprs | ExprLitArray exprs ->
    visit_list_delimited v.visit_expr v ctx exprs
  | ExprLitRecord { name; fields; _ } ->
    visit_opt v.visit_ident v ctx name;
    let ({ value = { with_expr; fields = { elems; _ } }; _ }
          : record_lit_content delimited) =
      fields
    in
    visit_opt
      (fun v ctx (e : expr preceded) -> v.visit_expr v ctx e.value)
      v
      ctx
      with_expr;
    List.iter
      (fun (f : record_field) ->
        v.visit_ident v ctx f.name;
        visit_opt
          (fun v ctx (t : ty preceded) -> v.visit_ty v ctx t.value)
          v
          ctx
          f.ty_annot;
        visit_opt
          (fun v ctx (e : expr preceded) -> v.visit_expr v ctx e.value)
          v
          ctx
          f.init)
      elems
  | ExprBlock block ->
    List.iter (v.visit_stmt v ctx) block.value.stmts;
    visit_opt v.visit_expr v ctx block.value.result_expr
  | ExprIf { cond; then_branch; else_if_branches; else_branch; _ } ->
    v.visit_expr v ctx cond;
    v.visit_expr v ctx then_branch;
    List.iter
      (fun b ->
        v.visit_expr v ctx b.cond;
        v.visit_expr v ctx b.branch)
      else_if_branches;
    Option.iter (fun (_, b) -> v.visit_expr v ctx b) else_branch
  | ExprWhile { cond; body; _ } ->
    v.visit_expr v ctx cond;
    v.visit_expr v ctx body
  | ExprFor { pat; target; body; _ } ->
    v.visit_pat v ctx pat;
    v.visit_expr v ctx target;
    v.visit_expr v ctx body
  | ExprMatch { target; cases; _ } ->
    v.visit_expr v ctx target;
    visit_list_delimited
      (fun v ctx (c : match_case) ->
        v.visit_pat v ctx c.pat;
        visit_opt
          (fun v ctx (e : expr preceded) -> v.visit_expr v ctx e.value)
          v
          ctx
          c.guard;
        v.visit_expr v ctx c.expr)
      v
      ctx
      cases
  | ExprReturn (_, expr_opt) | ExprBreak (_, expr_opt) ->
    visit_opt v.visit_expr v ctx expr_opt
  | ExprDefer (_, expr) | ExprUnsafe (_, expr) -> v.visit_expr v ctx expr
  | ExprImport (_, ident) -> v.visit_ident v ctx ident
  | ExprExtern { sigs; _ } ->
    visit_list_delimited
      (fun v ctx s -> traverse_fn_sig v ctx s.sig_)
      v
      ctx
      sigs
  | ExprBind { pat; ty_annot; init; _ } ->
    v.visit_pat v ctx pat;
    visit_opt
      (fun v ctx (t : ty preceded) -> v.visit_ty v ctx t.value)
      v
      ctx
      ty_annot;
    v.visit_expr v ctx init.value
  | ExprFn { sig_; body; _ } ->
    traverse_fn_sig v ctx sig_;
    v.visit_expr v ctx body
  | ExprRecord { name; ty_params; fields; _ } ->
    visit_opt v.visit_ident v ctx name;
    visit_opt
      (fun v ctx d -> visit_list_delimited v.visit_ident v ctx d)
      v
      ctx
      ty_params;
    let (fields : (record_field_def, _) separated delimited) = fields in
    visit_list_delimited
      (fun v ctx (f : record_field_def) ->
        v.visit_ident v ctx f.name;
        visit_opt
          (fun v ctx (t : ty preceded) -> v.visit_ty v ctx t.value)
          v
          ctx
          f.ty_annot;
        visit_opt
          (fun v ctx (e : expr preceded) -> v.visit_expr v ctx e.value)
          v
          ctx
          f.init)
      v
      ctx
      fields
  | ExprSum { name; ty_params; cases; _ } ->
    visit_opt v.visit_ident v ctx name;
    visit_opt
      (fun v ctx d -> visit_list_delimited v.visit_ident v ctx d)
      v
      ctx
      ty_params;
    let (cases : (sum_case, _) separated delimited) = cases in
    visit_list_delimited
      (fun v ctx (c : sum_case) ->
        v.visit_ident v ctx c.name;
        visit_opt
          (fun v ctx (d : _ delimited) ->
            visit_list_delimited v.visit_ty v ctx d)
          v
          ctx
          c.ty_args;
        visit_opt
          (fun v ctx (d : (sum_case_arg, _) separated delimited) ->
            visit_list_delimited
              (fun v ctx (a : sum_case_arg) ->
                match a with
                | SumCaseArgTy t -> v.visit_ty v ctx t
                | SumCaseArgParam p -> traverse_param v ctx p)
              v
              ctx
              d)
          v
          ctx
          c.args)
      v
      ctx
      cases
  | ExprAlias { name; ty_params; init; _ } ->
    v.visit_ident v ctx name;
    visit_opt
      (fun v ctx d -> visit_list_delimited v.visit_ident v ctx d)
      v
      ctx
      ty_params;
    v.visit_ty v ctx init.value
  | ExprCall { callee; args } ->
    v.visit_expr v ctx callee;
    visit_list_delimited v.visit_expr v ctx args
  | ExprIndex { target; index } ->
    v.visit_expr v ctx target;
    v.visit_expr v ctx index.value
  | ExprField { target; field; _ } ->
    v.visit_expr v ctx target;
    v.visit_ident v ctx field
  | ExprUnaryPrefix (_, operand) | ExprUnaryPostfix (operand, _) ->
    v.visit_expr v ctx operand
  | ExprBinary { left; right; _ } | ExprAssign { left; right; _ } ->
    v.visit_expr v ctx left;
    v.visit_expr v ctx right
  | ExprRange { left; right; _ } ->
    v.visit_expr v ctx left;
    visit_opt v.visit_expr v ctx right

and traverse_pat (v : 'ctx visitor) (ctx : 'ctx) (pat : pat) =
  match pat.kind with
  | PatIdent ident -> v.visit_ident v ctx ident
  | PatWild _ -> ()
  | PatLit lit -> v.visit_lit v ctx lit.kind
  | PatLitTuple pats | PatLitArray pats ->
    visit_list_delimited v.visit_pat v ctx pats
  | PatLitRecord { name; fields; _ } ->
    visit_opt v.visit_ident v ctx name;
    let (d : (pat_field, _) separated delimited) = fields in
    List.iter (fun (f : pat_field) -> v.visit_ident v ctx f.name) d.value.elems
  | PatVariant { name; ty_args; args } ->
    v.visit_ident v ctx name;
    visit_opt
      (fun v ctx d -> visit_list_delimited v.visit_ty v ctx d)
      v
      ctx
      ty_args;
    visit_opt
      (fun v ctx d -> visit_list_delimited v.visit_pat v ctx d)
      v
      ctx
      args
  | PatCons (head, _, tail) -> visit_seq2 v.visit_pat v ctx head tail
  | PatOr { kind = { elems; _ }; _ } -> visit_list v.visit_pat v ctx elems
  | PatError -> ()

and traverse_ty (v : 'ctx visitor) (ctx : 'ctx) (ty : ty) =
  match ty.kind with
  | TyIdent ident -> v.visit_ident v ctx ident
  | TyApp { name; args } ->
    v.visit_ident v ctx name;
    visit_list_delimited v.visit_ty v ctx args
  | TyArray { ty; _ } -> v.visit_ty v ctx ty
  | TyOptional (_, inner) | TyPtr (_, inner) -> v.visit_ty v ctx inner
  | TyFn (arg, _, ret) -> visit_seq2 v.visit_ty v ctx arg ret
  | TyTuple tys -> visit_list_delimited v.visit_ty v ctx tys
  | TyError -> ()

and traverse_fn_sig v ctx sig_ =
  visit_opt v.visit_ident v ctx sig_.name;
  visit_opt
    (fun v ctx d -> visit_list_delimited v.visit_ident v ctx d)
    v
    ctx
    sig_.ty_params;
  visit_list_delimited (fun v ctx p -> traverse_param v ctx p) v ctx sig_.params;
  visit_opt
    (fun v ctx (t : ty preceded) -> v.visit_ty v ctx t.value)
    v
    ctx
    sig_.ret_ty

and traverse_param v ctx param =
  v.visit_ident v ctx param.name;
  visit_opt
    (fun v ctx (t : ty preceded) -> v.visit_ty v ctx t.value)
    v
    ctx
    param.ty_annot;
  visit_opt
    (fun v ctx (e : expr preceded) -> v.visit_expr v ctx e.value)
    v
    ctx
    param.init

and traverse_stmt v ctx stmt =
  match stmt.kind with
  | StmtExpr expr -> v.visit_expr v ctx expr.value
  | StmtError -> ()

let default_visitor =
  {
    visit_expr = traverse_expr
  ; visit_pat = traverse_pat
  ; visit_ty = traverse_ty
  ; visit_stmt = traverse_stmt
  ; visit_ident = (fun _ _ _ -> ())
  ; visit_lit = (fun _ _ _ -> ())
  }
