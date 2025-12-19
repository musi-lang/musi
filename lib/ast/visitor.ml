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

let visit_seq2 visit v ctx a b =
  visit v ctx a;
  visit v ctx b

let rec traverse_expr (v : 'ctx visitor) (ctx : 'ctx) (expr : expr) =
  match expr.kind with
  | ExprError -> ()
  | ExprLit lit -> v.visit_lit v ctx lit
  | ExprIdent ident -> v.visit_ident v ctx ident
  | ExprLitTuple exprs | ExprLitArray exprs ->
    visit_list v.visit_expr v ctx exprs
  | ExprLitRecord (name_opt, fields, base_opt) ->
    visit_opt v.visit_ident v ctx name_opt;
    List.iter
      (fun f ->
        v.visit_ident v ctx f.field_name;
        visit_opt v.visit_ty v ctx f.field_ty;
        visit_opt v.visit_expr v ctx f.field_default)
      fields;
    visit_opt v.visit_expr v ctx base_opt
  | ExprBlock (stmts, expr_opt) ->
    visit_list v.visit_stmt v ctx stmts;
    visit_opt v.visit_expr v ctx expr_opt
  | ExprIf (cond, then_, else_) ->
    v.visit_expr v ctx cond;
    v.visit_expr v ctx then_;
    v.visit_expr v ctx else_
  | ExprWhile (cond, body) ->
    v.visit_expr v ctx cond;
    v.visit_expr v ctx body
  | ExprFor (name, iter, body) ->
    v.visit_ident v ctx name;
    v.visit_expr v ctx iter;
    v.visit_expr v ctx body
  | ExprMatch (target, cases) ->
    v.visit_expr v ctx target;
    List.iter
      (fun c ->
        v.visit_pat v ctx c.case_pat;
        v.visit_expr v ctx c.case_expr)
      cases
  | ExprReturn expr_opt | ExprBreak expr_opt ->
    visit_opt v.visit_expr v ctx expr_opt
  | ExprDefer expr | ExprUnsafe expr -> v.visit_expr v ctx expr
  | ExprImport _ -> () (* string literal *)
  | ExprExtern (_, _, sigs) -> List.iter (fun s -> traverse_fn_sig v ctx s) sigs
  | ExprBind (_, _, name, ty_opt, init, _) ->
    v.visit_ident v ctx name;
    visit_opt v.visit_ty v ctx ty_opt;
    v.visit_expr v ctx init
  | ExprFn (_, _, sig_, body) ->
    traverse_fn_sig v ctx sig_;
    v.visit_expr v ctx body
  | ExprRecord (_, _, name_opt, _, fields) ->
    visit_opt v.visit_ident v ctx name_opt;
    List.iter
      (fun f ->
        v.visit_ident v ctx f.field_name;
        visit_opt v.visit_ty v ctx f.field_ty;
        visit_opt v.visit_expr v ctx f.field_default)
      fields
  | ExprSum (_, _, name_opt, _, cases) ->
    visit_opt v.visit_ident v ctx name_opt;
    List.iter
      (fun c ->
        v.visit_ident v ctx c.case_name;
        visit_list v.visit_ty v ctx c.case_tys;
        List.iter (fun p -> traverse_param v ctx p) c.case_params)
      cases
  | ExprCall (callee, args) ->
    v.visit_expr v ctx callee;
    visit_list v.visit_expr v ctx args
  | ExprIndex (callee, index) ->
    v.visit_expr v ctx callee;
    v.visit_expr v ctx index
  | ExprField (callee, field) ->
    v.visit_expr v ctx callee;
    v.visit_ident v ctx field
  | ExprUnaryPrefix (_, operand) -> v.visit_expr v ctx operand
  | ExprUnaryPostfix (operand, _) -> v.visit_expr v ctx operand
  | ExprBinary (left, _, right) | ExprAssign (left, right) ->
    v.visit_expr v ctx left;
    v.visit_expr v ctx right
  | ExprRange (start, _, end_opt) ->
    v.visit_expr v ctx start;
    visit_opt v.visit_expr v ctx end_opt

and traverse_pat (v : 'ctx visitor) (ctx : 'ctx) (pat : pat) =
  match pat.kind with
  | PatIdent ident -> v.visit_ident v ctx ident
  | PatWild -> ()
  | PatLit lit -> v.visit_lit v ctx lit
  | PatLitTuple pats | PatLitArray pats -> visit_list v.visit_pat v ctx pats
  | PatLitRecord (name, fields) ->
    v.visit_ident v ctx name;
    List.iter (fun (f : pat_field) -> v.visit_ident v ctx f.field_name) fields
  | PatVariant (name, tys, pat_opt) ->
    v.visit_ident v ctx name;
    visit_list v.visit_ty v ctx tys;
    visit_opt v.visit_pat v ctx pat_opt
  | PatCons (head, tail) -> visit_seq2 v.visit_pat v ctx head tail
  | PatError -> ()

and traverse_ty (v : 'ctx visitor) (ctx : 'ctx) (ty : ty) =
  match ty.kind with
  | TyIdent ident -> v.visit_ident v ctx ident
  | TyApp (name, args) ->
    v.visit_ident v ctx name;
    visit_list v.visit_ty v ctx args
  | TyArray (_, inner) ->
    (* size is int option, not expr *)
    v.visit_ty v ctx inner
  | TyOptional inner | TyPtr inner -> v.visit_ty v ctx inner
  | TyFn (arg, ret) -> visit_seq2 v.visit_ty v ctx arg ret
  | TyTuple tys -> visit_list v.visit_ty v ctx tys
  | TyError -> ()

and traverse_fn_sig v ctx sig_ =
  visit_opt v.visit_ident v ctx sig_.fn_name;
  List.iter (fun p -> traverse_param v ctx p) sig_.fn_params;
  visit_opt v.visit_ty v ctx sig_.fn_ret_ty

and traverse_param v ctx param =
  v.visit_ident v ctx param.param_name;
  visit_opt v.visit_ty v ctx param.param_ty;
  visit_opt v.visit_expr v ctx param.param_default

and traverse_stmt v ctx stmt =
  match stmt.kind with StmtExpr expr -> v.visit_expr v ctx expr

let default_visitor =
  {
    visit_expr = traverse_expr
  ; visit_pat = traverse_pat
  ; visit_ty = traverse_ty
  ; visit_stmt = traverse_stmt
  ; visit_ident = (fun _ _ _ -> ())
  ; visit_lit = (fun _ _ _ -> ())
  }
