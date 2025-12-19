open Nodes

type 'ctx visitor = {
    visit_expr : 'ctx visitor -> 'ctx -> expr -> unit
  ; visit_stmt : 'ctx visitor -> 'ctx -> stmt -> unit
  ; visit_pat : 'ctx visitor -> 'ctx -> pat -> unit
  ; visit_ty : 'ctx visitor -> 'ctx -> ty -> unit
  ; visit_ident : 'ctx visitor -> 'ctx -> ident -> unit
  ; visit_lit : 'ctx visitor -> 'ctx -> lit -> unit
  ; visit_fn_sig : 'ctx visitor -> 'ctx -> fn_sig -> unit
  ; visit_param : 'ctx visitor -> 'ctx -> param -> unit
  ; visit_match_case : 'ctx visitor -> 'ctx -> match_case -> unit
  ; visit_sum_case : 'ctx visitor -> 'ctx -> sum_case -> unit
  ; visit_record_field : 'ctx visitor -> 'ctx -> record_field -> unit
  ; visit_record_content : 'ctx visitor -> 'ctx -> record_content -> unit
  ; visit_prog : 'ctx visitor -> 'ctx -> prog -> unit
}

let visit_all f v ctx items = List.iter (f v ctx) items
let visit_opt f v ctx opt = Option.iter (f v ctx) opt

let visit_seq2 f v ctx a b =
  f v ctx a;
  f v ctx b

let visit_seq3 f v ctx a b c =
  f v ctx a;
  f v ctx b;
  f v ctx c

let traverse_expr (v : 'ctx visitor) (ctx : 'ctx) (expr : expr) =
  match expr.kind with
  | ExprError -> ()
  | ExprLit lit -> v.visit_lit v ctx lit
  | ExprIdent ident -> v.visit_ident v ctx ident
  | ExprLitTuple exprs | ExprLitArray exprs ->
    visit_all v.visit_expr v ctx exprs
  | ExprLitRecord (name_opt, content) ->
    visit_opt v.visit_ident v ctx name_opt;
    v.visit_record_content v ctx content
  | ExprBlock (stmts, expr_opt) ->
    visit_all v.visit_stmt v ctx stmts;
    visit_opt v.visit_expr v ctx expr_opt
  | ExprIf (cond, then_, else_) ->
    visit_seq3 v.visit_expr v ctx cond then_ else_
  | ExprWhile (cond, body) -> visit_seq2 v.visit_expr v ctx cond body
  | ExprFor (ident, iter, body) ->
    v.visit_ident v ctx ident;
    visit_seq2 v.visit_expr v ctx iter body
  | ExprMatch (target, cases) ->
    v.visit_expr v ctx target;
    visit_all v.visit_match_case v ctx cases
  | ExprTry (body, catch_opt) ->
    v.visit_expr v ctx body;
    visit_opt
      (fun v ctx (ident_opt, catch_body) ->
        visit_opt v.visit_ident v ctx ident_opt;
        v.visit_expr v ctx catch_body)
      v
      ctx
      catch_opt
  | ExprReturn expr_opt | ExprBreak expr_opt ->
    visit_opt v.visit_expr v ctx expr_opt
  | ExprDefer expr | ExprUnsafe expr -> v.visit_expr v ctx expr
  | ExprCycle -> ()
  | ExprImport _ -> ()
  | ExprExtern (_, _, sigs) -> visit_all v.visit_fn_sig v ctx sigs
  | ExprRecord (_, _, name_opt, _, fields) ->
    visit_opt v.visit_ident v ctx name_opt;
    visit_all v.visit_record_field v ctx fields
  | ExprSum (_, _, name_opt, _, cases) ->
    visit_opt v.visit_ident v ctx name_opt;
    visit_all v.visit_sum_case v ctx cases
  | ExprFn (_, _, sig_, body) ->
    v.visit_fn_sig v ctx sig_;
    v.visit_expr v ctx body
  | ExprBind (_, _, ident, ty_opt, init, next) ->
    v.visit_ident v ctx ident;
    visit_opt v.visit_ty v ctx ty_opt;
    visit_seq2 v.visit_expr v ctx init next
  | ExprCall (callee, args) ->
    v.visit_expr v ctx callee;
    visit_all v.visit_expr v ctx args
  | ExprIndex (target, index) -> visit_seq2 v.visit_expr v ctx target index
  | ExprField (target, _) -> v.visit_expr v ctx target
  | ExprUnaryPostfix (expr, _) -> v.visit_expr v ctx expr
  | ExprUnaryPrefix (_, expr) -> v.visit_expr v ctx expr
  | ExprBinary (left, _, right) -> visit_seq2 v.visit_expr v ctx left right
  | ExprRange (start, _, end_opt) ->
    v.visit_expr v ctx start;
    visit_opt v.visit_expr v ctx end_opt
  | ExprAssign (target, value) -> visit_seq2 v.visit_expr v ctx target value

let traverse_stmt v ctx stmt =
  match stmt.kind with StmtExpr expr -> v.visit_expr v ctx expr

let traverse_pat v ctx pat =
  match pat.kind with
  | PatIdent ident -> v.visit_ident v ctx ident
  | PatLit lit -> v.visit_lit v ctx lit
  | PatWild -> ()
  | PatLitTuple pats | PatLitArray pats -> visit_all v.visit_pat v ctx pats
  | PatLitRecord (ident, fields) ->
    v.visit_ident v ctx ident;
    visit_all
      (fun v ctx (field : pat_field) -> v.visit_ident v ctx field.field_name)
      v
      ctx
      fields
  | PatVariant (ident, tys, pat_opt) ->
    v.visit_ident v ctx ident;
    visit_all v.visit_ty v ctx tys;
    visit_opt v.visit_pat v ctx pat_opt
  | PatCons (head, tail) -> visit_seq2 v.visit_pat v ctx head tail
  | PatError -> ()

let traverse_ty v ctx ty =
  match ty.kind with
  | TyIdent ident -> v.visit_ident v ctx ident
  | TyApp (ident, args) ->
    v.visit_ident v ctx ident;
    visit_all v.visit_ty v ctx args
  | TyOptional inner -> v.visit_ty v ctx inner
  | TyArray (_, inner) -> v.visit_ty v ctx inner
  | TyPtr inner -> v.visit_ty v ctx inner
  | TyFn (arg, ret) -> visit_seq2 v.visit_ty v ctx arg ret
  | TyTuple tys -> visit_all v.visit_ty v ctx tys
  | TyError -> ()

let traverse_fn_sig v ctx sig_ =
  visit_opt v.visit_ident v ctx sig_.fn_name;
  visit_all v.visit_param v ctx sig_.fn_params;
  visit_opt v.visit_ty v ctx sig_.fn_ret_ty

let traverse_param v ctx param =
  v.visit_ident v ctx param.param_name;
  visit_opt v.visit_ty v ctx param.param_ty;
  visit_opt v.visit_expr v ctx param.param_default

let traverse_match_case v ctx case =
  v.visit_pat v ctx case.case_pat;
  v.visit_expr v ctx case.case_expr

let traverse_sum_case v ctx case =
  v.visit_ident v ctx case.case_name;
  visit_all v.visit_ty v ctx case.case_tys;
  visit_all v.visit_ty v ctx case.case_params

let traverse_record_field v ctx field =
  v.visit_ident v ctx field.field_name;
  visit_opt v.visit_ty v ctx field.field_ty;
  visit_opt v.visit_expr v ctx field.field_default

let traverse_record_content v ctx content =
  match content with
  | RecordFields fields -> visit_all v.visit_record_field v ctx fields
  | RecordWith (base, fields) ->
    v.visit_expr v ctx base;
    visit_all v.visit_record_field v ctx fields

let traverse_prog v ctx prog = visit_all v.visit_stmt v ctx prog

let default_visitor : 'ctx visitor =
  {
    visit_expr = traverse_expr
  ; visit_stmt = traverse_stmt
  ; visit_pat = traverse_pat
  ; visit_ty = traverse_ty
  ; visit_ident = (fun _ _ _ -> ())
  ; visit_lit = (fun _ _ _ -> ())
  ; visit_fn_sig = traverse_fn_sig
  ; visit_param = traverse_param
  ; visit_match_case = traverse_match_case
  ; visit_sum_case = traverse_sum_case
  ; visit_record_field = traverse_record_field
  ; visit_record_content = traverse_record_content
  ; visit_prog = traverse_prog
  }
