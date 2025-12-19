open Basic
open Lex

type ident = string
type span = Span.t
type 'a with_span = { kind : 'a; span : span }

type modifier = {
    is_export : bool
  ; is_extern : (string option * bool) option (* Some(path, unsafe) or None *)
  ; is_unsafe : bool
}

type lit =
  | LitInt of string
  | LitFloat of string
  | LitString of string
  | LitTemplate of template_part list
  | LitRune of char
  | LitBool of bool

and template_part = TemplateText of string | TemplateExpr of expr
and ty = ty_kind with_span

and ty_kind =
  | TyIdent of ident
  | TyApp of ident * ty list
  | TyOptional of ty
  | TyArray of int option * ty
  | TyPtr of ty
  | TyFn of ty * ty
  | TyTuple of ty list
  | TyError

and pat = pat_kind with_span

and pat_kind =
  | PatIdent of ident
  | PatLit of lit
  | PatWild
  | PatLitTuple of pat list
  | PatLitArray of pat list
  | PatLitRecord of ident * pat_field list
  | PatVariant of ident * ty list * pat option
  | PatCons of pat * pat
  | PatError

and pat_field = { field_name : ident }
and expr = expr_kind with_span

and expr_kind =
  | ExprLit of lit
  | ExprIdent of ident
  | ExprLitTuple of expr list
  | ExprLitArray of expr list
  | ExprLitRecord of ident option * record_content
  | ExprBlock of stmt list * expr option
  | ExprIf of expr * expr * expr
  | ExprWhile of expr * expr
  | ExprFor of ident * expr * expr
  | ExprMatch of expr * match_case list
  | ExprTry of expr * (ident option * expr) option
  | ExprReturn of expr option
  | ExprDefer of expr
  | ExprBreak of expr option
  | ExprCycle
  | ExprUnsafe of expr
  | ExprImport of string
  | ExprExtern of string option * bool * fn_sig list
  | ExprRecord of
      attr list * modifier * ident option * ty_params * record_field list
  | ExprSum of attr list * modifier * ident option * ty_params * sum_case list
  | ExprFn of attr list * modifier * fn_sig * expr
  | ExprBind of modifier * bool * ident * ty option * expr * expr
  | ExprCall of expr * expr list
  | ExprIndex of expr * expr
  | ExprField of expr * ident
  | ExprUnaryPostfix of expr * Token.t
  | ExprUnaryPrefix of Token.t * expr
  | ExprBinary of expr * Token.t * expr
  | ExprRange of expr * Token.t * expr option
  | ExprAssign of expr * expr
  | ExprError

and stmt = stmt_kind with_span
and stmt_kind = StmtExpr of expr
and match_case = { case_pat : pat; case_expr : expr }

and record_content =
  | RecordFields of record_field list
  | RecordWith of expr * record_field list

and record_field = {
    field_mutable : bool
  ; field_name : ident
  ; field_ty : ty option
  ; field_default : expr option
}

and sum_case = { case_name : ident; case_tys : ty list; case_params : ty list }

and fn_sig = {
    fn_name : ident option
  ; fn_ty_params : ty_params
  ; fn_params : param list
  ; fn_ret_ty : ty option
}

and param = {
    param_mutable : bool
  ; param_name : ident
  ; param_ty : ty option
  ; param_default : expr option
}

and ty_params = ident list
and attr = { attr_name : ident; attr_args : attr_arg list }
and attr_arg = AttrArgPos of ident * lit | AttrArgNamed of ident * ident * lit

type prog = stmt list
