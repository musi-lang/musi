open Basic
open Lex

type 'a with_span = { kind : 'a; span : Span.t }

type modifier = {
    is_export : bool
  ; is_extern : string option * bool
  ; is_unsafe : bool
}

type attr_arg =
  | AttrArgNamed of ident * ident * lit
  | AttrArgPos of ident * lit

and attr = { attr_name : ident; attr_args : attr_arg list }

and lit =
  | LitInt of ident
  | LitFloat of ident
  | LitString of ident
  | LitRune of char
  | LitBool of bool

and ident = string

type ty = ty_kind with_span

and ty_kind =
  | TyIdent of ident
  | TyApp of ident * ty list
  | TyArray of int option * ty
  | TyOptional of ty
  | TyPtr of ty
  | TyFn of ty * ty
  | TyTuple of ty list
  | TyError

and pat = pat_kind with_span

and pat_kind =
  | PatIdent of ident
  | PatWild
  | PatLit of lit
  | PatLitTuple of pat list
  | PatLitArray of pat list
  | PatLitRecord of ident * pat_field list
  | PatVariant of ident * ty list * pat list
  | PatCons of pat * pat
  | PatError

and pat_field = { field_name : ident }
and expr = expr_kind with_span

and expr_kind =
  | ExprLit of lit
  | ExprTemplate of (string * expr) list * string
  | ExprLitTuple of expr list
  | ExprLitArray of expr list
  | ExprLitRecord of ident option * record_field list * expr option
  | ExprIdent of ident
  | ExprBlock of stmt list * expr option
  | ExprIf of cond list * expr * expr option
  | ExprWhile of cond * expr option * expr
  | ExprFor of bool * pat * expr * expr option * expr
  | ExprMatch of expr * match_case list
  | ExprReturn of expr option
  | ExprBreak of expr option
  | ExprDefer of expr
  | ExprUnsafe of expr
  | ExprImport of string
  | ExprExtern of string option * bool * fn_sig list
  | ExprBind of modifier * bool * pat * ty option * expr * expr
  | ExprFn of attr list * modifier * fn_sig * expr
  | ExprRecord of
      attr list * modifier * ident option * ident list * record_field list
  | ExprSum of attr list * modifier * ident option * ident list * sum_case list
  | ExprAlias of attr list * modifier * ident * ident list * ty
  | ExprCall of expr * expr list
  | ExprIndex of expr * expr
  | ExprField of expr * ident
  | ExprUnaryPrefix of Token.t * expr
  | ExprUnaryPostfix of expr * Token.t
  | ExprBinary of expr * Token.t * expr
  | ExprRange of expr * Token.t * expr option
  | ExprAssign of expr * expr
  | ExprError

and stmt = stmt_kind with_span
and stmt_kind = StmtExpr of expr
and cond = CondExpr of expr | CondPat of pat * expr
and match_case = { case_pat : pat; case_guard : expr option; case_expr : expr }

and fn_sig = {
    fn_name : ident option
  ; fn_ty_params : ident list
  ; fn_params : param list
  ; fn_ret_ty : ty option
}

and param = {
    param_mutable : bool
  ; param_name : ident
  ; param_ty : ty option
  ; param_default : expr option
}

and record_field = {
    field_mutable : bool
  ; field_name : ident
  ; field_ty : ty option
  ; field_default : expr option
}

and sum_case = {
    case_name : ident
  ; case_tys : ty list
  ; case_params : param list
}

let make_ty kind span = { kind; span }
let make_pat kind span = { kind; span }
let make_expr kind span = { kind; span }
let make_stmt kind span = { kind; span }
