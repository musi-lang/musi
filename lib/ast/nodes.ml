open Basic
open Lex

type 'a spanned = { kind : 'a; span : Span.t }
type 'a delimited = { ldelim : Span.t; value : 'a; rdelim : Span.t }
type ('a, 's) separated = { elems : 'a list; sep_spans : Span.t list }

type modifier = {
    is_export : bool
  ; is_extern : string option * bool
  ; is_unsafe : bool
}

type attr_arg =
  | AttrArgNamed of string * string * lit
  | AttrArgPos of string * lit

and attr = { attr_name : string; attr_args : attr_arg list }

and lit =
  | LitInt of string
  | LitFloat of string
  | LitString of string
  | LitRune of char
  | LitBool of bool

type ty = ty_kind spanned

and ty_kind =
  | TyIdent of string
  | TyApp of { name : string spanned; args : (ty, Token.t) separated delimited }
  | TyArray of { ldelim : Span.t; len : int option; rdelim : Span.t; ty : ty }
  | TyOptional of Span.t * ty
  | TyPtr of Span.t * ty
  | TyFn of ty * Span.t * ty
  | TyTuple of (ty, Token.t) separated delimited
  | TyError

type pat = pat_kind spanned

and pat_kind =
  | PatOr of (pat, Token.t) separated
  | PatIdent of string
  | PatWild
  | PatLit of lit
  | PatLitTuple of (pat, Token.t) separated delimited
  | PatLitArray of (pat, Token.t) separated delimited
  | PatLitRecord of {
        name : string spanned option
      ; fields : (pat_field, Token.t) separated delimited
    }
  | PatVariant of {
        name : string spanned
      ; args : (ty, Token.t) separated option
      ; params : (pat, Token.t) separated delimited option
    }
  | PatCons of pat * Span.t * pat
  | PatError

and pat_field = { field_name : string spanned }

type expr = expr_kind spanned

and expr_kind =
  | ExprLit of lit
  | ExprTemplate of (string * expr) list * string
  | ExprLitTuple of (expr, Token.t) separated delimited
  | ExprLitArray of (expr, Token.t) separated delimited
  | ExprLitRecord of {
        name : string spanned option
      ; with_expr : expr option
      ; fields : (record_field, Token.t) separated delimited
    }
  | ExprIdent of string
  | ExprBlock of {
        stmts : stmt list
      ; result : expr option
      ; ldelim : Span.t
      ; rdelim : Span.t
    }
  | ExprIf of {
        if_kw : Span.t
      ; cond : expr
      ; then_branch : expr
      ; else_branches : (expr * expr) list
      ; else_branch : expr option
    }
  | ExprWhile of { while_kw : Span.t; cond : expr; body : expr }
  | ExprFor of {
        for_kw : Span.t
      ; pat : pat
      ; in_kw : Span.t
      ; target : expr
      ; body : expr
    }
  | ExprMatch of {
        match_kw : Span.t
      ; target : expr
      ; cases : (match_case, Token.t) separated delimited
    }
  | ExprReturn of Span.t * expr option
  | ExprBreak of Span.t * expr option
  | ExprDefer of Span.t * expr
  | ExprUnsafe of Span.t * expr
  | ExprImport of Span.t * string
  | ExprExtern of {
        extern_kw : Span.t
      ; abi : string option
      ; unsafe : bool
      ; sigs : (fn_sig spanned, Token.t) separated delimited
    }
  | ExprBind of {
        modifier : modifier
      ; is_var : bool
      ; pat : pat
      ; ty : ty option
      ; init : expr
    }
  | ExprFn of {
        attrs : attr list
      ; modifier : modifier
      ; sig_ : fn_sig
      ; body : expr
    }
  | ExprRecord of {
        attrs : attr list
      ; modifier : modifier
      ; name : string option
      ; ty_params : (string, Token.t) separated delimited option
      ; fields : (record_field, Token.t) separated delimited
    }
  | ExprSum of {
        attrs : attr list
      ; modifier : modifier
      ; name : string option
      ; ty_params : (string, Token.t) separated delimited option
      ; cases : (sum_case, Token.t) separated delimited
    }
  | ExprAlias of {
        attrs : attr list
      ; modifier : modifier
      ; name : string
      ; ty_params : (string, Token.t) separated delimited option
      ; ty : ty
    }
  | ExprCall of { callee : expr; args : (expr, Token.t) separated delimited }
  | ExprIndex of { target : expr; index : expr delimited (* [expr] *) }
  | ExprField of { target : expr; dot : Span.t; field : string spanned }
  | ExprUnaryPrefix of Token.t spanned * expr
  | ExprUnaryPostfix of expr * Token.t spanned
  | ExprBinary of { left : expr; op : Token.t spanned; right : expr }
  | ExprRange of { left : expr; op : Token.t spanned; right : expr option }
  | ExprAssign of { left : expr; op : Token.t spanned; right : expr }
  | ExprError

and stmt = stmt_kind spanned
and stmt_kind = StmtExpr of expr

and match_case = {
    case_kw : Span.t
  ; case_pat : pat
  ; case_guard : (Span.t * expr) option
  ; case_arrow : Span.t
  ; case_expr : expr
}

and fn_sig = {
    fn_name : string spanned option
  ; fn_ty_params : (string, Token.t) separated delimited option
  ; fn_params : (param, Token.t) separated delimited
  ; fn_ret_ty : ty option
}

and param = {
    param_mutable : bool
  ; param_name : string spanned
  ; param_ty : ty option
  ; param_default : expr option
}

and record_field = {
    field_mutable : bool
  ; field_name : string spanned
  ; field_ty : ty option
  ; field_default : expr option
}

and sum_case = {
    case_kw : Span.t
  ; case_name : string spanned
  ; case_tys : (ty, Token.t) separated delimited option
  ; case_params : (param, Token.t) separated delimited option
}

let make_ty kind span = { kind; span }
let make_pat kind span = { kind; span }
let make_expr kind span = { kind; span }
let make_stmt kind span = { kind; span }
