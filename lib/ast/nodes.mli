open Basic
open Lex

type 'a spanned = { kind : 'a; span : Span.t }
type ident = Interner.ident spanned

type 'a delimited = {
    ldelim : Token.t spanned
  ; value : 'a
  ; rdelim : Token.t spanned
}

type ('a, 's) separated = { elems : 'a list; seps : 's spanned list }
type 'a preceded = { leader : Token.t spanned; value : 'a }
type 'a followed = { value : 'a; trailer : Token.t spanned }

type modifier = {
    is_export : Token.t spanned option
  ; is_extern : (Token.t spanned * ident preceded option * bool) option
  ; is_unsafe : Token.t spanned option
}

type attr_arg =
  | AttrArgNamed of ident * Token.t spanned (* := *) * lit spanned
  | AttrArgPos of lit spanned

and attr = {
    ldelim : Token.t spanned (* [< *)
  ; name : ident
  ; args : (attr_arg, Token.t) separated delimited option
  ; rdelim : Token.t spanned (* >] *)
}

and lit =
  | LitInt of Interner.ident
  | LitFloat of Interner.ident
  | LitString of Interner.ident
  | LitRune of char
  | LitBool of bool

type ty = ty_kind spanned

and ty_kind =
  | TyIdent of ident
  | TyApp of { name : ident; args : (ty, Token.t) separated delimited }
  | TyArray of {
        ldelim : Token.t spanned
      ; len : int option
      ; rdelim : Token.t spanned
      ; ty : ty
    }
  | TyOptional of Token.t spanned (* ? *) * ty
  | TyPtr of Token.t spanned (* ^ *) * ty
  | TyFn of ty * Token.t spanned (* -> *) * ty
  | TyTuple of (ty, Token.t) separated delimited
  | TyError

type pat = pat_kind spanned

and pat_kind =
  | PatOr of (pat, Token.t) separated spanned
  | PatIdent of ident
  | PatWild of Token.t spanned (* _ *)
  | PatLit of lit spanned
  | PatLitTuple of (pat, Token.t) separated delimited
  | PatLitArray of (pat, Token.t) separated delimited
  | PatLitRecord of {
        name : ident option
      ; dot : Token.t spanned (* . *)
      ; fields : (pat_field, Token.t) separated delimited
    }
  | PatVariant of {
        name : ident
      ; ty_args : (ty, Token.t) separated delimited option
      ; args : (pat, Token.t) separated delimited option
    }
  | PatCons of pat * Token.t spanned (* :: *) * pat
  | PatError

and pat_field = { name : ident }

type expr = expr_kind spanned

and expr_kind =
  | ExprLit of lit spanned
  | ExprTemplate of (Interner.ident * expr) list * Interner.ident
  | ExprLitTuple of (expr, Token.t) separated delimited
  | ExprLitArray of (expr, Token.t) separated delimited
  | ExprLitRecord of {
        name : ident option
      ; dot : Token.t spanned (* . *)
      ; fields : record_lit_content delimited
    }
  | ExprIdent of ident
  | ExprBlock of block delimited
  | ExprIf of {
        if_kw : Token.t spanned
      ; cond : expr
      ; then_branch : expr
      ; else_if_branches : else_if_branch list
      ; else_branch : (Token.t spanned (* else *) * expr) option
    }
  | ExprWhile of { while_kw : Token.t spanned; cond : expr; body : expr }
  | ExprFor of {
        for_kw : Token.t spanned
      ; pat : pat
      ; in_kw : Token.t spanned
      ; target : expr
      ; body : expr
    }
  | ExprMatch of {
        match_kw : Token.t spanned
      ; target : expr
      ; cases : (match_case, Token.t) separated delimited
    }
  | ExprReturn of Token.t spanned * expr option
  | ExprBreak of Token.t spanned * expr option
  | ExprDefer of Token.t spanned * expr
  | ExprUnsafe of Token.t spanned * expr
  | ExprImport of Token.t spanned * ident
  | ExprExtern of {
        modifier : modifier
      ; extern_kw : Token.t spanned
      ; abi : ident preceded option
      ; sigs : (fn_sig_extern, Token.t) separated delimited
    }
  | ExprBind of {
        modifier : modifier
      ; kind_kw : Token.t spanned (* val or var *)
      ; pat : pat
      ; ty_annot : ty preceded option
      ; init : expr preceded
    }
  | ExprFn of {
        attrs : attr list
      ; modifier : modifier
      ; fn_kw : Token.t spanned
      ; sig_ : fn_sig
      ; body : expr
    }
  | ExprRecord of {
        attrs : attr list
      ; modifier : modifier
      ; record_kw : Token.t spanned
      ; name : ident option
      ; ty_params : (ident, Token.t) separated delimited option
      ; fields : (record_field_def, Token.t) separated delimited
    }
  | ExprSum of {
        attrs : attr list
      ; modifier : modifier
      ; sum_kw : Token.t spanned
      ; name : ident option
      ; ty_params : (ident, Token.t) separated delimited option
      ; cases : (sum_case, Token.t) separated delimited
    }
  | ExprAlias of {
        attrs : attr list
      ; modifier : modifier
      ; alias_kw : Token.t spanned
      ; name : ident
      ; ty_params : (ident, Token.t) separated delimited option
      ; init : ty preceded
    }
  | ExprCall of { callee : expr; args : (expr, Token.t) separated delimited }
  | ExprIndex of { target : expr; index : expr delimited }
  | ExprField of { target : expr; dot : Token.t spanned; field : ident }
  | ExprUnaryPrefix of Token.t spanned * expr
  | ExprUnaryPostfix of expr * Token.t spanned
  | ExprBinary of { left : expr; op : Token.t spanned; right : expr }
  | ExprRange of { left : expr; op : Token.t spanned; right : expr option }
  | ExprAssign of { left : expr; op : Token.t spanned; right : expr }
  | ExprError

and record_lit_content = {
    with_expr : expr preceded option (* with *)
  ; fields : (record_field, Token.t) separated
}

and block = { stmts : stmt list; result_expr : expr option }
and stmt = stmt_kind spanned

and stmt_kind =
  | StmtExpr of expr followed
  (* ; *)
  | StmtError

and else_if_branch = {
    else_kw : Token.t spanned
  ; if_kw : Token.t spanned
  ; cond : expr
  ; branch : expr
}

and match_case = {
    case_kw : Token.t spanned
  ; pat : pat
  ; guard : expr preceded option (* if *)
  ; arrow : Token.t spanned
  ; expr : expr
}

and fn_sig = {
    name : ident option
  ; ty_params : (ident, Token.t) separated delimited option
  ; params : (param, Token.t) separated delimited
  ; ret_ty : ty preceded option
}

and fn_sig_extern = {
    fn_kw : Token.t spanned
  ; sig_ : fn_sig
  ; semi : Token.t spanned
}

and param = {
    is_var : Token.t spanned option
  ; name : ident
  ; ty_annot : ty preceded option
  ; init : expr preceded option
}

and record_field = {
    name : ident
  ; ty_annot : ty preceded option
  ; init : expr preceded option
}

and record_field_def = {
    is_var : Token.t spanned option
  ; name : ident
  ; ty_annot : ty preceded option
  ; init : expr preceded option
}

and sum_case = {
    case_kw : Token.t spanned
  ; name : ident
  ; ty_args : (ty, Token.t) separated delimited option
  ; args : (sum_case_arg, Token.t) separated delimited option
}

and sum_case_arg = SumCaseArgTy of ty | SumCaseArgParam of param

val make_ty : ty_kind -> Span.t -> ty
val make_pat : pat_kind -> Span.t -> pat
val make_expr : expr_kind -> Span.t -> expr
val make_stmt : stmt_kind -> Span.t -> stmt
