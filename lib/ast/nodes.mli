open Basic
open Lex

(** AST node with source span *)
type 'a with_span = { kind : 'a; span : Span.t }

(** Access modifiers for function or variable *)
type modifier = {
    is_export : bool
  ; is_extern : string option * bool
  ; is_unsafe : bool
}

(** Attribute argument value *)
type attr_arg =
  | AttrArgNamed of ident * ident * lit
  | AttrArgPos of ident * lit

(** Attribute attached to declaration *)
and attr = { attr_name : ident; attr_args : attr_arg list }

(** Literal value *)
and lit =
  | LitInt of ident
  | LitFloat of ident
  | LitString of ident
  | LitRune of char
  | LitBool of bool

(** Identifier string *)
and ident = string

(** Type definition node *)
type ty = ty_kind with_span

(** Type definition variant *)
and ty_kind =
  | TyIdent of ident
  | TyApp of ident * ty list
  | TyArray of int option * ty
  | TyOptional of ty
  | TyPtr of ty
  | TyFn of ty * ty
  | TyTuple of ty list
  | TyError

(** Pattern matching node *)
and pat = pat_kind with_span

(** Pattern matching variant *)
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

(** Record pattern field *)
and pat_field = { field_name : ident }

(** Expression node *)
and expr = expr_kind with_span

(** Expression variant *)
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

(** Statement node *)
and stmt = stmt_kind with_span

(** Statement variant *)
and stmt_kind = StmtExpr of expr

(** Match case branch *)
and cond = CondExpr of expr | CondPat of pat * expr

(** Match case branch *)
and match_case = { case_pat : pat; case_guard : expr option; case_expr : expr }

(** Function signature *)
and fn_sig = {
    fn_name : ident option
  ; fn_ty_params : ident list
  ; fn_params : param list
  ; fn_ret_ty : ty option
}

(** Function parameter *)
and param = {
    param_mutable : bool
  ; param_name : ident
  ; param_ty : ty option
  ; param_default : expr option
}

(** Record field definition *)
and record_field = {
    field_mutable : bool
  ; field_name : ident
  ; field_ty : ty option
  ; field_default : expr option
}

(** Sum type case definition *)
and sum_case = {
    case_name : ident
  ; case_tys : ty list
  ; case_params : param list
}

(** Create type node with span *)
val make_ty : ty_kind -> Span.t -> ty

(** Create pattern node with span *)
val make_pat : pat_kind -> Span.t -> pat

(** Create expression node with span *)
val make_expr : expr_kind -> Span.t -> expr

(** Create statement node with span *)
val make_stmt : stmt_kind -> Span.t -> stmt
