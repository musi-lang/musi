open Basic
open Lex

type ident = Interner.name

type literal_kind =
  | LitInt of string
  | LitFloat of string
  | LitString of ident
  | LitRune of char
  | LitTemplate of ident

type attr_arg = AttrIdent of ident | AttrInt of string | AttrString of ident
type attr = { name : ident; args : attr_arg list }

type param = { name : ident; typ : typ_expr option }
and typ_field = { name : ident; typ : typ_expr }
and typ_case = { name : ident; fields : typ_expr list option }
and data_kind = DataRecord of typ_field list | DataSum of typ_case list
and func_sig = { name : ident; params : param list; ret_type : typ_expr option }
and field_init = { name : ident; value : expr }
and pat_field = { name : ident; pat : pat option }
and match_arm = { pattern : pat; body : expr }
and block = { unsafeness : bool; stmts : stmt list; ret : expr option }
and stmt = { attrs : attr list; kind : stmt_kind; span : Span.t }

and stmt_kind =
  | StmtImport of ident * string
  | StmtExport of ident * string option
  | StmtBinding of {
        mutable_ : bool
      ; name : ident
      ; typ : typ_expr option
      ; value : expr
    }
  | StmtAssign of ident * expr
  | StmtExpr of expr
  | StmtData of ident * data_kind
  | StmtExtern of string option * func_sig list

and expr = { kind : expr_kind; span : Span.t }

and expr_kind =
  | ExprLiteral of literal_kind
  | ExprIdent of ident
  | ExprTuple of expr list
  | ExprRecord of field_init list
  | ExprBlock of block
  | ExprIf of expr * block * block option
  | ExprMatch of expr * match_arm list
  | ExprFor of {
        elem : ident
      ; typ : typ_expr option
      ; init : expr
      ; range : expr
      ; step : expr option
      ; body : block
    }
  | ExprWhile of expr * block
  | ExprFunc of {
        abi : string option
      ; name : ident option
      ; params : param list
      ; ret_type : typ_expr option
      ; body : block
    }
  | ExprBinary of Token.t * expr * expr
  | ExprUnary of Token.t * expr
  | ExprCall of expr * expr list
  | ExprIndex of expr * expr
  | ExprField of expr * ident

and pat = { kind : pat_kind; span : Span.t }

and pat_kind =
  | PatBind of { mutable_ : bool; name : ident }
  | PatLiteral of literal_kind
  | PatWild
  | PatIdent of ident
  | PatRecord of ident * pat_field list
  | PatCtor of ident * pat list option
  | PatTuple of pat list

and typ_expr = { kind : typ_expr_kind; span : Span.t }

and typ_expr_kind =
  | TypExprPtr of typ_expr
  | TypExprIdent of ident
  | TypExprTuple of typ_expr list
  | TypExprArray of expr option * typ_expr
  | TypExprFunc of typ_expr list * typ_expr option
  | TypExprRecord of typ_field list
  | TypExprSum of typ_case list
