open Basic
open Lex

type ident = Interner.name

type literal_kind =
  | LitInt of string
  | LitFloat of string
  | LitString of ident
  | LitRune of char
  | LitTemplate of ident
  | LitUnit

type attr_arg = AttrIdent of ident | AttrInt of string | AttrString of ident
type attr = { name : ident; args : attr_arg list }

type modifiers = {
    attrs : attr list
  ; export_ : bool
  ; extern_ : bool
  ; unsafe_ : bool
}

type 'a with_span = { kind : 'a; span : Span.t }

type typ_expr = typ_expr_kind with_span

and typ_expr_kind =
  | TypExprPtr of typ_expr
  | TypExprIdent of ident
  | TypExprTuple of typ_expr * typ_expr list
  | TypExprArray of expr option * typ_expr
  | TypExprFunc of typ_expr list * typ_expr option
  | TypExprRecord of typ_field list
  | TypExprSum of typ_case list

and typ_field = { name : ident; typ : typ_expr }
and typ_case = { name : ident; fields : typ_expr list }
and param = { name : ident; typ : typ_expr option }
and data_kind = DataRecord of typ_field list | DataSum of typ_case list
and func_sig = { name : ident; params : param list; ret_type : typ_expr option }
and expr = expr_kind with_span

and expr_kind =
  | ExprLiteral of literal_kind
  | ExprIdent of ident
  | ExprTuple of expr * expr list
  | ExprRecord of field_init list
  | ExprBlock of block
  | ExprIf of cond list * block * block option
  | ExprMatch of expr * match_arm list
  | ExprFor of {
        binding : for_binding
      ; range : expr
      ; guard : expr option
      ; body : block
    }
  | ExprWhile of cond * expr option * block
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
  | ExprDefer of expr
  | ExprExit of expr option
  | ExprSkip
  | ExprUnsafe of block

and field_init = { name : ident; value : expr }
and cond = CondExpr of expr | CondCase of pat * expr
and for_binding = ForIdent of ident | ForCase of pat
and match_arm = { pattern : pat; guard : expr option; body : expr }
and block = { stmts : stmt list; ret : expr option }
and pat = pat_kind with_span

and pat_kind =
  | PatBinding of { mutable_ : bool; name : ident }
  | PatLiteral of literal_kind
  | PatWild
  | PatIdent of ident
  | PatRecord of ident * pat_field list
  | PatCtor of ident * pat list
  | PatTuple of pat * pat list

and pat_field = { name : ident; pat : pat option }
and stmt = { modifiers : modifiers; kind : stmt_kind; span : Span.t }
and import_clause = ImportAll of ident | ImportNamed of ident list
and export_clause = ExportAll of ident | ExportNamed of ident list

and stmt_kind =
  | StmtImport of import_clause * ident
  | StmtExport of export_clause * ident option
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
