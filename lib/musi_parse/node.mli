open Musi_basic
open Musi_lex

type name = Interner.name

type modifiers = {
    is_exported : bool
  ; is_async : bool
  ; is_unsafe : bool
  ; is_weak : bool
  ; abi : name option
}

type capture = { cname : name; is_weak : bool }
type import_spec = ImportNamed of name list | ImportNamespace of name
type export_spec = ExportNamed of name list | ExportNamespace of name

type expr = { ekind : expr_kind; span : Span.t }

and expr_kind =
  | ExprIdent of name
  | ExprLiteral of literal_kind
  | ExprBinary of Token.kind * expr * expr
  | ExprUnary of Token.kind * expr
  | ExprCall of expr * expr list * bool
  | ExprField of expr * name * bool
  | ExprIndex of expr * expr * bool
  | ExprTuple of expr list
  | ExprArray of expr list
  | ExprRecord of name list * field list * modifiers
  | ExprBlock of expr list
  | ExprIf of expr * expr * expr option
  | ExprMatch of expr * case list
  | ExprWhile of expr * expr
  | ExprDo of expr * expr option
  | ExprFor of pat * expr * expr
  | ExprRange of expr * expr * bool
  | ExprAssign of expr * expr
  | ExprReturn of expr option
  | ExprBreak of expr option
  | ExprContinue
  | ExprYield of expr option
  | ExprAwait of expr
  | ExprTry of expr
  | ExprDefer of expr
  | ExprUnwrap of expr
  | ExprCast of expr * ty
  | ExprTest of expr * ty
  | ExprAsync of expr
  | ExprUnsafe of expr
  | ExprChoice of name list * variant list * modifiers
  | ExprBinding of bool * name list * pat * ty option * expr * modifiers
  | ExprProc of
      name list
      * capture list
      * param list
      * ty option
      * expr option
      * modifiers
  | ExprError

and stmt = { skind : stmt_kind; span : Span.t }

and stmt_kind =
  | StmtImport of import_spec * name
  | StmtExport of export_spec * name option
  | StmtExpr of expr * bool
  | StmtError

and ty = { tkind : ty_kind; span : Span.t }

and ty_kind =
  | TyNamed of name
  | TyApp of ty * ty list
  | TyProc of ty list * ty option
  | TyTuple of ty list
  | TyArray of ty
  | TyRecord of field list
  | TyOptional of ty
  | TyError

and pat = { pkind : pat_kind; span : Span.t }

and pat_kind =
  | PatIdent of name
  | PatWild
  | PatTuple of pat list
  | PatArray of pat list * pat option
  | PatRecord of (name * pat) list
  | PatChoice of name * pat option
  | PatLiteral of Token.kind
  | PatRest of name
  | PatBinding of name
  | PatError

and literal_kind =
  | LitInt of string
  | LitBin of string
  | LitStr of name
  | LitRune of int
  | LitBool of bool
  | LitRecord of (name * expr) list

and param = { pname : name; pty : ty option; is_inout : bool }
and field = { fname : name; fty : ty; is_var : bool; is_weak : bool }
and case = { cpat : pat; guard : expr option; body : expr }
and variant = { vname : name; vdata : variant_data }
and variant_data = VUnit | VTuple of ty list | VRecord of field list

val empty_modifiers : modifiers
val make_expr : expr_kind -> Span.t -> expr
val make_stmt : stmt_kind -> Span.t -> stmt
val make_ty : ty_kind -> Span.t -> ty
val make_pat : pat_kind -> Span.t -> pat
val string_of_expr : expr -> string
