open Musi_basic
open Musi_lex

type name = Interner.name
type modifiers = { is_async : bool; is_unsafe : bool; abi : name option }
type capture = { cname : name; is_weak : bool }
type import_spec = ImportNamed of name list | ImportNamespace of name
type export_spec = ExportNamed of name list | ExportNamespace of name

type expr = { ekind : expr_kind; span : Span.t }

and expr_kind =
  | ExprIdent of name
  | ExprLiteral of Token.kind
  | ExprBinary of Token.kind * expr * expr
  | ExprUnary of Token.kind * expr
  | ExprCall of expr * expr list
  | ExprField of expr * name
  | ExprIndex of expr * expr
  | ExprTuple of expr list
  | ExprArray of expr list
  | ExprRecord of (name * expr) list
  | ExprBlock of expr list
  | ExprIf of expr * expr * expr option
  | ExprMatch of expr * case list
  | ExprWhile of expr * expr
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
  | ExprBinding of bool * pat * ty option * expr * modifiers
  | ExprProc of
      name list * capture list * param list * ty option * expr * modifiers
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
  | PatError

and param = { pname : name; pty : ty option; is_inout : bool }
and field = { fname : name; fty : ty; is_var : bool; is_weak : bool }
and case = { cpat : pat; guard : expr option; body : expr }

let empty_modifiers = { is_async = false; is_unsafe = false; abi = None }
let make_expr ekind span = { ekind; span }
let make_stmt skind span = { skind; span }
let make_ty tkind span = { tkind; span }
let make_pat pkind span = { pkind; span }
