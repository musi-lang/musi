open Basic
module Token = Lex.Token

type ident = Interner.name
type attr_arg = Ident of ident | Number of string | String of ident

type lit =
  | LitWhole of string
  | LitFloat of string
  | LitString of ident
  | LitRune of char

type import_clause = ImportAll of ident | ImportNamed of ident list
type export_clause = ExportAll of ident | ExportNamed of ident list

type 'a node = { span : Span.t; data : 'a }
and typ = typ_kind node
and pat = pat_kind node
and expr = expr_kind node
and stmt = stmt_kind node

and binding = {
    mutable_ : bool (* "val" | "var" *)
  ; name : ident
  ; typ_annot : typ option
  ; span : Span.t
}

and attr = { name : ident; args : attr_arg list; span : Span.t }
and fn_param = { name : ident; typ_annot : typ option; span : Span.t }
and fn_params = fn_param list
and fn_sig = { params : fn_params; ret_typ : typ option; span : Span.t }
and fn_sig_decl = { name : ident; sig_ : fn_sig; span : Span.t }
and typ_params = ident list
and typ_args = typ list
and field_decl = { name : ident; typ_annot : typ; span : Span.t }
and field_init = { name : ident; value : expr; shorthand : bool; span : Span.t }
and field_pat = { name : ident; pat : pat option; span : Span.t }
and tmpl_string = { parts : tmpl_part list; span : Span.t }
and tmpl_part = TmplText of ident | TmplExpr of expr
and cond = CondCaseBinding of { pat : pat; expr : expr } | CondExpr of expr
and for_binding = ForIdent of ident | ForPat of pat
and guard = expr option
and match_arm = { pat : pat; guard : guard; body : expr; span : Span.t }
and choice_case = { name : ident; fields : typ list; span : Span.t }
and block = { stmts : stmt list; ret : expr option; span : Span.t }

and typ_kind =
  | TypPtr of typ
  | TypArray of { size : expr option; elem : typ }
  | TypIdent of ident
  | TypApp of { base : ident; args : typ_args }
  | TypTuple of typ list
  | TypFn of { params : typ list; ret : typ option }
  | TypRecord of field_decl list
  | TypOptional of typ

and pat_kind =
  | PatBind of binding
  | PatLit of lit
  | PatWild
  | PatIdent of ident
  | PatRecord of { name : ident; fields : field_pat list }
  | PatCtor of { name : ident; args : pat list }
  | PatTuple of pat list

and expr_kind =
  | ExprLit of lit
  | ExprTmpl of tmpl_string
  | ExprIdent of ident
  | ExprTuple of expr list
  | ExprBlock of block
  | ExprRange of { start : expr option; end_ : expr option; exclusive : bool }
  | ExprIf of {
        conds : (pat * expr) list
      ; then_block : block
      ; else_block : block option
    }
  | ExprMatch of { scrutinee : expr; arms : match_arm list }
  | ExprFor of {
        binding : for_binding
      ; range : expr
      ; guard : guard
      ; body : block
    }
  | ExprWhile of { cond : cond option; guard : guard; body : block }
  | ExprDefer of expr
  | ExprBreak of expr option
  | ExprCycle
  | ExprReturn of expr option
  | ExprUnsafe of block
  | ExprAssign of { target : ident; value : expr }
  | ExprUnary of { op : Token.t; arg : expr }
  | ExprCall of {
        callee : expr
      ; typ_args : typ_args option
      ; args : expr list
      ; optional : bool
    }
  | ExprMember of { obj : expr; prop : ident; computed : bool; optional : bool }
  | ExprRecordLit of { name : ident option; fields : field_init list }
  | ExprFn of {
        abi : string option
      ; name : ident option
      ; typ_params : typ_params option
      ; sig_ : fn_sig
      ; body : block
    }
  | ExprRecord of {
        typ_params : typ_params option
      ; typ_annot : typ option
      ; fields : field_decl list
    }
  | ExprChoice of { typ_params : typ_params option; cases : choice_case list }

and stmt_kind =
  | StmtImport of { clause : import_clause; source : ident }
  | StmtExport of { clause : export_clause; source : ident option }
  | StmtBind of { binding : binding; value : expr; attr : attr option }
  | StmtExtern of { abi : string option; sig_decls : fn_sig_decl list }
  | StmtExpr of expr

type prog = stmt list

let span { span; _ } = span
let update_span span node = { node with span }
let map f { span; data } = { span; data = f data }
