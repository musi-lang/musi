open Basic
open Lex

type ident = Interner.name
type 'a with_span = { kind : 'a; span : Span.t }
type 'a delimited = 'a * 'a list
type lit = LitNumber of string | LitString of ident | LitRune of char

type attr_arg =
  | AttrIdent of ident
  | AttrNumber of string
  | AttrString of ident

type attr = { name : ident; args : attr_arg list }
type import_clause = ImportAll of ident | ImportNamed of ident list
type export_clause = ExportAll of ident | ExportNamed of ident list

(* Statements *)
type stmt = { attr : attr option; kind : stmt_kind; span : Span.t }

and stmt_kind =
  | StmtImport of { clause : import_clause; source : ident }
  | StmtExport of { clause : export_clause; source : ident option }
  | StmtBind of {
        mutable_ : bool
      ; name : ident
      ; typ : typ option
      ; value : expr
    }
  | StmtExtern of { abi : string option; decls : extern_fn_decl list }
  | StmtExpr of expr

and extern_fn_decl = {
    name : ident
  ; params : fn_param list
  ; ret_typ : typ option
}

and block = { stmts : stmt list; ret : expr option }

(* Expressions *)
and expr = expr_kind with_span

and expr_kind =
  | ExprLit of lit
  | ExprTemplate of ident
  | ExprIdent of ident
  | ExprTuple of expr delimited
  | ExprBlock of block
  | ExprIf of {
        conds : cond delimited
      ; then_block : block
      ; else_block : block option
    }
  | ExprMatch of { scrutinee : expr; arms : match_arm list }
  | ExprFor of {
        binding : for_binding
      ; range : expr
      ; guard : expr option
      ; body : block
    }
  | ExprWhile of { cond : cond; guard : expr option; body : block }
  | ExprDefer of expr
  | ExprExit of expr option
  | ExprNext
  | ExprUnsafe of block
  | ExprAssign of { target : ident; value : expr }
  | ExprBinary of { op : Token.t; left : expr; right : expr }
  | ExprUnary of { op : Token.t; operand : expr }
  | ExprPostfix of { base : expr; op : postfix_op }
  | ExprRecordLit of { name : ident option; fields : record_field_init list }
  | ExprFn of {
        abi : string option
      ; name : ident option
      ; typ_params : ident list
      ; params : fn_param list
      ; ret_typ : typ option
      ; body : block
    }
  | ExprRecord of {
        typ_params : ident list
      ; trait_bound : typ option
      ; items : record_item list
    }
  | ExprTrait of {
        typ_params : ident list
      ; trait_bound : typ option
      ; items : trait_item list
    }

and cond = CondExpr of expr | CondCase of { pat : pat; value : expr }
and for_binding = ForIdent of ident | ForCase of pat
and match_arm = { pat : pat; guard : expr option; body : expr }

and postfix_op =
  | PostfixField of { field : ident; optional : bool }
  | PostfixIndex of { index : expr; optional : bool }
  | PostfixCall of { typ_args : typ list; args : expr list; optional : bool }

and record_field_init = { shorthand : bool; name : ident; value : expr }

and record_item =
  | RecordFieldDecl of { name : ident; typ : typ }
  | RecordCaseDecl of { name : ident; fields : typ list }
  | RecordMethodDecl of { name : ident; value : expr }

and trait_item = { name : ident; typ : typ }
and fn_param = { name : ident; typ : typ option }

(* Patterns *)
and pat = pat_kind with_span

and pat_kind =
  | PatBind of { mutable_ : bool; name : ident }
  | PatLit of lit
  | PatWild
  | PatIdent of ident
  | PatRecord of { name : ident; fields : pat_field list }
  | PatCtor of { name : ident; args : pat list }
  | PatTuple of pat delimited

and pat_field = { name : ident; pat : pat option }

(* Types *)
and typ = typ_kind with_span

and typ_kind =
  | TypSum of { base : typ; cases : typ_sum_case list }
  | TypPtr of typ
  | TypArr of { size : expr option; elem : typ }
  | TypIdent of ident
  | TypApp of { base : ident; args : typ list }
  | TypTuple of typ delimited
  | TypFn of { params : typ list; ret : typ option }
  | TypRecord of typ_record_field list
  | TypOptional of typ

and typ_sum_case = { name : ident; fields : typ list }
and typ_record_field = { name : ident; typ : typ }

type prog = stmt list
