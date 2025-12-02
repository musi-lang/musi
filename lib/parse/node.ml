module type S = sig
  type ident = Basic.Interner.name
  type 'a with_span = { kind : 'a; span : Basic.Span.t }
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
  type stmt = { attr : attr option; kind : stmt_kind; span : Basic.Span.t }

  and stmt_kind =
    | StmtImport of { clause : import_clause; source : ident }
    | StmtExport of { clause : export_clause; source : ident option }
    | StmtBind of {
          mutable_ : bool
        ; name : ident
        ; typ : typ option
        ; value : expr
      }
    | StmtExtern of { abi : string option; decls : fn_sig list }
    | StmtExpr of expr

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
    | ExprBreak of expr option
    | ExprCycle
    | ExprUnsafe of block
    | ExprAssign of { target : ident; value : expr }
    | ExprBinary of { op : Lex.Token.t; left : expr; right : expr }
    | ExprUnary of { op : Lex.Token.t; operand : expr }
    | ExprCall of {
          callee : expr
        ; typ_args : typ list
        ; args : expr list
        ; optional : bool
      }
    | ExprField of { base : expr; field : ident; optional : bool }
    | ExprIndex of { base : expr; index : expr; optional : bool }
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
        ; fields : record_field list
        ; body : stmt list
      }
    | ExprChoice of {
          typ_params : ident list
        ; cases : choice_case list
        ; body : stmt list
      }
    | ExprTrait of {
          typ_params : ident list
        ; trait_bound : typ option
        ; items : fn_sig list
      }

  and cond = CondExpr of expr | CondCase of { pat : pat; value : expr }
  and for_binding = ForIdent of ident | ForCase of pat
  and match_arm = { pat : pat; guard : expr option; body : expr }
  and record_field_init = { shorthand : bool; name : ident; value : expr }
  and record_field = { name : ident; typ : typ }
  and choice_case = { name : ident; fields : typ list }
  and fn_sig = { name : ident; typ : typ option }
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
    | TypPtr of typ
    | TypArr of { size : expr option; elem : typ }
    | TypIdent of ident
    | TypApp of { base : ident; args : typ list }
    | TypTuple of typ delimited
    | TypFn of { params : typ list; ret : typ option }
    | TypRecord of typ_record_field list
    | TypOptional of typ

  and typ_record_field = { name : ident; typ : typ }

  type prog = stmt list
end

module Make () : S = struct
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
    | StmtExtern of { abi : string option; decls : fn_sig list }
    | StmtExpr of expr

  and block = { stmts : stmt list; ret : expr option }
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
    | ExprBreak of expr option
    | ExprCycle
    | ExprUnsafe of block
    | ExprAssign of { target : ident; value : expr }
    | ExprBinary of { op : Token.t; left : expr; right : expr }
    | ExprUnary of { op : Token.t; operand : expr }
    | ExprCall of {
          callee : expr
        ; typ_args : typ list
        ; args : expr list
        ; optional : bool
      }
    | ExprField of { base : expr; field : ident; optional : bool }
    | ExprIndex of { base : expr; index : expr; optional : bool }
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
        ; fields : record_field list
        ; body : stmt list
      }
    | ExprChoice of {
          typ_params : ident list
        ; cases : choice_case list
        ; body : stmt list
      }
    | ExprTrait of {
          typ_params : ident list
        ; trait_bound : typ option
        ; items : fn_sig list
      }

  and cond = CondExpr of expr | CondCase of { pat : pat; value : expr }
  and for_binding = ForIdent of ident | ForCase of pat
  and match_arm = { pat : pat; guard : expr option; body : expr }
  and record_field_init = { shorthand : bool; name : ident; value : expr }
  and record_field = { name : ident; typ : typ }
  and choice_case = { name : ident; fields : typ list }
  and fn_sig = { name : ident; typ : typ option }
  and fn_param = { name : ident; typ : typ option }
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
  and typ = typ_kind with_span

  and typ_kind =
    | TypPtr of typ
    | TypArr of { size : expr option; elem : typ }
    | TypIdent of ident
    | TypApp of { base : ident; args : typ list }
    | TypTuple of typ delimited
    | TypFn of { params : typ list; ret : typ option }
    | TypRecord of typ_record_field list
    | TypOptional of typ

  and typ_record_field = { name : ident; typ : typ }

  type prog = stmt list
end
