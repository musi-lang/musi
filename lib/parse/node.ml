open Basic

module type S = sig
  type ident = Interner.name
  type 'a with_span = { kind : 'a; span : Span.t }
  type 'a delimited = 'a * 'a list

  type lit =
    | LitNumber of { value : string; negative : bool }
    | LitString of ident
    | LitRune of char

  type attr_arg =
    | AttrIdent of ident
    | AttrNumber of string
    | AttrString of ident

  type attr = { name : ident; args : attr_arg list }
  type import_clause = ImportAll of ident | ImportNamed of ident list
  type export_clause = ExportAll of ident | ExportNamed of ident list

  type stmt = { attr : attr option; kind : stmt_kind; span : Span.t }
  and expr = expr_kind with_span
  and pat = pat_kind with_span
  and typ = typ_kind with_span
  and tmpl_string = TmplString of { parts : tmpl_part list }

  and stmt_kind =
    | StmtImport of { clause : import_clause; source : ident }
    | StmtExport of { clause : export_clause; source : ident option }
    | StmtBind of {
          mutable_ : bool
        ; name : ident
        ; typ_ : typ option
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

  and expr_kind =
    | ExprLit of lit
    | ExprTmpl of tmpl_string
    | ExprIdent of ident
    | ExprTuple of expr list
    | ExprBlock of block
    | ExprIf of {
          conds : (pat * expr) list
        ; then_block : block
        ; else_block : block option
      }
    | ExprMatch of { scrutinee : expr; arms : match_arm list }
    | ExprFor of {
          binding : loop_binding
        ; range : expr
        ; guard : expr option
        ; body : block
      }
    | ExprWhile of {
          cond : (pat * expr) option
        ; guard : expr option
        ; body : block
      }
    | ExprDefer of expr
    | ExprBreak of expr option
    | ExprCycle
    | ExprUnsafe of block
    | ExprAssign of { target : ident; value : expr }
    | ExprUnary of { op : string; arg : expr }
    | ExprCall of {
          callee : expr
        ; typ_args : typ list
        ; args : expr list
        ; optional : bool
      }
    | ExprMember of {
          obj_ : expr
        ; prop : ident
        ; computed : bool
        ; optional : bool
      }
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
      }
    | ExprChoice of { typ_params : ident list; cases : choice_case list }

  and tmpl_part = TmplText of ident | TmplExpr of expr
  and loop_binding = LoopIdent of ident | LoopPat of pat
  and match_arm = { pat : pat; guard : expr option; body : expr }
  and fn_param = { name : ident; typ_ : typ option }
  and record_field_init = { shorthand : bool; name : ident; value : expr }
  and record_field = { name : ident; typ_ : typ }
  and choice_case = { name : ident; fields : typ list }

  and pat_kind =
    | PatBind of { mutable_ : bool; name : ident }
    | PatLit of lit
    | PatWild
    | PatIdent of ident
    | PatRecord of { name : ident; fields : pat_field list }
    | PatCtor of { name : ident; args : pat list }
    | PatTuple of pat list

  and pat_field = { name : ident; pat_ : pat option }

  and typ_kind =
    | TypPtr of typ
    | TypArr of { size : expr option; elem : typ }
    | TypIdent of ident
    | TypApp of { base : ident; args : typ list }
    | TypTuple of typ list
    | TypFn of { params : typ list; ret : typ option }
    | TypRecord of typ_record_field list
    | TypOptional of typ

  and typ_record_field = { name : ident; typ_ : typ }

  type prog = stmt list
end

module Make () : S = struct
  type ident = Interner.name
  type 'a with_span = { kind : 'a; span : Span.t }
  type 'a delimited = 'a * 'a list

  type lit =
    | LitNumber of { value : string; negative : bool }
    | LitString of ident
    | LitRune of char

  type attr_arg =
    | AttrIdent of ident
    | AttrNumber of string
    | AttrString of ident

  type attr = { name : ident; args : attr_arg list }
  type import_clause = ImportAll of ident | ImportNamed of ident list
  type export_clause = ExportAll of ident | ExportNamed of ident list

  (* All types defined together for mutual recursion *)
  type stmt = { attr : attr option; kind : stmt_kind; span : Span.t }
  and expr = expr_kind with_span
  and pat = pat_kind with_span
  and typ = typ_kind with_span
  and tmpl_string = TmplString of { parts : tmpl_part list }

  and stmt_kind =
    | StmtImport of { clause : import_clause; source : ident }
    | StmtExport of { clause : export_clause; source : ident option }
    | StmtBind of {
          mutable_ : bool
        ; name : ident
        ; typ_ : typ option
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

  and expr_kind =
    | ExprLit of lit
    | ExprTmpl of tmpl_string
    | ExprIdent of ident
    | ExprTuple of expr list
    | ExprBlock of block
    | ExprIf of {
          conds : (pat * expr) list
        ; then_block : block
        ; else_block : block option
      }
    | ExprMatch of { scrutinee : expr; arms : match_arm list }
    | ExprFor of {
          binding : loop_binding
        ; range : expr
        ; guard : expr option
        ; body : block
      }
    | ExprWhile of {
          cond : (pat * expr) option
        ; guard : expr option
        ; body : block
      }
    | ExprDefer of expr
    | ExprBreak of expr option
    | ExprCycle
    | ExprUnsafe of block
    | ExprAssign of { target : ident; value : expr }
    | ExprUnary of { op : string; arg : expr }
    | ExprCall of {
          callee : expr
        ; typ_args : typ list
        ; args : expr list
        ; optional : bool
      }
    | ExprMember of {
          obj_ : expr
        ; prop : ident
        ; computed : bool
        ; optional : bool
      }
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
      }
    | ExprChoice of { typ_params : ident list; cases : choice_case list }

  and tmpl_part = TmplText of ident | TmplExpr of expr
  and loop_binding = LoopIdent of ident | LoopPat of pat
  and match_arm = { pat : pat; guard : expr option; body : expr }
  and fn_param = { name : ident; typ_ : typ option }
  and record_field_init = { shorthand : bool; name : ident; value : expr }
  and record_field = { name : ident; typ_ : typ }
  and choice_case = { name : ident; fields : typ list }

  and pat_kind =
    | PatBind of { mutable_ : bool; name : ident }
    | PatLit of lit
    | PatWild
    | PatIdent of ident
    | PatRecord of { name : ident; fields : pat_field list }
    | PatCtor of { name : ident; args : pat list }
    | PatTuple of pat list

  and pat_field = { name : ident; pat_ : pat option }

  and typ_kind =
    | TypPtr of typ
    | TypArr of { size : expr option; elem : typ }
    | TypIdent of ident
    | TypApp of { base : ident; args : typ list }
    | TypTuple of typ list
    | TypFn of { params : typ list; ret : typ option }
    | TypRecord of typ_record_field list
    | TypOptional of typ

  and typ_record_field = { name : ident; typ_ : typ }

  type prog = stmt list
end

include Make ()
