(** Represents list whose items may end with trailing delimiter. *)
type 'a separated = { items : 'a list; trailing : bool }

(** Enumerates kinds of AST nodes along with associated payloads. *)
type kind =
  | ExprIdent of Interner.name
  | ExprLitNumeric of string * Token.suffix option
  | ExprLitText of Interner.name
  | ExprLitBool of bool
  | ExprLitRecord of { name : Interner.name option; fields : record_field list }
  | ExprArray of t separated
  | ExprTuple of t separated
  | ExprBinary of { op : Token.kind; left : t; right : t }
  | ExprUnary of { op : Token.kind; operand : t }
  | ExprAssign of { target : t; value : t }
  | ExprCall of { callee : t; args : t separated }
  | ExprIndex of { target : t; index : t }
  | ExprField of { target : t; field : Interner.name }
  | ExprIf of { pat : t; then_branch : t; else_branch : t option }
  | ExprWhile of { pat : t; body : t }
  | ExprFor of { pat : t; iter : t; body : t }
  | ExprLoop of t
  | ExprBlock of { stmts : t list; expr : t option }
  | ExprBlockUnsafe of { stmts : t list; expr : t option }
  | ExprBinding of {
        modifiers : modifiers
      ; is_mutable : bool
      ; pat : t
      ; ty : ty option
      ; init : t
    }
  | ExprProc of {
        modifiers : modifiers
      ; params : param list
      ; ret_ty : ty option
      ; body : t option
    }
  | ExprReturn of t option
  | ExprMatch of { scrutinee : t; cases : match_case list }
  | ExprBreak of t option
  | ExprContinue
  | PatIdent of Interner.name
  | PatBinding of Interner.name
  | PatWildcard
  | PatTuple of t separated
  | PatExpr of t
  | StmtImport of { path : Interner.name; items : import_item separated }
  | StmtExport of { items : Interner.name separated }
  | StmtAlias of { name : Interner.name; ty : t }
  | StmtExpr of t

and decorator = { name : Interner.name; args : t list; span : Span.t }

and modifiers = {
    decorators : decorator list
  ; is_exported : bool
  ; is_unsafe : bool
  ; is_extern : bool * string option
  ; is_async : bool
}

and param = { is_mutable : bool; name : Interner.name; ty : ty option }
and match_case = { pattern : t; guard : t option; body : t }
and import_item = { name : Interner.name; alias : Interner.name option }
and record_field = { label : Interner.name; value : t }
and t = { kind : kind; span : Span.t }

and ty_kind =
  | TyNamed of Interner.name
  | TyGeneric of ty * ty list
  | TyArray of ty
  | TySizedArray of ty * t
  | TyTuple of ty list
  | TyProc of ty list * ty option
  | TyOptional of ty
  | TyFallible of ty * ty option

and ty = { ty_kind : ty_kind; ty_span : Span.t }

(** Constructs AST node from provided kind and span. *)
val make : kind -> Span.t -> t

(** Constructs syntactic type node from provided kind and span. *)
val make_ty : ty_kind -> Span.t -> ty

(** Produces readable description of node kind. *)
val show_kind : kind -> string
