open Basic
module Token = Lex.Token

type ident = Interner.name
type attr_arg = Ident of ident | Number of string | String of ident
type lit = Whole of string | Float of string | String of ident | Rune of char
type import_clause = All of ident | Named of ident list
type export_clause = All of ident | Named of ident list
type stmt = [ `Stmt ]
type expr = [ `Expr ]
type pat = [ `Pat ]
type typ = [ `Typ ]

type 'a node = { span : Span.t; data : 'a node_data }

and 'a node_data =
  | Stmt_data : stmt_data -> stmt node_data
  | Expr_data : expr_data -> expr node_data
  | Pat_data : pat_data -> pat node_data
  | Typ_data : typ_data -> typ node_data

and binding = {
    mutable_ : bool (* "val" | "var" *)
  ; name : ident
  ; typ_annot : typ node option
  ; span : Span.t
}

and attr = { name : ident; args : attr_arg list; span : Span.t }
and fn_param = { name : ident; typ_annot : typ node option; span : Span.t }
and fn_params = fn_param list
and fn_sig = { params : fn_params; ret_typ : typ node option; span : Span.t }
and fn_sig_decl = { name : ident; sig_ : fn_sig; span : Span.t }
and typ_params = ident list
and typ_args = typ node list
and field_decl = { name : ident; typ_annot : typ node; span : Span.t }

and field_init = {
    name : ident
  ; value : expr node
  ; shorthand : bool
  ; span : Span.t
}

and field_pat = { name : ident; pat : pat node option; span : Span.t }
and tmpl_string = { parts : tmpl_part list; span : Span.t }
and tmpl_part = Text of ident | Expr of expr node

and cond =
  | CaseBinding of { pat : pat node; expr : expr node }
  | Expr of expr node

and for_binding = ForIdent of ident | ForPat of pat node
and guard = expr node option

and match_arm = {
    pat : pat node
  ; guard : guard
  ; body : expr node
  ; span : Span.t
}

and choice_case = { name : ident; fields : typ node list; span : Span.t }
and block = { stmts : stmt node list; ret : expr node option; span : Span.t }

and stmt_data =
  | Import of { clause : import_clause; source : ident }
  | Export of { clause : export_clause; source : ident option }
  | Bind of { binding : binding; value : expr node; attr : attr option }
  | Extern of { abi : string option; sig_decls : fn_sig_decl list }
  | Expr of expr node

and expr_data =
  | Lit of lit
  | Tmpl of tmpl_string
  | Ident of ident
  | Tuple of expr node list
  | Block of block
  | Range of {
        start : expr node option
      ; end_ : expr node option
      ; exclusive : bool
    }
  | If of {
        conds : (pat node * expr node) list
      ; then_block : block
      ; else_block : block option
    }
  | Match of { scrutinee : expr node; arms : match_arm list }
  | For of {
        binding : for_binding
      ; range : expr node
      ; guard : guard
      ; body : block
    }
  | While of { cond : cond option; guard : guard; body : block }
  | Defer of expr node
  | Break of expr node option
  | Cycle
  | Unsafe of block
  | Assign of { target : ident; value : expr node }
  | Unary of { op : Token.t; arg : expr node }
  | Call of {
        callee : expr node
      ; typ_args : typ_args option
      ; args : expr node list
      ; optional : bool
    }
  | Member of {
        obj : expr node
      ; prop : ident
      ; computed : bool
      ; optional : bool
    }
  | RecordLit of { name : ident option; fields : field_init list }
  | Fn of {
        abi : string option
      ; name : ident option
      ; typ_params : typ_params option
      ; sig_ : fn_sig
      ; body : block
    }
  | Record of {
        typ_params : typ_params option
      ; typ_annot : typ node option
      ; fields : field_decl list
    }
  | Choice of { typ_params : typ_params option; cases : choice_case list }

and pat_data =
  | Bind of binding
  | Lit of lit
  | Wild
  | Ident of ident
  | Record of { name : ident; fields : field_pat list }
  | Ctor of { name : ident; args : pat node list }
  | Tuple of pat node list

and typ_data =
  | Ptr of typ node
  | Array of { size : expr node option; elem : typ node }
  | Ident of ident
  | App of { base : ident; args : typ_args }
  | Tuple of typ node list
  | Fn of { params : typ node list; ret : typ node option }
  | Record of field_decl list
  | Optional of typ node

type prog = stmt node list

module Node = struct
  let span { span; _ } = span
  let update_span span node = { node with span }

  let map f { span; data } =
    (* This function is intentionally generic - actual mapping requires type-specific modules *)
    ignore f;
    { span; data }
end

module Stmt = struct
  type t = stmt node

  type data = stmt_data =
    | Import of { clause : import_clause; source : ident }
    | Export of { clause : export_clause; source : ident option }
    | Bind of { binding : binding; value : expr node; attr : attr option }
    | Extern of { abi : string option; sig_decls : fn_sig_decl list }
    | Expr of expr node

  let import ~clause ~source span =
    { span; data = Stmt_data (Import { clause; source }) }

  let export ~clause ?source span =
    { span; data = Stmt_data (Export { clause; source }) }

  let bind ~binding ~value ?attr span =
    { span; data = Stmt_data (Bind { binding; value; attr }) }

  let extern ?abi ~sig_decls span =
    { span; data = Stmt_data (Extern { abi; sig_decls }) }

  let expr ~node span = { span; data = Stmt_data (Expr node) }
  let data { data; _ } = match data with Stmt_data d -> d
  let span = Node.span
end

module Expr = struct
  type t = expr node

  type data = expr_data =
    | Lit of lit
    | Tmpl of tmpl_string
    | Ident of ident
    | Tuple of expr node list
    | Block of block
    | Range of {
          start : expr node option
        ; end_ : expr node option
        ; exclusive : bool
      }
    | If of {
          conds : (pat node * expr node) list
        ; then_block : block
        ; else_block : block option
      }
    | Match of { scrutinee : expr node; arms : match_arm list }
    | For of {
          binding : for_binding
        ; range : expr node
        ; guard : guard
        ; body : block
      }
    | While of { cond : cond option; guard : guard; body : block }
    | Defer of expr node
    | Break of expr node option
    | Cycle
    | Unsafe of block
    | Assign of { target : ident; value : expr node }
    | Unary of { op : Token.t; arg : expr node }
    | Call of {
          callee : expr node
        ; typ_args : typ_args option
        ; args : expr node list
        ; optional : bool
      }
    | Member of {
          obj : expr node
        ; prop : ident
        ; computed : bool
        ; optional : bool
      }
    | RecordLit of { name : ident option; fields : field_init list }
    | Fn of {
          abi : string option
        ; name : ident option
        ; typ_params : typ_params option
        ; sig_ : fn_sig
        ; body : block
      }
    | Record of {
          typ_params : typ_params option
        ; typ_annot : typ node option
        ; fields : field_decl list
      }
    | Choice of { typ_params : typ_params option; cases : choice_case list }

  let lit ~value span = { span; data = Expr_data (Lit value) }
  let tmpl ~value span = { span; data = Expr_data (Tmpl value) }
  let ident ~value span = { span; data = Expr_data (Ident value) }
  let tuple ~values span = { span; data = Expr_data (Tuple values) }
  let block ~value span = { span; data = Expr_data (Block value) }

  let range ?start ?end_ ~exclusive span =
    { span; data = Expr_data (Range { start; end_; exclusive }) }

  let if_ ~conds ~then_block ?else_block span =
    { span; data = Expr_data (If { conds; then_block; else_block }) }

  let match_ ~scrutinee ~arms span =
    { span; data = Expr_data (Match { scrutinee; arms }) }

  let for_ ~binding ~range ?guard ~body span =
    { span; data = Expr_data (For { binding; range; guard; body }) }

  let while_ ?cond ?guard ~body span =
    { span; data = Expr_data (While { cond; guard; body }) }

  let defer ~value span = { span; data = Expr_data (Defer value) }
  let break ?value span = { span; data = Expr_data (Break value) }
  let cycle span = { span; data = Expr_data Cycle }
  let unsafe ~value span = { span; data = Expr_data (Unsafe value) }

  let assign ~target ~value span =
    { span; data = Expr_data (Assign { target; value }) }

  let unary ~op ~arg span = { span; data = Expr_data (Unary { op; arg }) }

  let call ~callee ?typ_args ~args ?optional span =
    let optional = Option.value optional ~default:false in
    { span; data = Expr_data (Call { callee; typ_args; args; optional }) }

  let member ~obj ~prop ?computed ?optional span =
    let computed = Option.value computed ~default:false in
    let optional = Option.value optional ~default:false in
    { span; data = Expr_data (Member { obj; prop; computed; optional }) }

  let record_lit ?name ~fields span =
    { span; data = Expr_data (RecordLit { name; fields }) }

  let fn ?abi ?name ?typ_params ~sig_ ~body span =
    { span; data = Expr_data (Fn { abi; name; typ_params; sig_; body }) }

  let record ?typ_params ?typ_annot ~fields span =
    { span; data = Expr_data (Record { typ_params; typ_annot; fields }) }

  let choice ?typ_params ~cases span =
    { span; data = Expr_data (Choice { typ_params; cases }) }

  let data { data; _ } = match data with Expr_data d -> d
  let span = Node.span
end

module Pat = struct
  type t = pat node

  type data = pat_data =
    | Bind of binding
    | Lit of lit
    | Wild
    | Ident of ident
    | Record of { name : ident; fields : field_pat list }
    | Ctor of { name : ident; args : pat node list }
    | Tuple of pat node list

  let bind ~binding span = { span; data = Pat_data (Bind binding) }
  let lit ~value span = { span; data = Pat_data (Lit value) }
  let wild span = { span; data = Pat_data Wild }
  let ident ~value span = { span; data = Pat_data (Ident value) }

  let record ~name ~fields span =
    { span; data = Pat_data (Record { name; fields }) }

  let ctor ~name ~args span = { span; data = Pat_data (Ctor { name; args }) }
  let tuple ~values span = { span; data = Pat_data (Tuple values) }
  let data { data; _ } = match data with Pat_data d -> d
  let span = Node.span
end

module Typ = struct
  type t = typ node

  type data = typ_data =
    | Ptr of typ node
    | Array of { size : expr node option; elem : typ node }
    | Ident of ident
    | App of { base : ident; args : typ_args }
    | Tuple of typ node list
    | Fn of { params : typ node list; ret : typ node option }
    | Record of field_decl list
    | Optional of typ node

  let ptr ~value span = { span; data = Typ_data (Ptr value) }
  let array ?size ~elem span = { span; data = Typ_data (Array { size; elem }) }
  let ident ~value span = { span; data = Typ_data (Ident value) }
  let app ~base ~args span = { span; data = Typ_data (App { base; args }) }
  let tuple ~values span = { span; data = Typ_data (Tuple values) }
  let fn_ ~params ?ret span = { span; data = Typ_data (Fn { params; ret }) }
  let record ~fields span = { span; data = Typ_data (Record fields) }
  let optional ~value span = { span; data = Typ_data (Optional value) }
  let data { data; _ } = match data with Typ_data d -> d
  let span = Node.span
end
