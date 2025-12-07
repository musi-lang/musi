open Basic

type state
type 'a t = state -> ('a * state, Diagnostic.t) Result.t

val mk_state :
  (Lex.Token.t * Span.t) list -> Interner.file_id -> Interner.t -> state

val advance : unit t
val advance_n : int -> unit t
val curr : state -> Lex.Token.t
val curr_span : state -> Span.t
val parse_typ : Ast.Node.typ t
val parse_pat : Ast.Node.pat t
val parse_expr : Ast.Node.expr t
val parse_stmt : Ast.Node.stmt t
val parse_prog : Ast.Node.prog t
