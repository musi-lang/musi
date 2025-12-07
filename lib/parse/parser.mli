open Basic

type state
type 'a t = state -> ('a * state, Diagnostic.t) Result.t

val mk_state :
  (Lex.Token.t * Span.t) list -> Interner.file_id -> Interner.t -> state

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val parse_error : Errors.parse_error -> Span.t -> 'a t
val catch_error : 'a t -> (Diagnostic.t -> 'a t) -> 'a t
val or_else : 'a t -> 'a t -> 'a t
val ( <|> ) : 'a t -> 'a t -> 'a t
val advance : unit t
val token : (Lex.Token.t * Span.t) t
val peek : state -> Lex.Token.t
val peek_span : state -> Span.t
val curr_opt : state -> (Lex.Token.t * Span.t) option
val choice : 'a t list -> 'a t
val optional : 'a t -> 'a option t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val sep_by : 'a t -> 'b t -> 'a list t
val between : 'a t -> 'b t -> 'c t -> 'c t
val token_match : (Lex.Token.t -> bool) -> Lex.Token.t t
val token_expect : Lex.Token.t -> unit t
val ident_parser : Ast.Node.ident t
val keyword_parser : Lex.Token.t -> unit t
val report_error : state -> Errors.parse_error -> Span.t -> unit
val current_token : state -> Lex.Token.t
val current_span : state -> Span.t
val make_node : Span.t -> 'a -> 'a Ast.Node.node

type bp = { lhs : int; rhs : int }

val infix_bp : Lex.Token.t -> bp
val prefix_bp : Lex.Token.t -> int
val postfix_bp : Lex.Token.t -> int
val can_bind_infix : Lex.Token.t -> bool
val can_bind_prefix : Lex.Token.t -> bool
val can_bind_postfix : Lex.Token.t -> bool
val parse_expr_bp : int -> Ast.Node.expr t
val parse_expr : Ast.Node.expr t
