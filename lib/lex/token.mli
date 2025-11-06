(** Defines token types produced by Musi lexer. *)

(** Enumerates suffixes allowed on numeric literals. *)
type suffix =
  | I8
  | I16
  | I32
  | I64
  | I128
  | N8
  | N16
  | N32
  | N64
  | N128
  | B16
  | B32
  | B64

(** Enumerates every lexical token kind recognised by lexer. *)
type kind =
  | Ident of Interner.name
  | LitNumeric of string * suffix option
  | LitText of Interner.name
  | LitRune of int
  | LitTemplate of Interner.name
  | TemplateHead of Interner.name
  | TemplateMiddle of Interner.name
  | TemplateTail of Interner.name
  | KwAlias
  | KwAnd
  | KwAs
  | KwAsync
  | KwAwait
  | KwBreak
  | KwCase
  | KwChoice
  | KwConst
  | KwContinue
  | KwDefer
  | KwDo
  | KwElse
  | KwExport
  | KwExtern
  | KwFalse
  | KwFor
  | KwFrom
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwMatch
  | KwMod
  | KwNot
  | KwOr
  | KwProc
  | KwRecord
  | KwReturn
  | KwShl
  | KwShr
  | KwThen
  | KwTrue
  | KwTry
  | KwUnsafe
  | KwVar
  | KwWeak
  | KwWhere
  | KwWhile
  | KwWith
  | KwXor
  | KwYield
  | Underscore
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Colon
  | Semi
  | At
  | Question
  | Bang
  | Dollar
  | Plus
  | Minus
  | Star
  | Slash
  | Caret
  | Eq
  | EqSlashEq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | LtMinus
  | MinusGt
  | ColonEq
  | DotDotLt
  | DotDot
  | Whitespace
  | Newline
  | LineComment of Interner.name
  | BlockComment of { content : Interner.name; docstyle : bool }
  | Error
  | Eof

(** Represents lexical token paired with its source span. *)
type t = { kind : kind; span : Span.t }

(** Represents mutable token stream consumed by parser. *)
type stream

(** Produces token from provided kind and span. *)
val make : kind -> Span.t -> t

(** Produces end-of-file token for given span. *)
val eof : Span.t -> t

(** Wraps list of tokens in mutable stream interface. *)
val make_stream : t list -> stream

(** Checks whether stream has consumed all tokens. *)
val at_end : stream -> bool

(** Retrieves current token without advancing stream. *)
val curr : stream -> t

(** Peeks at next token without advancing stream. *)
val peek : stream -> t

(** Moves stream forward by one token. *)
val advance : stream -> unit

(** Consumes current token when it matches expected kind and returns it. *)
val expect : stream -> kind -> t option

(** Renders token kind as readable string. *)
val show_kind : Interner.t -> kind -> string
