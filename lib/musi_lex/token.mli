open Musi_basic

type kind =
  | Ident of Interner.name
  | LitNum of string
  | LitStr of Interner.name
  | LitChr of int
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
  | BlockComment of Interner.name
  | Error
  | Eof

type t = { kind : kind; span : Span.t }
type stream

val make : kind -> Span.t -> t
val eof : Span.t -> t
val make_stream : t list -> stream
val at_end : stream -> bool
val curr : stream -> t
val peek : stream -> t
val advance : stream -> unit
val expect : stream -> kind -> t option
val show_kind : Interner.t -> kind -> string
