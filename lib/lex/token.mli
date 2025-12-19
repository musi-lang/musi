open Basic

(** Lexical token kind *)
type t =
  | Ident of Interner.ident
  | LitInt of Interner.ident
  | LitReal of Interner.ident
  | LitString of Interner.ident
  | LitRune of char
  | LitTemplateNoSubst of Interner.ident
  | TemplateHead of Interner.ident
  | TemplateMiddle of Interner.ident
  | TemplateTail of Interner.ident
  | KwAnd
  | KwAs
  | KwBreak
  | KwCase
  | KwCycle
  | KwDefer
  | KwElse
  | KwExtern
  | KwFalse
  | KwFn
  | KwFor
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwMatch
  | KwMod
  | KwNot
  | KwOr
  | KwRecord
  | KwReturn
  | KwSum
  | KwTrue
  | KwTry
  | KwUnsafe
  | KwVal
  | KwVar
  | KwWhile
  | KwWith
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | LBrackLt
  | GtRBrack
  | LParen
  | RParen
  | Comma
  | Dot
  | Colon
  | Semicolon
  | Eq
  | SlashEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | LtLt
  | GtGt
  | StarStar
  | Amp
  | Bar
  | BarGt
  | Caret
  | Tilde
  | At
  | DotCaret
  | ColonColon
  | QuestionQuestion
  | DotDot
  | DotDotLt
  | MinusGt
  | LtMinus
  | ColonEq
  | EqGt
  | Question
  | Underscore
  | Dollar
  | EOF
  | Unknown of Interner.ident

(** Convert token to string representation *)
val show : Interner.t -> t -> string
