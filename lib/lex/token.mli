type t =
  | Ident of string
  | LitInt of string
  | LitReal of string
  | LitString of string
  | LitRune of char
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
  | KwNot
  | KwOr
  | KwRecord
  | KwReturn
  | KwSum
  | KwTrue
  | KwUnsafe
  | KwVal
  | KwVar
  | KwWhile
  | KwWith
  | LBrace
  | RBrace
  | LBrack
  | RBrack
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
  | StarStar
  | Amp
  | Bar
  | BarGt
  | Caret
  | Tilde
  | At
  | ColonColon
  | QuestionQuestion
  | DotDot
  | DotDotLt
  | MinusGt
  | EqGt
  | Question
  | Underscore
  | EOF

val show : t -> string
