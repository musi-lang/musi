type t =
  (* Literals *)
  | LitNumber of string
  | LitString of int
  | LitRune of char
  | LitTemplate of int
  (* Identifiers *)
  | Ident of int
  (* Keywords (alphabetically) *)
  | KwAnd
  | KwAs
  | KwBreak
  | KwCase
  | KwChoice
  | KwCycle
  | KwDefer
  | KwElse
  | KwExport
  | KwExtern
  | KwFn
  | KwFor
  | KwFrom
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwMatch
  | KwNot
  | KwOr
  | KwRecord
  | KwReturn
  | KwUnsafe
  | KwVal
  | KwVar
  | KwWhile
  (* Operators *)
  | DotDotLt
  | DotDot
  | LtMinus
  | ColonEq
  | Eq
  | BangEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  | Plus
  | Minus
  | Star
  | Slash
  | StarStar
  | PipeGt
  | LtLt
  | GtGt
  | Amp
  | Pipe
  | Caret
  | Tilde
  | At
  | Bang
  (* Punctuation *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Comma
  | Semi
  | Colon
  | Dot
  | MinusGt
  | EqGt
  | Underscore
  | Question
  (* Special *)
  | Unknown
  | EOF
  | Newline
  | Whitespace
  | Comment of string

val show : t -> string
