open Musi_basic

type kind =
  | Ident of Interner.name
  | LitNum of string
  | LitStr of Interner.name
  | LitRune of int
  | LitTpl of Interner.name
  | TplHead of Interner.name
  | TplMid of Interner.name
  | TplTail of Interner.name
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
  | Pipe
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
type stream = { tokens : t array; mutable pos : int }

let make kind span = { kind; span }
let eof span = { kind = Eof; span }
let make_stream tokens = { tokens = Array.of_list tokens; pos = 0 }
let at_end s = s.pos >= Array.length s.tokens
let curr s = if at_end s then eof Span.dummy else s.tokens.(s.pos)

let peek s =
  if s.pos + 1 >= Array.length s.tokens then eof Span.dummy
  else s.tokens.(s.pos + 1)

let advance s = if not (at_end s) then s.pos <- s.pos + 1

let expect s kind =
  let tok = curr s in
  if tok.kind = kind then (
    advance s;
    Some tok)
  else None

let show_kind interner = function
  | Ident n -> Interner.lookup interner n
  | LitNum s -> s
  | LitStr n -> Printf.sprintf "\"%s\"" (Interner.lookup interner n)
  | LitRune c ->
    Printf.sprintf "'%s'" (Uchar.to_char (Uchar.of_int c) |> String.make 1)
  | LitTpl n -> Printf.sprintf "`%s`" (Interner.lookup interner n)
  | TplHead n -> Printf.sprintf "`%s${" (Interner.lookup interner n)
  | TplMid n -> Printf.sprintf "}%s${" (Interner.lookup interner n)
  | TplTail n -> Printf.sprintf "}%s`" (Interner.lookup interner n)
  | KwAnd -> "and"
  | KwAs -> "as"
  | KwAsync -> "async"
  | KwAwait -> "await"
  | KwBreak -> "break"
  | KwCase -> "case"
  | KwChoice -> "choice"
  | KwConst -> "const"
  | KwContinue -> "continue"
  | KwDefer -> "defer"
  | KwDo -> "do"
  | KwElse -> "else"
  | KwExport -> "export"
  | KwExtern -> "extern"
  | KwFalse -> "false"
  | KwFor -> "for"
  | KwFrom -> "from"
  | KwIf -> "if"
  | KwImport -> "import"
  | KwIn -> "in"
  | KwIs -> "is"
  | KwMatch -> "match"
  | KwMod -> "mod"
  | KwNot -> "not"
  | KwOr -> "or"
  | KwProc -> "proc"
  | KwRecord -> "record"
  | KwReturn -> "return"
  | KwShl -> "shl"
  | KwShr -> "shr"
  | KwThen -> "then"
  | KwTrue -> "true"
  | KwTry -> "try"
  | KwUnsafe -> "unsafe"
  | KwVar -> "var"
  | KwWeak -> "weak"
  | KwWhere -> "where"
  | KwWhile -> "while"
  | KwWith -> "with"
  | KwXor -> "xor"
  | KwYield -> "yield"
  | Underscore -> "_"
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Pipe -> "|"
  | Comma -> ","
  | Dot -> "."
  | Colon -> ":"
  | Semi -> ";"
  | At -> "@"
  | Question -> "?"
  | Bang -> "!"
  | Dollar -> "$"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Caret -> "^"
  | Eq -> "="
  | EqSlashEq -> "=/="
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | LtMinus -> "<-"
  | MinusGt -> "->"
  | ColonEq -> ":="
  | DotDotLt -> "..<"
  | DotDot -> ".."
  | Whitespace -> "WHITESPACE"
  | Newline -> "NEWLINE"
  | LineComment n -> Printf.sprintf "// %s" (Interner.lookup interner n)
  | BlockComment n -> Printf.sprintf "/* %s */" (Interner.lookup interner n)
  | Error -> "<ERROR>"
  | Eof -> "<EOF>"
