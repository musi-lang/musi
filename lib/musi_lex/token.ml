open Musi_basic

type kind =
  | Ident of Interner.name
  | LitNumber of string
  | LitString of Interner.name
  | LitRune of int
  | LitNoSubstTemplate of Interner.name
  | TemplateHead of Interner.name
  | TemplateMiddle of Interner.name
  | TemplateTail of Interner.name
  | KwAnd
  | KwAs
  | KwBreak
  | KwCase
  | KwChoice
  | KwContinue
  | KwDefer
  | KwDo
  | KwElse
  | KwExport
  | KwExtern
  | KwFalse
  | KwFor
  | KwFn
  | KwFrom
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwMatch
  | KwMod
  | KwNot
  | KwOr
  | KwRecord
  | KwRef
  | KwReturn
  | KwShl
  | KwShr
  | KwThen
  | KwTrue
  | KwTry
  | KwVal
  | KwVar
  | KwWhere
  | KwWhile
  | KwWith
  | KwXor
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
  | LitNumber s -> s
  | LitString n -> Printf.sprintf "\"%s\"" (Interner.lookup interner n)
  | LitRune c ->
    Printf.sprintf "'%s'" (Uchar.to_char (Uchar.of_int c) |> String.make 1)
  | LitNoSubstTemplate n -> Printf.sprintf "`%s`" (Interner.lookup interner n)
  | TemplateHead n -> Printf.sprintf "`%s${" (Interner.lookup interner n)
  | TemplateMiddle n -> Printf.sprintf "}%s${" (Interner.lookup interner n)
  | TemplateTail n -> Printf.sprintf "}%s`" (Interner.lookup interner n)
  | KwAnd -> "and"
  | KwAs -> "as"
  | KwBreak -> "break"
  | KwCase -> "case"
  | KwChoice -> "choice"
  | KwContinue -> "continue"
  | KwDefer -> "defer"
  | KwDo -> "do"
  | KwElse -> "else"
  | KwExport -> "export"
  | KwExtern -> "extern"
  | KwFalse -> "false"
  | KwFor -> "for"
  | KwFrom -> "from"
  | KwFn -> "fn"
  | KwIf -> "if"
  | KwImport -> "import"
  | KwIn -> "in"
  | KwIs -> "is"
  | KwMatch -> "match"
  | KwMod -> "mod"
  | KwNot -> "not"
  | KwOr -> "or"
  | KwRecord -> "record"
  | KwRef -> "ref"
  | KwReturn -> "return"
  | KwShl -> "shl"
  | KwShr -> "shr"
  | KwThen -> "then"
  | KwTrue -> "true"
  | KwTry -> "try"
  | KwVal -> "val"
  | KwVar -> "var"
  | KwWhere -> "where"
  | KwWhile -> "while"
  | KwWith -> "with"
  | KwXor -> "xor"
  | Underscore -> "_"
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
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
  | Whitespace -> "\\s"
  | Newline -> "\\n"
  | LineComment n -> Printf.sprintf "// %s" (Interner.lookup interner n)
  | BlockComment n -> Printf.sprintf "/* %s */" (Interner.lookup interner n)
  | Error -> "<error>"
  | Eof -> "<eof>"
