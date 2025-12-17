open Basic

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

let ident_to_string interner id =
  match Interner.lookup_opt interner id with Some s -> s | None -> "<ident>"

let show interner = function
  | Ident s -> ident_to_string interner s
  | LitInt s -> ident_to_string interner s
  | LitReal s -> ident_to_string interner s
  | LitString s -> "\"" ^ ident_to_string interner s ^ "\""
  | LitRune c -> "'" ^ String.make 1 c ^ "'"
  | LitTemplateNoSubst s -> "$\"" ^ ident_to_string interner s ^ "\""
  | TemplateHead s -> "$\"" ^ ident_to_string interner s
  | TemplateMiddle s -> ident_to_string interner s
  | TemplateTail s -> ident_to_string interner s ^ "\""
  | KwAnd -> "and"
  | KwAs -> "as"
  | KwBreak -> "break"
  | KwCase -> "case"
  | KwCycle -> "cycle"
  | KwDefer -> "defer"
  | KwElse -> "else"
  | KwExtern -> "extern"
  | KwFalse -> "false"
  | KwFn -> "fn"
  | KwFor -> "for"
  | KwIf -> "if"
  | KwImport -> "import"
  | KwIn -> "in"
  | KwIs -> "is"
  | KwMatch -> "match"
  | KwNot -> "not"
  | KwOr -> "or"
  | KwRecord -> "record"
  | KwReturn -> "return"
  | KwSum -> "sum"
  | KwTrue -> "true"
  | KwTry -> "try"
  | KwUnsafe -> "unsafe"
  | KwVal -> "val"
  | KwVar -> "var"
  | KwWhile -> "while"
  | KwWith -> "with"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrackLt -> "[<"
  | GtRBrack -> ">]"
  | LParen -> "("
  | RParen -> ")"
  | Comma -> ","
  | Dot -> "."
  | Colon -> ":"
  | Semicolon -> ";"
  | Eq -> "="
  | SlashEq -> "/="
  | Lt -> "<"
  | LtEq -> "<="
  | Gt -> ">"
  | GtEq -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | StarStar -> "**"
  | Amp -> "&"
  | Bar -> "|"
  | BarGt -> "|>"
  | Caret -> "^"
  | Tilde -> "~"
  | At -> "@"
  | DotCaret -> ".^"
  | ColonColon -> "::"
  | QuestionQuestion -> "??"
  | DotDot -> ".."
  | DotDotLt -> "..<"
  | MinusGt -> "->"
  | LtMinus -> "<-"
  | ColonEq -> ":="
  | EqGt -> "=>"
  | Question -> "?"
  | Underscore -> "_"
  | Dollar -> "$"
  | EOF -> "EOF"
  | Unknown s -> ident_to_string interner s
