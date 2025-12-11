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

let show = function
  | Ident s -> s
  | LitInt s -> s
  | LitReal s -> s
  | LitString s -> "\"" ^ s ^ "\""
  | LitRune c -> "'" ^ String.make 1 c ^ "'"
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
  | KwUnsafe -> "unsafe"
  | KwVal -> "val"
  | KwVar -> "var"
  | KwWhile -> "while"
  | KwWith -> "with"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBrack -> "["
  | RBrack -> "]"
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
  | ColonColon -> "::"
  | QuestionQuestion -> "??"
  | DotDot -> ".."
  | DotDotLt -> "..<"
  | MinusGt -> "->"
  | EqGt -> "=>"
  | Question -> "?"
  | Underscore -> "_"
  | EOF -> "EOF"
