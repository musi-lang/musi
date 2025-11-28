type t =
  (* Literals *)
  | LitNumber of string
  | LitString of string
  | LitRune of char
  | LitTemplate of string
  (* Identifiers *)
  | Ident of string
  (* Keywords *)
  | KwVal
  | KwVar
  | KwDef
  | KwData
  | KwImport
  | KwExport
  | KwFrom
  | KwIf
  | KwElse
  | KwMatch
  | KwCase
  | KwFor
  | KwIn
  | KwStep
  | KwWhile
  | KwReturn
  | KwAnd
  | KwOr
  | KwNot
  | KwIs
  | KwAs
  | KwWhere
  | KwInstance
  | KwExtern
  (* Operators *)
  | LtMinus (* <- *)
  | ColonEq (* := *)
  | Eq (* = *)
  | BangEq (* != *)
  | Lt (* < *)
  | LtEq (* <= *)
  | Gt (* > *)
  | GtEq (* >= *)
  | Plus (* + *)
  | Minus (* - *)
  | Star (* * *)
  | Slash (* / *)
  | StarStar (* ** *)
  | PipeGt (* |> *)
  | Amp (* & *)
  | Pipe (* | *)
  | Caret (* ^ *)
  | Tilde (* ~ *)
  | At (* @ *)
  | Bang (* ! *)
  (* Punctuation *)
  | LParen (* ( *)
  | RParen (* ) *)
  | LBrace (* { *)
  | RBrace (* } *)
  | LBrack (* [ *)
  | RBrack (* ] *)
  | Comma (* , *)
  | Semi (* ; *)
  | Colon (* : *)
  | Dot (* . *)
  | MinusGt (* -> *)
  (* Special *)
  | EOF
  | Newline
  | Whitespace
  | Comment of string
  | Error

let keyword_strings =
  [
    ("val", KwVal)
  ; ("var", KwVar)
  ; ("def", KwDef)
  ; ("data", KwData)
  ; ("import", KwImport)
  ; ("export", KwExport)
  ; ("from", KwFrom)
  ; ("if", KwIf)
  ; ("else", KwElse)
  ; ("match", KwMatch)
  ; ("case", KwCase)
  ; ("for", KwFor)
  ; ("in", KwIn)
  ; ("step", KwStep)
  ; ("while", KwWhile)
  ; ("return", KwReturn)
  ; ("and", KwAnd)
  ; ("or", KwOr)
  ; ("not", KwNot)
  ; ("is", KwIs)
  ; ("as", KwAs)
  ; ("where", KwWhere)
  ; ("instance", KwInstance)
  ; ("extern", KwExtern)
  ]

let symbol_strings =
  [
    ("<-", LtMinus)
  ; (":=", ColonEq)
  ; ("!=", BangEq)
  ; ("<=", LtEq)
  ; (">=", GtEq)
  ; ("**", StarStar)
  ; ("|>", PipeGt)
  ; ("->", MinusGt)
  ; ("=", Eq)
  ; ("<", Lt)
  ; (">", Gt)
  ; ("+", Plus)
  ; ("-", Minus)
  ; ("*", Star)
  ; ("/", Slash)
  ; ("&", Amp)
  ; ("|", Pipe)
  ; ("^", Caret)
  ; ("~", Tilde)
  ; ("@", At)
  ; ("!", Bang)
  ; ("(", LParen)
  ; (")", RParen)
  ; ("{", LBrace)
  ; ("}", RBrace)
  ; ("[", LBrack)
  ; ("]", RBrack)
  ; (",", Comma)
  ; (";", Semi)
  ; (":", Colon)
  ; (".", Dot)
  ]

let base_table_size = 32

let keyword_to_string =
  let tbl = Hashtbl.create base_table_size in
  List.iter (fun (k, v) -> Hashtbl.add tbl v k) keyword_strings;
  tbl

let symbol_to_string =
  let tbl = Hashtbl.create base_table_size in
  List.iter (fun (k, v) -> Hashtbl.add tbl v k) symbol_strings;
  tbl

let to_string = function
  | LitNumber s -> "NUMBER(" ^ s ^ ")"
  | LitString s -> "STRING(" ^ s ^ ")"
  | LitRune c -> "RUNE(" ^ String.make 1 c ^ ")"
  | LitTemplate s -> "TEMPLATE(" ^ s ^ ")"
  | Ident s -> "IDENT(" ^ s ^ ")"
  | ( KwVal | KwVar | KwDef | KwData | KwImport | KwExport | KwFrom | KwIf
    | KwElse | KwMatch | KwCase | KwFor | KwIn | KwStep | KwWhile | KwReturn
    | KwAnd | KwOr | KwNot | KwIs | KwAs | KwWhere | KwInstance | KwExtern ) as
    kw ->
    Hashtbl.find keyword_to_string kw
  | ( LtMinus | ColonEq | Eq | BangEq | Lt | LtEq | Gt | GtEq | Plus | Minus
    | Star | Slash | StarStar | PipeGt | Amp | Pipe | Caret | Tilde | At | Bang
    | LParen | RParen | LBrace | RBrace | LBrack | RBrack | Comma | Semi | Colon
    | Dot | MinusGt ) as sym ->
    Hashtbl.find symbol_to_string sym
  | EOF -> "EOF"
  | Newline -> "NEWLINE"
  | Whitespace -> "WHITESPACE"
  | Comment s -> "COMMENT(" ^ s ^ ")"
  | Error -> "ERROR"

let keywords =
  let tbl = Hashtbl.create base_table_size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) keyword_strings;
  tbl

let lookup_keyword s =
  match Hashtbl.find_opt keywords s with Some kw -> kw | None -> Ident s

let symbols =
  let tbl = Hashtbl.create base_table_size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) symbol_strings;
  tbl
