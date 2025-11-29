open Basic

type t =
  (* Literals *)
  | LitNumber of string
  | LitString of Interner.name
  | LitRune of char
  | LitTemplate of Interner.name
  (* Identifiers *)
  | Ident of Interner.name
  (* Keywords (alphabetically) *)
  | KwAnd
  | KwAs
  | KwCase
  | KwData
  | KwDef
  | KwElse
  | KwExport
  | KwExtern
  | KwFor
  | KwFrom
  | KwIf
  | KwImport
  | KwIn
  | KwInstance
  | KwIs
  | KwMatch
  | KwNot
  | KwOr
  | KwReturn
  | KwStep
  | KwUnsafe
  | KwVal
  | KwVar
  | KwWhere
  | KwWhile
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
  | Dollar (* $ *)
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
    ("and", KwAnd)
  ; ("as", KwAs)
  ; ("case", KwCase)
  ; ("data", KwData)
  ; ("def", KwDef)
  ; ("else", KwElse)
  ; ("export", KwExport)
  ; ("extern", KwExtern)
  ; ("for", KwFor)
  ; ("from", KwFrom)
  ; ("if", KwIf)
  ; ("import", KwImport)
  ; ("in", KwIn)
  ; ("instance", KwInstance)
  ; ("is", KwIs)
  ; ("match", KwMatch)
  ; ("not", KwNot)
  ; ("or", KwOr)
  ; ("return", KwReturn)
  ; ("step", KwStep)
  ; ("unsafe", KwUnsafe)
  ; ("val", KwVal)
  ; ("var", KwVar)
  ; ("where", KwWhere)
  ; ("while", KwWhile)
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
  ; ("$", Dollar)
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
  | LitString name -> "STRING(id:" ^ string_of_int name ^ ")"
  | LitRune c -> "RUNE(" ^ String.make 1 c ^ ")"
  | LitTemplate name -> "TEMPLATE(id:" ^ string_of_int name ^ ")"
  | Ident name -> "IDENT(id:" ^ string_of_int name ^ ")"
  | ( KwAnd | KwAs | KwCase | KwData | KwDef | KwElse | KwExport | KwExtern
    | KwFor | KwFrom | KwIf | KwImport | KwIn | KwInstance | KwIs | KwMatch
    | KwNot | KwOr | KwReturn | KwStep | KwUnsafe | KwVal | KwVar | KwWhere
    | KwWhile ) as kw ->
    Hashtbl.find keyword_to_string kw
  | ( LtMinus | ColonEq | Eq | BangEq | Lt | LtEq | Gt | GtEq | Plus | Minus
    | Star | Slash | StarStar | PipeGt | Amp | Pipe | Caret | Tilde | At | Bang
    | Dollar | LParen | RParen | LBrace | RBrace | LBrack | RBrack | Comma
    | Semi | Colon | Dot | MinusGt ) as sym ->
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

let lookup_keyword interner s =
  match Hashtbl.find_opt keywords s with
  | Some kw -> kw
  | None ->
    let name = Interner.intern interner s in
    Ident name

let symbols =
  let tbl = Hashtbl.create base_table_size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) symbol_strings;
  tbl
