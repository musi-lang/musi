module type S = sig
  type t =
    (* Literals *)
    | LitNumber of string
    | LitString of Basic.Interner.name
    | LitRune of char
    | LitTemplate of Basic.Interner.name
    (* Identifiers *)
    | Ident of Basic.Interner.name
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
    | KwTrait
    | KwUnsafe
    | KwVal
    | KwVar
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
    | LtLt (* << *)
    | GtGt (* >> *)
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
    | EqGt
    | Underscore (* _ *)
    | Question (* ? *)
    (* Special *)
    | EOF
    | Newline
    | Whitespace
    | Comment of string
    | Error

  val to_string : t -> string
  val keyword_strings : (string * t) list
  val symbol_strings : (string * t) list
  val keyword_to_string : (t, string) Hashtbl.t
  val symbol_to_string : (t, string) Hashtbl.t
  val keywords : (string, t) Hashtbl.t
  val symbols : (string, t) Hashtbl.t
  val lookup_keyword : Basic.Interner.t -> string -> t
end

module Make () : S = struct
  open Basic

  type t =
    | LitNumber of string
    | LitString of Interner.name
    | LitRune of char
    | LitTemplate of Interner.name
    | Ident of Interner.name
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
    | KwTrait
    | KwUnsafe
    | KwVal
    | KwVar
    | KwWhile
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
    | Dollar
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
    | EOF
    | Newline
    | Whitespace
    | Comment of string
    | Error

  let keyword_strings =
    [
      ("and", KwAnd)
    ; ("as", KwAs)
    ; ("break", KwBreak)
    ; ("case", KwCase)
    ; ("choice", KwChoice)
    ; ("cycle", KwCycle)
    ; ("defer", KwDefer)
    ; ("else", KwElse)
    ; ("export", KwExport)
    ; ("extern", KwExtern)
    ; ("fn", KwFn)
    ; ("for", KwFor)
    ; ("from", KwFrom)
    ; ("if", KwIf)
    ; ("import", KwImport)
    ; ("in", KwIn)
    ; ("is", KwIs)
    ; ("match", KwMatch)
    ; ("not", KwNot)
    ; ("or", KwOr)
    ; ("record", KwRecord)
    ; ("return", KwReturn)
    ; ("trait", KwTrait)
    ; ("unsafe", KwUnsafe)
    ; ("val", KwVal)
    ; ("var", KwVar)
    ; ("while", KwWhile)
    ]

  let symbol_strings =
    [
      ("<-", LtMinus)
    ; (":=", ColonEq)
    ; ("!=", BangEq)
    ; ("<=", LtEq)
    ; (">=", GtEq)
    ; ("<<", LtLt)
    ; (">>", GtGt)
    ; ("**", StarStar)
    ; ("|>", PipeGt)
    ; ("->", MinusGt)
    ; ("=>", EqGt)
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
    ; ("_", Underscore)
    ; ("?", Question)
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
    | ( KwAnd | KwAs | KwBreak | KwCase | KwChoice | KwCycle | KwDefer | KwElse
      | KwExport | KwExtern | KwFn | KwFor | KwFrom | KwIf | KwImport | KwIn
      | KwIs | KwMatch | KwNot | KwOr | KwRecord | KwReturn | KwTrait | KwUnsafe
      | KwVal | KwVar | KwWhile ) as kw ->
      Hashtbl.find keyword_to_string kw
    | ( LtMinus | ColonEq | Eq | BangEq | Lt | LtEq | Gt | GtEq | LtLt | GtGt
      | Plus | Minus | Star | Slash | StarStar | PipeGt | Amp | Pipe | Caret
      | Tilde | At | Bang | Dollar | LParen | RParen | LBrace | RBrace | LBrack
      | RBrack | Comma | Semi | Colon | Dot | MinusGt | EqGt | Underscore
      | Question ) as sym ->
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
end

include Make ()
