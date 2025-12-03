open Basic
open Token

type t = Token.t

type state = {
    source : string
  ; mutable pos : int
  ; len : int
  ; file_id : Span.file_id
  ; interner : Interner.t
  ; mutable diags : Diagnostic.bag
}

let mk_state source file_id interner =
  {
    source
  ; pos = 0
  ; len = String.length source
  ; file_id
  ; interner
  ; diags = Diagnostic.empty_bag
  }

let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_digit c = '0' <= c && c <= '9'
let is_space c = c = ' ' || c = '\t' || c = '\r'
let is_ident_start c = is_letter c || c = '_'
let is_ident_cont c = is_letter c || is_digit c || c = '_'
let is_xdigit c = is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
let peek st = if st.pos >= st.len then '\000' else st.source.[st.pos]

let peek_n st n =
  if st.pos + n >= st.len then '\000' else st.source.[st.pos + n]

let advance st = st.pos <- st.pos + 1
let advance_n st n = st.pos <- st.pos + n
let span st start = Span.make st.file_id start st.pos
let substr st start len = String.sub st.source start len

let add_err st err_code start args =
  let sp = Span.make st.file_id start st.pos in
  st.diags <- Diagnostic.add st.diags (Error.lex_diag err_code sp args)

let keyword_table =
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

let symbols =
  let all_symbols =
    [
      (":=", ColonEq)
    ; (">=", GtEq)
    ; ("<=", LtEq)
    ; ("!=", BangEq)
    ; ("<<", LtLt)
    ; (">>", GtGt)
    ; ("**", StarStar)
    ; ("|>", PipeGt)
    ; ("->", MinusGt)
    ; ("=>", EqGt)
    ; ("!", Bang)
    ; ("&", Amp)
    ; ("(", LParen)
    ; (")", RParen)
    ; ("*", Star)
    ; ("+", Plus)
    ; (",", Comma)
    ; ("-", Minus)
    ; (".", Dot)
    ; ("/", Slash)
    ; (":", Colon)
    ; (";", Semi)
    ; ("<", Lt)
    ; ("=", Eq)
    ; (">", Gt)
    ; ("?", Question)
    ; ("@", At)
    ; ("[", LBrack)
    ; ("]", RBrack)
    ; ("^", Caret)
    ; ("_", Underscore)
    ; ("{", LBrace)
    ; ("|", Pipe)
    ; ("}", RBrace)
    ; ("~", Tilde)
    ]
  in
  List.sort
    (fun (s1, _) (s2, _) -> compare (String.length s2) (String.length s1))
    all_symbols

let lookup_escape c =
  match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | '0' -> '\000'
  | _ -> c

let rec skip_whitespace st =
  if st.pos < st.len && is_space st.source.[st.pos] then (
    advance st;
    skip_whitespace st)

let rec scan_ident st start =
  if st.pos < st.len && is_ident_cont st.source.[st.pos] then (
    advance st;
    scan_ident st start)
  else
    let text = substr st start (st.pos - start) in
    match List.assoc_opt text keyword_table with
    | Some kw -> kw
    | None -> Ident (Interner.intern st.interner text)

let rec scan_number st start =
  if st.pos < st.len && is_digit st.source.[st.pos] then (
    advance st;
    scan_number st start)
  else
    let has_decimal = st.pos < st.len && st.source.[st.pos] = '.' in
    if has_decimal then (
      advance st;
      let rec scan_fraction st =
        if st.pos < st.len && is_digit st.source.[st.pos] then (
          advance st;
          scan_fraction st)
        else ()
      in
      scan_fraction st);
    LitNumber (substr st start (st.pos - start))

let rec scan_string_content st =
  if st.pos >= st.len then ""
  else
    match st.source.[st.pos] with
    | '"' -> ""
    | '\\' when st.pos + 1 < st.len ->
      let esc = lookup_escape st.source.[st.pos + 1] in
      advance_n st 2;
      String.make 1 esc ^ scan_string_content st
    | c ->
      advance st;
      String.make 1 c ^ scan_string_content st

let scan_string st start =
  advance st;
  (* skip opening quote *)
  let content = scan_string_content st in
  if st.pos < st.len && st.source.[st.pos] = '"' then (
    advance st;
    (* skip closing quote *)
    LitString (Interner.intern st.interner content))
  else (
    add_err st Error.E0201 start [ "string" ];
    LitString (Interner.intern st.interner content))

let scan_rune st start =
  advance st;
  (* skip opening quote *)
  if st.pos >= st.len then (
    add_err st Error.E0201 start [ "rune" ];
    LitRune '\000')
  else
    match st.source.[st.pos] with
    | '\'' ->
      advance st;
      LitRune '\000'
    | '\\' when st.pos + 1 < st.len ->
      let esc = lookup_escape st.source.[st.pos + 1] in
      advance_n st 2;
      if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
      LitRune esc
    | c ->
      advance st;
      if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
      LitRune c

let rec try_symbol st start symbols =
  match symbols with
  | [] -> Token.Error
  | (sym, token) :: rest ->
    let len = String.length sym in
    if st.pos + len <= st.len && substr st st.pos len = sym then (
      for _i = 1 to len do
        advance st
      done;
      token)
    else try_symbol st start rest

let next_token st =
  skip_whitespace st;
  let start = st.pos in

  if st.pos >= st.len then EOF
  else
    match st.source.[st.pos] with
    | '\n' ->
      advance st;
      Newline
    | c when is_ident_start c -> scan_ident st start
    | c when is_digit c -> scan_number st start
    | '"' -> scan_string st start
    | '\'' -> scan_rune st start
    | '$' when st.pos + 1 < st.len && st.source.[st.pos + 1] = '"' ->
      advance_n st 2;
      let rec scan_template_content st acc =
        if st.pos >= st.len then acc
        else
          match st.source.[st.pos] with
          | '"' -> acc
          | c ->
            advance st;
            scan_template_content st (acc ^ String.make 1 c)
      in
      let content = scan_template_content st "" in
      if st.pos < st.len && st.source.[st.pos] = '"' then (
        advance st;
        LitTemplate (Interner.intern st.interner content))
      else (
        add_err st Error.E0201 start [ "template" ];
        LitTemplate (Interner.intern st.interner content))
    | _ -> try_symbol st start symbols

let tokenize source file_id interner =
  let st = mk_state source file_id interner in
  let rec collect acc =
    let start = st.pos in
    let token = next_token st in
    let token_span = Span.make st.file_id start st.pos in
    if token = EOF then
      let final_tokens = (EOF, token_span) :: List.rev acc in
      (final_tokens, st.diags)
    else
      match token with
      | Newline -> collect acc
      | _ -> collect ((token, token_span) :: acc)
  in
  collect []
