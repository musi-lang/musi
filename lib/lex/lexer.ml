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

let peek st = if st.pos >= st.len then '\000' else st.source.[st.pos]

let peek_n st n =
  if st.pos + n >= st.len then '\000' else st.source.[st.pos + n]

let adv st = st.pos <- st.pos + 1
let adv_n st n = st.pos <- st.pos + n
let span st start = Span.make st.file_id start st.pos
let substr st s e = String.sub st.source s (e - s)

let add_err st code start end_ args =
  st.diags <-
    Diagnostic.add
      st.diags
      (Error.lex_diag code (Span.make st.file_id start end_) args)

let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_digit c = '0' <= c && c <= '9'
let is_space c = c = ' ' || c = '\t' || c = '\r'
let is_ident_start c = is_letter c || c = '_'
let is_ident_cont c = is_letter c || is_digit c || c = '_'
let is_xdigit c = is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')

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

let symbols =
  [
    (">=", GtEq)
  ; ("<=", LtEq)
  ; ("!=", BangEq)
  ; ("<<", LtLt)
  ; (">>", GtGt)
  ; ("**", StarStar)
  ; ("|>", PipeGt)
  ; ("->", MinusGt)
  ; ("=>", EqGt)
  ; (":=", ColonEq)
  ; ("!", Bang)
  ; ("$", Dollar)
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

type 'a scanner = state -> ('a * state, state) Result.t

let bind_error scanner fallback st =
  match scanner st with
  | Result.Ok result -> Result.Ok result
  | Result.Error st' -> fallback st'

let map_error f scanner st =
  match scanner st with
  | Result.Ok result -> Result.Ok result
  | Result.Error st' -> Result.Error (f st')

let rec scan_whitespace_impl st start =
  if st.pos < st.len && is_space st.source.[st.pos] then (
    adv st;
    scan_whitespace_impl st start)
  else (Whitespace, span st start)

let scan_whitespace st =
  if st.pos < st.len && is_space st.source.[st.pos] then
    Result.Ok (scan_whitespace_impl st st.pos)
  else Result.Error st

let scan_newline st =
  if st.pos < st.len && st.source.[st.pos] = '\n' then (
    let start = st.pos in
    adv st;
    Result.Ok (Newline, span st start))
  else Result.Error st

let rec scan_line_comment_impl st start =
  if st.pos < st.len && st.source.[st.pos] <> '\n' then (
    adv st;
    scan_line_comment_impl st start)
  else
    let content = substr st (start + 2) st.pos in
    (Comment content, span st start)

let scan_line_comment st =
  if
    st.pos + 1 < st.len
    && st.source.[st.pos] = '/'
    && st.source.[st.pos + 1] = '/'
  then (
    adv_n st 2;
    Result.Ok (scan_line_comment_impl st st.pos))
  else Result.Error st

let rec scan_block_comment_impl st start depth =
  if st.pos + 1 < st.len then (
    match (st.source.[st.pos], st.source.[st.pos + 1]) with
    | '/', '*' ->
      adv_n st 2;
      scan_block_comment_impl st start (depth + 1)
    | '*', '/' ->
      adv_n st 2;
      if depth = 0 then
        let content = substr st (start + 2) (st.pos - 2) in
        (Comment content, span st start)
      else scan_block_comment_impl st start (depth - 1)
    | _ ->
      adv st;
      scan_block_comment_impl st start depth)
  else
    let content = substr st (start + 2) st.pos in
    (Comment content, span st start)

let scan_block_comment st =
  if
    st.pos + 1 < st.len
    && st.source.[st.pos] = '/'
    && st.source.[st.pos + 1] = '*'
  then (
    adv_n st 2;
    Result.Ok (scan_block_comment_impl st st.pos 0))
  else Result.Error st

let scan_comment st =
  bind_error scan_line_comment (fun st -> scan_block_comment st) st

let rec scan_ident_impl st start =
  if st.pos < st.len && is_ident_cont st.source.[st.pos] then (
    adv st;
    scan_ident_impl st start)
  else
    let text = substr st start st.pos in
    let token =
      match List.assoc_opt text keyword_table with
      | Some kw -> kw
      | None ->
        let name = Interner.intern st.interner text in
        Ident name
    in
    (token, span st start)

let scan_ident st =
  if st.pos < st.len && is_ident_start st.source.[st.pos] then
    Result.Ok (scan_ident_impl st st.pos)
  else Result.Error st

let rec scan_number_digits_impl st =
  if st.pos < st.len && is_digit st.source.[st.pos] then (
    adv st;
    scan_number_digits_impl st)

let rec scan_number_fraction_impl st =
  if st.pos < st.len && is_digit st.source.[st.pos] then (
    adv st;
    scan_number_fraction_impl st)

let scan_number st =
  if st.pos < st.len && is_digit st.source.[st.pos] then (
    let start = st.pos in
    scan_number_digits_impl st;
    if st.pos < st.len && st.source.[st.pos] = '.' then (
      adv st;
      scan_number_fraction_impl st);
    let text = substr st start st.pos in
    Result.Ok (LitNumber text, span st start))
  else Result.Error st

let rec scan_string_content_impl st =
  if st.pos >= st.len then false
  else
    match st.source.[st.pos] with
    | '"' -> true
    | '\\' when st.pos + 1 < st.len ->
      adv_n st 2;
      scan_string_content_impl st
    | _ ->
      adv st;
      scan_string_content_impl st

let scan_string st =
  if st.pos < st.len && st.source.[st.pos] = '"' then (
    let start = st.pos in
    adv st;
    let finished = scan_string_content_impl st in
    if finished then adv st
    else add_err st Error.E0201 start st.pos [ "string" ];
    let text = substr st (start + 1) (st.pos - 1) in
    let name = Interner.intern st.interner text in
    Result.Ok (LitString name, span st start))
  else Result.Error st

let rec scan_template_content_impl st depth =
  if st.pos >= st.len then false
  else
    match st.source.[st.pos] with
    | '"' when depth = 0 -> true
    | '{' ->
      adv st;
      scan_template_content_impl st (depth + 1)
    | '}' when depth > 0 ->
      adv st;
      scan_template_content_impl st (depth - 1)
    | '\\' when st.pos + 1 < st.len ->
      adv_n st 2;
      scan_template_content_impl st depth
    | _ ->
      adv st;
      scan_template_content_impl st depth

let scan_template st =
  if
    st.pos + 1 < st.len
    && st.source.[st.pos] = '$'
    && st.source.[st.pos + 1] = '"'
  then (
    let start = st.pos in
    adv_n st 2;
    let finished = scan_template_content_impl st 0 in
    if finished then adv st
    else add_err st Error.E0201 start st.pos [ "template" ];
    let text = substr st (start + 2) (st.pos - 1) in
    let name = Interner.intern st.interner text in
    Result.Ok (LitTemplate name, span st start))
  else Result.Error st

let rec scan_rune_content_impl st char_count =
  if st.pos >= st.len then (false, '\000')
  else
    match st.source.[st.pos] with
    | '\'' -> (true, '\000')
    | '\\' when st.pos + 1 < st.len ->
      let esc = lookup_escape st.source.[st.pos + 1] in
      adv_n st 2;
      let finished, _ = scan_rune_content_impl st (char_count + 1) in
      (finished, esc)
    | c when char_count = 0 ->
      adv st;
      let finished, _ = scan_rune_content_impl st (char_count + 1) in
      (finished, c)
    | _ ->
      adv st;
      scan_rune_content_impl st (char_count + 1)

let scan_rune st =
  if st.pos < st.len && st.source.[st.pos] = '\'' then (
    let start = st.pos in
    adv st;
    let finished, char_val = scan_rune_content_impl st 0 in
    if finished then adv st else add_err st Error.E0201 start st.pos [ "rune" ];
    Result.Ok (LitRune char_val, span st start))
  else Result.Error st

let scan_literal st =
  bind_error
    scan_number
    (bind_error scan_string (bind_error scan_template scan_rune))
    st

let rec try_symbol_impl st start symbols =
  match symbols with
  | [] -> Result.Error st
  | (sym, token) :: rest ->
    let len = String.length sym in
    if st.pos + len <= st.len && substr st st.pos (st.pos + len) = sym then (
      adv_n st len;
      Result.Ok (token, span st start))
    else try_symbol_impl st start rest

let scan_symbol st = try_symbol_impl st st.pos symbols

let scan_invalid_char st c =
  let start = st.pos in
  add_err
    st
    Error.E0302
    st.pos
    (st.pos + 1)
    [ Printf.sprintf "%02X" (Char.code c) ];
  adv st;
  Result.Ok (Whitespace, span st start)

let scan_utf8_char st =
  let start = st.pos in
  add_err st Error.E0001 st.pos (st.pos + 1) [];
  adv st;
  Result.Ok (Token.Error, span st start)

let scan_error_chars st =
  if st.pos < st.len then
    let c = st.source.[st.pos] in
    if Char.code c < 32 && c <> '\t' && c <> '\n' && c <> '\r' then
      scan_invalid_char st c
    else if Char.code c >= 0x80 then scan_utf8_char st
    else Result.Error st
  else Result.Error st

let lex_next_token st =
  let start = st.pos in
  if st.pos >= st.len then Result.Ok (EOF, span st start)
  else
    let scanner_result =
      scan_whitespace |> bind_error scan_newline |> bind_error scan_comment
      |> bind_error scan_literal |> bind_error scan_ident
      |> bind_error scan_symbol
      |> bind_error scan_error_chars
    in
    match scanner_result st with
    | Result.Ok result -> Result.Ok result
    | Result.Error st' ->
      if st'.pos <= start then (
        add_err st' Error.E0501 st'.pos (st'.pos + 1) [];
        adv st';
        Result.Ok (Token.Error, span st' start))
      else Result.Ok (Token.Error, span st' start)

let lex_token st =
  match lex_next_token st with
  | Result.Ok (token, sp) -> (token, sp)
  | Result.Error _ -> assert false (* if happens, f*cked *)

let tokenize source file_id interner =
  let st = mk_state source file_id interner in
  let rec loop acc =
    let token, sp = lex_token st in
    if token = EOF then (List.rev ((token, sp) :: acc), st.diags)
    else loop ((token, sp) :: acc)
  in
  loop []
