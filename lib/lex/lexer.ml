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
let is_odigit c = '0' <= c && c <= '7'
let is_bdigit c = '0' <= c && c <= '1'
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

type number_base = Decimal | Hexadecimal | Octal | Binary

let scan_digit_with_sep st digit_checker =
  let rec scan_digits acc =
    if st.pos < st.len then
      match st.source.[st.pos] with
      | '_' when st.pos + 1 < st.len && digit_checker st.source.[st.pos + 1] ->
        advance_n st 2;
        scan_digits (acc + 1)
      | c when digit_checker c ->
        advance st;
        scan_digits (acc + 1)
      | _ -> acc
    else acc
  in
  scan_digits 0

let scan_number st start =
  let number_base =
    if st.pos + 1 < st.len && st.source.[st.pos] = '0' then
      match st.source.[st.pos + 1] with
      | 'x' | 'X' ->
        advance_n st 2;
        Hexadecimal
      | 'o' | 'O' ->
        advance_n st 2;
        Octal
      | 'b' | 'B' ->
        advance_n st 2;
        Binary
      | _ -> Decimal
    else Decimal
  in

  let digit_checker =
    match number_base with
    | Decimal -> is_digit
    | Hexadecimal -> is_xdigit
    | Octal -> is_odigit
    | Binary -> is_bdigit
  in

  if st.pos >= st.len || not (digit_checker st.source.[st.pos]) then (
    add_err
      st
      Error.E0103
      start
      [
        (match number_base with
        | Decimal -> "decimal"
        | Hexadecimal -> "hexadecimal"
        | Octal -> "octal"
        | Binary -> "binary")
      ];
    LitNumber (substr st start (st.pos - start)))
  else
    let _ = scan_digit_with_sep st digit_checker in
    if number_base = Decimal && st.pos < st.len && st.source.[st.pos] = '.' then (
      advance st;
      let _ = scan_digit_with_sep st is_digit in
      ());

    LitNumber (substr st start (st.pos - start))

type escape_checker = char -> bool
type escape_parser = string -> int -> int * bool
type escape_post_processor = int -> string * bool

let scan_braced_escape st ~validator ~parser ~post_processor =
  let start_pos = st.pos in
  advance_n st 2;
  if st.pos >= st.len then (
    add_err st Error.E0210 start_pos [];
    ("", 0x0))
  else if st.source.[st.pos] != '{' then (
    add_err st Error.E0210 start_pos [];
    ("", 0x0))
  else (
    advance st;
    if st.pos >= st.len then (
      add_err st Error.E0205 start_pos [];
      ("", 0x0))
    else if st.source.[st.pos] = '}' then (
      add_err st Error.E0205 start_pos [];
      ("", 0x0))
    else
      let digits = ref "" in
      let valid = ref true in
      while st.pos < st.len && st.source.[st.pos] != '}' && !valid do
        let c = st.source.[st.pos] in
        if validator c then (
          digits := !digits ^ String.make 1 c;
          advance st)
        else valid := false
      done;

      if !valid then
        if st.pos >= st.len || st.source.[st.pos] != '}' then (
          add_err st Error.E0209 start_pos [];
          ("", 0x0))
        else if !digits = "" then (
          add_err st Error.E0205 start_pos [];
          ("", 0x0))
        else (
          advance st;
          let code_point, valid_parse = parser !digits 0 in
          if not valid_parse then (
            add_err st Error.E0208 start_pos [];
            ("", 0x0))
          else
            let result_char, valid_result = post_processor code_point in
            if not valid_result then (
              add_err st Error.E0207 start_pos [ "10FFFF" ];
              ("", 0x0))
            else (result_char, code_point))
      else (
        add_err st Error.E0208 start_pos [];
        ("", 0x0)))

let rec unicode_parser str acc =
  if str = "" then (acc, true)
  else
    let digit =
      match str.[0] with
      | c when '0' <= c && c <= '9' -> Char.code c - Char.code '0'
      | c when 'a' <= c && c <= 'f' -> Char.code c - Char.code 'a' + 10
      | c when 'A' <= c && c <= 'F' -> Char.code c - Char.code 'A' + 10
      | _ -> 0
    in
    unicode_parser
      (String.sub str 1 (String.length str - 1))
      ((acc * 16) + digit)

let unicode_post_processor code_point =
  if code_point > 0x10FFFF then ("", false)
  else if code_point < 256 then (String.make 1 (Char.chr code_point), true)
  else (String.make 1 '?', true)

let scan_unicode_escape st =
  scan_braced_escape
    st
    ~validator:is_xdigit
    ~parser:unicode_parser
    ~post_processor:unicode_post_processor

let hex_parser str acc = unicode_parser str acc

let hex_post_processor code_point =
  if code_point > 0xFF then ("", false)
  else (String.make 1 (Char.chr code_point), true)

let scan_hex_escape st =
  scan_braced_escape
    st
    ~validator:is_xdigit
    ~parser:hex_parser
    ~post_processor:hex_post_processor

let rec scan_string_content st =
  if st.pos >= st.len then ""
  else
    match st.source.[st.pos] with
    | '"' -> ""
    | '\\' when st.pos + 1 < st.len -> begin
      match st.source.[st.pos + 1] with
      | 'u' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
        let char_str, _code_point = scan_unicode_escape st in
        char_str ^ scan_string_content st
      | 'x' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
        let char_str, _code_point = scan_hex_escape st in
        char_str ^ scan_string_content st
      | c ->
        let esc = lookup_escape c in
        advance_n st 2;
        String.make 1 esc ^ scan_string_content st
    end
    | c ->
      advance st;
      String.make 1 c ^ scan_string_content st

let scan_string st start =
  advance st;
  let content = scan_string_content st in
  if st.pos < st.len && st.source.[st.pos] = '"' then (
    advance st;
    LitString (Interner.intern st.interner content))
  else (
    add_err st Error.E0201 start [ "string" ];
    LitString (Interner.intern st.interner content))

let scan_rune st start =
  advance st;
  if st.pos >= st.len then (
    add_err st Error.E0201 start [ "rune" ];
    LitRune '\000')
  else
    match st.source.[st.pos] with
    | '\'' ->
      advance st;
      LitRune '\000'
    | '\\' when st.pos + 1 < st.len -> begin
      match st.source.[st.pos + 1] with
      | 'u' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
        let char_str, _code_point = scan_unicode_escape st in
        if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
        if char_str = "" then LitRune '\000' else LitRune char_str.[0]
      | 'x' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
        let char_str, _code_point = scan_hex_escape st in
        if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
        if char_str = "" then LitRune '\000' else LitRune char_str.[0]
      | c ->
        let esc = lookup_escape c in
        advance_n st 2;
        if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
        LitRune esc
    end
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
      let rev_tokens = (EOF, token_span) :: List.rev acc in
      (rev_tokens, st.diags)
    else
      match token with
      | Newline -> collect acc
      | _ -> collect ((token, token_span) :: acc)
  in
  collect []
