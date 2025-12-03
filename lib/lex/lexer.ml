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

let char_in_range c (lo, hi) = lo <= c && c <= hi
let is_letter c = char_in_range c ('a', 'z') || char_in_range c ('A', 'Z')
let is_digit c = char_in_range c ('0', '9')
let is_space c = c = ' ' || c = '\t' || c = '\r'
let is_ident_start c = is_letter c || c = '_'
let is_ident_cont c = is_letter c || is_digit c || c = '_'

let digit_in_base c base =
  if base <= 10 then char_in_range c ('0', Char.chr (Char.code '0' + base - 1))
  else if base <= 16 then
    is_digit c
    || char_in_range c ('a', Char.chr (Char.code 'a' + base - 11))
    || char_in_range c ('A', Char.chr (Char.code 'A' + base - 11))
  else false

let is_xdigit c = digit_in_base c 16
let is_odigit c = digit_in_base c 8
let is_bdigit c = digit_in_base c 2
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

type number_base = {
    name : string
  ; prefix : string option
  ; digit_checker : char -> bool
}

let dec_base = { name = "decimal"; prefix = None; digit_checker = is_digit }

let hex_base =
  { name = "hexadecimal"; prefix = Some "x"; digit_checker = is_xdigit }

let oct_base = { name = "octal"; prefix = Some "o"; digit_checker = is_odigit }
let bin_base = { name = "binary"; prefix = Some "b"; digit_checker = is_bdigit }

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

let classify_number_base st =
  if st.pos + 1 < st.len && st.source.[st.pos] = '0' then
    match st.source.[st.pos + 1] with
    | 'x' | 'X' -> (hex_base, 2)
    | 'o' | 'O' -> (oct_base, 2)
    | 'b' | 'B' -> (bin_base, 2)
    | _ -> (dec_base, 0)
  else (dec_base, 0)

let scan_number st start =
  let number_base, prefix_len = classify_number_base st in
  advance_n st prefix_len;

  if st.pos >= st.len || not (number_base.digit_checker st.source.[st.pos]) then (
    add_err st Error.E0103 start [ number_base.name ];
    LitNumber (substr st start (st.pos - start)))
  else
    let _ = scan_digit_with_sep st number_base.digit_checker in
    if prefix_len = 0 && st.pos < st.len && st.source.[st.pos] = '.' then (
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

let scan_escape st continuation_fn =
  if st.pos + 1 < st.len then (
    match st.source.[st.pos + 1] with
    | 'u' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
      let char_str, _code_point = scan_unicode_escape st in
      continuation_fn char_str
    | 'x' when st.pos + 2 < st.len && st.source.[st.pos + 2] = '{' ->
      let char_str, _code_point = scan_hex_escape st in
      continuation_fn char_str
    | c ->
      let esc = lookup_escape c in
      advance_n st 2;
      continuation_fn (String.make 1 esc))
  else continuation_fn ""

let rec scan_string_content st =
  if st.pos >= st.len then ""
  else
    match st.source.[st.pos] with
    | '"' -> ""
    | '\\' -> scan_escape st (fun char_str -> char_str ^ scan_string_content st)
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
    | '\\' ->
      scan_escape st (fun char_str ->
        if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
        if char_str = "" then LitRune '\000' else LitRune char_str.[0])
    | c ->
      advance st;
      if st.pos < st.len && st.source.[st.pos] = '\'' then advance st;
      LitRune c

let symbol_table =
  let entries =
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
  let table = Hashtbl.create (List.length entries) in
  List.iter (fun (sym, token) -> Hashtbl.add table sym token) entries;
  table

let try_symbol st =
  let max_len = 3 in
  let rec find_longest_match len =
    if len <= 0 then Token.Error
    else
      let sym_len = min len (st.len - st.pos) in
      if sym_len > 0 then
        let candidate = substr st st.pos sym_len in
        match Hashtbl.find_opt symbol_table candidate with
        | Some token ->
          advance_n st sym_len;
          token
        | None -> find_longest_match (len - 1)
      else find_longest_match (len - 1)
  in
  find_longest_match max_len

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
    | _ -> try_symbol st

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
