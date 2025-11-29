open Basic

type state = {
    source : string
  ; pos : int
  ; len : int
  ; file_id : Span.file_id
  ; interner : Interner.t
  ; diags : Diagnostic.bag
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

let peek_char_opt st =
  if st.pos >= st.len then None else Some st.source.[st.pos]

let advance st = { st with pos = st.pos + 1 }
let mk_span st start = Span.make st.file_id start st.pos

let add_error st msg start end_ =
  let sp = Span.make st.file_id start end_ in
  { st with diags = Diagnostic.add st.diags (Diagnostic.error msg sp) }

let add_error_code st code start end_ args =
  let sp = Span.make st.file_id start end_ in
  { st with diags = Diagnostic.add st.diags (Lex_errors.diag code sp args) }

let extract st s e = String.sub st.source s (e - s)

let classify = function
  | 'a' .. 'z' | 'A' .. 'Z' -> `Alpha
  | '0' .. '9' -> `Digit
  | ' ' | '\t' | '\r' -> `Space
  | '\n' -> `Newline
  | '_' -> `Underscore
  | _ -> `Other

let is_alpha c = classify c = `Alpha
let is_digit c = classify c = `Digit
let is_whitespace c = classify c = `Space

let is_ident_start c =
  match classify c with `Alpha | `Underscore -> true | _ -> false

let is_ident_cont c =
  match classify c with `Alpha | `Digit | `Underscore -> true | _ -> false

let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_bdigit c = c = '0' || c = '1'
let is_odigit c = c >= '0' && c <= '7'

let escape_chars =
  [
    ('n', '\n')
  ; ('t', '\t')
  ; ('r', '\r')
  ; ('\\', '\\')
  ; ('"', '"')
  ; ('\'', '\'')
  ; ('0', '\000')
  ; ('a', '\007')
  ; ('b', '\008')
  ; ('f', '\012')
  ; ('v', '\011')
  ; ('e', '\027')
  ; ('/', '/')
  ]

let is_utf8_segment_cont b = b land 0xC0 = 0x80

let check_utf8_char st pos =
  if pos >= st.len then (pos, None)
  else
    let first = Char.code st.source.[pos] in
    if first < 0x80 then (pos + 1, None)
    else if first < 0xC0 then (pos + 1, Some (Lex_errors.E0001, pos, pos + 1))
    else
      let exp =
        if first < 0xE0 then 2
        else if first < 0xF0 then 3
        else if first < 0xF8 then 4
        else 0
      in
      if exp = 0 then (pos + 1, Some (Lex_errors.E0002, pos, pos + 1))
      else if pos + exp > st.len then
        (st.len, Some (Lex_errors.E0003, pos, st.len))
      else
        let rec chk idx =
          if idx >= pos + exp then None
          else if not (is_utf8_segment_cont (Char.code st.source.[idx])) then
            Some idx
          else chk (idx + 1)
        in
        match chk (pos + 1) with
        | Some inv -> (pos + exp, Some (Lex_errors.E0001, inv, inv + 1))
        | None -> (pos + exp, None)

let check_utf8_in_range st sp ep =
  let rec go p s =
    if p >= ep then s
    else if Char.code st.source.[p] >= 0x80 then
      match check_utf8_char s p with
      | _, Some (code, a, b) -> go b (add_error_code s code a b [])
      | nxt, None -> go nxt s
    else go (p + 1) s
  in
  go sp st

let find_rbrace content len start =
  let rec go p =
    if p >= len then None
    else if content.[p] = '}' then Some p
    else if is_xdigit content.[p] then go (p + 1)
    else None
  in
  go start

let process_escape_chars content =
  let len = String.length content in
  let buf = Buffer.create len in
  let rec go pos =
    if pos >= len then ()
    else
      match content.[pos] with
      | '\\' when pos + 1 < len -> (
        match content.[pos + 1] with
        | ('u' | 'U') when pos + 3 < len && content.[pos + 2] = '{' -> (
          match find_rbrace content len (pos + 3) with
          | Some eb -> (
            try
              Buffer.add_char
                buf
                (Char.chr
                   (int_of_string
                      ("0x" ^ String.sub content (pos + 3) (eb - pos - 3))
                   mod 256));
              go (eb + 1)
            with _ ->
              Buffer.add_string buf (String.sub content pos 2);
              go (pos + 2))
          | None ->
            Buffer.add_string buf (String.sub content pos 2);
            go (pos + 2))
        | c -> (
          match List.assoc_opt c escape_chars with
          | Some esc ->
            Buffer.add_char buf esc;
            go (pos + 2)
          | None ->
            Buffer.add_string buf (String.sub content pos 2);
            go (pos + 2)))
      | c ->
        Buffer.add_char buf c;
        go (pos + 1)
  in
  go 0;
  Buffer.contents buf

let scan_while st start pred =
  let rec go p =
    if p >= st.len || not (pred st.source.[p]) then p else go (p + 1)
  in
  go start

let scan_number_chars st start valid base =
  let rec go p s =
    if p >= st.len then (p, s)
    else
      let c = st.source.[p] in
      if valid c then go (p + 1) s
      else if is_alpha c || is_digit c then
        ( p
        , add_error_code s Lex_errors.E0101 p (p + 1) [ base; String.make 1 c ]
        )
      else (p, s)
  in
  go start st

let scan_decimal st start =
  let rec go pos s dots =
    if pos >= st.len then (pos, s, dots)
    else
      let c = st.source.[pos] in
      if is_digit c then go (pos + 1) s dots
      else if c = '.' then
        if dots > 0 then
          (pos, add_error_code s Lex_errors.E0102 pos pos [], dots + 1)
        else go (pos + 1) s (dots + 1)
      else (pos, s, dots)
  in
  let ep, fs, _ = go st.pos st 0 in
  ({ fs with pos = ep }, extract st start ep, mk_span st start)

let scan_based_number st start pfx valid base =
  let pos = st.pos + pfx in
  let mkErr s =
    { (add_error_code s Lex_errors.E0103 start pos [ base ]) with pos }
  in
  if pos >= st.len then (mkErr st, extract st start pos, mk_span st start)
  else
    let ep, fs = scan_number_chars st pos valid base in
    if ep = pos then (mkErr fs, extract st start ep, mk_span st start)
    else ({ fs with pos = ep }, extract st start ep, mk_span st start)

let scan_number st =
  let start = st.pos in
  if st.pos + 1 < st.len && st.source.[st.pos] = '0' then
    match st.source.[st.pos + 1] with
    | 'x' | 'X' -> scan_based_number st start 2 is_xdigit "hex"
    | 'b' | 'B' -> scan_based_number st start 2 is_bdigit "binary"
    | 'o' | 'O' -> scan_based_number st start 2 is_odigit "octal"
    | '0' .. '9' ->
      scan_decimal
        (add_error
           st
           "leading zeros in decimal numbers not allowed"
           start
           st.pos)
        start
    | _ -> scan_decimal st start
  else scan_decimal st start

let scan_quoted st quote tpl =
  let rec go p depth extra =
    if p >= st.len then (p, true, extra)
    else
      match st.source.[p] with
      | c when c = quote && depth = 0 -> (p + 1, false, extra)
      | '\\' when p + 1 < st.len -> go (p + 2) depth extra
      | '{' when tpl -> go (p + 1) (depth + 1) extra
      | '}' when tpl && depth > 0 -> go (p + 1) (depth - 1) extra
      | '}' when tpl -> go (p + 1) depth (p :: extra)
      | _ -> go (p + 1) depth extra
  in
  go (st.pos + 1) 0 []

let is_valid_escape st pos =
  pos >= 0
  && pos + 1 < st.len
  && (List.mem_assoc st.source.[pos + 1] escape_chars
     || st.source.[pos + 1] = 'u'
     || st.source.[pos + 1] = 'U')

let parse_unicode_escape st pos =
  if pos + 1 < st.len && (st.source.[pos + 1] = 'u' || st.source.[pos + 1] = 'U')
  then
    let big = st.source.[pos + 1] = 'U' in
    if pos + 3 < st.len && st.source.[pos + 2] = '{' then
      match find_rbrace st.source st.len (pos + 3) with
      | Some eb -> (
        let hex = String.sub st.source (pos + 3) (eb - pos - 3) in
        if String.length hex = 0 then
          Some (add_error_code st Lex_errors.E0205 pos (eb + 1) [], eb + 1)
        else
          try
            let v = int_of_string ("0x" ^ hex) in
            let max_v = if big then 0x10FFFF else 0xFFFF in
            if v > max_v then
              Some
                ( add_error_code
                    st
                    Lex_errors.E0207
                    pos
                    (eb + 1)
                    [ Printf.sprintf "%X" max_v ]
                , eb + 1 )
            else None
          with _ ->
            Some (add_error_code st Lex_errors.E0208 pos (eb + 1) [], eb + 1))
      | None ->
        if pos + 3 < st.len then
          Some (add_error_code st Lex_errors.E0209 pos (pos + 4) [], pos + 3)
        else Some (add_error_code st Lex_errors.E0206 pos st.len [], st.len)
    else
      Some
        ( add_error_code st Lex_errors.E0210 pos (min (pos + 3) st.len) []
        , pos + 2 )
  else None

let validate_escapes st start ep =
  let rec go pos s =
    if pos >= ep - 1 then s
    else if st.source.[pos] = '\\' then
      if pos + 1 >= ep - 1 then
        add_error_code s Lex_errors.E0211 pos (pos + 1) []
      else if not (is_valid_escape st pos) then
        go
          (pos + 2)
          (add_error_code
             s
             Lex_errors.E0212
             pos
             (pos + 2)
             [ String.make 1 st.source.[pos + 1] ])
      else if st.source.[pos + 1] = 'u' || st.source.[pos + 1] = 'U' then
        match parse_unicode_escape st pos with
        | Some (es, nxt) -> go nxt es
        | None -> go (pos + 2) s
      else go (pos + 2) s
    else go (pos + 1) s
  in
  go start st

let scan_whitespace st =
  ({ st with pos = scan_while st st.pos is_whitespace }, (), mk_span st st.pos)

let scan_newline st = (advance st, (), mk_span st st.pos)

let scan_line_comment st =
  let rec go p =
    if p >= st.len || st.source.[p] = '\n' then p else go (p + 1)
  in
  let start = st.pos + 2 in
  let ep = go start in
  ({ st with pos = ep }, extract st start ep, mk_span st st.pos)

let scan_block_comment st =
  let start = st.pos in
  let rec go p s =
    if p + 1 >= st.len then
      (p, add_error_code s Lex_errors.E0401 start st.pos [])
    else if st.source.[p] = '*' && st.source.[p + 1] = '/' then (p + 2, s)
    else if st.source.[p] = '/' && st.source.[p + 1] = '*' then
      go (p + 2) (add_error_code s Lex_errors.E0402 p (p + 2) [])
    else go (p + 1) s
  in
  let sc = st.pos + 2 in
  let ep, fs = go sc st in
  ({ fs with pos = ep }, if ep > sc + 2 then extract st sc (ep - 2) else "")

let scan_ident st =
  let start = st.pos in
  let rec go p s =
    if p >= st.len then (p, s)
    else
      let c = st.source.[p] in
      if is_ident_cont c then go (p + 1) s
      else if Char.code c > 127 then
        go
          (p + 1)
          (add_error_code s Lex_errors.E0304 p (p + 1) [ String.make 1 c ])
      else (p, s)
  in
  let ep, fs = go st.pos st in
  ( { fs with pos = ep }
  , Basic.Interner.intern st.interner (extract st start ep)
  , mk_span st start )

let scan_literal st quote lit_name process =
  let start = st.pos in
  let ep, unterm, extra = scan_quoted st quote (lit_name = "template") in
  let sp = mk_span st start in
  let se =
    if lit_name = "template" then
      List.fold_left
        (fun s p -> add_error_code s Lex_errors.E0204 p (p + 1) [])
        st
        extra
    else st
  in
  if unterm || ep > st.len || (ep = st.len && st.source.[ep - 1] <> quote) then
    (add_error_code se Lex_errors.E0201 start st.pos [ lit_name ], None, sp)
  else
    let content =
      extract st (if lit_name = "template" then start else st.pos + 1) (ep - 1)
    in
    let fs = validate_escapes se (st.pos + 1) ep in
    ({ fs with pos = ep }, Some (process content), sp)

let scan_string st =
  match scan_literal st '"' "string" process_escape_chars with
  | s, None, sp -> (s, Basic.Interner.empty_name st.interner, sp)
  | s, Some c, sp -> (s, Basic.Interner.intern st.interner c, sp)

let scan_template st =
  match scan_literal st '"' "template" process_escape_chars with
  | s, None, sp -> (s, Basic.Interner.empty_name st.interner, sp)
  | s, Some c, sp -> (s, Basic.Interner.intern st.interner c, sp)

let scan_rune st =
  match
    scan_literal st '\'' "rune" (fun c ->
      if String.length c > 1 then process_escape_chars c else c)
  with
  | s, None, sp -> (s, '\000', sp)
  | s, Some proc, sp ->
    if String.length proc = 0 then
      (add_error_code s Lex_errors.E0203 st.pos (st.pos + 1) [], '\000', sp)
    else if String.length proc > 1 then
      ( add_error
          s
          "rune literal contains multiple characters"
          st.pos
          (st.pos + 1)
      , proc.[0]
      , sp )
    else (s, proc.[0], sp)

let scan_symbol st =
  let rec go = function
    | [] -> None
    | (sym, tok) :: rest ->
      let len = String.length sym in
      if st.pos + len <= st.len && String.sub st.source st.pos len = sym then
        Some (tok, len)
      else go rest
  in
  match go Token.symbol_strings with
  | Some (tok, len) -> ({ st with pos = st.pos + len }, tok, mk_span st st.pos)
  | None ->
    ( add_error_code
        (advance st)
        Lex_errors.E0301
        st.pos
        (st.pos + 1)
        [ String.make 1 st.source.[st.pos] ]
    , Token.Error
    , mk_span st st.pos )

let scan_comment_or_symbol st =
  if st.pos + 1 < st.len then
    match st.source.[st.pos + 1] with
    | '/' ->
      let s, c, sp = scan_line_comment st in
      (s, Token.Comment c, sp)
    | '*' ->
      let s, c = scan_block_comment st in
      (s, Token.Comment c, mk_span st st.pos)
    | _ -> scan_symbol st
  else scan_symbol st

let scan_template_or_dollar st =
  if st.pos + 1 < st.len && st.source.[st.pos + 1] = '"' then
    let fs, content, _ = scan_template { st with pos = st.pos + 2 } in
    (fs, Token.LitTemplate content, mk_span st st.pos)
  else scan_symbol st

let wrap scanner wrapper st =
  let s, r, sp = scanner st in
  (s, wrapper r, sp)

let dispatch_char c =
  match c with
  | _ when is_whitespace c -> wrap scan_whitespace (fun () -> Token.Whitespace)
  | '\n' -> wrap scan_newline (fun () -> Token.Newline)
  | '/' -> scan_comment_or_symbol
  | _ when is_ident_start c ->
    fun st ->
      let s, name, sp = scan_ident st in
      ( s
      , Token.lookup_keyword
          st.interner
          (Basic.Interner.lookup st.interner name)
      , sp )
  | _ when is_digit c -> wrap scan_number (fun text -> Token.LitNumber text)
  | '"' -> wrap scan_string (fun content -> Token.LitString content)
  | '\'' -> wrap scan_rune (fun char -> Token.LitRune char)
  | '$' -> scan_template_or_dollar
  | _ -> scan_symbol

let rec lex_token st =
  match peek_char_opt st with
  | None -> (st, Token.EOF, mk_span st st.pos)
  | Some c ->
    let code = Char.code c in
    if code < 32 && c <> '\t' && c <> '\n' && c <> '\r' then
      ( add_error_code
          (advance st)
          Lex_errors.E0302
          st.pos
          (st.pos + 1)
          [ Printf.sprintf "%02X" code ]
      , Token.Whitespace
      , mk_span st st.pos )
    else if c = '\000' then
      ( add_error_code (advance st) Lex_errors.E0303 st.pos (st.pos + 1) []
      , Token.Whitespace
      , mk_span st st.pos )
    else if code >= 0x80 then
      match check_utf8_char st st.pos with
      | _, Some (msg, s, e) ->
        ( { (add_error_code st msg s e []) with pos = max (st.pos + 1) e }
        , Token.Error
        , Span.make st.file_id s e )
      | nxt, None -> (dispatch_char c) { st with pos = nxt }
    else (dispatch_char c) st

and tokenize source file_id interner =
  let rec go st tokens =
    let old = st.pos in
    let ns, tok, sp = lex_token st in
    match tok with
    | Token.EOF -> (List.rev ((tok, sp) :: tokens), ns.diags)
    | _ ->
      if ns.pos <= old then
        go
          (advance (add_error_code ns Lex_errors.E0501 old ns.pos []))
          ((Token.Error, sp) :: tokens)
      else go ns ((tok, sp) :: tokens)
  in
  go (mk_state source file_id interner) []
