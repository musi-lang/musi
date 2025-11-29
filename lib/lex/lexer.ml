open Basic

type state = {
    source : string
  ; pos : int
  ; len : int
  ; file_id : Span.file_id
  ; interner : Interner.t
  ; diags : Diagnostic.bag
}

let make_state source file_id interner =
  {
    source
  ; pos = 0
  ; len = String.length source
  ; file_id
  ; interner
  ; diags = Diagnostic.empty_bag
  }

let peek_char state =
  if state.pos >= state.len then None else Some state.source.[state.pos]

let advance state = { state with pos = state.pos + 1 }
let make_span state start = Span.make state.file_id start state.pos
let get_byte state pos = Char.code state.source.[pos]

let error state msg start end_ =
  let span = Span.make state.file_id start end_ in
  { state with diags = Diagnostic.add state.diags (Diagnostic.error msg span) }

let char_error st p msg = error st msg p (p + 1)
let range_error st s e msg = error st msg s e
let extract_content st s e = String.sub st.source s (e - s)

type char_class = Alpha | Digit | Space | Newline | Underscore | Other

let classify = function
  | 'a' .. 'z' | 'A' .. 'Z' -> Alpha
  | '0' .. '9' -> Digit
  | ' ' | '\t' | '\r' -> Space
  | '\n' -> Newline
  | '_' -> Underscore
  | _ -> Other

let is_alpha c = classify c = Alpha
let is_digit c = classify c = Digit
let is_whitespace c = classify c = Space

let is_ident_start c =
  match classify c with Alpha | Underscore -> true | _ -> false

let is_ident_cont c =
  match classify c with Alpha | Digit | Underscore -> true | _ -> false

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

let is_utf8_continuation b = b land 0xC0 = 0x80

let validate_utf8_char st pos =
  if pos >= st.len then (pos, None)
  else
    let first = get_byte st pos in
    if first < 0x80 then (pos + 1, None)
    else if first < 0xC0 then
      (pos + 1, Some ("invalid UTF-8 continuation byte", pos, pos + 1))
    else
      let exp =
        if first < 0xE0 then 2
        else if first < 0xF0 then 3
        else if first < 0xF8 then 4
        else 0
      in
      if exp = 0 then (pos + 1, Some ("invalid UTF-8 start byte", pos, pos + 1))
      else if pos + exp > st.len then
        (st.len, Some ("incomplete UTF-8 sequence", pos, st.len))
      else
        let rec chk p =
          if p >= pos + exp then None
          else if not (is_utf8_continuation (get_byte st p)) then Some p
          else chk (p + 1)
        in
        match chk (pos + 1) with
        | Some inv ->
          (pos + exp, Some ("invalid UTF-8 continuation byte", inv, inv + 1))
        | None -> (pos + exp, None)

let validate_utf8_in_range st sp ep =
  let rec go p s =
    if p >= ep then s
    else if get_byte st p >= 0x80 then
      match validate_utf8_char s p with
      | _, Some (msg, a, b) -> go b (error s msg a b)
      | nxt, None -> go nxt s
    else go (p + 1) s
  in
  go sp st

let find_end_brace content len start =
  let rec go p =
    if p >= len then None
    else if content.[p] = '}' then Some p
    else if is_xdigit content.[p] then go (p + 1)
    else None
  in
  go start

let process_escape_seqs content =
  let len = String.length content in
  let buf = Buffer.create len in
  let rec go pos =
    if pos >= len then ()
    else
      match content.[pos] with
      | '\\' when pos + 1 < len -> (
        match content.[pos + 1] with
        | ('u' | 'U') when pos + 3 < len && content.[pos + 2] = '{' -> (
          match find_end_brace content len (pos + 3) with
          | Some eb -> (
            try
              let v =
                int_of_string
                  ("0x" ^ String.sub content (pos + 3) (eb - pos - 3))
              in
              Buffer.add_char buf (Char.chr (v mod 256));
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
    if p >= st.len then p else if pred st.source.[p] then go (p + 1) else p
  in
  go start

let scan_number_chars st start valid base =
  let rec go p s =
    if p >= st.len then (p, s)
    else
      let c = st.source.[p] in
      if valid c then go (p + 1) s
      else if is_alpha c || is_digit c then
        (p, char_error s p (Printf.sprintf "invalid %s digit '%c'" base c))
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
          ( pos
          , range_error s pos pos "multiple decimal points in number"
          , dots + 1 )
        else go (pos + 1) s (dots + 1)
      else (pos, s, dots)
  in
  let ep, fs, _ = go st.pos st 0 in
  ({ fs with pos = ep }, extract_content st start ep, make_span st start)

let scan_based_number st start prefix valid base_name =
  let pos = st.pos + prefix in
  let incomplete_err s =
    {
      (range_error s start pos ("incomplete " ^ base_name ^ " number")) with
      pos
    }
  in
  if pos >= st.len then
    (incomplete_err st, extract_content st start pos, make_span st start)
  else
    let ep, fs = scan_number_chars st pos valid base_name in
    if ep = pos then
      (incomplete_err fs, extract_content st start ep, make_span st start)
    else ({ fs with pos = ep }, extract_content st start ep, make_span st start)

let scan_number st =
  let start = st.pos in
  if st.pos + 1 < st.len && st.source.[st.pos] = '0' then
    match st.source.[st.pos + 1] with
    | 'x' | 'X' -> scan_based_number st start 2 is_xdigit "hex"
    | 'b' | 'B' -> scan_based_number st start 2 is_bdigit "binary"
    | 'o' | 'O' -> scan_based_number st start 2 is_odigit "octal"
    | '0' .. '9' ->
      scan_decimal
        (range_error
           st
           start
           st.pos
           "leading zeros in decimal numbers not allowed")
        start
    | _ -> scan_decimal st start
  else scan_decimal st start

let scan_quoted st quote tpl =
  let start = st.pos + 1 in
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
  go start 0 []

let validate_escape st pos =
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
      match find_end_brace st.source st.len (pos + 3) with
      | Some eb -> (
        let hex = String.sub st.source (pos + 3) (eb - pos - 3) in
        if String.length hex = 0 then
          Some (error st "empty unicode escape sequence" pos (eb + 1), eb + 1)
        else
          try
            let v = int_of_string ("0x" ^ hex) in
            let max_v = if big then 0x10FFFF else 0xFFFF in
            if v > max_v then
              Some
                ( error
                    st
                    (Printf.sprintf
                       "unicode code point exceeds maximum 0x%X"
                       max_v)
                    pos
                    (eb + 1)
                , eb + 1 )
            else None
          with _ ->
            Some
              ( error
                  st
                  "invalid hex digits in unicode escape sequence"
                  pos
                  (eb + 1)
              , eb + 1 ))
      | None ->
        if pos + 3 < st.len then
          Some
            ( error
                st
                "unclosed unicode escape sequence (missing '}')"
                pos
                (pos + 4)
            , pos + 3 )
        else
          Some (error st "incomplete unicode escape sequence" pos st.len, st.len)
    else
      Some
        ( error
            st
            "expected '{' after unicode escape prefix"
            pos
            (min (pos + 3) st.len)
        , pos + 2 )
  else None

let validate_escapes st start ep =
  let rec go pos s =
    if pos >= ep - 1 then s
    else if st.source.[pos] = '\\' then
      if pos + 1 >= ep - 1 then
        error s "unterminated escape sequence" pos (pos + 1)
      else if not (validate_escape st pos) then
        go
          (pos + 2)
          (error
             s
             (Printf.sprintf
                "invalid escape sequence '\\%c'"
                st.source.[pos + 1])
             pos
             (pos + 2))
      else if st.source.[pos + 1] = 'u' || st.source.[pos + 1] = 'U' then
        match parse_unicode_escape st pos with
        | Some (es, nxt) -> go nxt es
        | None -> go (pos + 2) s
      else go (pos + 2) s
    else go (pos + 1) s
  in
  go start st

let scan_whitespace st =
  let ep = scan_while st st.pos is_whitespace in
  ({ st with pos = ep }, (), make_span st st.pos)

let scan_newline st = (advance st, (), make_span st st.pos)

let scan_line_comment st =
  let rec go p =
    if p >= st.len || st.source.[p] = '\n' then p else go (p + 1)
  in
  let start = st.pos + 2 in
  let ep = go start in
  ({ st with pos = ep }, extract_content st start ep, make_span st st.pos)

let scan_block_comment st =
  let start = st.pos in
  let rec go p s =
    if p + 1 >= st.len then
      (p, range_error s start st.pos "unterminated block comment")
    else if st.source.[p] = '*' && st.source.[p + 1] = '/' then (p + 2, s)
    else if st.source.[p] = '/' && st.source.[p + 1] = '*' then
      go (p + 2) (range_error s p (p + 2) "nested block comments not allowed")
    else go (p + 1) s
  in
  let sc = st.pos + 2 in
  let ep, fs = go sc st in
  let content = if ep > sc + 2 then extract_content st sc (ep - 2) else "" in
  ({ fs with pos = ep }, content, make_span st start)

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
          (char_error
             s
             p
             ("non-ASCII character '" ^ String.make 1 c ^ "' in identifier"))
      else (p, s)
  in
  let ep, fs = go st.pos st in
  ( { fs with pos = ep }
  , Basic.Interner.intern st.interner (extract_content st start ep)
  , make_span st start )

let scan_string st =
  let start = st.pos in
  let ep, unterm, _ = scan_quoted st '"' false in
  let span = make_span st start in
  if unterm || ep > st.len || (ep = st.len && st.source.[ep - 1] <> '"') then
    ( error st "unterminated string literal" start st.pos
    , Basic.Interner.empty_name st.interner
    , span )
  else
    let content = extract_content st (st.pos + 1) (ep - 1) in
    let fs = validate_escapes st (st.pos + 1) ep in
    ( { fs with pos = ep }
    , Basic.Interner.intern st.interner (process_escape_seqs content)
    , span )

let scan_template st =
  let start = st.pos in
  let ep, unterm, extra = scan_quoted st '"' true in
  let span = make_span st start in
  let se =
    List.fold_left
      (fun s p -> error s "extra closing brace in template literal" p (p + 1))
      st
      extra
  in
  if unterm then
    ( error se "unterminated template literal" start st.pos
    , Basic.Interner.empty_name se.interner
    , span )
  else
    let content = extract_content st start (ep - 1) in
    ( { se with pos = ep }
    , Basic.Interner.intern se.interner (process_escape_seqs content)
    , span )

let scan_rune st =
  let start = st.pos in
  let ep, unterm, _ = scan_quoted st '\'' false in
  let span = make_span st start in
  if unterm || ep > st.len || (ep = st.len && st.source.[ep - 1] <> '\'') then
    (error st "unterminated rune literal" start st.pos, '\000', span)
  else
    let content = extract_content st (st.pos + 1) (ep - 1) in
    let fs = validate_escapes st (st.pos + 1) ep in
    let proc =
      if String.length content > 1 then process_escape_seqs content else content
    in
    if String.length proc = 0 then
      (error fs "empty rune literal" start ep, '\000', span)
    else if String.length proc > 1 then
      ( error fs "rune literal contains multiple characters" start ep
      , proc.[0]
      , span )
    else ({ fs with pos = ep }, proc.[0], span)

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
  | Some (tok, len) -> ({ st with pos = st.pos + len }, tok, make_span st st.pos)
  | None ->
    let span = make_span st st.pos in
    ( char_error
        (advance st)
        st.pos
        (Printf.sprintf "unexpected character '%c'" st.source.[st.pos])
    , Token.Error
    , span )

let scan_comment_or_symbol st =
  if st.pos + 1 < st.len then
    match st.source.[st.pos + 1] with
    | '/' ->
      let s, c, sp = scan_line_comment st in
      (s, Token.Comment c, sp)
    | '*' ->
      let s, c, sp = scan_block_comment st in
      (s, Token.Comment c, sp)
    | _ -> scan_symbol st
  else scan_symbol st

let scan_template_or_dollar st =
  if st.pos + 1 < st.len && st.source.[st.pos + 1] = '"' then
    let ns = { st with pos = st.pos + 2 } in
    let fs, content, _ = scan_template ns in
    (fs, Token.LitTemplate content, make_span st st.pos)
  else scan_symbol st

let wrap_scanner scanner wrapper st =
  let s, result, span = scanner st in
  (s, wrapper result, span)

let dispatch_char c =
  match c with
  | _ when is_whitespace c ->
    wrap_scanner scan_whitespace (fun () -> Token.Whitespace)
  | '\n' -> wrap_scanner scan_newline (fun () -> Token.Newline)
  | '/' -> scan_comment_or_symbol
  | _ when is_ident_start c ->
    fun st ->
      let s, name, span = scan_ident st in
      ( s
      , Token.lookup_keyword
          st.interner
          (Basic.Interner.lookup st.interner name)
      , span )
  | _ when is_digit c ->
    wrap_scanner scan_number (fun text -> Token.LitNumber text)
  | '"' -> wrap_scanner scan_string (fun content -> Token.LitString content)
  | '\'' -> wrap_scanner scan_rune (fun char -> Token.LitRune char)
  | '$' -> scan_template_or_dollar
  | _ -> scan_symbol

let rec lex_token st =
  match peek_char st with
  | None -> (st, Token.EOF, make_span st st.pos)
  | Some c ->
    let code = Char.code c in
    if code < 32 && c <> '\t' && c <> '\n' && c <> '\r' then
      let span = make_span st st.pos in
      ( char_error
          (advance st)
          st.pos
          (Printf.sprintf "control character '\\x%02X' in source" code)
      , Token.Whitespace
      , span )
    else if c = '\000' then
      let span = make_span st st.pos in
      ( char_error (advance st) st.pos "null byte in source"
      , Token.Whitespace
      , span )
    else if code >= 0x80 then
      match validate_utf8_char st st.pos with
      | _, Some (msg, s, e) ->
        let span = Span.make st.file_id s e in
        ({ (error st msg s e) with pos = max (st.pos + 1) e }, Token.Error, span)
      | nxt, None -> (dispatch_char c) { st with pos = nxt }
    else (dispatch_char c) st

and tokenize source file_id interner =
  let rec go st tokens =
    let old = st.pos in
    let ns, tok, span = lex_token st in
    match tok with
    | Token.EOF -> (List.rev ((tok, span) :: tokens), ns.diags)
    | _ ->
      if ns.pos <= old then
        let es = error ns "lexer failed to advance" old ns.pos in
        go (advance es) ((Token.Error, span) :: tokens)
      else go ns ((tok, span) :: tokens)
  in
  go (make_state source file_id interner) []
