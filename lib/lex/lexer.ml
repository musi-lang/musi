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
  let len = String.length source in
  { source; pos = 0; len; file_id; interner; diags = Diagnostic.empty_bag }

let peek_char state =
  if state.pos >= state.len then None else Some state.source.[state.pos]

let advance state = { state with pos = state.pos + 1 }
let make_span state start = Span.make state.file_id start state.pos
let get_byte state pos = Char.code state.source.[pos]

let error state msg start end_ =
  let span = Span.make state.file_id start end_ in
  { state with diags = Diagnostic.add state.diags (Diagnostic.error msg span) }

let char_error state pos msg = error state msg pos (pos + 1)
let range_error state start end_ msg = error state msg start end_

let extract_content state start_offset end_offset =
  String.sub state.source start_offset (end_offset - start_offset)

type char_class = Alpha | Digit | Space | Newline | Underscore | Other

let classify c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' -> Alpha
  | '0' .. '9' -> Digit
  | ' ' | '\t' | '\r' -> Space
  | '\n' -> Newline
  | '_' -> Underscore
  | _ -> Other

let is_alpha c = match classify c with Alpha -> true | _ -> false
let is_digit c = match classify c with Digit -> true | _ -> false
let is_whitespace c = match classify c with Space -> true | _ -> false
let is_newline c = match classify c with Newline -> true | _ -> false

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

let is_utf8_continuation byte = byte land 0xC0 = 0x80

let validate_utf8_char state pos =
  if pos >= state.len then (pos, None)
  else
    let first = get_byte state pos in
    if first < 0x80 then (pos + 1, None)
    else if first < 0xC0 then
      (pos + 1, Some ("invalid UTF-8 continuation byte", pos, pos + 1))
    else
      let expected =
        if first < 0xE0 then 2
        else if first < 0xF0 then 3
        else if first < 0xF8 then 4
        else 0
      in
      if expected = 0 then
        (pos + 1, Some ("invalid UTF-8 start byte", pos, pos + 1))
      else if pos + expected > state.len then
        (state.len, Some ("incomplete UTF-8 sequence", pos, state.len))
      else
        let rec check_continuation p =
          if p >= pos + expected then None
          else if not (is_utf8_continuation (get_byte state p)) then Some p
          else check_continuation (p + 1)
        in
        match check_continuation (pos + 1) with
        | Some invalid ->
          ( pos + expected
          , Some ("invalid UTF-8 continuation byte", invalid, invalid + 1) )
        | None -> (pos + expected, None)

let validate_utf8_in_range state start_pos end_pos =
  let rec validate pos st =
    if pos >= end_pos then st
    else if get_byte state pos >= 0x80 then
      match validate_utf8_char st pos with
      | _, Some (msg, s, e) -> validate e (error st msg s e)
      | next, None -> validate next st
    else validate (pos + 1) st
  in
  validate start_pos state

let find_end_brace content len start =
  let rec find p =
    if p >= len then None
    else if content.[p] = '}' then Some p
    else if is_xdigit content.[p] then find (p + 1)
    else None
  in
  find start

let process_escape_seqs content =
  let len = String.length content in
  let result = Buffer.create len in
  let rec process pos =
    if pos >= len then ()
    else
      match content.[pos] with
      | '\\' when pos + 1 < len -> (
        match content.[pos + 1] with
        | ('u' | 'U') when pos + 3 < len && content.[pos + 2] = '{' -> (
          match find_end_brace content len (pos + 3) with
          | Some end_brace -> (
            let hex = String.sub content (pos + 3) (end_brace - (pos + 3)) in
            try
              let value = int_of_string ("0x" ^ hex) in
              Buffer.add_char result (Char.chr (value mod 256));
              process (end_brace + 1)
            with _ ->
              Buffer.add_string result (String.sub content pos 2);
              process (pos + 2))
          | None ->
            Buffer.add_string result (String.sub content pos 2);
            process (pos + 2))
        | c -> (
          match List.assoc_opt c escape_chars with
          | Some esc ->
            Buffer.add_char result esc;
            process (pos + 2)
          | None ->
            Buffer.add_string result (String.sub content pos 2);
            process (pos + 2)))
      | c ->
        Buffer.add_char result c;
        process (pos + 1)
  in
  process 0;
  Buffer.contents result

let scan_while state start pred =
  let rec loop p =
    if p >= state.len then p
    else if pred state.source.[p] then loop (p + 1)
    else p
  in
  loop start

let scan_number_chars state start is_valid base_name =
  let rec scan p st =
    if p >= state.len then (p, st)
    else
      let c = state.source.[p] in
      if is_valid c then scan (p + 1) st
      else if is_alpha c || is_digit c then
        (p, char_error st p (Printf.sprintf "invalid %s digit '%c'" base_name c))
      else (p, st)
  in
  scan start state

let scan_decimal_with_dot state start =
  let rec scan pos st dots =
    if pos >= state.len then (pos, st, dots)
    else
      let c = state.source.[pos] in
      if is_digit c then scan (pos + 1) st dots
      else if c = '.' then
        if dots > 0 then
          ( pos
          , range_error st pos pos "multiple decimal points in number"
          , dots + 1 )
        else scan (pos + 1) st (dots + 1)
      else (pos, st, dots)
  in
  let end_pos, final_st, _ = scan state.pos state 0 in
  let text = extract_content state start end_pos in
  ({ final_st with pos = end_pos }, text, make_span state start)

let scan_number state =
  let start = state.pos in
  if state.pos + 1 < state.len && state.source.[state.pos] = '0' then
    match state.source.[state.pos + 1] with
    | 'x' | 'X' ->
      let pos = state.pos + 2 in
      if pos >= state.len then
        ( { (range_error state start pos "incomplete hex number") with pos }
        , extract_content state start pos
        , make_span state start )
      else
        let end_pos, st = scan_number_chars state pos is_xdigit "hex" in
        if end_pos = pos then
          ( {
              (range_error st start end_pos "incomplete hex number") with
              pos = end_pos
            }
          , extract_content state start end_pos
          , make_span state start )
        else
          ( { st with pos = end_pos }
          , extract_content state start end_pos
          , make_span state start )
    | 'b' | 'B' ->
      let pos = state.pos + 2 in
      if pos >= state.len then
        ( { (range_error state start pos "incomplete binary number") with pos }
        , extract_content state start pos
        , make_span state start )
      else
        let end_pos, st = scan_number_chars state pos is_bdigit "binary" in
        if end_pos = pos then
          ( {
              (range_error st start end_pos "incomplete binary number") with
              pos = end_pos
            }
          , extract_content state start end_pos
          , make_span state start )
        else
          ( { st with pos = end_pos }
          , extract_content state start end_pos
          , make_span state start )
    | 'o' | 'O' ->
      let pos = state.pos + 2 in
      if pos >= state.len then
        ( { (range_error state start pos "incomplete octal number") with pos }
        , extract_content state start pos
        , make_span state start )
      else
        let end_pos, st = scan_number_chars state pos is_odigit "octal" in
        if end_pos = pos then
          ( {
              (range_error st start end_pos "incomplete octal number") with
              pos = end_pos
            }
          , extract_content state start end_pos
          , make_span state start )
        else
          ( { st with pos = end_pos }
          , extract_content state start end_pos
          , make_span state start )
    | '0' .. '9' ->
      scan_decimal_with_dot
        (range_error
           state
           start
           state.pos
           "leading zeros in decimal numbers not allowed")
        start
    | _ -> scan_decimal_with_dot state start
  else scan_decimal_with_dot state start

let scan_quoted state quote is_template =
  let start = state.pos + 1 in
  let rec scan p depth extra =
    if p >= state.len then (p, true, extra)
    else
      match state.source.[p] with
      | c when c = quote && depth = 0 -> (p + 1, false, extra)
      | '\\' when p + 1 < state.len -> scan (p + 2) depth extra
      | '{' when is_template -> scan (p + 1) (depth + 1) extra
      | '}' when is_template && depth > 0 -> scan (p + 1) (depth - 1) extra
      | '}' when is_template -> scan (p + 1) depth (p :: extra)
      | _ -> scan (p + 1) depth extra
  in
  scan start 0 []

let validate_escape state pos =
  pos >= 0
  && pos + 1 < state.len
  && (List.mem_assoc state.source.[pos + 1] escape_chars
     || state.source.[pos + 1] = 'u'
     || state.source.[pos + 1] = 'U')

let parse_unicode_escape state pos =
  if
    pos + 1 < state.len
    && (state.source.[pos + 1] = 'u' || state.source.[pos + 1] = 'U')
  then
    let is_big = state.source.[pos + 1] = 'U' in
    if pos + 3 < state.len && state.source.[pos + 2] = '{' then
      match find_end_brace state.source state.len (pos + 3) with
      | Some end_brace -> (
        let hex = String.sub state.source (pos + 3) (end_brace - (pos + 3)) in
        if String.length hex = 0 then
          Some
            ( error state "empty unicode escape sequence" pos (end_brace + 1)
            , end_brace + 1 )
        else
          try
            let value = int_of_string ("0x" ^ hex) in
            let max_val = if is_big then 0x10FFFF else 0xFFFF in
            if value > max_val then
              Some
                ( error
                    state
                    (Printf.sprintf
                       "unicode code point exceeds maximum 0x%X"
                       max_val)
                    pos
                    (end_brace + 1)
                , end_brace + 1 )
            else None
          with _ ->
            Some
              ( error
                  state
                  "invalid hex digits in unicode escape sequence"
                  pos
                  (end_brace + 1)
              , end_brace + 1 ))
      | None ->
        if pos + 3 < state.len then
          Some
            ( error
                state
                "unclosed unicode escape sequence (missing '}')"
                pos
                (pos + 4)
            , pos + 3 )
        else
          Some
            ( error state "incomplete unicode escape sequence" pos state.len
            , state.len )
    else
      Some
        ( error
            state
            "expected '{' after unicode escape prefix"
            pos
            (min (pos + 3) state.len)
        , pos + 2 )
  else None

let validate_escapes_in_content state start end_pos =
  let rec check pos st =
    if pos >= end_pos - 1 then st
    else if state.source.[pos] = '\\' then
      if pos + 1 >= end_pos - 1 then
        error st "unterminated escape sequence" pos (pos + 1)
      else if not (validate_escape state pos) then
        check
          (pos + 2)
          (error
             st
             (Printf.sprintf
                "invalid escape sequence '\\%c'"
                state.source.[pos + 1])
             pos
             (pos + 2))
      else if state.source.[pos + 1] = 'u' || state.source.[pos + 1] = 'U' then
        match parse_unicode_escape state pos with
        | Some (err_st, next) -> check next err_st
        | None -> check (pos + 2) st
      else check (pos + 2) st
    else check (pos + 1) st
  in
  check start state

let scan_whitespace state =
  let end_pos = scan_while state state.pos is_whitespace in
  ({ state with pos = end_pos }, (), make_span state state.pos)

let scan_newline state =
  let new_state = advance state in
  (new_state, (), make_span state state.pos)

let scan_line_comment state =
  let rec scan p =
    if p >= state.len || state.source.[p] = '\n' then p else scan (p + 1)
  in
  let start = state.pos + 2 in
  let end_pos = scan start in
  ( { state with pos = end_pos }
  , extract_content state start end_pos
  , make_span state state.pos )

let scan_block_comment state =
  let start = state.pos in
  let rec scan p st =
    if p + 1 >= state.len then
      (p, range_error st start state.pos "unterminated block comment")
    else if state.source.[p] = '*' && state.source.[p + 1] = '/' then (p + 2, st)
    else if state.source.[p] = '/' && state.source.[p + 1] = '*' then
      scan
        (p + 2)
        (range_error st p (p + 2) "nested block comments not allowed")
    else scan (p + 1) st
  in
  let start_content = state.pos + 2 in
  let end_pos, final_st = scan start_content state in
  let content =
    if end_pos > start_content + 2 then
      extract_content state start_content (end_pos - 2)
    else ""
  in
  ({ final_st with pos = end_pos }, content, make_span state start)

let scan_ident state =
  let start = state.pos in
  let rec scan p st =
    if p >= state.len then (p, st)
    else
      let c = state.source.[p] in
      if is_ident_cont c then scan (p + 1) st
      else if Char.code c > 127 then
        scan
          (p + 1)
          (char_error
             st
             p
             ("non-ASCII character '" ^ String.make 1 c ^ "' in identifier"))
      else (p, st)
  in
  let end_pos, final_st = scan state.pos state in
  let text = extract_content state start end_pos in
  ( { final_st with pos = end_pos }
  , Basic.Interner.intern state.interner text
  , make_span state start )

let scan_string state =
  let start = state.pos in
  let end_pos, unterm, _ = scan_quoted state '"' false in
  let span = make_span state start in
  if
    unterm || end_pos > state.len
    || (end_pos = state.len && state.source.[end_pos - 1] <> '"')
  then
    let empty = Basic.Interner.empty_name state.interner in
    (error state "unterminated string literal" start state.pos, empty, span)
  else
    let content = extract_content state (state.pos + 1) (end_pos - 1) in
    let final_st = validate_escapes_in_content state (state.pos + 1) end_pos in
    let processed = process_escape_seqs content in
    ( { final_st with pos = end_pos }
    , Basic.Interner.intern state.interner processed
    , span )

let scan_template state =
  let start = state.pos in
  let end_pos, unterm, extra = scan_quoted state '"' true in
  let span = make_span state start in
  let st_with_errs =
    List.fold_left
      (fun st p -> error st "extra closing brace in template literal" p (p + 1))
      state
      extra
  in
  if unterm then
    let empty = Basic.Interner.empty_name st_with_errs.interner in
    ( error st_with_errs "unterminated template literal" start state.pos
    , empty
    , span )
  else
    let content = extract_content state start (end_pos - 1) in
    let processed = process_escape_seqs content in
    ( { st_with_errs with pos = end_pos }
    , Basic.Interner.intern st_with_errs.interner processed
    , span )

let scan_rune state =
  let start = state.pos in
  let end_pos, unterm, _ = scan_quoted state '\'' false in
  let span = make_span state start in
  if
    unterm || end_pos > state.len
    || (end_pos = state.len && state.source.[end_pos - 1] <> '\'')
  then (error state "unterminated rune literal" start state.pos, '\000', span)
  else
    let content = extract_content state (state.pos + 1) (end_pos - 1) in
    let final_st = validate_escapes_in_content state (state.pos + 1) end_pos in
    let processed =
      if String.length content > 1 then process_escape_seqs content else content
    in
    if String.length processed = 0 then
      (error final_st "empty rune literal" start end_pos, '\000', span)
    else if String.length processed > 1 then
      ( error final_st "rune literal contains multiple characters" start end_pos
      , processed.[0]
      , span )
    else ({ final_st with pos = end_pos }, processed.[0], span)

let scan_symbol state =
  let rec try_match = function
    | [] -> None
    | (sym, tok) :: rest ->
      let len = String.length sym in
      if
        state.pos + len <= state.len
        && String.sub state.source state.pos len = sym
      then Some (tok, len)
      else try_match rest
  in
  match try_match Token.symbol_strings with
  | Some (tok, len) ->
    ({ state with pos = state.pos + len }, tok, make_span state state.pos)
  | None ->
    let span = make_span state state.pos in
    ( char_error
        (advance state)
        state.pos
        (Printf.sprintf "unexpected character '%c'" state.source.[state.pos])
    , Token.Error
    , span )

let scan_comment_or_symbol state =
  if state.pos + 1 < state.len then
    match state.source.[state.pos + 1] with
    | '/' ->
      let st, c, sp = scan_line_comment state in
      (st, Token.Comment c, sp)
    | '*' ->
      let st, c, sp = scan_block_comment state in
      (st, Token.Comment c, sp)
    | _ -> scan_symbol state
  else scan_symbol state

let scan_template_or_dollar state =
  if state.pos + 1 < state.len && state.source.[state.pos + 1] = '"' then
    let new_st = { state with pos = state.pos + 2 } in
    let final_st, content, _ = scan_template new_st in
    (final_st, Token.LitTemplate content, make_span state state.pos)
  else scan_symbol state

let wrap_scanner scanner wrapper state =
  let st, result, span = scanner state in
  (st, wrapper result, span)

let dispatch_char c =
  match c with
  | _ when is_whitespace c ->
    wrap_scanner scan_whitespace (fun () -> Token.Whitespace)
  | '\n' -> wrap_scanner scan_newline (fun () -> Token.Newline)
  | '/' -> scan_comment_or_symbol
  | _ when is_ident_start c ->
    fun state ->
      let st, name, span = scan_ident state in
      ( st
      , Token.lookup_keyword
          state.interner
          (Basic.Interner.lookup state.interner name)
      , span )
  | _ when is_digit c ->
    wrap_scanner scan_number (fun text -> Token.LitNumber text)
  | '"' -> wrap_scanner scan_string (fun content -> Token.LitString content)
  | '\'' -> wrap_scanner scan_rune (fun char -> Token.LitRune char)
  | '$' -> scan_template_or_dollar
  | _ -> scan_symbol

let rec lex_token state =
  match peek_char state with
  | None -> (state, Token.EOF, make_span state state.pos)
  | Some c ->
    let code = Char.code c in
    if code < 32 && c <> '\t' && c <> '\n' && c <> '\r' then
      let span = make_span state state.pos in
      ( char_error
          (advance state)
          state.pos
          (Printf.sprintf "control character '\\x%02X' in source" code)
      , Token.Whitespace
      , span )
    else if c = '\000' then
      let span = make_span state state.pos in
      ( char_error (advance state) state.pos "null byte in source"
      , Token.Whitespace
      , span )
    else if code >= 0x80 then
      match validate_utf8_char state state.pos with
      | _, Some (msg, s, e) ->
        let span = Span.make state.file_id s e in
        let next = max (state.pos + 1) e in
        ({ (error state msg s e) with pos = next }, Token.Error, span)
      | next, None -> (dispatch_char c) { state with pos = next }
    else (dispatch_char c) state

and tokenize source file_id interner =
  let rec process st tokens =
    let old_pos = st.pos in
    let new_st, tok, span = lex_token st in
    match tok with
    | Token.EOF -> (List.rev ((tok, span) :: tokens), new_st.diags)
    | _ ->
      if new_st.pos <= old_pos then
        let err_st =
          error new_st "lexer failed to advance" old_pos new_st.pos
        in
        process (advance err_st) ((Token.Error, span) :: tokens)
      else process new_st ((tok, span) :: tokens)
  in
  process (make_state source file_id interner) []
