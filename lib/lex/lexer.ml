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

let add_error state msg span =
  let diag = Diagnostic.error msg span in
  { state with diags = Diagnostic.add state.diags diag }

let extract_content state start_offset end_offset =
  String.sub state.source start_offset (end_offset - start_offset)

let in_range c low high = c >= low && c <= high
let is_alpha c = in_range c 'a' 'z' || in_range c 'A' 'Z'
let is_digit c = in_range c '0' '9'
let is_whitespace c = c = ' ' || c = '\t' || c = '\r'
let is_newline c = c = '\n'
let is_xdigit c = is_digit c || in_range c 'a' 'f' || in_range c 'A' 'F'
let is_bdigit c = c = '0' || c = '1'
let is_odigit c = in_range c '0' '7'
let is_ident_start c = is_alpha c || c = '_'
let is_ident_cont c = is_alpha c || is_digit c || c = '_'
let is_template_delim c = c = '{' || c = '}'
let error_span state start end_ = Span.make state.file_id start end_

let char_error state pos msg =
  add_error state msg (error_span state pos (pos + 1))

let range_error state start end_ msg =
  add_error state msg (error_span state start end_)

let is_utf8_continuation byte = byte land 0xC0 = 0x80
let get_byte state pos = Char.code state.source.[pos]

let validate_utf8_sequence state pos expected_bytes =
  if pos + expected_bytes <= state.len then
    let rec validate_bytes byte_pos =
      if byte_pos >= pos + expected_bytes then true
      else if is_utf8_continuation (get_byte state byte_pos) then
        validate_bytes (byte_pos + 1)
      else false
    in
    if validate_bytes (pos + 1) then (pos + expected_bytes, None)
    else
      let invalid_pos =
        let rec find_invalid byte_pos =
          if byte_pos >= pos + expected_bytes then pos + 1
          else if not (is_utf8_continuation (get_byte state byte_pos)) then
            byte_pos
          else find_invalid (byte_pos + 1)
        in
        find_invalid (pos + 1)
      in
      ( pos + expected_bytes
      , Some ("invalid UTF-8 continuation byte", invalid_pos, invalid_pos + 1)
      )
  else (state.len, Some ("incomplete UTF-8 sequence", pos, state.len))

let validate_utf8_char state pos =
  if pos >= state.len then (pos, None)
  else
    let first_byte = get_byte state pos in
    if first_byte < 0x80 then (pos + 1, None)
    else if first_byte < 0xC0 then
      (pos + 1, Some ("invalid UTF-8 continuation byte", pos, pos + 1))
    else if first_byte < 0xE0 then validate_utf8_sequence state pos 2
    else if first_byte < 0xF0 then validate_utf8_sequence state pos 3
    else if first_byte < 0xF8 then validate_utf8_sequence state pos 4
    else (pos + 1, Some ("invalid UTF-8 start byte", pos, pos + 1))

let validate_utf8_in_range state start_pos end_pos =
  let rec validate_pos pos acc_state =
    if pos >= end_pos then acc_state
    else
      let char_code = Char.code state.source.[pos] in
      if char_code >= 0x80 then
        match validate_utf8_char acc_state pos with
        | _, Some (msg, error_start, error_end) ->
          let error_span = Span.make state.file_id error_start error_end in
          validate_pos error_end (add_error acc_state msg error_span)
        | next_pos, None -> validate_pos next_pos acc_state
      else validate_pos (pos + 1) acc_state
  in
  validate_pos start_pos state

let escape_chars =
  [
    ('n', '\n')
  ; (* newline *)
    ('t', '\t')
  ; (* tab *)
    ('r', '\r')
  ; (* carriage return *)
    ('\\', '\\')
  ; (* backslash *)
    ('"', '"')
  ; (* double quote *)
    ('\'', '\'')
  ; (* single quote *)
    ('0', '\000')
  ; (* null *)
    ('a', '\007')
  ; (* bell/alert *)
    ('b', '\008')
  ; (* backspace *)
    ('f', '\012')
  ; (* form feed *)
    ('v', '\011')
  ; (* vertical tab *)
    ('e', '\027')
  ; (* escape *)
    ('/', '/') (* forward slash *)
  ]

let find_end_brace content len start_pos =
  let rec find_end p =
    if p >= len then None
    else if content.[p] = '}' then Some p
    else if is_xdigit content.[p] then find_end (p + 1)
    else None
  in
  find_end start_pos

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
            let hex_content =
              String.sub content (pos + 3) (end_brace - (pos + 3))
            in
            try
              let value = int_of_string ("0x" ^ hex_content) in
              let unicode_char = Char.chr (value mod 256) in
              Buffer.add_char result unicode_char;
              process (end_brace + 1)
            with _ ->
              Buffer.add_char result content.[pos];
              Buffer.add_char result content.[pos + 1];
              process (pos + 2))
          | None ->
            Buffer.add_char result content.[pos];
            Buffer.add_char result content.[pos + 1];
            process (pos + 2))
        | c -> (
          match List.assoc_opt c escape_chars with
          | Some escaped_char ->
            Buffer.add_char result escaped_char;
            process (pos + 2)
          | None ->
            Buffer.add_char result content.[pos];
            Buffer.add_char result content.[pos + 1];
            process (pos + 2)))
      | c ->
        Buffer.add_char result c;
        process (pos + 1)
  in
  process 0;
  Buffer.contents result

let create_char_error state pos msg = char_error state pos msg

let create_incomplete_number_error state start_pos base_name =
  range_error state start_pos state.pos ("incomplete " ^ base_name ^ " number")

let create_invalid_digit_error state pos base_name digit =
  create_char_error
    state
    pos
    (Printf.sprintf "invalid %s digit '%c'" base_name digit)

let scan_while state start_pos predicate =
  let rec loop pos =
    if pos >= state.len then pos
    else if predicate state.source.[pos] then loop (pos + 1)
    else pos
  in
  loop start_pos

let scan_until condition state start_pos =
  let rec scan pos =
    if pos >= state.len then pos
    else if condition state.source.[pos] then pos + 1
    else if state.source.[pos] = '\\' && pos + 1 < state.len then scan (pos + 2)
    else scan (pos + 1)
  in
  scan start_pos

let scan_quoted_content state quote_char =
  scan_until (fun c -> c = quote_char) state (state.pos + 1)

let scan_template_content state =
  let start_pos = state.pos + 1 in
  let rec scan_template pos depth extra_closing =
    if pos >= state.len then (pos, true, extra_closing)
    else
      match state.source.[pos] with
      | '"' when depth = 0 -> (pos + 1, false, extra_closing)
      | '\\' when pos + 1 < state.len ->
        scan_template (pos + 2) depth extra_closing
      | '{' -> scan_template (pos + 1) (depth + 1) extra_closing
      | '}' when depth > 0 -> scan_template (pos + 1) (depth - 1) extra_closing
      | '}' when depth = 0 -> (pos, false, pos :: extra_closing)
      | _ -> scan_template (pos + 1) depth extra_closing
  in
  let end_pos, is_unterminated, extra_closing = scan_template start_pos 0 [] in
  (end_pos, is_unterminated, [], extra_closing)

let validate_escape_sequence state pos =
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
    let is_big_unicode = state.source.[pos + 1] = 'U' in
    if pos + 3 < state.len && state.source.[pos + 2] = '{' then
      match find_end_brace state.source state.len (pos + 3) with
      | Some end_brace -> (
        let hex_content =
          String.sub state.source (pos + 3) (end_brace - (pos + 3))
        in
        if String.length hex_content = 0 then
          let span = Span.make state.file_id pos (end_brace + 1) in
          Some
            (add_error state "empty unicode escape sequence" span, end_brace + 1)
        else
          try
            let value = int_of_string ("0x" ^ hex_content) in
            let max_value = if is_big_unicode then 0x10FFFF else 0xFFFF in
            if value > max_value then
              let span = Span.make state.file_id pos (end_brace + 1) in
              let max_str = Printf.sprintf "0x%X" max_value in
              Some
                ( add_error
                    state
                    ("unicode code point exceeds maximum " ^ max_str)
                    span
                , end_brace + 1 )
            else None
          with _ ->
            let span = Span.make state.file_id pos (end_brace + 1) in
            Some
              ( add_error
                  state
                  "invalid hex digits in unicode escape sequence"
                  span
              , end_brace + 1 ))
      | None ->
        if pos + 3 < state.len then
          let span = Span.make state.file_id pos (pos + 4) in
          Some
            ( add_error
                state
                "unclosed unicode escape sequence (missing '}')"
                span
            , pos + 3 )
        else
          let span = Span.make state.file_id pos state.len in
          Some
            ( add_error state "incomplete unicode escape sequence" span
            , state.len )
    else
      let span = Span.make state.file_id pos (min (pos + 3) state.len) in
      Some
        ( add_error state "expected '{' after unicode escape prefix" span
        , pos + 2 )
  else None

let validate_escapes_in_content state start_pos end_pos =
  let rec check_escapes pos acc_state =
    if pos >= end_pos - 1 then acc_state
    else if state.source.[pos] = '\\' then
      if pos + 1 >= end_pos - 1 then
        add_error
          acc_state
          "unterminated escape sequence"
          (Span.make state.file_id pos (pos + 1))
      else if not (validate_escape_sequence state pos) then
        let escape_span = Span.make state.file_id pos (pos + 2) in
        let msg =
          Printf.sprintf "invalid escape sequence '\\%c'" state.source.[pos + 1]
        in
        check_escapes (pos + 2) (add_error acc_state msg escape_span)
      else if state.source.[pos + 1] = 'u' || state.source.[pos + 1] = 'U' then
        match parse_unicode_escape state pos with
        | Some (error_state, next_pos) -> check_escapes next_pos error_state
        | None -> check_escapes (pos + 2) acc_state
      else check_escapes (pos + 2) acc_state
    else check_escapes (pos + 1) acc_state
  in
  check_escapes start_pos state

let scan_number_chars state start_pos is_valid_digit base_name =
  let rec scan_chars p acc_state =
    if p >= state.len then (p, acc_state)
    else
      let c = state.source.[p] in
      if is_valid_digit c then scan_chars (p + 1) acc_state
      else if is_alpha c || is_digit c then
        (p, create_invalid_digit_error acc_state p base_name c)
      else (p, acc_state)
  in
  scan_chars start_pos state

let scan_decimal_with_dot state start_pos =
  let rec scan_with_decimal pos acc_state dot_count =
    if pos >= state.len then (pos, acc_state, dot_count)
    else
      let c = state.source.[pos] in
      if is_digit c then scan_with_decimal (pos + 1) acc_state dot_count
      else if c = '.' then
        if dot_count > 0 then
          ( pos
          , range_error acc_state pos pos "multiple decimal points in number"
          , dot_count + 1 )
        else scan_with_decimal (pos + 1) acc_state (dot_count + 1)
      else (pos, acc_state, dot_count)
  in
  let end_pos, final_state, _ = scan_with_decimal state.pos state 0 in
  let text = extract_content state start_pos end_pos in
  ({ final_state with pos = end_pos }, text, make_span state start_pos)

let scan_number_with_base state start_pos prefix_len digit_predicate base_name =
  let pos = state.pos + prefix_len in
  if pos >= state.len then
    let error_state =
      create_incomplete_number_error state start_pos base_name
    in
    ( { error_state with pos }
    , extract_content state start_pos pos
    , make_span state start_pos )
  else
    let end_pos, final_state =
      scan_number_chars state pos digit_predicate base_name
    in
    if end_pos = pos then
      let error_state =
        create_incomplete_number_error final_state start_pos base_name
      in
      ( { error_state with pos = end_pos }
      , extract_content state start_pos end_pos
      , make_span state start_pos )
    else
      let text = extract_content state start_pos end_pos in
      ({ final_state with pos = end_pos }, text, make_span state start_pos)

let scan_whitespace state =
  let end_pos = scan_while state state.pos is_whitespace in
  ({ state with pos = end_pos }, (), make_span state state.pos)

let scan_newline state =
  let new_state = advance state in
  (new_state, (), make_span state state.pos)

let scan_line_comment state =
  let rec scan_to_eol pos =
    if pos >= state.len || state.source.[pos] = '\n' then pos
    else scan_to_eol (pos + 1)
  in
  let start_pos = state.pos + 2 in
  let end_pos = scan_to_eol start_pos in
  ( { state with pos = end_pos }
  , extract_content state start_pos end_pos
  , make_span state state.pos )

let scan_block_comment state =
  let start_pos = state.pos in
  let rec scan_block pos acc_state =
    if pos + 1 >= state.len then
      ( pos
      , range_error acc_state start_pos state.pos "unterminated block comment"
      )
    else if state.source.[pos] = '*' && state.source.[pos + 1] = '/' then
      (pos + 2, acc_state)
    else if state.source.[pos] = '/' && state.source.[pos + 1] = '*' then
      scan_block
        (pos + 2)
        (range_error
           acc_state
           pos
           (pos + 2)
           "nested block comments not allowed")
    else scan_block (pos + 1) acc_state
  in
  let start_content = state.pos + 2 in
  let end_pos, final_state = scan_block start_content state in
  let content =
    if end_pos > start_content + 2 then
      extract_content state start_content (end_pos - 2)
    else ""
  in
  ({ final_state with pos = end_pos }, content, make_span state start_pos)

let scan_ident state =
  let start_pos = state.pos in
  let rec scan_ident_chars pos acc_state =
    if pos >= state.len then (pos, acc_state)
    else
      let c = state.source.[pos] in
      if is_ident_cont c then scan_ident_chars (pos + 1) acc_state
      else if Char.code c > 127 then
        scan_ident_chars
          (pos + 1)
          (create_char_error
             acc_state
             pos
             ("non-ASCII character '" ^ String.make 1 c ^ "' in identifier"))
      else (pos, acc_state)
  in
  let end_pos, final_state = scan_ident_chars state.pos state in
  let text = extract_content state start_pos end_pos in
  ( { final_state with pos = end_pos }
  , Basic.Interner.intern state.interner text
  , make_span state start_pos )

let scan_number state =
  let start_pos = state.pos in
  if state.pos + 1 < state.len && state.source.[state.pos] = '0' then
    match state.source.[state.pos + 1] with
    | 'x' | 'X' -> scan_number_with_base state start_pos 2 is_xdigit "hex"
    | 'b' | 'B' -> scan_number_with_base state start_pos 2 is_bdigit "binary"
    | 'o' | 'O' -> scan_number_with_base state start_pos 2 is_odigit "octal"
    | '0' .. '9' ->
      scan_decimal_with_dot
        (range_error
           state
           start_pos
           state.pos
           "leading zeros in decimal numbers not allowed")
        start_pos
    | _ -> scan_decimal_with_dot state start_pos
  else scan_decimal_with_dot state start_pos

let scan_quoted_literal state quote_char literal_name =
  let start_pos = state.pos in
  let end_pos = scan_quoted_content state quote_char in
  let span = make_span state start_pos in
  if
    end_pos > state.len
    || (end_pos = state.len && state.source.[end_pos - 1] <> quote_char)
  then
    let error_state =
      add_error state ("unterminated " ^ literal_name ^ " literal") span
    in
    (error_state, None, span)
  else
    let content = extract_content state (state.pos + 1) (end_pos - 1) in
    let final_state =
      validate_escapes_in_content state (state.pos + 1) end_pos
    in
    ({ final_state with pos = end_pos }, Some content, span)

let scan_string state =
  match scan_quoted_literal state '"' "string" with
  | state', None, span ->
    let empty_name = Basic.Interner.empty_name state.interner in
    (state', empty_name, span)
  | state', Some content, span ->
    let processed_content = process_escape_seqs content in
    let name = Basic.Interner.intern state.interner processed_content in
    (state', name, span)

let scan_template state =
  let start_pos = state.pos in
  let end_pos, is_unterminated, _, extra_closing =
    scan_template_content state
  in
  let span = make_span state start_pos in
  let state_with_brace_errors =
    List.fold_left
      (fun acc pos ->
        add_error
          acc
          "extra closing brace in template literal"
          (error_span acc pos (pos + 1)))
      state
      extra_closing
  in
  if is_unterminated then
    let error_state =
      add_error state_with_brace_errors "unterminated template literal" span
    in
    let empty_name = Basic.Interner.empty_name error_state.interner in
    (error_state, empty_name, span)
  else
    let content = extract_content state start_pos (end_pos - 1) in
    let processed_content = process_escape_seqs content in
    let name =
      Basic.Interner.intern state_with_brace_errors.interner processed_content
    in
    ({ state_with_brace_errors with pos = end_pos }, name, span)

let scan_rune state =
  match scan_quoted_literal state '\'' "rune" with
  | state', None, span -> (state', '\000', span)
  | state', Some content, span ->
    let processed_content =
      if String.length content > 1 then process_escape_seqs content else content
    in
    if String.length processed_content = 0 then
      (add_error state' "empty rune literal" span, '\000', span)
    else if String.length processed_content > 1 then
      ( add_error state' "rune literal contains multiple characters" span
      , processed_content.[0]
      , span )
    else (state', processed_content.[0], span)

let scan_symbol state =
  let rec try_match_symbols symbols =
    match symbols with
    | [] -> None
    | (sym, token) :: rest ->
      let sym_len = String.length sym in
      if
        state.pos + sym_len <= state.len
        && String.sub state.source state.pos sym_len = sym
      then Some (token, sym_len)
      else try_match_symbols rest
  in
  match try_match_symbols Token.symbol_strings with
  | Some (token, len) ->
    ({ state with pos = state.pos + len }, token, make_span state state.pos)
  | None ->
    let span = make_span state state.pos in
    let msg =
      Printf.sprintf "unexpected character '%c'" state.source.[state.pos]
    in
    (add_error (advance state) msg span, Token.Error, span)

let scan_comment_or_symbol state =
  if state.pos + 1 < state.len then
    match state.source.[state.pos + 1] with
    | '/' ->
      let state', content, span = scan_line_comment state in
      (state', Token.Comment content, span)
    | '*' ->
      let state', content, span = scan_block_comment state in
      (state', Token.Comment content, span)
    | _ -> scan_symbol state
  else scan_symbol state

let scan_template_or_dollar state =
  if state.pos + 1 < state.len && state.source.[state.pos + 1] = '"' then
    let new_state = { state with pos = state.pos + 2 } in
    let final_state', content, _ = scan_template new_state in
    (final_state', Token.LitTemplate content, make_span state state.pos)
  else scan_symbol state

let wrap_scanner scanner token_wrapper state =
  let state', result, span = scanner state in
  (state', token_wrapper result, span)

let dispatch_char c =
  match c with
  | _ when is_whitespace c ->
    wrap_scanner scan_whitespace (fun () -> Token.Whitespace)
  | '\n' -> wrap_scanner scan_newline (fun () -> Token.Newline)
  | '/' -> scan_comment_or_symbol
  | _ when is_ident_start c ->
    fun state ->
      let state', name, span = scan_ident state in
      ( state'
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
    let char_code = Char.code c in
    if char_code < 32 && c <> '\t' && c <> '\n' && c <> '\r' then
      let span = make_span state state.pos in
      let msg =
        Printf.sprintf "control character '\\x%02X' in source" char_code
      in
      let error_state = add_error (advance state) msg span in
      (error_state, Token.Whitespace, span)
    else if c = '\000' then
      let span = make_span state state.pos in
      let error_state = add_error (advance state) "null byte in source" span in
      (error_state, Token.Whitespace, span)
    else if char_code >= 0x80 then
      match validate_utf8_char state state.pos with
      | _, Some (msg, error_start, error_end) ->
        let error_span = Span.make state.file_id error_start error_end in
        let error_state = add_error state msg error_span in
        let next_pos = max (state.pos + 1) error_end in
        ({ error_state with pos = next_pos }, Token.Error, error_span)
      | next_pos, None ->
        let scanner = dispatch_char c in
        scanner { state with pos = next_pos }
    else
      let scanner = dispatch_char c in
      scanner state

and tokenize source file_id interner =
  let rec process_tokens state tokens =
    let old_pos = state.pos in
    let new_state, token, span = lex_token state in
    match token with
    | Token.EOF -> (List.rev ((token, span) :: tokens), new_state.diags)
    | _ ->
      if new_state.pos <= old_pos then
        let error_state = add_error new_state "lexer failed to advance" span in
        let final_state = advance error_state in
        process_tokens final_state ((Token.Error, span) :: tokens)
      else process_tokens new_state ((token, span) :: tokens)
  in
  let initial_state = make_state source file_id interner in
  process_tokens initial_state []
