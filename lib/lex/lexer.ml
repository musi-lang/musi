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

let add_error state msg span =
  let diag = Diagnostic.error msg span in
  { state with diags = Diagnostic.add state.diags diag }

let extract_content state start_offset end_offset =
  String.sub state.source start_offset (end_offset - start_offset)

let scan_while state start_pos predicate =
  let rec loop pos =
    if pos >= state.len then pos
    else if predicate state.source.[pos] then loop (pos + 1)
    else pos
  in
  loop start_pos

let create_char_error state pos msg_format arg =
  let char_span = Span.make state.file_id pos (pos + 1) in
  let msg = Printf.sprintf msg_format arg in
  add_error state msg char_span

let create_incomplete_number_error state start_pos base_name =
  let span = make_span state start_pos in
  add_error state ("incomplete " ^ base_name ^ " number") span

let create_invalid_digit_error state pos base_name digit =
  let msg = Printf.sprintf "invalid %s digit '%c'" base_name digit in
  create_char_error state pos "%s" msg

let check_char_in_set c char_set =
  let rec loop i =
    if i >= String.length char_set then false
    else if c = char_set.[i] then true
    else loop (i + 1)
  in
  loop 0

let is_alpha c =
  check_char_in_set c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let is_digit c = check_char_in_set c "0123456789"
let is_whitespace c = check_char_in_set c " \t\r"
let is_newline c = c = '\n'
let is_xdigit c = is_digit c || check_char_in_set c "abcdefABCDEF"
let is_bdigit c = check_char_in_set c "01"
let is_odigit c = check_char_in_set c "01234567"
let is_ident_start c = is_alpha c || c = '_'
let is_ident_cont c = is_alpha c || is_digit c || c = '_'

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
    ('/', '/') (* forward slash (JSON-style) *)
  ]

let scan_quoted_content state quote_char =
  let rec loop pos =
    if pos >= state.len then pos
    else
      match state.source.[pos] with
      | c when c = quote_char -> pos + 1
      | '\\' -> if pos + 1 < state.len then loop (pos + 2) else loop (pos + 1)
      | _ -> loop (pos + 1)
  in
  loop (state.pos + 1)

let validate_escape_sequence state pos =
  if pos + 1 >= state.len then false
  else
    let escape_char = state.source.[pos + 1] in
    List.exists (fun (c, _) -> c = escape_char) escape_chars

let check_unterminated_literal state quote_char end_pos span msg =
  if
    end_pos > state.len
    || (end_pos = state.len && state.source.[end_pos - 1] != quote_char)
  then Some (add_error state msg span)
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
      else check_escapes (pos + 2) acc_state
    else check_escapes (pos + 1) acc_state
  in
  check_escapes start_pos state

let scan_decimal_with_dot state start_pos =
  let rec loop pos acc_state dot_count =
    if pos >= state.len then (pos, acc_state, dot_count)
    else
      let c = state.source.[pos] in
      if is_digit c then loop (pos + 1) acc_state dot_count
      else if c = '.' then
        if dot_count > 0 then
          let error_state =
            add_error
              acc_state
              "multiple decimal points in number"
              (make_span acc_state pos)
          in
          (pos, error_state, dot_count + 1)
        else loop (pos + 1) acc_state (dot_count + 1)
      else (pos, acc_state, dot_count)
  in
  let end_pos, final_state, _ = loop state.pos state 0 in
  let text = extract_content state start_pos end_pos in
  ({ final_state with pos = end_pos }, text, make_span state start_pos)

let scan_whitespace state =
  let end_pos = scan_while state state.pos is_whitespace in
  ({ state with pos = end_pos }, make_span state state.pos)

let scan_newline state =
  let new_state = advance state in
  (new_state, make_span state state.pos)

let scan_line_comment state =
  let rec loop pos =
    if pos >= state.len then pos
    else match state.source.[pos] with '\n' -> pos | _ -> loop (pos + 1)
  in
  let end_pos = loop (state.pos + 2) in
  let content = extract_content state (state.pos + 2) end_pos in
  ({ state with pos = end_pos }, content, make_span state state.pos)

let scan_block_comment state =
  let start_pos = state.pos in
  let rec loop pos acc_state =
    if pos + 1 >= state.len then
      ( pos
      , add_error
          acc_state
          "unterminated block comment"
          (make_span state start_pos) )
    else if state.source.[pos] = '*' && state.source.[pos + 1] = '/' then
      (pos + 2, acc_state)
    else if state.source.[pos] = '/' && state.source.[pos + 1] = '*' then
      let nested_span = Span.make state.file_id pos (pos + 2) in
      let error_state =
        add_error acc_state "nested block comments not allowed" nested_span
      in
      loop (pos + 2) error_state
    else loop (pos + 1) acc_state
  in
  let end_pos, final_state = loop (state.pos + 2) state in
  let content =
    if end_pos > state.pos + 2 then
      extract_content state (state.pos + 2) (min (end_pos - 2) (state.len - 2))
    else ""
  in
  ({ final_state with pos = end_pos }, content, make_span state start_pos)

let scan_ident state =
  let start_pos = state.pos in
  let rec loop pos acc_state =
    if pos >= state.len then (pos, acc_state)
    else
      let c = state.source.[pos] in
      if is_ident_cont c then loop (pos + 1) acc_state
      else if Char.code c > 127 then
        let error_state =
          create_char_error
            acc_state
            pos
            "non-ASCII character '%c' in identifier"
            c
        in
        loop (pos + 1) error_state
      else (pos, acc_state)
  in
  let end_pos, final_state = loop state.pos state in
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
    let rec loop p acc_state =
      if p >= state.len then (p, acc_state)
      else
        let c = state.source.[p] in
        if digit_predicate c then loop (p + 1) acc_state
        else if is_alpha c || is_digit c then
          (p, create_invalid_digit_error acc_state p base_name c)
        else (p, acc_state)
    in
    let end_pos, final_state = loop pos state in
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

let scan_decimal_number state start_pos = scan_decimal_with_dot state start_pos

let scan_number state =
  let start_pos = state.pos in
  let span = make_span state start_pos in
  if state.pos + 1 < state.len && state.source.[state.pos] = '0' then
    match state.source.[state.pos + 1] with
    | 'x' | 'X' -> scan_number_with_base state start_pos 2 is_xdigit "hex"
    | 'b' | 'B' -> scan_number_with_base state start_pos 2 is_bdigit "binary"
    | 'o' | 'O' -> scan_number_with_base state start_pos 2 is_odigit "octal"
    | '0' .. '9' ->
      let error_state =
        add_error state "leading zeros in decimal numbers not allowed" span
      in
      scan_decimal_number error_state start_pos
    | _ -> scan_decimal_number state start_pos
  else scan_decimal_number state start_pos

let scan_quoted_literal state quote_char literal_name =
  let start_pos = state.pos in
  let end_pos = scan_quoted_content state quote_char in
  let span = make_span state start_pos in
  match
    check_unterminated_literal
      state
      quote_char
      end_pos
      span
      ("unterminated " ^ literal_name ^ " literal")
  with
  | Some error_state -> (error_state, None, span)
  | None ->
    let content = extract_content state (state.pos + 1) (end_pos - 1) in
    let final_state =
      validate_escapes_in_content state (state.pos + 1) end_pos
    in
    ({ final_state with pos = end_pos }, Some content, span)

let scan_string state =
  match scan_quoted_literal state '"' "string" with
  | state', None, span -> (state', "", span)
  | state', Some content, span -> (state', content, span)

let scan_rune state =
  match scan_quoted_literal state '\'' "rune" with
  | state', None, span -> (state', '\000', span)
  | state', Some content, span ->
    if String.length content = 0 then
      let error_state = add_error state' "empty rune literal" span in
      (error_state, '\000', span)
    else if
      String.length content > 2
      || (String.length content = 2 && content.[0] != '\\')
    then
      let error_state =
        add_error state' "rune literal contains multiple characters" span
      in
      (error_state, content.[0], span)
    else
      let final_state, final_char =
        if String.length content = 2 && content.[0] = '\\' then
          if
            not
              (validate_escape_sequence
                 state
                 (state.pos - String.length content - 1))
          then
            let msg =
              Printf.sprintf "invalid escape sequence '\\%c'" content.[1]
            in
            (add_error state' msg span, '\000')
          else (state', List.assoc content.[1] escape_chars)
        else if String.length content = 1 then (state', content.[0])
        else (add_error state' "unterminated escape sequence" span, '\000')
      in
      (final_state, final_char, span)

let scan_symbol state =
  let rec try_symbols symbols =
    match symbols with
    | [] -> None
    | (sym, token) :: rest ->
      let sym_len = String.length sym in
      if
        state.pos + sym_len <= state.len
        && String.sub state.source state.pos sym_len = sym
      then Some (token, sym_len)
      else try_symbols rest
  in
  match try_symbols Token.symbol_strings with
  | Some (token, len) ->
    let new_state = { state with pos = state.pos + len } in
    (new_state, token, make_span state state.pos)
  | None ->
    let char = state.source.[state.pos] in
    let span = make_span state state.pos in
    let msg = Printf.sprintf "unexpected character '%c'" char in
    let error_state = add_error (advance state) msg span in
    (error_state, Token.Error, span)

let scan_comment_or_symbol state =
  if state.pos + 1 < state.len then
    match state.source.[state.pos + 1] with
    | '/' ->
      let state', content, span = scan_line_comment state in
      (state', Token.Comment content, span)
    | '*' ->
      let state', content, span = scan_block_comment state in
      (state', Token.Comment content, span)
    | _ ->
      let state', token, span = scan_symbol state in
      (state', token, span)
  else
    let state', token, span = scan_symbol state in
    (state', token, span)

let dispatch_char = function
  | c when is_whitespace c ->
    fun state ->
      let state', span = scan_whitespace state in
      (state', Token.Whitespace, span)
  | '\n' ->
    fun state ->
      let state', span = scan_newline state in
      (state', Token.Newline, span)
  | '/' -> scan_comment_or_symbol
  | c when is_ident_start c ->
    fun state ->
      let state', text, span = scan_ident state in
      (state', Token.lookup_keyword text, span)
  | c when is_digit c ->
    fun state ->
      let state', text, span = scan_number state in
      (state', Token.LitNumber text, span)
  | '"' ->
    fun state ->
      let state', content, span = scan_string state in
      (state', Token.LitString content, span)
  | '\'' ->
    fun state ->
      let state', char, span = scan_rune state in
      (state', Token.LitRune char, span)
  | _ ->
    fun state ->
      let state', token, span = scan_symbol state in
      (state', token, span)

let lex_token state =
  match peek_char state with
  | None -> (state, Token.EOF, make_span state state.pos)
  | Some c ->
    let char_code = Char.code c in
    if char_code < 32 && c != '\t' && c != '\n' && c != '\r' then
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
    else
      let scanner = dispatch_char c in
      scanner state

let tokenize source file_id interner =
  let rec loop state tokens =
    let old_pos = state.pos in
    let new_state, token, span = lex_token state in
    match token with
    | Token.EOF -> (List.rev ((token, span) :: tokens), new_state.diags)
    | _ ->
      if new_state.pos <= old_pos then
        let error_state = add_error new_state "lexer failed to advance" span in
        let final_state = advance error_state in
        loop final_state ((Token.Error, span) :: tokens)
      else loop new_state ((token, span) :: tokens)
  in
  let initial_state = make_state source file_id interner in
  loop initial_state []
