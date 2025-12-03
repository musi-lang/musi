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

let peek_n_opt st n =
  if st.pos + n >= st.len then None else Some st.source.[st.pos + n]

let adv st = { st with pos = st.pos + 1 }
let adv_n st n = { st with pos = st.pos + n }
let span st start = Span.make st.file_id start st.pos

let add_err st code start end_ args =
  let sp = Span.make st.file_id start end_ in
  { st with diags = Diagnostic.add st.diags (Error.lex_diag code sp args) }

let extract st s e = String.sub st.source s (e - s)
let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_digit c = '0' <= c && c <= '9'
let is_space c = c = ' ' || c = '\t' || c = '\r'
let is_newline c = c = '\n'
let is_underscore c = c = '_'
let is_ident_start c = is_alpha c || is_underscore c
let is_ident_cont c = is_alpha c || is_digit c || is_underscore c
let is_xdigit c = is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
let is_bdigit c = c = '0' || c = '1'
let is_odigit c = '0' <= c && c <= '7'

let rec scan_while p st =
  let rec loop st acc =
    match peek_char_opt st with
    | Some c when p c -> loop (adv st) (c :: acc)
    | _ -> (st, List.rev acc)
  in
  loop st []

let scan_char_opt p st =
  match peek_char_opt st with
  | Some c when p c -> (adv st, Some c)
  | _ -> (st, None)

let scan_string s st =
  let len = String.length s in
  if st.pos + len <= st.len && extract st st.pos (st.pos + len) = s then
    (adv_n st len, true)
  else (st, false)

let rec scan_until stop_char st =
  let rec loop st acc =
    match peek_char_opt st with
    | Some c when c = stop_char -> (st, List.rev acc)
    | None -> (st, List.rev acc)
    | Some c -> loop (adv st) (c :: acc)
  in
  loop st []

let scan_ws st =
  let final_st, _chars = scan_while is_space st in
  (final_st, span st st.pos)

let scan_nl st = (adv st, span st st.pos)

let scan_line_comment st =
  let start = st.pos in
  let final_st, _ = scan_until '\n' (adv_n st 2) in
  let content = extract st (start + 2) final_st.pos in
  (final_st, content, span st start)

let rec scan_block_comment depth st =
  match (peek_char_opt st, peek_n_opt st 1) with
  | Some '*', Some '/' when depth = 0 -> (adv_n st 2, depth)
  | Some '*', Some '/' -> scan_block_comment (depth - 1) (adv_n st 2)
  | Some '/', Some '*' -> scan_block_comment (depth + 1) (adv_n st 2)
  | None, _ when depth > 0 -> (st, depth)
  | None, _ -> (st, 0)
  | _ -> scan_block_comment depth (adv st)

let scan_block_comment st =
  let start = st.pos in
  let final_st, _depth = scan_block_comment 0 (adv_n st 2) in
  let content =
    if final_st.pos > start + 4 then extract st (start + 2) (final_st.pos - 2)
    else ""
  in
  (final_st, content, span st start)

let scan_ident st =
  let start = st.pos in
  match scan_char_opt is_ident_start st with
  | st', Some _ ->
    let final_st, _chars = scan_while is_ident_cont st' in
    let text = extract st start final_st.pos in
    let name = Interner.intern st.interner text in
    (final_st, name, span st start)
  | _ -> (st, Interner.empty_name st.interner, span st st.pos)

let rec scan_digits st valid_chars =
  match peek_char_opt st with
  | Some c when valid_chars c -> scan_digits (adv st) valid_chars
  | Some c when is_alpha c || is_digit c ->
    let err_st =
      add_err st Error.E0101 st.pos (st.pos + 1) [ "digit"; String.make 1 c ]
    in
    (adv err_st, false)
  | _ -> (st, true)

let scan_decimal_part st =
  let rec loop st dot_seen =
    match peek_char_opt st with
    | Some '.' when not dot_seen -> loop (adv st) true
    | Some c when is_digit c -> loop (adv st) dot_seen
    | Some '.' when dot_seen ->
      let err_st = add_err st Error.E0102 st.pos (st.pos + 1) [] in
      (adv err_st, true)
    | _ -> (st, dot_seen)
  in
  loop st false

let scan_number_with_base st prefix valid_chars base_name =
  let base_pos = st.pos + prefix in
  if base_pos >= st.len then
    let err_st = add_err st Error.E0103 st.pos base_pos [ base_name ] in
    (err_st, extract st st.pos base_pos)
  else
    let digit_st, valid = scan_digits { st with pos = base_pos } valid_chars in
    if (not valid) || digit_st.pos = base_pos then
      let err_st = add_err digit_st Error.E0103 st.pos base_pos [ base_name ] in
      (err_st, extract st st.pos base_pos)
    else (digit_st, extract st st.pos digit_st.pos)

let scan_number st =
  let start = st.pos in
  match (peek_char_opt st, peek_n_opt st 1) with
  | Some '0', Some 'x' | Some '0', Some 'X' ->
    scan_number_with_base st 2 is_xdigit "hex"
  | Some '0', Some 'b' | Some '0', Some 'B' ->
    scan_number_with_base st 2 is_bdigit "binary"
  | Some '0', Some 'o' | Some '0', Some 'O' ->
    scan_number_with_base st 2 is_odigit "octal"
  | Some '0', Some d when is_digit d ->
    let err_st = add_err st Error.E0103 start (start + 1) [ "decimal" ] in
    let final_st, _ = scan_decimal_part err_st in
    (final_st, extract st start final_st.pos)
  | _ ->
    let final_st, _ = scan_decimal_part st in
    (final_st, extract st start final_st.pos)

let rec scan_string_literal st =
  let start = st.pos in
  let rec loop st =
    match peek_char_opt st with
    | None -> (st, true)
    | Some '"' -> (adv st, false)
    | Some '\\' when st.pos + 1 < st.len -> loop (adv_n st 2)
    | _ -> loop (adv st)
  in
  let final_st, unterminated = loop (adv st) in
  let content = extract st (st.pos + 1) (final_st.pos - 1) in
  let sp = span st start in
  if unterminated then
    let err_st = add_err final_st Error.E0201 start final_st.pos [ "string" ] in
    (err_st, None, sp)
  else (final_st, Some content, sp)

let rec scan_rune_literal st =
  let start = st.pos in
  let rec loop st =
    match peek_char_opt st with
    | None -> (st, true)
    | Some '\'' -> (adv st, false)
    | Some '\\' when st.pos + 1 < st.len -> loop (adv_n st 2)
    | _ -> loop (adv st)
  in
  let final_st, unterminated = loop (adv st) in
  let content = extract st (st.pos + 1) (final_st.pos - 1) in
  let sp = span st start in
  if unterminated then
    let err_st = add_err final_st Error.E0201 start final_st.pos [ "rune" ] in
    (err_st, '\000', sp)
  else if String.length content = 0 then
    let err_st = add_err final_st Error.E0203 start (start + 1) [] in
    (err_st, '\000', sp)
  else if String.length content > 1 then
    let err_st =
      add_err
        final_st
        Error.E0212
        start
        (start + 1)
        [ String.make 1 content.[0] ]
    in
    (err_st, content.[0], sp)
  else (final_st, content.[0], sp)

let scan_symbol st =
  let start = st.pos in
  let rec find_match = function
    | [] -> None
    | (sym, tok) :: rest ->
      let len = String.length sym in
      if st.pos + len <= st.len && extract st st.pos (st.pos + len) = sym then
        Some (tok, len)
      else find_match rest
  in
  match find_match Token.symbol_strings with
  | Some (tok, len) -> (adv_n st len, tok, span st start)
  | None ->
    let err_st =
      add_err
        (adv st)
        Error.E0301
        st.pos
        (st.pos + 1)
        [ String.make 1 st.source.[st.pos] ]
    in
    (err_st, Token.Error, span st start)

let scan_comment_or_symbol st =
  match (peek_char_opt st, peek_n_opt st 1) with
  | Some '/', Some '/' ->
    let final_st, comment, sp = scan_line_comment st in
    (final_st, Token.Comment comment, sp)
  | Some '/', Some '*' ->
    let final_st, comment, sp = scan_block_comment st in
    (final_st, Token.Comment comment, sp)
  | _ -> scan_symbol st

let scan_template_or_dollar st =
  match (peek_char_opt st, peek_n_opt st 1) with
  | Some '$', Some '"' ->
    let start = st.pos in
    let final_st, content_opt, _ =
      scan_string_literal { st with pos = st.pos + 1 }
    in
    let content =
      match content_opt with
      | Some c -> Interner.intern st.interner c
      | None -> Interner.empty_name st.interner
    in
    (final_st, Token.LitTemplate content, span st start)
  | _ -> scan_symbol st

let dispatch_token st =
  match peek_char_opt st with
  | None -> (st, Token.EOF, span st st.pos)
  | Some c when is_space c ->
    let final_st, sp = scan_ws st in
    (final_st, Token.Whitespace, sp)
  | Some '\n' ->
    let final_st, sp = scan_nl st in
    (final_st, Token.Newline, sp)
  | Some '/' -> scan_comment_or_symbol st
  | Some '_' -> scan_symbol st
  | Some c when is_ident_start c ->
    let final_st, name, sp = scan_ident st in
    let ident_str = Interner.lookup st.interner name in
    let token = Token.lookup_keyword st.interner ident_str in
    (final_st, token, sp)
  | Some c when is_digit c ->
    let final_st, num_str = scan_number st in
    (final_st, Token.LitNumber num_str, span st st.pos)
  | Some '"' ->
    let final_st, content_opt, sp = scan_string_literal st in
    let content =
      match content_opt with
      | Some c -> Interner.intern st.interner c
      | None -> Interner.empty_name st.interner
    in
    (final_st, Token.LitString content, sp)
  | Some '\'' ->
    let final_st, char_val, sp = scan_rune_literal st in
    (final_st, Token.LitRune char_val, sp)
  | Some '$' -> scan_template_or_dollar st
  | Some c when Char.code c < 32 && c <> '\t' && c <> '\n' && c <> '\r' ->
    let err_st =
      add_err
        (adv st)
        Error.E0302
        st.pos
        (st.pos + 1)
        [ Printf.sprintf "%02X" (Char.code c) ]
    in
    (err_st, Token.Whitespace, span st st.pos)
  | Some '\000' ->
    let err_st = add_err (adv st) Error.E0303 st.pos (st.pos + 1) [] in
    (err_st, Token.Whitespace, span st st.pos)
  | Some c when Char.code c >= 0x80 -> (adv st, Token.Error, span st st.pos)
  | _ -> scan_symbol st

let recover st action =
  let err_st = add_err st Error.E0501 st.pos (st.pos + 1) [] in
  match action with
  | `Skip_char -> adv err_st
  | `Skip_to_nl ->
    let rec loop s =
      match peek_char_opt s with None | Some '\n' -> s | _ -> loop (adv s)
    in
    loop err_st
  | `Skip_to_ws ->
    let rec loop s =
      match peek_char_opt s with
      | None -> s
      | Some c when is_space c -> s
      | _ -> loop (adv s)
    in
    loop err_st
  | `Skip_to_delim ->
    let rec loop s =
      match peek_char_opt s with
      | None -> s
      | Some c
        when is_space c || c = ';' || c = ',' || c = ')' || c = '}' || c = ']'
        ->
        s
      | _ -> loop (adv s)
    in
    loop err_st

let lex_token st =
  let start = st.pos in
  let result = dispatch_token st in
  let new_st = match result with s, _, _ -> s in
  if new_st.pos <= start then
    let recovery_st = recover st `Skip_char in
    (recovery_st, Token.Error, span st start)
  else result

let rec tokenize source file_id interner =
  let rec loop st acc =
    let new_st, token, sp = lex_token st in
    match token with
    | Token.EOF -> (List.rev ((token, sp) :: acc), new_st.diags)
    | _ -> loop new_st ((token, sp) :: acc)
  in
  loop (mk_state source file_id interner) []
