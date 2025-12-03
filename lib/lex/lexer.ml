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

let peek st = if st.pos >= st.len then None else Some st.source.[st.pos]

let peek_n st n =
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
let is_underscore c = c = '_'
let is_ident_start c = is_alpha c || is_underscore c
let is_ident_cont c = is_alpha c || is_digit c || is_underscore c
let is_xdigit c = is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
let is_bdigit c = c = '0' || c = '1'
let is_odigit c = '0' <= c && c <= '7'
let is_utf8_continuation_byte c = Char.code c land 0xC0 = 0x80

let utf8_seq_len first_byte =
  let code = Char.code first_byte in
  if code land 0x80 = 0 then 1 (* ASCII *)
  else if code land 0xE0 = 0xC0 then 2
  else if code land 0xF0 = 0xE0 then 3
  else if code land 0xF8 = 0xF0 then 4
  else 0

let check_utf8_seq st =
  match peek st with
  | None -> (st, 0, false)
  | Some first_byte ->
    let expected_len = utf8_seq_len first_byte in
    if expected_len = 0 then (st, 1, false)
    else if expected_len = 1 then (st, 1, true)
    else
      let rec check_bytes pos count valid =
        if count = 0 then (pos, valid)
        else
          match peek_n st pos with
          | Some c when is_utf8_continuation_byte c ->
            check_bytes (pos + 1) (count - 1) valid
          | Some _ -> check_bytes pos 0 false
          | None -> check_bytes pos 0 false
      in
      let final_pos, is_valid = check_bytes 1 (expected_len - 1) true in
      (adv_n st (final_pos + 1), final_pos + 1, is_valid)

let take_while p st =
  let rec loop st acc =
    match peek st with
    | Some c when p c -> loop (adv st) (c :: acc)
    | _ -> (st, List.rev acc)
  in
  loop st []

let take_if p st =
  match peek st with Some c when p c -> (adv st, Some c) | _ -> (st, None)

let take_until p st =
  let rec loop st acc =
    match peek st with
    | Some c when p c -> (st, List.rev acc)
    | None -> ({ st with pos = st.len }, List.rev acc)
    | Some c -> loop (adv st) (c :: acc)
  in
  loop st []

let scan_ws st =
  let start = st.pos in
  let st, _chars = take_while is_space st in
  (st, span st start)

let scan_nl st =
  let start = st.pos in
  (adv st, span st start)

let scan_line_comment st =
  let start = st.pos in
  let st, chars = take_until (fun c -> c = '\n') (adv_n st 2) in
  let content = String.of_seq (List.to_seq chars) in
  (st, content, span st start)

let scan_block_comment st =
  let start = st.pos in
  let rec loop depth st =
    match (peek st, peek_n st 1) with
    | Some '*', Some '/' when depth = 0 -> adv_n st 2
    | Some '*', Some '/' -> loop (depth - 1) (adv_n st 2)
    | Some '/', Some '*' -> loop (depth + 1) (adv_n st 2)
    | None, _ -> { st with pos = st.len }
    | _ -> loop depth (adv st)
  in
  let st = loop 0 (adv_n st 2) in
  let content =
    if st.pos > start + 4 then extract st (start + 2) (st.pos - 2) else ""
  in
  (st, content, span st start)

let scan_ident st =
  let start = st.pos in
  let st, first = take_if is_ident_start st in
  match first with
  | None -> (st, Interner.empty_name st.interner, span st start)
  | Some _ ->
    let st, _rest = take_while is_ident_cont st in
    let text = extract st start st.pos in
    let name = Interner.intern st.interner text in
    (st, name, span st start)

let scan_digits valid_chars st =
  let rec loop st =
    match peek st with
    | Some c when valid_chars c -> loop (adv st)
    | Some c when is_alpha c || is_digit c ->
      let st =
        add_err st Error.E0101 st.pos (st.pos + 1) [ "digit"; String.make 1 c ]
      in
      (adv st, false)
    | _ -> (st, true)
  in
  loop st

let scan_decimal_part st =
  let rec loop dot_seen st =
    match peek st with
    | Some '.' when not dot_seen -> loop true (adv st)
    | Some c when is_digit c -> loop dot_seen (adv st)
    | Some '.' ->
      let st = add_err st Error.E0102 st.pos (st.pos + 1) [] in
      loop true (adv st)
    | _ -> (st, dot_seen)
  in
  loop false st

let scan_number st =
  let start = st.pos in

  let scan_with_base prefix valid_chars base_name st =
    let base_pos = st.pos + prefix in
    if base_pos >= st.len then
      let st = add_err st Error.E0103 st.pos base_pos [ base_name ] in
      (st, extract st st.pos base_pos)
    else
      let st = { st with pos = base_pos } in
      let st, valid = scan_digits valid_chars st in
      if (not valid) || st.pos = base_pos then
        let st = add_err st Error.E0103 start base_pos [ base_name ] in
        (st, extract st start base_pos)
      else (st, extract st start st.pos)
  in

  match (peek st, peek_n st 1) with
  | Some '0', Some ('x' | 'X') -> scan_with_base 2 is_xdigit "hex" st
  | Some '0', Some ('b' | 'B') -> scan_with_base 2 is_bdigit "binary" st
  | Some '0', Some ('o' | 'O') -> scan_with_base 2 is_odigit "octal" st
  | Some '0', Some d when is_digit d ->
    let st = add_err st Error.E0103 start (start + 1) [ "decimal" ] in
    let st, _ = scan_decimal_part st in
    (st, extract st start st.pos)
  | _ ->
    let st, _ = scan_decimal_part st in
    (st, extract st start st.pos)

let process_escape st =
  let escape_start = st.pos in
  if st.pos + 1 >= st.len then (st, Some '\000')
  else
    let next_char = st.source.[st.pos + 1] in
    match next_char with
    | 'n' -> (adv_n st 2, Some '\n')
    | 't' -> (adv_n st 2, Some '\t')
    | 'r' -> (adv_n st 2, Some '\r')
    | '\\' -> (adv_n st 2, Some '\\')
    | '"' -> (adv_n st 2, Some '"')
    | '\'' -> (adv_n st 2, Some '\'')
    | '0' -> (adv_n st 2, Some '\000')
    | 'u' -> (
      match peek_n st 2 with
      | Some '{' ->
        let brace_start = st.pos + 2 in
        let rec scan_hex acc count st =
          match peek st with
          | Some c when is_xdigit c ->
            let digit =
              if '0' <= c && c <= '9' then Char.code c - Char.code '0'
              else if 'a' <= c && c <= 'f' then Char.code c - Char.code 'a' + 10
              else Char.code c - Char.code 'A' + 10
            in
            scan_hex ((acc * 16) + digit) (count + 1) (adv st)
          | Some '}' ->
            let st = adv st in
            if count = 0 then
              (add_err st Error.E0206 brace_start st.pos [], None)
            else if acc > 0x10FFFF then
              (add_err st Error.E0207 brace_start st.pos [], None)
            else if acc > 255 then
              (add_err st Error.E0208 brace_start st.pos [], None)
            else (st, Some (Char.chr acc))
          | Some c ->
            let st =
              add_err st Error.E0204 st.pos (st.pos + 1) [ String.make 1 c ]
            in
            let st, _ = take_until (fun c -> c = '}') st in
            let st = match peek st with Some '}' -> adv st | _ -> st in
            (st, None)
          | None -> (add_err st Error.E0205 brace_start st.pos [], None)
        in
        scan_hex 0 0 (adv_n st 3)
      | _ -> (add_err st Error.E0209 escape_start st.pos [], None))
    | 'U' -> (
      match peek_n st 2 with
      | Some '{' ->
        let brace_start = st.pos + 2 in
        let rec scan_hex acc count st =
          match peek st with
          | Some c when is_xdigit c ->
            let digit =
              if '0' <= c && c <= '9' then Char.code c - Char.code '0'
              else if 'a' <= c && c <= 'f' then Char.code c - Char.code 'a' + 10
              else Char.code c - Char.code 'A' + 10
            in
            scan_hex ((acc * 16) + digit) (count + 1) (adv st)
          | Some '}' ->
            let st = adv st in
            if count = 0 then
              (add_err st Error.E0206 brace_start st.pos [], None)
            else if acc > 0x10FFFF then
              (add_err st Error.E0207 brace_start st.pos [], None)
            else (st, Some (Char.chr (acc mod 256)))
          | Some c ->
            let st =
              add_err st Error.E0204 st.pos (st.pos + 1) [ String.make 1 c ]
            in
            let st, _ = take_until (fun c -> c = '}') st in
            let st = match peek st with Some '}' -> adv st | _ -> st in
            (st, None)
          | None -> (add_err st Error.E0205 brace_start st.pos [], None)
        in
        scan_hex 0 0 (adv_n st 3)
      | _ -> (add_err st Error.E0209 escape_start st.pos [], None))
    | _ ->
      let st =
        add_err
          st
          Error.E0210
          escape_start
          (st.pos + 2)
          [ String.make 1 next_char ]
      in
      (adv_n st 2, None)

let scan_lit_string st =
  let start = st.pos in
  let rec loop acc st =
    match peek st with
    | None ->
      let st = add_err st Error.E0201 start st.pos [ "string" ] in
      (st, None, span st start)
    | Some '"' ->
      let content = String.of_seq (List.to_seq (List.rev acc)) in
      (adv st, Some content, span st start)
    | Some '\\' -> (
      let st, escaped = process_escape st in
      match escaped with
      | Some escaped_char -> loop (escaped_char :: acc) st
      | None -> loop acc (adv st))
    | Some c -> loop (c :: acc) (adv st)
  in
  loop [] (adv st)

let scan_lit_rune st =
  let start = st.pos in
  let rec loop acc st =
    match peek st with
    | None ->
      let st = add_err st Error.E0201 start st.pos [ "rune" ] in
      (st, '\000', span st start)
    | Some '\'' ->
      let chars = List.rev acc in
      let st = adv st in
      let sp = span st start in
      if List.length chars = 0 then
        let st = add_err st Error.E0203 start (start + 1) [] in
        (st, '\000', sp)
      else if List.length chars > 1 then
        let st =
          add_err
            st
            Error.E0212
            start
            (start + 1)
            [ String.make 1 (List.hd chars) ]
        in
        (st, List.hd chars, sp)
      else (st, List.hd chars, sp)
    | Some '\\' -> (
      let st, escaped = process_escape st in
      match escaped with
      | Some escaped_char -> loop (escaped_char :: acc) st
      | None -> loop acc (adv st))
    | Some c -> loop (c :: acc) (adv st)
  in
  loop [] (adv st)

let scan_template_string st =
  let start = st.pos in
  let rec loop depth acc st =
    match peek st with
    | None ->
      let st = add_err st Error.E0201 start st.pos [ "template" ] in
      let content = String.of_seq (List.to_seq (List.rev acc)) in
      (st, content, span st start)
    | Some '"' when depth = 0 ->
      let content = String.of_seq (List.to_seq (List.rev acc)) in
      (adv st, content, span st start)
    | Some '{' -> loop (depth + 1) ('{' :: acc) (adv st)
    | Some '}' when depth > 0 -> loop (depth - 1) ('}' :: acc) (adv st)
    | Some '}' ->
      let st = add_err st Error.E0211 st.pos (st.pos + 1) [] in
      (st, String.of_seq (List.to_seq (List.rev acc)), span st start)
    | Some '\\' -> (
      let st, escaped = process_escape st in
      match escaped with
      | Some escaped_char -> loop depth (escaped_char :: acc) st
      | None -> loop depth acc (adv st))
    | Some c -> loop depth (c :: acc) (adv st)
  in
  loop 0 [] (adv st)

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
    let st =
      add_err
        st
        Error.E0301
        st.pos
        (st.pos + 1)
        [ String.make 1 st.source.[st.pos] ]
    in
    (adv st, Token.Error, span st start)

let dispatch_token st =
  let start = st.pos in

  match (peek st, peek_n st 1) with
  | None, _ -> (st, Token.EOF, span st start)
  | Some c, _ when is_space c ->
    let st, sp = scan_ws st in
    (st, Token.Whitespace, sp)
  | Some '\n', _ ->
    let st, sp = scan_nl st in
    (st, Token.Newline, sp)
  | Some '/', Some '/' ->
    let st, content, sp = scan_line_comment st in
    (st, Token.Comment content, sp)
  | Some '/', Some '*' ->
    let st, content, sp = scan_block_comment st in
    (st, Token.Comment content, sp)
  | Some '$', Some '"' ->
    let st, content, sp = scan_template_string (adv st) in
    let name = Interner.intern st.interner content in
    (st, Token.LitTemplate name, sp)
  | Some c, _ when is_ident_start c ->
    let st, name, sp = scan_ident st in
    let ident_str = Interner.lookup st.interner name in
    let token = Token.lookup_keyword st.interner ident_str in
    (st, token, sp)
  | Some c, _ when is_digit c ->
    let st, num_str = scan_number st in
    (st, Token.LitNumber num_str, span st start)
  | Some '"', _ ->
    let st, content_opt, sp = scan_lit_string st in
    let content =
      match content_opt with
      | Some c -> Interner.intern st.interner c
      | None -> Interner.empty_name st.interner
    in
    (st, Token.LitString content, sp)
  | Some '\'', _ ->
    let st, char_val, sp = scan_lit_rune st in
    (st, Token.LitRune char_val, sp)
  | Some c, _ when Char.code c < 32 && c <> '\t' && c <> '\n' && c <> '\r' ->
    let st =
      add_err
        st
        Error.E0302
        st.pos
        (st.pos + 1)
        [ Printf.sprintf "%02X" (Char.code c) ]
    in
    (adv st, Token.Whitespace, span st start)
  | Some '\000', _ ->
    let st = add_err st Error.E0303 st.pos (st.pos + 1) [] in
    (adv st, Token.Whitespace, span st start)
  | Some c, _ when Char.code c >= 0x80 ->
    let new_st, seq_len, is_valid = check_utf8_seq st in
    if is_valid then (new_st, Token.Error, span st start)
    else
      let error_code =
        let code = Char.code c in
        if code land 0xE0 = 0xC0 then Error.E0002
        else if seq_len = 0 then Error.E0002
        else Error.E0001
      in
      let error_st = add_err st error_code st.pos new_st.pos [] in
      (error_st, Token.Error, span st start)
  | _ -> scan_symbol st

let lex_token st =
  let start = st.pos in
  let st, token, sp = dispatch_token st in
  if st.pos <= start then
    let st = add_err st Error.E0501 st.pos (st.pos + 1) [] in
    (adv st, Token.Error, span st start)
  else (st, token, sp)

let tokenize source file_id interner =
  let rec loop st acc =
    let start = st.pos in
    let st, token, sp = lex_token st in
    match token with
    | Token.EOF -> (List.rev ((token, sp) :: acc), st.diags)
    | _ ->
      if st.pos <= start then
        let st = add_err st Error.E0501 st.pos (st.pos + 1) [] in
        loop (adv st) ((token, sp) :: acc)
      else loop st ((token, sp) :: acc)
  in
  loop (mk_state source file_id interner) []
