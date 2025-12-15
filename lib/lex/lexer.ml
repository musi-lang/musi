open Token
open Basic

type t = {
    interner : Interner.t
  ; error_bag : Reporter.bag
  ; source : Source.t
  ; file_id : int
  ; mutable curr_pos : int
  ; text : string
  ; text_len : int
  ; mutable in_template : bool
}

let create ?(interner = None) source file_id =
  let text = Source.text source in
  let interner_val =
    match interner with Some i -> i | None -> Interner.create ()
  in
  {
    interner = interner_val
  ; error_bag = Reporter.empty_bag
  ; source
  ; file_id
  ; curr_pos = 0
  ; text
  ; text_len = String.length text
  ; in_template = false
  }

let curr_pos lexer = lexer.curr_pos
let source lexer = lexer.source
let file_id lexer = lexer.file_id
let has_errors lexer = not (Reporter.is_empty lexer.error_bag)
let error_bag lexer = lexer.error_bag

let span lexer start_pos end_pos =
  let _ = Source.line_col lexer.source start_pos in
  let _ = Source.line_col lexer.source end_pos in
  { Span.file = lexer.file_id; Span.start = start_pos; Span.end_ = end_pos }

let intern lexer s = Interner.intern lexer.interner s
let advance lexer n = lexer.curr_pos <- lexer.curr_pos + n

let rec scan_while pred text pos =
  if pos >= String.length text || not (pred text.[pos]) then pos
  else scan_while pred text (pos + 1)

let extract_content text start_pos end_pos =
  String.sub text start_pos (end_pos - start_pos)

let scan_exact_match_opt expected text pos =
  let len = String.length expected in
  if pos + len <= String.length text && String.sub text pos len = expected then
    Some len
  else None

let rec scan_quoted_content_opt text pos stop_chars =
  if pos >= String.length text then None
  else
    match text.[pos] with
    | c when List.mem c stop_chars -> Some pos
    | '\\' when pos + 1 < String.length text ->
      scan_quoted_content_opt text (pos + 2) stop_chars
    | _ -> scan_quoted_content_opt text (pos + 1) stop_chars

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_odigit c = c >= '0' && c <= '7'
let is_bdigit c = c = '0' || c = '1'
let is_ident_start c = c = '_' || is_alpha c
let is_ident_char c = is_ident_start c || is_digit c
let is_digit_or_underscore c = is_digit c || c = '_'
let is_sync_char c = c = ';' || c = '\n' || c = '}' || c = ')' || c = ']'

let handle_underscores text pos end_pos =
  let rec loop i acc prev_underscore =
    if i >= end_pos then (String.concat "" (List.rev acc), true)
    else
      let c = text.[i] in
      if c = '_' then
        if i = pos || i = end_pos - 1 || prev_underscore then ("", false)
        else loop (i + 1) acc true
      else loop (i + 1) (String.make 1 c :: acc) false
  in
  loop pos [] false

let find_sync_point text pos =
  let rec loop i =
    if i >= String.length text then i
    else if is_sync_char text.[i] then i + 1
    else loop (i + 1)
  in
  loop pos

let keyword_table = Hashtbl.create 64

let () =
  let keywords =
    [
      ("and", KwAnd)
    ; ("as", KwAs)
    ; ("break", KwBreak)
    ; ("case", KwCase)
    ; ("cycle", KwCycle)
    ; ("defer", KwDefer)
    ; ("else", KwElse)
    ; ("extern", KwExtern)
    ; ("false", KwFalse)
    ; ("fn", KwFn)
    ; ("for", KwFor)
    ; ("if", KwIf)
    ; ("import", KwImport)
    ; ("in", KwIn)
    ; ("is", KwIs)
    ; ("match", KwMatch)
    ; ("not", KwNot)
    ; ("or", KwOr)
    ; ("record", KwRecord)
    ; ("return", KwReturn)
    ; ("sum", KwSum)
    ; ("true", KwTrue)
    ; ("unsafe", KwUnsafe)
    ; ("val", KwVal)
    ; ("var", KwVar)
    ; ("while", KwWhile)
    ; ("with", KwWith)
    ]
  in
  List.iter (fun (k, v) -> Hashtbl.add keyword_table k v) keywords

let symbols =
  [
    ("..<", DotDotLt)
  ; ("[<", LBrackLt)
  ; (">]", GtRBrack)
  ; ("/=", SlashEq)
  ; ("<=", LtEq)
  ; (">=", GtEq)
  ; ("**", StarStar)
  ; ("|>", BarGt)
  ; ("::", ColonColon)
  ; ("??", QuestionQuestion)
  ; ("..", DotDot)
  ; ("->", MinusGt)
  ; ("<-", LtMinus)
  ; (":=", ColonEq)
  ; ("=>", EqGt)
  ; ("{", LBrace)
  ; ("}", RBrace)
  ; ("[", LBrack)
  ; ("]", RBrack)
  ; ("(", LParen)
  ; (")", RParen)
  ; (",", Comma)
  ; (".", Dot)
  ; (":", Colon)
  ; (";", Semicolon)
  ; ("=", Eq)
  ; ("<", Lt)
  ; (">", Gt)
  ; ("+", Plus)
  ; ("-", Minus)
  ; ("*", Star)
  ; ("/", Slash)
  ; ("&", Amp)
  ; ("|", Bar)
  ; ("^", Caret)
  ; ("~", Tilde)
  ; ("@", At)
  ; ("?", Question)
  ; ("_", Underscore)
  ; ("$", Dollar)
  ]

let symbols_sorted =
  List.sort
    (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
    symbols

let try_scan_quoted_literal lexer pos stop_chars tok_ctor err_msg =
  match scan_quoted_content_opt lexer.text (pos + 1) stop_chars with
  | Some end_pos ->
    let content = extract_content lexer.text (pos + 1) end_pos in
    Reporter.try_ok
      (tok_ctor (intern lexer content), span lexer pos (end_pos + 1))
  | None -> Reporter.try_error_info err_msg (span lexer pos (pos + 1))

let try_scan_string lexer pos =
  try_scan_quoted_literal
    lexer
    pos
    [ '"' ]
    (fun content -> LitString content)
    "unterminated string literal"

let try_check_rune_content lexer pos =
  if pos + 1 >= lexer.text_len || lexer.text.[pos + 1] = '\'' then
    Reporter.try_error_info
      "empty character literal"
      (span lexer pos (pos + if pos + 1 >= lexer.text_len then 1 else 2))
  else if pos + 2 < lexer.text_len then
    match (lexer.text.[pos + 1], lexer.text.[pos + 2]) with
    | '\\', c when pos + 3 < lexer.text_len && lexer.text.[pos + 3] = '\'' ->
      Reporter.try_ok (c, pos + 4)
    | c, '\'' when c <> '\'' -> Reporter.try_ok (c, pos + 3)
    | _, d when d <> '\'' ->
      Reporter.try_error_info
        "multiple characters not allowed in rune literal"
        (span lexer pos (pos + 3))
    | _ ->
      Reporter.try_error_info "invalid rune literal" (span lexer pos (pos + 1))
  else
    Reporter.try_error_info
      "unterminated rune literal"
      (span lexer pos (pos + 1))

let try_scan_rune lexer pos =
  match try_check_rune_content lexer pos with
  | Ok (char, end_pos) -> Reporter.try_ok (LitRune char, span lexer pos end_pos)
  | Error bag -> Error bag

let try_scan_template_content lexer pos offset stop_chars =
  match scan_quoted_content_opt lexer.text (pos + offset) stop_chars with
  | Some end_pos when lexer.text.[end_pos] = '{' ->
    let content = extract_content lexer.text (pos + offset) end_pos in
    Reporter.try_ok (content, end_pos, true)
  | Some end_pos ->
    let content = extract_content lexer.text (pos + offset) end_pos in
    Reporter.try_ok (content, end_pos, false)
  | None ->
    Reporter.try_error_info
      "unterminated template literal"
      (span lexer pos (pos + 1))

let try_scan_template lexer pos offset =
  match try_scan_template_content lexer pos offset [ '"'; '{' ] with
  | Ok (content, end_pos, is_expr) ->
    if is_expr then
      if offset = 2 then (
        lexer.in_template <- true;
        if String.length content = 0 then
          Reporter.try_error_info
            "empty template expression"
            (span lexer pos (end_pos + 1))
        else
          Reporter.try_ok
            (TemplateHead (intern lexer content), span lexer pos (end_pos + 1)))
      else
        Reporter.try_ok
          (TemplateMiddle (intern lexer content), span lexer pos (end_pos + 1))
    else (
      if offset = 0 then lexer.in_template <- false;
      Reporter.try_ok
        (LitTemplateNoSubst (intern lexer content), span lexer pos (end_pos + 1)))
  | Error bag -> Error bag

let try_scan_number lexer =
  let pos = lexer.curr_pos in
  let text = lexer.text in
  let fail msg end_pos = Reporter.try_error_info msg (span lexer pos end_pos) in

  let prefix =
    if pos + 1 < lexer.text_len && text.[pos] = '0' then
      match text.[pos + 1] with
      | 'x' -> Some ("0x", is_xdigit, "hexadecimal")
      | 'o' -> Some ("0o", is_odigit, "octal")
      | 'b' -> Some ("0b", is_bdigit, "binary")
      | _ -> None
    else None
  in

  match prefix with
  | Some (pfx, pred, name) ->
    let end_pos = scan_while pred text (pos + String.length pfx) in
    if end_pos = pos + String.length pfx then
      fail (Printf.sprintf "invalid %s literal" name) (pos + 1)
    else
      let clean, valid =
        handle_underscores text (pos + String.length pfx) end_pos
      in
      if not valid then
        fail (Printf.sprintf "malformed '_' in %s literal" name) end_pos
      else
        Reporter.try_ok
          (LitInt (intern lexer (pfx ^ clean)), span lexer pos end_pos)
  | None ->
    let int_end = scan_while is_digit_or_underscore text pos in
    if int_end <= pos then fail "invalid numeric literal" (pos + 1)
    else
      let clean_int, valid = handle_underscores text pos int_end in
      if not valid then fail "malformed '_' in decimal literal" int_end
      else if int_end >= lexer.text_len || text.[int_end] <> '.' then
        Reporter.try_ok (LitInt (intern lexer clean_int), span lexer pos int_end)
      else
        let frac_end = scan_while is_digit_or_underscore text (int_end + 1) in
        if frac_end = int_end + 1 then
          Reporter.try_ok
            (LitInt (intern lexer clean_int), span lexer pos int_end)
        else
          let has_exp =
            frac_end < lexer.text_len
            && (text.[frac_end] = 'e' || text.[frac_end] = 'E')
          in
          let exp_end =
            if not has_exp then frac_end
            else
              let exp_start = frac_end + 1 in
              let exp_digits =
                if
                  exp_start < lexer.text_len
                  && (text.[exp_start] = '+' || text.[exp_start] = '-')
                then exp_start + 1
                else exp_start
              in
              let exp_end2 =
                scan_while is_digit_or_underscore text exp_digits
              in
              if exp_end2 > exp_digits then
                let _, valid = handle_underscores text exp_digits exp_end2 in
                if valid then exp_end2 else frac_end
              else frac_end
          in
          let clean, _ = handle_underscores text pos exp_end in
          Reporter.try_ok (LitReal (intern lexer clean), span lexer pos exp_end)

let try_scan_ident_or_keyword lexer =
  let end_pos = scan_while is_ident_char lexer.text lexer.curr_pos in
  if end_pos <= lexer.curr_pos then
    Reporter.try_error_info
      "invalid identifier"
      (span lexer lexer.curr_pos (lexer.curr_pos + 1))
  else
    let ident = extract_content lexer.text lexer.curr_pos end_pos in
    let token =
      match Hashtbl.find_opt keyword_table ident with
      | Some kw -> kw
      | None -> Ident (intern lexer ident)
    in
    Reporter.try_ok (token, span lexer lexer.curr_pos end_pos)

let try_scan_ident_escape lexer =
  let pos = lexer.curr_pos in
  match scan_quoted_content_opt lexer.text (pos + 1) [ '`' ] with
  | Some end_pos ->
    let content = extract_content lexer.text (pos + 1) end_pos in
    Reporter.try_ok (Ident (intern lexer content), span lexer pos (end_pos + 1))
  | None ->
    Reporter.try_error_info
      "unterminated escaped identifier"
      (span lexer pos (pos + 1))

let try_scan_symbol lexer =
  let rec try_match = function
    | [] ->
      let c = lexer.text.[lexer.curr_pos] in
      Reporter.try_error_info
        (Printf.sprintf "unknown character '%c'" c)
        (span lexer lexer.curr_pos (lexer.curr_pos + 1))
    | (sym, token) :: rest -> (
      match scan_exact_match_opt sym lexer.text lexer.curr_pos with
      | Some len ->
        Reporter.try_ok (token, span lexer lexer.curr_pos (lexer.curr_pos + len))
      | None -> try_match rest)
  in
  try_match symbols_sorted

let skip_whitespace lexer =
  let end_pos =
    scan_while
      (fun c -> c = ' ' || c = '\t' || c = '\r' || c = '\n')
      lexer.text
      lexer.curr_pos
  in
  advance lexer (end_pos - lexer.curr_pos)

let skip_line_comment lexer =
  let end_pos =
    scan_while (fun c -> c <> '\n') lexer.text (lexer.curr_pos + 2)
  in
  advance lexer (end_pos - lexer.curr_pos)

let try_skip_block_comment_opt lexer =
  let pos = lexer.curr_pos in
  let rec try_scan pos depth =
    if pos + 1 >= lexer.text_len then
      Reporter.try_error_info
        "unterminated block comment"
        (span lexer pos (pos + 1))
    else
      match (lexer.text.[pos], lexer.text.[pos + 1]) with
      | '/', '*' -> try_scan (pos + 2) (depth + 1)
      | '*', '/' when depth = 1 ->
        advance lexer (pos + 2 - lexer.curr_pos);
        Reporter.try_ok ()
      | '*', '/' -> try_scan (pos + 2) (depth - 1)
      | _ -> try_scan (pos + 1) depth
  in
  match try_scan (pos + 2) 1 with
  | Ok () -> None
  | Error bag -> Some (Error bag)

let rec try_scan_token_opt lexer =
  if lexer.curr_pos >= lexer.text_len then
    Some (Reporter.try_ok (EOF, span lexer lexer.curr_pos lexer.text_len))
  else if lexer.in_template then
    match lexer.text.[lexer.curr_pos] with
    | '}' ->
      advance lexer 1;
      Some (try_scan_template lexer lexer.curr_pos 0)
    | _ -> Some (try_scan_symbol lexer)
  else
    match lexer.text.[lexer.curr_pos] with
    | ' ' | '\t' | '\r' | '\n' ->
      skip_whitespace lexer;
      None
    | '/' when lexer.curr_pos + 1 < lexer.text_len -> (
      match lexer.text.[lexer.curr_pos + 1] with
      | '/' ->
        skip_line_comment lexer;
        None
      | '*' -> (
        match try_skip_block_comment_opt lexer with
        | None -> None
        | Some error -> Some error)
      | _ -> Some (try_scan_symbol lexer))
    | '$'
      when lexer.curr_pos + 1 < lexer.text_len
           && lexer.text.[lexer.curr_pos + 1] = '"' ->
      Some (try_scan_template lexer lexer.curr_pos 2)
    | '"' -> Some (try_scan_string lexer lexer.curr_pos)
    | '\'' -> Some (try_scan_rune lexer lexer.curr_pos)
    | '`' -> Some (try_scan_ident_escape lexer)
    | '_'
      when lexer.curr_pos + 1 = lexer.text_len
           || not (is_ident_char lexer.text.[lexer.curr_pos + 1]) ->
      Some (try_scan_symbol lexer)
    | c when is_ident_start c -> Some (try_scan_ident_or_keyword lexer)
    | c when is_digit c -> Some (try_scan_number lexer)
    | _ -> Some (try_scan_symbol lexer)

and try_next_token lexer =
  match try_scan_token_opt lexer with
  | Some (Ok (token, span)) ->
    advance lexer (span.end_ - span.start);
    Ok (token, span)
  | Some (Error bag) ->
    advance lexer 1;
    Error bag
  | None ->
    if lexer.curr_pos >= lexer.text_len then
      Ok (EOF, span lexer lexer.curr_pos lexer.text_len)
    else try_next_token lexer

let try_tokenize lexer =
  let tokens = ref [] in
  let errors = ref [] in
  let orig_pos = lexer.curr_pos in

  while lexer.curr_pos < lexer.text_len do
    match try_scan_token_opt lexer with
    | Some (Ok (token, span)) -> tokens := (token, span) :: !tokens
    | Some (Error bag) ->
      errors := !errors @ Reporter.to_list bag;
      let sync_pos = find_sync_point lexer.text lexer.curr_pos in
      lexer.curr_pos <- sync_pos
    | None -> ()
  done;

  lexer.curr_pos <- orig_pos;

  if !errors = [] then Ok (List.rev !tokens)
  else
    let error_bags =
      lexer.error_bag
      :: List.map
           (fun diag -> { Reporter.diags = [ diag ]; errors = 1; warnings = 0 })
           !errors
    in
    Error (Reporter.merge error_bags)

let token_stream_opt lexer =
  let done_flag = ref false in
  let next_state_opt () =
    if !done_flag then None
    else
      match try_next_token lexer with
      | Ok (Token.EOF, span) ->
        done_flag := true;
        Some ((Token.EOF, span), ())
      | Ok (token, span) -> Some ((token, span), ())
      | Error _ -> None
  in
  Some (Seq.unfold next_state_opt ())

let emit_errors fmt lexer =
  Reporter.emit_all fmt lexer.error_bag [ (lexer.file_id, lexer.source) ]
