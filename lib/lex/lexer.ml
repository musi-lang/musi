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

let create source file_id =
  let text = Source.text source in
  {
    interner = Interner.create ()
  ; error_bag = Reporter.empty_bag
  ; source
  ; file_id
  ; curr_pos = 0
  ; text
  ; text_len = String.length text
  ; in_template = false
  }

let with_interner interner source file_id =
  let text = Source.text source in
  {
    interner
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

let make_char_checker base_chars c = List.mem c base_chars

let make_digit_checker base_chars c =
  (c >= '0' && c <= '9') || make_char_checker base_chars c

let is_digit c = c >= '0' && c <= '9'

let is_xdigit =
  make_digit_checker
    [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' ]

let is_odigit = make_digit_checker [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7' ]
let is_bdigit = make_digit_checker [ '0'; '1' ]
let is_digit_or_underscore c = is_digit c || c = '_'

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

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char c = is_ident_start c || is_digit c
let is_sync_char c = c = ';' || c = '\n' || c = '}' || c = ')' || c = ']'

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

let try_scan_template lexer pos =
  match try_scan_template_content lexer pos 2 [ '"'; '{' ] with
  | Ok (content, end_pos, is_expr) ->
    if is_expr then (
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
        (LitTemplateNoSubst (intern lexer content), span lexer pos (end_pos + 1))
  | Error bag -> Error bag

let try_scan_template_cont lexer pos =
  match try_scan_template_content lexer pos 0 [ '"'; '{' ] with
  | Ok (content, end_pos, is_expr) ->
    if is_expr then
      Reporter.try_ok
        (TemplateMiddle (intern lexer content), span lexer pos (end_pos + 1))
    else (
      lexer.in_template <- false;
      Reporter.try_ok
        (TemplateTail (intern lexer content), span lexer pos (end_pos + 1)))
  | Error bag -> Error bag

let try_scan_number_with_base prefix digit_chars base_name lexer pos =
  let end_pos = scan_while digit_chars lexer.text (pos + 2) in
  if end_pos = pos + 2 then
    Reporter.try_error_info
      (Printf.sprintf "invalid %s literal" base_name)
      (span lexer pos (pos + 1))
  else
    let digit_start = pos + 2 in
    let clean_digits, valid =
      handle_underscores lexer.text digit_start end_pos
    in
    if not valid then
      Reporter.try_error_info
        (Printf.sprintf "malformed '_' placement in %s literal" base_name)
        (span lexer pos end_pos)
    else
      let lit = prefix ^ clean_digits in
      Reporter.try_ok (LitInt (intern lexer lit), span lexer pos end_pos)

let scan_integer_part_opt lexer pos =
  let int_end = scan_while is_digit_or_underscore lexer.text pos in
  if int_end <= pos then None
  else
    let clean_int, valid = handle_underscores lexer.text pos int_end in
    if valid then Some (clean_int, int_end) else None

let try_scan_exponent_part lexer pos =
  if
    pos >= lexer.text_len
    || not (lexer.text.[pos] = 'e' || lexer.text.[pos] = 'E')
  then Reporter.try_ok pos
  else
    let exp_start = pos + 1 in
    let exp_digits_start =
      if
        exp_start < lexer.text_len
        && (lexer.text.[exp_start] = '+' || lexer.text.[exp_start] = '-')
      then exp_start + 1
      else exp_start
    in
    let exp_end =
      scan_while is_digit_or_underscore lexer.text exp_digits_start
    in
    if exp_end > exp_digits_start then
      let _, exp_valid =
        handle_underscores lexer.text exp_digits_start exp_end
      in
      if exp_valid then Reporter.try_ok exp_end else Reporter.try_ok pos
    else Reporter.try_ok pos

let try_scan_int_or_real lexer pos =
  match scan_integer_part_opt lexer pos with
  | None ->
    Reporter.try_error_info "invalid numeric literal" (span lexer pos (pos + 1))
  | Some (clean_int, int_end) -> (
    if int_end >= lexer.text_len || lexer.text.[int_end] <> '.' then
      Reporter.try_ok (LitInt (intern lexer clean_int), span lexer pos int_end)
    else
      let frac_end =
        scan_while is_digit_or_underscore lexer.text (int_end + 1)
      in
      if frac_end = int_end + 1 then
        Reporter.try_ok (LitInt (intern lexer clean_int), span lexer pos int_end)
      else
        match try_scan_exponent_part lexer frac_end with
        | Error _ ->
          Reporter.try_ok
            (LitInt (intern lexer clean_int), span lexer pos int_end)
        | Ok real_end ->
          let dec_value, _ = handle_underscores lexer.text pos real_end in
          Reporter.try_ok
            (LitReal (intern lexer dec_value), span lexer pos real_end))

let try_scan_number lexer =
  let pos = lexer.curr_pos in
  if pos + 1 < lexer.text_len then
    match (lexer.text.[pos], lexer.text.[pos + 1]) with
    | '0', 'x' ->
      try_scan_number_with_base "0x" is_xdigit "hexadecimal" lexer pos
    | '0', 'o' -> try_scan_number_with_base "0o" is_odigit "octal" lexer pos
    | '0', 'b' -> try_scan_number_with_base "0b" is_bdigit "binary" lexer pos
    | _ -> try_scan_int_or_real lexer pos
  else try_scan_int_or_real lexer pos

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

let rec try_skip_block_comment_opt lexer pos depth =
  if pos + 1 >= lexer.text_len then
    Some
      (Reporter.try_error_info
         "unterminated block comment"
         (span lexer pos (pos + 1)))
  else
    match (lexer.text.[pos], lexer.text.[pos + 1]) with
    | '/', '*' -> try_skip_block_comment_opt lexer (pos + 2) (depth + 1)
    | '*', '/' when depth = 1 ->
      advance lexer (pos + 2 - lexer.curr_pos);
      None
    | '*', '/' -> try_skip_block_comment_opt lexer (pos + 2) (depth - 1)
    | _ -> try_skip_block_comment_opt lexer (pos + 1) depth

let rec try_scan_token_opt lexer =
  if lexer.curr_pos >= lexer.text_len then
    Some (Reporter.try_ok (EOF, span lexer lexer.curr_pos lexer.text_len))
  else if lexer.in_template then
    match lexer.text.[lexer.curr_pos] with
    | '}' ->
      advance lexer 1;
      Some (try_scan_template_cont lexer lexer.curr_pos)
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
      | '*' -> try_skip_block_comment_opt lexer lexer.curr_pos 1
      | _ -> Some (try_scan_symbol lexer))
    | '$'
      when lexer.curr_pos + 1 < lexer.text_len
           && lexer.text.[lexer.curr_pos + 1] = '"' ->
      Some (try_scan_template lexer lexer.curr_pos)
    | '"' -> Some (try_scan_string lexer lexer.curr_pos)
    | '\'' -> Some (try_scan_rune lexer lexer.curr_pos)
    | '`' -> Some (try_scan_ident_escape lexer)
    | c when is_ident_start c -> Some (try_scan_ident_or_keyword lexer)
    | c when is_digit c -> Some (try_scan_number lexer)
    | _ -> Some (try_scan_symbol lexer)

and try_next_token lexer =
  match try_scan_token_opt lexer with
  | Some result -> result
  | None -> try_next_token lexer

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
  let next_state_opt () =
    match try_next_token lexer with
    | Ok (token, span) -> Some ((token, span), ())
    | Error _ -> None
  in
  Some (Seq.unfold next_state_opt ())

let emit_errors fmt lexer =
  Reporter.emit_all fmt lexer.error_bag [ (lexer.file_id, lexer.source) ]
