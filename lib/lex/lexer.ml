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

let scan_exact_opt expected text pos =
  let len = String.length expected in
  if pos + len <= String.length text && String.sub text pos len = expected then
    Some len
  else None

let rec scan_string_content_opt text pos =
  if pos >= String.length text then None
  else
    match text.[pos] with
    | '"' -> Some pos
    | '\\' when pos + 1 < String.length text ->
      scan_string_content_opt text (pos + 2)
    | _ -> scan_string_content_opt text (pos + 1)

let is_digit c = c >= '0' && c <= '9'
let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_odigit c = c >= '0' && c <= '7'
let is_bdigit c = c = '0' || c = '1'

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
  List.iter
    (fun (k, v) -> Hashtbl.add keyword_table k v)
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

let symbols =
  [
    ("..<", DotDotLt)
  ; ("|>", BarGt)
  ; ("**", StarStar)
  ; ("/=", SlashEq)
  ; ("<=", LtEq)
  ; (">=", GtEq)
  ; ("::", ColonColon)
  ; ("??", QuestionQuestion)
  ; ("..", DotDot)
  ; ("->", MinusGt)
  ; ("=>", EqGt)
  ; ("$", Dollar)
  ; ("+", Plus)
  ; ("-", Minus)
  ; ("*", Star)
  ; ("/", Slash)
  ; ("&", Amp)
  ; ("|", Bar)
  ; ("^", Caret)
  ; ("~", Tilde)
  ; ("@", At)
  ; ("<", Lt)
  ; (">", Gt)
  ; ("=", Eq)
  ; ("?", Question)
  ; (":", Colon)
  ; (";", Semicolon)
  ; (",", Comma)
  ; (".", Dot)
  ; ("_", Underscore)
  ; ("(", LParen)
  ; (")", RParen)
  ; ("[", LBrack)
  ; ("]", RBrack)
  ; ("{", LBrace)
  ; ("}", RBrace)
  ]

let try_scan_string lexer pos =
  match scan_string_content_opt lexer.text (pos + 1) with
  | Some end_pos ->
    let content = String.sub lexer.text (pos + 1) (end_pos - pos - 1) in
    Reporter.try_ok
      (LitString (intern lexer content), span lexer pos (end_pos + 1))
  | None ->
    Reporter.try_error_info
      "unterminated string literal"
      (span lexer pos (pos + 1))

let try_scan_rune lexer pos =
  if pos + 2 < lexer.text_len then
    match (lexer.text.[pos + 1], lexer.text.[pos + 2]) with
    | '\\', c when pos + 3 < lexer.text_len && lexer.text.[pos + 3] = '\'' ->
      Reporter.try_ok (LitRune c, span lexer pos (pos + 4))
    | c, '\'' when c <> '\'' ->
      Reporter.try_ok (LitRune c, span lexer pos (pos + 3))
    | _ ->
      Reporter.try_error_info "invalid rune literal" (span lexer pos (pos + 1))
  else
    Reporter.try_error_info
      "unterminated rune literal"
      (span lexer pos (pos + 1))

let try_scan_template lexer pos =
  match scan_string_content_opt lexer.text (pos + 2) with
  | Some end_pos when end_pos < lexer.text_len && lexer.text.[end_pos] = '{' ->
    let content = String.sub lexer.text (pos + 2) (end_pos - pos - 2) in
    Reporter.try_ok
      (TemplateHead (intern lexer content), span lexer pos (end_pos + 1))
  | Some end_pos ->
    let content = String.sub lexer.text (pos + 2) (end_pos - pos - 2) in
    Reporter.try_ok
      (LitTemplateNoSubst (intern lexer content), span lexer pos (end_pos + 1))
  | None ->
    Reporter.try_error_info
      "unterminated template literal"
      (span lexer pos (pos + 1))

let try_scan_number_with_base prefix digit_chars base_name lexer pos =
  let end_pos = scan_while digit_chars lexer.text (pos + 2) in
  if end_pos = pos + 2 then
    Reporter.try_error_info
      (Printf.sprintf "invalid %s literal" base_name)
      (span lexer pos (pos + 1))
  else
    let lit = prefix ^ String.sub lexer.text pos (end_pos - pos) in
    Reporter.try_ok (LitInt (intern lexer lit), span lexer pos end_pos)

let try_scan_decimal_or_float lexer pos =
  let int_end = scan_while is_digit lexer.text pos in
  if int_end < lexer.text_len && lexer.text.[int_end] = '.' then
    let frac_end = scan_while is_digit lexer.text (int_end + 1) in
    if frac_end = int_end + 1 then
      let int_lit = String.sub lexer.text pos (int_end - pos) in
      Reporter.try_ok (LitInt (intern lexer int_lit), span lexer pos int_end)
    else
      let real = String.sub lexer.text pos (frac_end - pos) in
      Reporter.try_ok (LitReal (intern lexer real), span lexer pos frac_end)
  else if int_end > pos then
    let int_lit = String.sub lexer.text pos (int_end - pos) in
    Reporter.try_ok (LitInt (intern lexer int_lit), span lexer pos int_end)
  else
    Reporter.try_error_info "invalid numeric literal" (span lexer pos (pos + 1))

let try_scan_number lexer =
  let pos = lexer.curr_pos in
  if pos + 1 < lexer.text_len then
    match (lexer.text.[pos], lexer.text.[pos + 1]) with
    | '0', 'x' ->
      try_scan_number_with_base "0x" is_xdigit "hexadecimal" lexer pos
    | '0', 'o' -> try_scan_number_with_base "0o" is_odigit "octal" lexer pos
    | '0', 'b' -> try_scan_number_with_base "0b" is_bdigit "binary" lexer pos
    | _ -> try_scan_decimal_or_float lexer pos
  else try_scan_decimal_or_float lexer pos

let try_scan_identifier_or_keyword lexer =
  let end_pos = scan_while is_ident_char lexer.text lexer.curr_pos in
  if end_pos > lexer.curr_pos then
    let ident =
      String.sub lexer.text lexer.curr_pos (end_pos - lexer.curr_pos)
    in
    let token =
      match Hashtbl.find_opt keyword_table ident with
      | Some kw -> kw
      | None -> Ident (intern lexer ident)
    in
    Reporter.try_ok (token, span lexer lexer.curr_pos end_pos)
  else
    Reporter.try_error_info
      (Printf.sprintf
         "invalid identifier '%s'"
         (String.sub lexer.text lexer.curr_pos 1))
      (span lexer lexer.curr_pos (lexer.curr_pos + 1))

let try_scan_symbol lexer =
  let symbols_sorted =
    List.sort
      (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
      symbols
  in
  let rec try_match = function
    | [] ->
      let c = lexer.text.[lexer.curr_pos] in
      Reporter.try_error_info
        (Printf.sprintf "unknown character '%c'" c)
        (span lexer lexer.curr_pos (lexer.curr_pos + 1))
    | (sym, token) :: rest -> (
      match scan_exact_opt sym lexer.text lexer.curr_pos with
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
  let rec loop pos =
    if pos >= lexer.text_len || lexer.text.[pos] = '\n' then pos
    else loop (pos + 1)
  in
  let end_pos = loop (lexer.curr_pos + 2) in
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
    | c when is_ident_start c -> Some (try_scan_identifier_or_keyword lexer)
    | c when is_digit c -> Some (try_scan_number lexer)
    | _ -> Some (try_scan_symbol lexer)

and try_next_token lexer =
  match try_scan_token_opt lexer with
  | Some result -> result
  | None -> try_next_token lexer

let try_tokenize lexer =
  let tokens = ref [] in
  let errors = ref [] in
  let original_pos = lexer.curr_pos in

  while lexer.curr_pos < lexer.text_len do
    match try_scan_token_opt lexer with
    | Some (Ok (token, span)) -> tokens := (token, span) :: !tokens
    | Some (Error bag) ->
      errors := !errors @ Reporter.to_list bag;
      let sync_pos = find_sync_point lexer.text lexer.curr_pos in
      lexer.curr_pos <- sync_pos
    | None -> ()
  done;

  lexer.curr_pos <- original_pos;

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
  let next_state () =
    match try_next_token lexer with
    | Ok (token, span) -> Some ((token, span), ())
    | Error _ -> None
  in
  Some (Seq.unfold next_state ())

let emit_errors fmt lexer =
  Reporter.emit_all fmt lexer.error_bag [ (lexer.file_id, lexer.source) ]
