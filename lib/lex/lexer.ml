(* ========================================
   LEXER STATE
   ======================================== *)

type t = {
    file_id : Span.file_id
  ; filename : string
       [@warning "-69"] (* TODO(@xsyetopz): maybe read this IF needed? *)
  ; source : string
  ; len : int
  ; mutable pos : int
  ; interner : Interner.t
  ; diags : Diagnostic.bag ref
}

let make file_id filename source interner =
  {
    file_id
  ; filename
  ; source
  ; len = String.length source
  ; pos = 0
  ; interner
  ; diags = ref Diagnostic.empty_bag
  }

(* ========================================
   POSITION AND NAVIGATION
   ======================================== *)

let at_end t = t.pos >= t.len [@@inline]
let curr_char t = if t.pos < t.len then t.source.[t.pos] else '\000' [@@inline]

let peek_char t n =
  let pos = t.pos + n in
  if pos < t.len then t.source.[pos] else '\000'
[@@inline]

let advance t = if t.pos < t.len then t.pos <- t.pos + 1 [@@inline]
let advance_by t n = t.pos <- min (t.pos + n) t.len

(* ========================================
   DIAGNOSTICS
   ======================================== *)

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

let warning t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.warning msg span)

let make_span t start = Span.make t.file_id start t.pos

(* ========================================
   CHARACTER PREDICATES
   ======================================== *)

let is_whitespace = function ' ' | '\t' | '\r' -> true | _ -> false [@@inline]
let is_newline = function '\n' -> true | _ -> false [@@inline]

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
[@@inline]

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
[@@inline]

let is_digit = function '0' .. '9' -> true | _ -> false [@@inline]
let is_bin_digit = function '0' | '1' -> true | _ -> false [@@inline]
let is_oct_digit = function '0' .. '7' -> true | _ -> false [@@inline]

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
[@@inline]

(* ========================================
   LOOKUP TABLES
   ======================================== *)

let keywords =
  let tbl = Hashtbl.create 42 in
  List.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [
      ("alias", Token.KwAlias)
    ; ("and", Token.KwAnd)
    ; ("as", Token.KwAs)
    ; ("async", Token.KwAsync)
    ; ("await", Token.KwAwait)
    ; ("break", Token.KwBreak)
    ; ("case", Token.KwCase)
    ; ("choice", Token.KwChoice)
    ; ("const", Token.KwConst)
    ; ("continue", Token.KwContinue)
    ; ("defer", Token.KwDefer)
    ; ("do", Token.KwDo)
    ; ("else", Token.KwElse)
    ; ("export", Token.KwExport)
    ; ("extern", Token.KwExtern)
    ; ("false", Token.KwFalse)
    ; ("for", Token.KwFor)
    ; ("from", Token.KwFrom)
    ; ("if", Token.KwIf)
    ; ("import", Token.KwImport)
    ; ("in", Token.KwIn)
    ; ("is", Token.KwIs)
    ; ("match", Token.KwMatch)
    ; ("mod", Token.KwMod)
    ; ("not", Token.KwNot)
    ; ("or", Token.KwOr)
    ; ("proc", Token.KwProc)
    ; ("record", Token.KwRecord)
    ; ("return", Token.KwReturn)
    ; ("shl", Token.KwShl)
    ; ("shr", Token.KwShr)
    ; ("then", Token.KwThen)
    ; ("true", Token.KwTrue)
    ; ("try", Token.KwTry)
    ; ("unsafe", Token.KwUnsafe)
    ; ("var", Token.KwVar)
    ; ("weak", Token.KwWeak)
    ; ("where", Token.KwWhere)
    ; ("while", Token.KwWhile)
    ; ("with", Token.KwWith)
    ; ("xor", Token.KwXor)
    ; ("yield", Token.KwYield)
    ];
  tbl

let operators =
  [
    ("=/=", Token.EqSlashEq)
  ; ("..<", Token.DotDotLt)
  ; (":=", Token.ColonEq)
  ; ("<-", Token.LtMinus)
  ; ("->", Token.MinusGt)
  ; ("<=", Token.LtEq)
  ; (">=", Token.GtEq)
  ; ("..", Token.DotDot)
  ; ("+", Token.Plus)
  ; ("-", Token.Minus)
  ; ("*", Token.Star)
  ; ("/", Token.Slash)
  ; ("^", Token.Caret)
  ; ("=", Token.Eq)
  ; ("<", Token.Lt)
  ; (">", Token.Gt)
  ; ("(", Token.LParen)
  ; (")", Token.RParen)
  ; ("[", Token.LBrack)
  ; ("]", Token.RBrack)
  ; ("{", Token.LBrace)
  ; ("}", Token.RBrace)
  ; (",", Token.Comma)
  ; (".", Token.Dot)
  ; (":", Token.Colon)
  ; (";", Token.Semi)
  ; ("@", Token.At)
  ; ("?", Token.Question)
  ; ("!", Token.Bang)
  ; ("$", Token.Dollar)
  ]

let suffixes =
  let tbl = Hashtbl.create 13 in
  List.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [
      ("i8", Token.I8)
    ; ("i16", Token.I16)
    ; ("i32", Token.I32)
    ; ("i64", Token.I64)
    ; ("i128", Token.I128)
    ; ("n8", Token.N8)
    ; ("n16", Token.N16)
    ; ("n32", Token.N32)
    ; ("n64", Token.N64)
    ; ("n128", Token.N128)
    ; ("b16", Token.B16)
    ; ("b32", Token.B32)
    ; ("b64", Token.B64)
    ];
  tbl

(* ========================================
   WHITESPACE & NEWLINES
   ======================================== *)

let lex_whitespace t =
  let start = t.pos in
  while is_whitespace (curr_char t) do
    advance t
  done;
  Token.make Token.Whitespace (make_span t start)

let lex_newline t =
  let start = t.pos in
  advance t;
  Token.make Token.Newline (make_span t start)

(* ========================================
   COMMENTS
   ======================================== *)

let lex_line_comment t =
  let start = t.pos in
  advance_by t 2;
  while (not (at_end t)) && not (is_newline (curr_char t)) do
    advance t
  done;
  let content = String.sub t.source (start + 2) (t.pos - start - 2) in
  Token.make
    (Token.LineComment (Interner.intern t.interner content))
    (make_span t start)

let lex_block_comment t =
  let start = t.pos in
  advance_by t 2;
  let docstyle = curr_char t = '*' in
  if docstyle then advance t;
  let depth = ref 1 in
  while !depth > 0 && not (at_end t) do
    if curr_char t = '/' && peek_char t 1 = '*' then (
      advance_by t 2;
      incr depth)
    else if curr_char t = '*' && peek_char t 1 = '/' then (
      advance_by t 2;
      decr depth)
    else advance t
  done;
  if !depth > 0 then error t "unterminated block comment" (make_span t start);
  let len = t.pos - start - 4 - if docstyle then 1 else 0 in
  let content_start = start + 2 + if docstyle then 1 else 0 in
  let content = String.sub t.source content_start len in
  Token.make
    (Token.BlockComment
       { content = Interner.intern t.interner content; docstyle })
    (make_span t start)

(* ========================================
   IDENTIFIERS & KEYWORDS
   ======================================== *)

let lex_ident_or_keyword t =
  let start = t.pos in
  while is_ident_continue (curr_char t) do
    advance t
  done;
  let text = String.sub t.source start (t.pos - start) in
  let kind =
    if text = "_" then Token.Underscore
    else
      match Hashtbl.find_opt keywords text with
      | Some kw -> kw
      | None -> Token.Ident (Interner.intern t.interner text)
  in
  Token.make kind (make_span t start)

(* ========================================
   NUMERIC LITERALS
   ======================================== *)

let scan_digits t pred =
  let has_consecutive = ref false in
  while pred (curr_char t) || curr_char t = '_' do
    if curr_char t = '_' && peek_char t 1 = '_' then has_consecutive := true;
    advance t
  done;
  !has_consecutive

let lex_numeric_base t start =
  advance t;
  match curr_char t with
  | 'b' | 'B' ->
    advance t;
    scan_digits t is_bin_digit
  | 'o' | 'O' ->
    advance t;
    if curr_char t = 'O' then
      warning
        t
        "uppercase 'O' in octal prefix may be confused with zero"
        (make_span t start);
    scan_digits t is_oct_digit
  | 'x' | 'X' ->
    advance t;
    scan_digits t is_hex_digit
  | '0' .. '9' ->
    error t "leading zeros not allowed in decimal literals" (make_span t start);
    scan_digits t is_digit
  | _ -> false

let lex_numeric_suffix t start =
  if not (is_ident_start (curr_char t)) then None
  else
    let suffix_start = t.pos in
    while is_ident_continue (curr_char t) do
      advance t
    done;
    let suffix_text = String.sub t.source suffix_start (t.pos - suffix_start) in
    match Hashtbl.find_opt suffixes suffix_text with
    | Some s -> Some s
    | None ->
      error
        t
        (Printf.sprintf "invalid numeric suffix '%s'" suffix_text)
        (make_span t start);
      None

let lex_lit_numeric t =
  let start = t.pos in
  let has_consecutive =
    if curr_char t = '0' then lex_numeric_base t start
    else scan_digits t is_digit
  in
  if curr_char t = '.' && is_digit (peek_char t 1) then (
    advance t;
    ignore (scan_digits t is_digit));
  if curr_char t = 'e' || curr_char t = 'E' then (
    advance t;
    if curr_char t = '+' || curr_char t = '-' then advance t;
    ignore (scan_digits t is_digit));
  let suffix = lex_numeric_suffix t start in
  if has_consecutive then
    warning t "consecutive underscores in numeric literal" (make_span t start);
  let text = String.sub t.source start (t.pos - start) in
  Token.make (Token.LitNumeric (text, suffix)) (make_span t start)

(* ========================================
   TEXT LITERALS
   ======================================== *)

let handle_escape t buf start =
  let add_and_continue ch =
    Buffer.add_char buf ch;
    advance t
  in
  match curr_char t with
  | 'n' -> add_and_continue '\n'
  | 't' -> add_and_continue '\t'
  | 'r' -> add_and_continue '\r'
  | '\\' -> add_and_continue '\\'
  | '"' -> add_and_continue '"'
  | '0' -> add_and_continue '\000'
  | 'x' ->
    advance t;
    if is_hex_digit (curr_char t) && is_hex_digit (peek_char t 1) then (
      let h1 = curr_char t in
      advance t;
      let h2 = curr_char t in
      advance t;
      let hex_str = String.init 2 (function 0 -> h1 | _ -> h2) in
      let code = int_of_string ("0x" ^ hex_str) in
      Buffer.add_char buf (Char.chr code))
    else
      error
        t
        (Printf.sprintf
           "invalid hex escape sequence '%c%c'"
           (curr_char t)
           (peek_char t 1))
        (make_span t start)
  | c ->
    error
      t
      (Printf.sprintf "unknown escape sequence '\\%c'" c)
      (make_span t start);
    advance t

let lex_lit_text t =
  let start = t.pos in
  advance t;
  let buf = Buffer.create 16 in
  let rec loop () =
    if at_end t then
      error t "missing closing '\"' for text literal" (make_span t start)
    else if curr_char t = '"' then advance t
    else if curr_char t = '\\' then (
      advance t;
      handle_escape t buf start;
      loop ())
    else (
      Buffer.add_char buf (curr_char t);
      advance t;
      loop ())
  in
  loop ();
  Token.make
    (Token.LitText (Interner.intern t.interner (Buffer.contents buf)))
    (make_span t start)

(* ========================================
   OPERATORS
   ======================================== *)

let matches_at t op_str =
  let len = String.length op_str in
  let rec check i =
    if i >= len then true
    else if peek_char t i <> op_str.[i] then false
    else check (i + 1)
  in
  check 0

let lex_operator t =
  let start = t.pos in
  let rec try_ops = function
    | [] -> None
    | (op_str, tok_kind) :: rest ->
      if matches_at t op_str then (
        advance_by t (String.length op_str);
        Some (Token.make tok_kind (make_span t start)))
      else try_ops rest
  in
  try_ops operators

(* ========================================
   MAIN LEXER LOOP
   ======================================== *)

let lex_token t =
  let start = t.pos in
  let c = curr_char t in
  match c with
  | ' ' | '\t' | '\r' -> lex_whitespace t
  | '\n' -> lex_newline t
  | '/' when peek_char t 1 = '/' -> lex_line_comment t
  | '/' when peek_char t 1 = '*' -> lex_block_comment t
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> lex_ident_or_keyword t
  | '0' .. '9' -> lex_lit_numeric t
  | '"' -> lex_lit_text t
  | '\000' -> Token.make Token.Eof (make_span t start)
  | _ -> (
    match lex_operator t with
    | Some tok -> tok
    | None ->
      error t (Printf.sprintf "invalid character '%c'" c) (make_span t start);
      advance t;
      Token.make Token.Error (make_span t start))

let lex t =
  let rec loop acc =
    if at_end t then List.rev (Token.make Token.Eof (make_span t t.pos) :: acc)
    else loop (lex_token t :: acc)
  in
  let tokens = loop [] in
  let bag = !(t.diags) in
  (tokens, bag)
