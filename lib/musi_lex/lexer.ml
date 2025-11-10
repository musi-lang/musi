open Musi_basic

type t = {
    file_id : Span.file_id
  ; source : string
  ; len : int
  ; mutable pos : int
  ; interner : Interner.t
  ; diags : Diagnostic.bag ref
}

let make file_id source interner =
  {
    file_id
  ; source
  ; len = String.length source
  ; pos = 0
  ; interner
  ; diags = ref Diagnostic.empty_bag
  }

let at_end t = t.pos >= t.len
let curr t = if at_end t then '\000' else t.source.[t.pos]

let peek t n =
  let p = t.pos + n in
  if p < t.len then t.source.[p] else '\000'

let advance t = if t.pos < t.len then t.pos <- t.pos + 1
let advance_by t n = t.pos <- min (t.pos + n) t.len
let span t start = Span.make t.file_id start t.pos

let error t msg sp =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg sp)

let scan_while t pred =
  while pred (curr t) do
    advance t
  done

let intern_slice t start len =
  Interner.intern t.interner (String.sub t.source start len)

let matches_at t s =
  let rec check i =
    i >= String.length s || (peek t i = s.[i] && check (i + 1))
  in
  check 0

let is_whitespace = function ' ' | '\t' | '\r' -> true | _ -> false

let is_ident_cont = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_xdigit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let digit_or_underscore c = is_digit c || c = '_'
let slice_from t start = String.sub t.source start (t.pos - start)
let make_tok t kind start = Token.make kind (span t start)

let keywords =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [
      ("and", Token.KwAnd)
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

let lex_ident t =
  let start = t.pos in
  scan_while t is_ident_cont;
  let text = slice_from t start in
  let kind =
    if text = "_" then Token.Underscore
    else
      match Hashtbl.find_opt keywords text with
      | Some kw -> kw
      | None -> Token.Ident (Interner.intern t.interner text)
  in
  make_tok t kind start

let lex_radix t pred =
  advance_by t 2;
  scan_while t (fun c -> pred c || c = '_')

let lex_lit_num t =
  let start = t.pos in
  let c1 = peek t 1 in
  if curr t = '0' && (c1 = 'x' || c1 = 'X') then lex_radix t is_xdigit
  else if curr t = '0' && (c1 = 'b' || c1 = 'B') then
    lex_radix t (function '0' | '1' -> true | _ -> false)
  else if curr t = '0' && (c1 = 'o' || c1 = 'O') then
    lex_radix t (function '0' .. '7' -> true | _ -> false)
  else (
    if curr t = '0' && is_digit (peek t 1) then
      error t "leading zero(s) not allowed" (span t start);
    scan_while t digit_or_underscore;
    if curr t = '.' && is_digit (peek t 1) then (
      advance t;
      scan_while t digit_or_underscore);
    if curr t = 'e' || curr t = 'E' then (
      advance t;
      if curr t = '+' || curr t = '-' then advance t;
      scan_while t digit_or_underscore));
  if is_ident_cont (curr t) then (
    let suffix_start = t.pos in
    scan_while t is_ident_cont;
    let suffix = slice_from t suffix_start in
    error t (Printf.sprintf "invalid numeric suffix '%s'" suffix) (span t start));
  make_tok t (Token.LitNum (slice_from t start)) start

let escapes =
  [
    ('n', '\n')
  ; ('t', '\t')
  ; ('r', '\r')
  ; ('b', '\b')
  ; ('\\', '\\')
  ; ('\'', '\'')
  ; ('"', '"')
  ; ('0', '\000')
  ]

let lex_xescape t =
  let h1 = curr t in
  advance t;
  let h2 = curr t in
  advance t;
  if is_xdigit h1 && is_xdigit h2 then
    Some (Char.chr (int_of_string (Printf.sprintf "0x%c%c" h1 h2)))
  else (
    error
      t
      (Printf.sprintf "invalid hexadecimal escape sequence '\\x%c%c'" h1 h2)
      (span t (t.pos - 4));
    None)

let lex_escape t =
  advance t;
  match curr t with
  | 'x' ->
    advance t;
    lex_xescape t
  | c -> (
    match List.assoc_opt c escapes with
    | Some ch ->
      advance t;
      Some ch
    | None ->
      error
        t
        (Printf.sprintf "unknown escape sequence '\\%c'" c)
        (span t (t.pos - 1));
      advance t;
      None)

let lex_lit_str t =
  let start = t.pos in
  advance t;
  let buf = Buffer.create 16 in
  let rec loop () =
    if at_end t then error t "unterminated text literal" (span t start)
    else
      match curr t with
      | '"' -> advance t
      | '\\' ->
        (match lex_escape t with
        | Some ch -> Buffer.add_char buf ch
        | None -> ());
        loop ()
      | c ->
        Buffer.add_char buf c;
        advance t;
        loop ()
  in
  loop ();
  Token.make
    (Token.LitStr (Interner.intern t.interner (Buffer.contents buf)))
    (span t start)

let lex_lit_rune t =
  let start = t.pos in
  advance t;
  if at_end t then (
    error t "unterminated rune literal" (span t start);
    Token.make Token.Error (span t start))
  else
    let code =
      match curr t with
      | '\\' -> ( match lex_escape t with Some ch -> Char.code ch | None -> 0)
      | c ->
        advance t;
        Char.code c
    in
    if curr t <> '\'' then
      error t "missing closing '\'' in rune literal" (span t start);
    advance t;
    Token.make (Token.LitRune code) (span t start)

let lex_line_comment t =
  let start = t.pos in
  advance_by t 2;
  scan_while t (fun c -> c <> '\n' && c <> '\000');
  Token.make
    (Token.LineComment (intern_slice t (start + 2) (t.pos - start - 2)))
    (span t start)

let lex_block_comment t =
  let start = t.pos in
  advance_by t 2;
  let depth = ref 1 in
  while !depth > 0 && not (at_end t) do
    if curr t = '/' && peek t 1 = '*' then (
      advance_by t 2;
      incr depth)
    else if curr t = '*' && peek t 1 = '/' then (
      advance_by t 2;
      decr depth)
    else advance t
  done;
  let content =
    if !depth > 0 then (
      error t "unterminated block comment" (span t start);
      intern_slice t (start + 2) (t.pos - start - 2))
    else intern_slice t (start + 2) (t.pos - start - 4)
  in
  Token.make (Token.BlockComment content) (span t start)

let symbols =
  [
    ("=/=", Token.EqSlashEq)
  ; ("..<", Token.DotDotLt)
  ; (":=", Token.ColonEq)
  ; ("<-", Token.LtMinus)
  ; ("->", Token.MinusGt)
  ; ("<=", Token.LtEq)
  ; (">=", Token.GtEq)
  ; ("..", Token.DotDot)
  ; ("(", Token.LParen)
  ; (")", Token.RParen)
  ; ("[", Token.LBrack)
  ; ("]", Token.RBrack)
  ; ("{", Token.LBrace)
  ; ("}", Token.RBrace)
  ; ("|", Token.Pipe)
  ; (",", Token.Comma)
  ; (".", Token.Dot)
  ; (":", Token.Colon)
  ; (";", Token.Semi)
  ; ("@", Token.At)
  ; ("?", Token.Question)
  ; ("!", Token.Bang)
  ; ("$", Token.Dollar)
  ; ("+", Token.Plus)
  ; ("-", Token.Minus)
  ; ("*", Token.Star)
  ; ("/", Token.Slash)
  ; ("^", Token.Caret)
  ; ("=", Token.Eq)
  ; ("<", Token.Lt)
  ; (">", Token.Gt)
  ]

let lex_symbol t =
  let start = t.pos in
  let rec try_ops = function
    | [] ->
      let c = curr t in
      error
        t
        (Printf.sprintf "unexpected character '%c' (0x%02X)" c (Char.code c))
        (span t start);
      advance t;
      Token.make Token.Error (span t start)
    | (op_str, kind) :: rest ->
      if matches_at t op_str then (
        advance_by t (String.length op_str);
        Token.make kind (span t start))
      else try_ops rest
  in
  try_ops symbols

let lex t =
  let start = t.pos in
  match curr t with
  | '\000' -> Token.make Token.Eof (span t start)
  | ' ' | '\t' | '\r' ->
    scan_while t is_whitespace;
    Token.make Token.Whitespace (span t start)
  | '\n' ->
    advance t;
    Token.make Token.Newline (span t start)
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> lex_ident t
  | '0' .. '9' -> lex_lit_num t
  | '"' -> lex_lit_str t
  | '\'' -> lex_lit_rune t
  | '/' ->
    if peek t 1 = '/' then lex_line_comment t
    else if peek t 1 = '*' then lex_block_comment t
    else lex_symbol t
  | _ -> lex_symbol t

let lex_all t =
  let rec loop acc =
    let tok = lex t in
    if tok.Token.kind = Token.Eof then List.rev (tok :: acc)
    else loop (tok :: acc)
  in
  let toks = loop [] in
  (toks, !(t.diags))
