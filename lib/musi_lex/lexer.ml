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

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let keywords =
  let tbl = Hashtbl.create 64 in
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

let lex_ident t =
  let start = t.pos in
  scan_while t is_ident_continue;
  let text = String.sub t.source start (t.pos - start) in
  let kind =
    if text = "_" then Token.Underscore
    else
      match Hashtbl.find_opt keywords text with
      | Some kw -> kw
      | None -> Token.Ident (Interner.intern t.interner text)
  in
  Token.make kind (span t start)

let lex_number t =
  let start = t.pos in
  if curr t = '0' && (peek t 1 = 'x' || peek t 1 = 'X') then (
    advance_by t 2;
    scan_while t (fun c -> is_hex c || c = '_'))
  else if curr t = '0' && (peek t 1 = 'b' || peek t 1 = 'B') then (
    advance_by t 2;
    scan_while t (fun c -> c = '0' || c = '1' || c = '_'))
  else (
    scan_while t (fun c -> is_digit c || c = '_');
    if curr t = '.' && is_digit (peek t 1) then (
      advance t;
      scan_while t (fun c -> is_digit c || c = '_'));
    if curr t = 'e' || curr t = 'E' then (
      advance t;
      if curr t = '+' || curr t = '-' then advance t;
      scan_while t (fun c -> is_digit c || c = '_')));
  let text = String.sub t.source start (t.pos - start) in
  Token.make (Token.LitNumeric text) (span t start)

let escapes =
  [
    ('n', '\n')
  ; ('t', '\t')
  ; ('r', '\r')
  ; ('\\', '\\')
  ; ('"', '"')
  ; ('0', '\000')
  ]

let lex_string t =
  let start = t.pos in
  advance t;
  let buf = Buffer.create 16 in
  let rec loop () =
    if at_end t then error t "unterminated string" (span t start)
    else
      match curr t with
      | '"' -> advance t
      | '\\' ->
        advance t;
        (match List.assoc_opt (curr t) escapes with
        | Some ch ->
          Buffer.add_char buf ch;
          advance t
        | None ->
          error t "invalid escape" (span t (t.pos - 1));
          advance t);
        loop ()
      | c ->
        Buffer.add_char buf c;
        advance t;
        loop ()
  in
  loop ();
  Token.make
    (Token.LitText (Interner.intern t.interner (Buffer.contents buf)))
    (span t start)

let lex_rune t =
  let start = t.pos in
  advance t;
  if at_end t then (
    error t "unterminated rune" (span t start);
    Token.make Token.Error (span t start))
  else
    let code = Char.code (curr t) in
    advance t;
    if curr t <> '\'' then error t "expected closing '" (span t start);
    advance t;
    Token.make (Token.LitRune code) (span t start)

let lex_line_comment t =
  let start = t.pos in
  advance_by t 2;
  scan_while t (fun c -> c <> '\n');
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
  if !depth > 0 then error t "unterminated block comment" (span t start);
  Token.make
    (Token.BlockComment (intern_slice t (start + 2) (t.pos - start - 4)))
    (span t start)

let symbols =
  [
    ("=/=", 3, Token.EqSlashEq)
  ; ("..<", 3, Token.DotDotLt)
  ; (":=", 2, Token.ColonEq)
  ; ("<-", 2, Token.LtMinus)
  ; ("->", 2, Token.MinusGt)
  ; ("<=", 2, Token.LtEq)
  ; (">=", 2, Token.GtEq)
  ; ("..", 2, Token.DotDot)
  ; ("(", 1, Token.LParen)
  ; (")", 1, Token.RParen)
  ; ("[", 1, Token.LBrack)
  ; ("]", 1, Token.RBrack)
  ; ("{", 1, Token.LBrace)
  ; ("}", 1, Token.RBrace)
  ; (",", 1, Token.Comma)
  ; (".", 1, Token.Dot)
  ; (":", 1, Token.Colon)
  ; (";", 1, Token.Semi)
  ; ("@", 1, Token.At)
  ; ("?", 1, Token.Question)
  ; ("!", 1, Token.Bang)
  ; ("$", 1, Token.Dollar)
  ; ("+", 1, Token.Plus)
  ; ("-", 1, Token.Minus)
  ; ("*", 1, Token.Star)
  ; ("^", 1, Token.Caret)
  ; ("=", 1, Token.Eq)
  ; ("<", 1, Token.Lt)
  ; (">", 1, Token.Gt)
  ]

let lex_symbol t =
  let start = t.pos in
  let rec try_ops = function
    | [] ->
      error
        t
        (Printf.sprintf "unexpected character '%c'" (curr t))
        (span t start);
      advance t;
      Token.make Token.Error (span t start)
    | (op_str, len, kind) :: rest ->
      if matches_at t op_str then (
        advance_by t len;
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
  | '0' .. '9' -> lex_number t
  | '"' -> lex_string t
  | '\'' -> lex_rune t
  | '/' when peek t 1 = '/' -> lex_line_comment t
  | '/' when peek t 1 = '*' -> lex_block_comment t
  | _ -> lex_symbol t

let lex_all t =
  let rec loop acc =
    if at_end t then List.rev (Token.make Token.Eof (span t t.pos) :: acc)
    else loop (lex t :: acc)
  in
  (loop [], !(t.diags))
