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

let is_blank_char = function ' ' | '\t' | '\r' -> true | _ -> false

let is_ident_cont = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_xdigit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

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
  let text = String.sub t.source start (t.pos - start) in
  let kind =
    if text = "_" then Token.Underscore
    else
      match Hashtbl.find_opt keywords text with
      | Some kw -> kw
      | None -> Token.Ident (Interner.intern t.interner text)
  in
  Token.make kind (span t start)

let lex_num t =
  let start = t.pos in
  if curr t = '0' && (peek t 1 = 'x' || peek t 1 = 'X') then (
    advance_by t 2;
    scan_while t (fun c -> is_xdigit c || c = '_'))
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
  Token.make (Token.LitNum text) (span t start)

let escapes =
  [
    ('n', '\n')
  ; ('t', '\t')
  ; ('r', '\r')
  ; ('\\', '\\')
  ; ('"', '"')
  ; ('0', '\000')
  ]

let lex_str_lit t =
  let start = t.pos in
  advance t;
  let buf = Buffer.create 16 in
  let rec loop () =
    if at_end t then error t "unterminated text literal" (span t start)
    else
      match curr t with
      | '"' -> advance t
      | '\\' ->
        advance t;
        let c = curr t in
        (match List.assoc_opt c escapes with
        | Some ch ->
          Buffer.add_char buf ch;
          advance t
        | None ->
          error
            t
            (Printf.sprintf "unknown escape sequence '\\%c'" c)
            (span t (t.pos - 1));
          advance t);
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

let lex_chr_lit t =
  let start = t.pos in
  advance t;
  if at_end t then (
    error t "unterminated rune literal" (span t start);
    Token.make Token.Error (span t start))
  else
    let code = Char.code (curr t) in
    advance t;
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
  if !depth > 0 then error t "unterminated block comment" (span t start);
  Token.make
    (Token.BlockComment (intern_slice t (start + 2) (t.pos - start - 4)))
    (span t start)

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
  ; ("^", Token.Caret)
  ; ("=", Token.Eq)
  ; ("<", Token.Lt)
  ; (">", Token.Gt)
  ]

let lex_sym t =
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
    scan_while t is_blank_char;
    Token.make Token.Whitespace (span t start)
  | '\n' ->
    advance t;
    Token.make Token.Newline (span t start)
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> lex_ident t
  | '0' .. '9' -> lex_num t
  | '"' -> lex_str_lit t
  | '\'' -> lex_chr_lit t
  | '/' when peek t 1 = '/' -> lex_line_comment t
  | '/' when peek t 1 = '*' -> lex_block_comment t
  | _ -> lex_sym t

let lex_all t =
  let rec loop acc =
    if at_end t then List.rev (Token.make Token.Eof (span t t.pos) :: acc)
    else
      let tok = lex t in
      if tok.Token.kind = Token.Eof then List.rev (tok :: acc)
      else loop (tok :: acc)
  in
  let toks = loop [] in
  let diags = !(t.diags) in
  (toks, diags)
