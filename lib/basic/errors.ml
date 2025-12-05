type lex_error =
  | E0001 of char
  | E0101 of string
  | E0102 of char * string
  | E0103 of string
  | E0104 of string
  | E0105 of string
  | E0106
  | E0201 of string
  | E0202
  | E0203 of char
  | E0204
  | E0205 of string
  | E0206
  | E0207
  | E0208
  | E0209 of string

type parse_error =
  | E1001 of string * string
  | E1002 of string
  | E1003
  | E1004
  | E1005
  | E1006
  | E1007

let lex_diag err span _args =
  let msg =
    match err with
    | E0001 c -> Printf.sprintf "wrong character '%c'" c
    | E0101 base -> Printf.sprintf "incomplete %s number" base
    | E0102 (digit, base) ->
      Printf.sprintf "wrong digit '%c' for %s number" digit base
    | E0103 num -> Printf.sprintf "malformed number separator in %s literal" num
    | E0104 lit -> Printf.sprintf "wrong numeric literal '%s'" lit
    | E0105 prefix -> Printf.sprintf "wrong number base prefix '%s'" prefix
    | E0106 -> "empty rune literal"
    | E0201 lit_type -> Printf.sprintf "unterminated %s literal" lit_type
    | E0202 -> "unterminated template string"
    | E0203 c -> Printf.sprintf "wrong escape sequence '\\%c'" c
    | E0204 -> "empty unicode escape sequence"
    | E0205 max_val ->
      Printf.sprintf "unicode code point exceeds maximum 0x%s" max_val
    | E0206 -> "wrong hexadecimal digits in unicode escape sequence"
    | E0207 -> "unclosed unicode escape sequence"
    | E0208 -> "unopened unicode escape sequence"
    | E0209 seq ->
      Printf.sprintf "wrong hexadecimal escape sequence '\\x{%s}'" seq
  in
  Diagnostic.error_with_code (Diagnostic.Lex "") msg span

let parse_diag err span _args =
  let msg =
    match err with
    | E1001 (expected, found) ->
      Printf.sprintf "expected '%s', found '%s'" expected found
    | E1002 token -> Printf.sprintf "unexpected token '%s'" token
    | E1003 -> "expected expression"
    | E1004 -> "expected statement"
    | E1005 -> "expected pattern"
    | E1006 -> "expected type"
    | E1007 -> "expected identifier"
  in
  Diagnostic.error_with_code (Diagnostic.Parse "") msg span
