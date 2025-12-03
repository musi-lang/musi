open Basic

type lex_error =
  | E0001
  | E0002
  | E0003
  | E0101
  | E0102
  | E0103
  | E0201
  | E0202
  | E0203
  | E0204
  | E0205
  | E0206
  | E0207
  | E0208
  | E0209
  | E0210
  | E0211
  | E0212
  | E0301
  | E0302
  | E0303
  | E0304
  | E0401
  | E0402
  | E0501

type parse_error =
  | E1003
  | E1101
  | E1102
  | E1103
  | E1104
  | E1105
  | E1108
  | E1115

let lex_error_string = function
  | E0001 -> "E0001"
  | E0002 -> "E0002"
  | E0003 -> "E0003"
  | E0101 -> "E0101"
  | E0102 -> "E0102"
  | E0103 -> "E0103"
  | E0201 -> "E0201"
  | E0202 -> "E0202"
  | E0203 -> "E0203"
  | E0204 -> "E0204"
  | E0205 -> "E0205"
  | E0206 -> "E0206"
  | E0207 -> "E0207"
  | E0208 -> "E0208"
  | E0209 -> "E0209"
  | E0210 -> "E0210"
  | E0211 -> "E0211"
  | E0212 -> "E0212"
  | E0301 -> "E0301"
  | E0302 -> "E0302"
  | E0303 -> "E0303"
  | E0304 -> "E0304"
  | E0401 -> "E0401"
  | E0402 -> "E0402"
  | E0501 -> "E0501"

let parse_error_string = function
  | E1003 -> "E1003"
  | E1101 -> "E1101"
  | E1102 -> "E1102"
  | E1103 -> "E1103"
  | E1104 -> "E1104"
  | E1105 -> "E1105"
  | E1108 -> "E1108"
  | E1115 -> "E1115"

let lex_diag err span args =
  let msg =
    match (err, args) with
    | E0001, [] -> "invalid UTF-8 continuation byte"
    | E0002, [] -> "invalid UTF-8 start byte"
    | E0003, [] -> "incomplete UTF-8 byte sequence"
    | E0101, [ base; char ] ->
      Printf.sprintf "invalid %s digit '%c'" base (String.get char 0)
    | E0102, [] -> "multiple decimal points in numeric literal"
    | E0103, [ base ] -> Printf.sprintf "incomplete %s number" base
    | E0201, [ lit_type ] -> Printf.sprintf "unterminated %s literal" lit_type
    | E0202, [ lit_type ] -> Printf.sprintf "unterminated %s literal" lit_type
    | E0203, [] -> "empty rune literal"
    | E0204, [] -> "unexpected '}' in template literal"
    | E0205, [] -> "empty unicode escape sequence"
    | E0206, [] -> "incomplete unicode escape sequence"
    | E0207, [ max_val ] ->
      Printf.sprintf "unicode code point exceeds maximum 0x%s" max_val
    | E0208, [] -> "invalid hex digits in unicode escape sequence"
    | E0209, [] -> "unclosed unicode escape sequence (missing '}')"
    | E0210, [] -> "missing '{' after unicode escape prefix"
    | E0211, [] -> "unterminated escape sequence"
    | E0212, [ char ] ->
      Printf.sprintf "invalid escape sequence '\\%c'" (String.get char 0)
    | E0301, [ char ] ->
      Printf.sprintf "unexpected character '%c'" (String.get char 0)
    | E0302, [ code ] ->
      Printf.sprintf "control character '\\x%s' not allowed in source" code
    | E0303, [] -> "null byte not allowed in source"
    | E0304, [ char ] ->
      Printf.sprintf
        "non-ASCII character '%c' in identifier"
        (String.get char 0)
    | E0401, [] -> "unterminated block comment"
    | E0402, [] -> "block comments cannot be nested"
    | E0501, [] -> "lexer failed to advance"
    | _, _ -> "unknown lex error"
  in
  Diagnostic.error_with_code (Diagnostic.Lex (lex_error_string err)) msg span

let parse_diag err span args =
  let msg =
    match (err, args) with
    | E1003, [ expected; found ] ->
      Printf.sprintf "expected %s, found %s" expected found
    | E1101, [] -> "expected expression"
    | E1102, [] -> "expected statement"
    | E1103, [] -> "expected pattern"
    | E1104, [] -> "expected type"
    | E1105, [] -> "expected identifier"
    | E1108, [] -> "expected '{'"
    | E1115, [] -> "expected 'from'"
    | _, _ -> "unknown parse error"
  in
  Diagnostic.error_with_code
    (Diagnostic.Parse (parse_error_string err))
    msg
    span
