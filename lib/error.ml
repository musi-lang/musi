open Basic

type lex_error = E0103 | E0201 | E0205 | E0207 | E0208 | E0209 | E0210
type parse_error = E1003 | E1101 | E1102 | E1103 | E1104 | E1105

let lex_error_string = function
  | E0103 -> "E0103"
  | E0201 -> "E0201"
  | E0205 -> "E0205"
  | E0207 -> "E0207"
  | E0208 -> "E0208"
  | E0209 -> "E0209"
  | E0210 -> "E0210"

let parse_error_string = function
  | E1003 -> "E1003"
  | E1101 -> "E1101"
  | E1102 -> "E1102"
  | E1103 -> "E1103"
  | E1104 -> "E1104"
  | E1105 -> "E1105"

let lex_diag err span args =
  let msg =
    match (err, args) with
    | E0103, [ base ] -> Printf.sprintf "incomplete %s number" base
    | E0201, [ lit_type ] -> Printf.sprintf "unterminated %s literal" lit_type
    | E0205, [] -> "empty unicode escape sequence"
    | E0207, [ max_val ] ->
      Printf.sprintf "unicode code point exceeds maximum 0x%s" max_val
    | E0208, [] -> "invalid hex digits in unicode escape sequence"
    | E0209, [] -> "unclosed unicode escape sequence (missing '}')"
    | E0210, [] -> "missing '{' after unicode escape prefix"
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
    | _, _ -> "unknown parse error"
  in
  Diagnostic.error_with_code
    (Diagnostic.Parse (parse_error_string err))
    msg
    span
