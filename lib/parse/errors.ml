open Basic

type code =
  | E1001
  | E1002
  | E1003
  | E1004
  | E1005
  | E1101
  | E1102
  | E1103
  | E1104
  | E1105
  | E1108
  | E1109
  | E1110
  | E1111
  | E1112
  | E1113
  | E1114
  | E1115
  | E1201
  | E1202
  | E1203
  | E1204
  | E1205
  | E1301
  | E1302
  | E1303
  | E1304
  | E1305
  | E1401
  | E1402
  | E1403
  | E1404
  | E1405
  | E1501
  | E1502
  | E1503
  | E1601
  | E1602
  | E1603

let code_string = function
  | E1001 -> "E1001"
  | E1002 -> "E1002"
  | E1003 -> "E1003"
  | E1004 -> "E1004"
  | E1005 -> "E1005"
  | E1101 -> "E1101"
  | E1102 -> "E1102"
  | E1103 -> "E1103"
  | E1104 -> "E1104"
  | E1105 -> "E1105"
  | E1108 -> "E1108"
  | E1109 -> "E1109"
  | E1110 -> "E1110"
  | E1111 -> "E1111"
  | E1112 -> "E1112"
  | E1113 -> "E1113"
  | E1114 -> "E1114"
  | E1115 -> "E1115"
  | E1201 -> "E1201"
  | E1202 -> "E1202"
  | E1203 -> "E1203"
  | E1204 -> "E1204"
  | E1205 -> "E1205"
  | E1301 -> "E1301"
  | E1302 -> "E1302"
  | E1303 -> "E1303"
  | E1304 -> "E1304"
  | E1305 -> "E1305"
  | E1401 -> "E1401"
  | E1402 -> "E1402"
  | E1403 -> "E1403"
  | E1404 -> "E1404"
  | E1405 -> "E1405"
  | E1501 -> "E1501"
  | E1502 -> "E1502"
  | E1503 -> "E1503"
  | E1601 -> "E1601"
  | E1602 -> "E1602"
  | E1603 -> "E1603"

let diag code span args =
  let msg =
    match (code, args) with
    | E1001, [ found ] -> Printf.sprintf "unexpected token '%s'" found
    | E1002, [] -> "unexpected end of input"
    | E1003, [ expected; found ] ->
      Printf.sprintf "expected '%s', but found '%s'" expected found
    | E1004, [ token ] -> Printf.sprintf "unexpected '%s' in expression" token
    | E1005, [ token ] -> Printf.sprintf "unexpected '%s' in statement" token
    | E1101, [] -> "expected expression"
    | E1102, [] -> "expected statement"
    | E1103, [] -> "expected pattern"
    | E1104, [] -> "expected type"
    | E1105, [] -> "expected identifier"
    | E1108, [] -> "expected '(' to start block"
    | E1109, [] -> "expected ')' to end block"
    | E1110, [] -> "expected ';' after statement"
    | E1111, [] -> "expected ',' in list"
    | E1112, [] -> "expected ':=' in binding"
    | E1113, [] -> "expected '<-' in assignment"
    | E1114, [] -> "expected '->' for function return type"
    | E1115, [] -> "expected 'from' in 'import' statement"
    | E1201, [] -> "invalid syntax"
    | E1202, [] -> "empty tuple needs at least one item or trailing ','"
    | E1203, [] -> "empty parameter list"
    | E1204, [] -> "empty 'match' needs at least one case"
    | E1205, [] -> "invalid attribute syntax"
    | E1301, [ op ] ->
      Printf.sprintf "binary operator '%s' needs value on left and right" op
    | E1302, [ op ] ->
      Printf.sprintf "unary operator '%s' needs value after operator" op
    | E1303, [] -> "function call needs function to call"
    | E1304, [] -> "field access needs value before '.'"
    | E1305, [] -> "index needs value before brackets"
    | E1401, [] -> "binding needs name and value"
    | E1402, [] -> "assignment needs name and value"
    | E1403, [] -> "'data' statement needs name and body"
    | E1404, [] -> "'extern' block needs at least one function signature"
    | E1405, [] -> "'export' statement needs name"
    | E1501, [] -> "pattern binding needs name"
    | E1502, [] -> "pattern constructor needs name"
    | E1503, [] -> "invalid pattern in match arm"
    | E1601, [] -> "pointer type needs base type"
    | E1602, [] -> "function type needs parameter types"
    | E1603, [] -> "array type needs element type"
    | _, _ -> "unknown parse error"
  in
  Diagnostic.error_with_code (Diagnostic.Parse (code_string code)) msg span
