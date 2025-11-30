open Basic

type code =
  | E1003
  | E1101
  | E1102
  | E1103
  | E1104
  | E1105
  | E1108
  | E1115
  | E1401
  | E1501

let code_string = function
  | E1003 -> "E1003"
  | E1101 -> "E1101"
  | E1102 -> "E1102"
  | E1103 -> "E1103"
  | E1104 -> "E1104"
  | E1105 -> "E1105"
  | E1108 -> "E1108"
  | E1115 -> "E1115"
  | E1401 -> "E1401"
  | E1501 -> "E1501"

let diag code span args =
  let msg =
    match (code, args) with
    | E1003, [ expected; found ] ->
      Printf.sprintf "expected %s, found %s" expected found
    | E1101, [] -> "expected expression"
    | E1102, [] -> "expected statement"
    | E1103, [] -> "expected pattern"
    | E1104, [] -> "expected type"
    | E1105, [] -> "expected identifier"
    | E1108, [] -> "expected '{'"
    | E1115, [] -> "expected 'from'"
    | E1401, [] -> "invalid binding"
    | E1501, [] -> "invalid pattern binding"
    | _, _ -> "parse error"
  in
  Diagnostic.error_with_code (Diagnostic.Parse (code_string code)) msg span
