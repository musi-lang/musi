type lex_error =
  (* character & symbol *)
  | E0001 of char
  | E0101 of string
  | E0103 of string
  | E0105 of string
  | E0106
  (* string & other literals  *)
  | E0201 of string
  | E0202
  | E0203 of char
  | E0204
  | E0205 of string
  | E0206
  | E0207
  | E0208

type parse_error =
  | E1001 of string * string
  | E1002 of string
  (* basic expects *)
  | E1003
  | E1004
  | E1005
  | E1006
  | E1007
  | E1008 of string
  | E1009
  (* import/export *)
  | E1010
  | E1011
  | E1012
  | E1013
  | E1014
  | E1015
  | E1016
  | E1017
  | E1018
  (* functions *)
  | E1019
  | E1020
  | E1021
  | E1022
  | E1023
  | E1024
  | E1025
  | E1026
  | E1027
  | E1028
  | E1029
  (* control flow *)
  | E1030
  | E1031
  | E1032
  | E1033
  | E1034
  | E1035
  | E1036
  | E1037
  | E1038
  | E1039
  | E1040
  | E1041
  | E1042
  (* types & annots *)
  | E1043
  | E1044
  | E1045
  | E1046
  | E1047
  | E1048
  | E1049
  (* patterns *)
  | E1050
  | E1051
  | E1052
  | E1053
  | E1054
  (* expressions *)
  | E1055
  | E1056
  | E1057
  | E1058
  | E1059
  | E1060
  | E1061
  | E1062
  (* general syntax *)
  | E1063
  | E1064
  | E1065
  | E1066
  | E1067
  | E1068
  | E1069

let lex_diag err span _args =
  let msg =
    match err with
    | E0001 c -> Printf.sprintf "invalid character '%c'" c
    | E0101 base -> Printf.sprintf "incomplete %s number" base
    | E0103 num -> Printf.sprintf "malformed number separator in %s literal" num
    | E0105 prefix -> Printf.sprintf "invalid number base prefix '%s'" prefix
    | E0106 -> "empty rune literal"
    (* ---------------------------------------- *)
    | E0201 lit_type -> Printf.sprintf "unclosed %s literal" lit_type
    | E0202 -> "unclosed template string"
    | E0203 c -> Printf.sprintf "invalid escape sequence '\\%c'" c
    | E0204 -> "empty unicode escape sequence"
    | E0205 max_val ->
      Printf.sprintf "unicode code point exceeds maximum 0x%s" max_val
    | E0206 -> "invalid hexadecimal digits in unicode escape sequence"
    | E0207 -> "unclosed unicode escape sequence"
    | E0208 -> "unopened unicode escape sequence"
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
    | E1008 token -> Printf.sprintf "expected '%s'" token
    | E1009 -> "expected string literal"
    (* ---------------------------------------- *)
    | E1010 -> "missing import source string"
    | E1011 -> "expected ';' after import statement"
    | E1012 -> "expected '{' in import clause"
    | E1013 -> "expected '}' in import clause"
    | E1014 -> "expected ',' in import clause"
    | E1015 -> "expected 'as' in import clause"
    | E1016 -> "expected '{' in export clause"
    | E1017 -> "expected '}' in export clause"
    | E1018 -> "expected ',' in export clause"
    (* ---------------------------------------- *)
    | E1019 -> "missing '->' in function signature"
    | E1020 -> "expected function parameter list"
    | E1021 -> "expected ':' after parameter name"
    | E1022 -> "expected ')' after parameter list"
    | E1023 -> "expected function body block"
    | E1024 -> "expected '(' to begin type parameters"
    | E1025 -> "expected '>' to close type parameters"
    | E1026 -> "expected ',' between type parameters"
    | E1027 -> "expected '(' to begin type arguments"
    | E1028 -> "expected '>' to close type arguments"
    | E1029 -> "expected ',' between type arguments"
    (* ---------------------------------------- *)
    | E1030 -> "expected '(' after if"
    | E1031 -> "expected ')' after if condition"
    | E1032 -> "expected 'else' after if block"
    | E1033 -> "expected '{' to begin match cases"
    | E1034 -> "expected '}' to close match cases"
    | E1035 -> "missing match cases"
    | E1036 -> "expected '=>' in match arm"
    | E1037 -> "missing match arm body"
    | E1038 -> "expected 'in' in for loop"
    | E1039 -> "expected '(' after for binding"
    | E1040 -> "expected ')' after for binding"
    | E1041 -> "expected '(' after while"
    | E1042 -> "expected ')' after while condition"
    (* ---------------------------------------- *)
    | E1043 -> "expected ':' before type annotation"
    | E1044 -> "expected '{' to begin record fields"
    | E1045 -> "expected '}' to close record fields"
    | E1046 -> "expected ',' between record fields"
    | E1047 -> "expected ',' between choice cases"
    | E1048 -> "expected '{' to begin choice cases"
    | E1049 -> "expected '}' to close choice cases"
    (* ---------------------------------------- *)
    | E1050 -> "expected ',' between tuple elements"
    | E1051 -> "expected ')' to close tuple"
    | E1052 -> "expected ':' in pattern binding"
    | E1053 -> "expected '{' to begin pattern fields"
    | E1054 -> "expected '}' to close pattern fields"
    (* ---------------------------------------- *)
    | E1055 -> "expected ',' between call arguments"
    | E1056 -> "expected ')' to close call arguments"
    | E1057 -> "expected '.' before member access"
    | E1058 -> "expected '[' before array index"
    | E1059 -> "expected ']' after array index"
    | E1060 -> "expected '<-' in assignment"
    | E1061 -> "expected '(' after unary operator"
    | E1062 -> "expected ')' after unary operator"
    (* ---------------------------------------- *)
    | E1063 -> "missing statement terminator ';'"
    | E1064 -> "expected '{' to begin block"
    | E1065 -> "expected '}' to close block"
    | E1066 -> "incomplete expression"
    | E1067 -> "expected '(' after attribute name"
    | E1068 -> "expected ')' after attribute arguments"
    | E1069 -> "expected ',' between attribute arguments"
  in
  Diagnostic.error_with_code (Diagnostic.Parse "") msg span
