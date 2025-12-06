type lex_error =
  | E0001 of char
  | E0101 of string
  | E0103 of string
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

type parse_error =
  | E1001 of string * string
  | E1002 of string
  | E1003
  | E1004
  | E1005
  | E1006
  | E1007
  | E1008 of string
  | E1009

val lex_diag : lex_error -> Span.t -> string list -> Diagnostic.t
val parse_diag : parse_error -> Span.t -> string list -> Diagnostic.t
