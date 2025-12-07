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
  | E1070

val lex_diag : lex_error -> Span.t -> string list -> Diagnostic.t
val parse_diag : parse_error -> Span.t -> string list -> Diagnostic.t
