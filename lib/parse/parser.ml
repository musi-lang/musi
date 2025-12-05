open Basic
module Token = Lex.Token
module Combinator = Combinator
module Node = Ast.Node

type bp = { left : int; right : int }

let infix_bp = function
  | Token.Dot -> { left = 90; right = 91 }
  | Token.LBrack -> { left = 89; right = 90 }
  | Token.LParen -> { left = 88; right = 89 }
  | Token.StarStar -> { left = 80; right = 81 }
  | Token.Star | Token.Slash -> { left = 70; right = 71 }
  | Token.Plus | Token.Minus -> { left = 60; right = 61 }
  | Token.LtLt | Token.GtGt -> { left = 50; right = 51 }
  | Token.Eq | Token.BangEq | Token.Lt | Token.LtEq | Token.Gt | Token.GtEq ->
    { left = 40; right = 41 }
  | Token.Amp | Token.KwAnd -> { left = 30; right = 31 }
  | Token.Caret -> { left = 25; right = 26 }
  | Token.Pipe | Token.KwOr -> { left = 20; right = 21 }
  | Token.LtMinus -> { left = 0; right = 1 }
  | _ -> raise Not_found

let prefix_bp = function
  | Token.Minus | Token.Tilde | Token.KwNot | Token.At -> 100
  | _ -> raise Not_found

let can_bind_infix (tok, _) =
  try
    ignore (infix_bp tok);
    true
  with Not_found -> false

let parse_prog stream = failwith "TODO: implement parse_prog"
let parse_stmt_list stream = failwith "TODO: implement parse_stmt_list"
let parse_stmt stream = failwith "TODO: implement parse_stmt"
let parse_pat stream = failwith "TODO: implement parse_pat"
let parse_typ stream = failwith "TODO: implement parse_typ"
let rec parse_expr bp stream = failwith "TODO: implement parse_expr"
let nud_expr_lit stream = failwith "TODO: implement nud_expr_lit"
let nud_expr_ident stream = failwith "TODO: implement nud_expr_ident"
let nud_expr_tuple stream = failwith "TODO: implement nud_expr_tuple"
let nud_expr_block stream = failwith "TODO: implement nud_expr_block"
let nud_expr_if stream = failwith "TODO: implement nud_expr_if"
let nud_expr_match stream = failwith "TODO: implement nud_expr_match"
let nud_expr_for stream = failwith "TODO: implement nud_expr_for"
let nud_expr_while stream = failwith "TODO: implement nud_expr_while"
let nud_expr_record stream = failwith "TODO: implement nud_expr_record"
let nud_expr_choice stream = failwith "TODO: implement nud_expr_choice"
let nud_expr_fn stream = failwith "TODO: implement nud_expr_fn"
let nud_expr_unary stream = failwith "TODO: implement nud_expr_unary"
let led_expr_call left stream = failwith "TODO: implement led_expr_call"
let led_expr_member left stream = failwith "TODO: implement led_expr_member"
let led_expr_index left stream = failwith "TODO: implement led_expr_index"
let led_expr_assign left stream = failwith "TODO: implement led_expr_assign"
let led_expr_infix left stream = failwith "TODO: implement led_expr_infix"
let parse_binding stream = failwith "TODO: implement parse_binding"
let parse_block stream = failwith "TODO: implement parse_block"
let parse_guard stream = failwith "TODO: implement parse_guard"
let parse_import_clause stream = failwith "TODO: implement parse_import_clause"
let parse_export_clause stream = failwith "TODO: implement parse_export_clause"
let pat_lit stream = failwith "TODO: implement pat_lit"
let pat_wild stream = failwith "TODO: implement pat_wild"
let pat_ident stream = failwith "TODO: implement pat_ident"
let pat_tuple stream = failwith "TODO: implement pat_tuple"
let pat_record stream = failwith "TODO: implement pat_record"
let pat_ctor stream = failwith "TODO: implement pat_ctor"
let typ_ptr stream = failwith "TODO: implement typ_ptr"
let typ_array stream = failwith "TODO: implement typ_array"
let typ_ident stream = failwith "TODO: implement typ_ident"
let typ_tuple stream = failwith "TODO: implement typ_tuple"
let typ_fn stream = failwith "TODO: implement typ_fn"
let typ_record stream = failwith "TODO: implement typ_record"
