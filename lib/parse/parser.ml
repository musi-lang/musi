open Basic
module Token = Lex.Token
module Combinator = Combinator
module Node = Ast.Node

type bp = { lhs : int; rhs : int }

let infix_bp = function
  | Token.Dot -> { lhs = 90; rhs = 91 }
  | Token.LBrack -> { lhs = 89; rhs = 90 }
  | Token.LParen -> { lhs = 88; rhs = 89 }
  | Token.StarStar -> { lhs = 80; rhs = 81 }
  | Token.Star | Token.Slash -> { lhs = 70; rhs = 71 }
  | Token.Plus | Token.Minus -> { lhs = 60; rhs = 61 }
  | Token.LtLt | Token.GtGt -> { lhs = 50; rhs = 51 }
  | Token.Eq | Token.BangEq | Token.Lt | Token.LtEq | Token.Gt | Token.GtEq ->
    { lhs = 40; rhs = 41 }
  | Token.Amp | Token.KwAnd -> { lhs = 30; rhs = 31 }
  | Token.Caret -> { lhs = 25; rhs = 26 }
  | Token.Pipe | Token.KwOr -> { lhs = 20; rhs = 21 }
  | Token.LtMinus -> { lhs = 0; rhs = 1 }
  | _ -> raise Not_found

let prefix_bp = function
  | Token.Minus | Token.Tilde | Token.KwNot | Token.At -> 100
  | _ -> raise Not_found

let can_bind_infix (tok, _) =
  try
    ignore (infix_bp tok);
    true
  with Not_found -> false

let parse_prog _stream = failwith "TODO: implement parse_prog"
let parse_stmt_list _stream = failwith "TODO: implement parse_stmt_list"
let parse_stmt _stream = failwith "TODO: implement parse_stmt"
let parse_pat _stream = failwith "TODO: implement parse_pat"
let parse_typ _stream = failwith "TODO: implement parse_typ"

let rec parse_expr _bp _stream = failwith "TODO: implement parse_expr"
and nud_expr_lit _stream = failwith "TODO: implement nud_expr_lit"
and nud_expr_ident _stream = failwith "TODO: implement nud_expr_ident"
and nud_expr_tuple _stream = failwith "TODO: implement nud_expr_tuple"
and nud_expr_block _stream = failwith "TODO: implement nud_expr_block"
and nud_expr_if _stream = failwith "TODO: implement nud_expr_if"
and nud_expr_match _stream = failwith "TODO: implement nud_expr_match"
and nud_expr_for _stream = failwith "TODO: implement nud_expr_for"
and nud_expr_while _stream = failwith "TODO: implement nud_expr_while"
and nud_expr_record _stream = failwith "TODO: implement nud_expr_record"
and nud_expr_choice _stream = failwith "TODO: implement nud_expr_choice"
and nud_expr_fn _stream = failwith "TODO: implement nud_expr_fn"
and nud_expr_unary _stream = failwith "TODO: implement nud_expr_unary"
and led_expr_call _lhs _stream = failwith "TODO: implement led_expr_call"
and led_expr_member _lhs _stream = failwith "TODO: implement led_expr_member"
and led_expr_index _lhs _stream = failwith "TODO: implement led_expr_index"
and led_expr_assign _lhs _stream = failwith "TODO: implement led_expr_assign"
and led_expr_infix _lhs _stream = failwith "TODO: implement led_expr_infix"

let parse_binding _stream = failwith "TODO: implement parse_binding"
let parse_block _stream = failwith "TODO: implement parse_block"
let parse_guard _stream = failwith "TODO: implement parse_guard"
let parse_import_clause _stream = failwith "TODO: implement parse_import_clause"
let parse_export_clause _stream = failwith "TODO: implement parse_export_clause"
let pat_lit _stream = failwith "TODO: implement pat_lit"
let pat_wild _stream = failwith "TODO: implement pat_wild"
let pat_ident _stream = failwith "TODO: implement pat_ident"
let pat_tuple _stream = failwith "TODO: implement pat_tuple"
let pat_record _stream = failwith "TODO: implement pat_record"
let pat_ctor _stream = failwith "TODO: implement pat_ctor"
let typ_ptr _stream = failwith "TODO: implement typ_ptr"
let typ_array _stream = failwith "TODO: implement typ_array"
let typ_ident _stream = failwith "TODO: implement typ_ident"
let typ_tuple _stream = failwith "TODO: implement typ_tuple"
let typ_fn _stream = failwith "TODO: implement typ_fn"
let typ_record _stream = failwith "TODO: implement typ_record"
