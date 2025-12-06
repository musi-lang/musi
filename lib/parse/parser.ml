open Basic
open Combinator
module Token = Lex.Token
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

let can_bind_infix tok =
  try
    ignore (infix_bp tok);
    true
  with Not_found -> false

module Exprs = struct
  let parse_lit = fun _stream -> failwith "TODO: implement literal parsing"
  let parse_ident = fun _stream -> failwith "TODO: implement identifier parsing"
  let parse_tuple = fun _stream -> failwith "TODO: implement tuple parsing"
  let parse_block = fun _stream -> failwith "TODO: implement block parsing"
  let parse_if = fun _stream -> failwith "TODO: implement if expression parsing"

  let parse_match =
   fun _stream -> failwith "TODO: implement match expression parsing"

  let parse_for = fun _stream -> failwith "TODO: implement for loop parsing"
  let parse_while = fun _stream -> failwith "TODO: implement while loop parsing"
  let parse_defer = fun _stream -> failwith "TODO: implement defer parsing"
  let parse_break = fun _stream -> failwith "TODO: implement break parsing"
  let parse_cycle = fun _stream -> failwith "TODO: implement cycle parsing"
  let parse_unsafe = fun _stream -> failwith "TODO: implement unsafe parsing"

  let parse_assign =
   fun _stream -> failwith "TODO: implement assignment parsing"

  let parse_record_lit =
   fun _stream -> failwith "TODO: implement record literal parsing"

  let parse_fn = fun _stream -> failwith "TODO: implement function parsing"
  let parse_record = fun _stream -> failwith "TODO: implement record parsing"
  let parse_choice = fun _stream -> failwith "TODO: implement choice parsing"

  let parse_grouped =
   fun _stream -> failwith "TODO: implement grouped expression parsing"

  let parse_atom =
    choice
      [
        parse_lit
      ; parse_ident
      ; parse_grouped
      ; parse_tuple
      ; parse_block
      ; parse_if
      ; parse_match
      ; parse_for
      ; parse_while
      ; parse_defer
      ; parse_break
      ; parse_cycle
      ; parse_unsafe
      ; parse_assign
      ; parse_record_lit
      ; parse_fn
      ; parse_record
      ; parse_choice
      ]

  let parse_prefix =
   fun _tok _stream -> failwith "TODO: implement prefix parsing"

  let parse_postfix =
   fun _left _stream -> failwith "TODO: implement postfix parsing"

  let parse_infix =
   fun _left _tok _bp _stream -> failwith "TODO: implement infix parsing"

  let rec parse_expr_with_bp =
   fun _min_bp _stream ->
    failwith "TODO: implement Pratt parsing with binding power"

  let parse_expr = parse_expr_with_bp 0
end

module Pats = struct
  let parse_pat_bind =
   fun _stream -> failwith "TODO: implement pattern binding parsing"

  let parse_pat_lit =
   fun _stream -> failwith "TODO: implement pattern literal parsing"

  let parse_pat_wild =
   fun _stream -> failwith "TODO: implement pattern wildcard parsing"

  let parse_pat_ident =
   fun _stream -> failwith "TODO: implement pattern identifier parsing"

  let parse_pat_record =
   fun _stream -> failwith "TODO: implement pattern record parsing"

  let parse_pat_ctor =
   fun _stream -> failwith "TODO: implement pattern constructor parsing"

  let parse_pat_tup =
   fun _stream -> failwith "TODO: implement pattern tuple parsing"

  let parse_pat =
    choice
      [
        parse_pat_bind
      ; parse_pat_lit
      ; parse_pat_wild
      ; parse_pat_ident
      ; parse_pat_record
      ; parse_pat_ctor
      ; parse_pat_tup
      ]
end

module Typs = struct
  let parse_typ_sum = fun _stream -> failwith "TODO: implement type sum parsing"

  let parse_typ_atom =
   fun _stream -> failwith "TODO: implement type atom parsing"

  let parse_typ_ptr =
   fun _stream -> failwith "TODO: implement pointer type parsing"

  let parse_typ_arr =
   fun _stream -> failwith "TODO: implement array type parsing"

  let parse_typ_ident =
   fun _stream -> failwith "TODO: implement type identifier parsing"

  let parse_typ_tup =
   fun _stream -> failwith "TODO: implement tuple type parsing"

  let parse_typ_fn =
   fun _stream -> failwith "TODO: implement function type parsing"

  let parse_typ_record =
   fun _stream -> failwith "TODO: implement record type parsing"

  let parse_type_params =
   fun _stream -> failwith "TODO: implement type parameters parsing"

  let parse_type_args =
   fun _stream -> failwith "TODO: implement type arguments parsing"

  let parse_typ = parse_typ_sum
end

let parse_attr = fun _stream -> failwith "TODO: implement attribute parsing"

module Stmts = struct
  let parse_stmt_import =
   fun _stream -> failwith "TODO: implement import statement parsing"

  let parse_stmt_export =
   fun _stream -> failwith "TODO: implement export statement parsing"

  let parse_stmt_bind =
   fun _stream -> failwith "TODO: implement bind statement parsing"

  let parse_stmt_extern =
   fun _stream -> failwith "TODO: implement extern statement parsing"

  let parse_stmt_expr =
   fun _stream -> failwith "TODO: implement expression statement parsing"

  let parse_stmt_inner =
   fun _stream -> failwith "TODO: implement inner statement parsing"

  let parse_stmt = fun _stream -> failwith "TODO: implement statement parsing"
end

let parse_prog = fun _stream -> failwith "TODO: implement program parsing"
