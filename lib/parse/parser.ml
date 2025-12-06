module Parser = struct
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

  let parse_lit =
    with_span
      ( token >>= fun (tok, span) ->
        match tok with
        | Token.LitNumber s ->
          ret_ok (Node.Expr.lit ~value:(Ast.Node.Whole s) span)
        | Token.LitString name ->
          ret_ok (Node.Expr.lit ~value:(Ast.Node.String name) span)
        | Token.LitRune c ->
          ret_ok (Node.Expr.lit ~value:(Ast.Node.Rune c) span)
        | _ -> parse_error Errors.E1003 span [] )

  let parse_ident =
    with_span
      ( token >>= fun (tok, span) ->
        match tok with
        | Token.Ident name -> ret_ok (Node.Expr.ident ~value:name span)
        | _ -> parse_error Errors.E1007 span [] )

  let parse_atom = choice [ parse_lit; parse_ident ]
end
