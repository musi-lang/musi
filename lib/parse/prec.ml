open Lex

type assoc = Left | Right | None

type t =
  | Pipe
  | LogicalOr
  | LogicalAnd
  | Bitwise
  | Comparison
  | Additive
  | Multiplicative
  | Exponentiation
  | Offset
  | Unary
  | Postfix

let prec_value = function
  | Pipe -> 0
  | LogicalOr -> 1
  | LogicalAnd -> 2
  | Bitwise -> 3
  | Comparison -> 4
  | Additive -> 5
  | Multiplicative -> 6
  | Exponentiation -> 7
  | Offset -> 8
  | Unary -> 9
  | Postfix -> 10

let token_prec = function
  | Token.PipeGt -> Some (Pipe, Left)
  | Token.KwOr -> Some (LogicalOr, Left)
  | Token.KwAnd -> Some (LogicalAnd, Left)
  | Token.Amp | Token.Pipe | Token.Caret -> Some (Bitwise, Left)
  | Token.Eq | Token.BangEq | Token.Gt | Token.Lt | Token.GtEq | Token.LtEq ->
    Some (Comparison, None)
  | Token.Plus | Token.Minus -> Some (Additive, Left)
  | Token.Star | Token.Slash -> Some (Multiplicative, Left)
  | Token.StarStar -> Some (Exponentiation, Right)
  | Token.Bang -> Some (Offset, Left)
  | _ -> None

let is_prefix_op = function
  | Token.Minus | Token.KwNot | Token.Tilde | Token.At -> true
  | _ -> false

let is_postfix_op = function
  | Token.Dot | Token.LBrack | Token.LParen -> true
  | _ -> false
