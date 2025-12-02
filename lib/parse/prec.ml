module Token = Lex.Token

module type S = sig
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
    | Unary
    | Postfix

  val prec_value : t -> int
  val token_prec : Token.t -> (t * assoc) option
  val is_prefix_op : Token.t -> bool
  val is_infix_op : Token.t -> bool
  val is_postfix_op : Token.t -> bool
end

module Make () : S = struct
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
    | Unary -> 8
    | Postfix -> 9

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
    | Token.LtLt | Token.GtGt -> Some (Bitwise, Left)
    | _ -> None

  let is_prefix_op = function
    | Token.Minus | Token.KwNot | Token.Tilde | Token.At -> true
    | _ -> false

  let is_infix_op = function
    | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.StarStar
    | Token.Lt | Token.Gt | Token.Eq | Token.BangEq | Token.LtEq | Token.GtEq
    | Token.Amp | Token.Pipe | Token.Caret | Token.LtLt | Token.GtGt
    | Token.MinusGt ->
      true
    | _ -> false

  let is_postfix_op = function
    | Token.Dot | Token.LBrack | Token.LParen -> true
    | _ -> false
end

include Make ()
