open Lex

type t =
  | None
  | Assign
  | Pipe
  | Coal
  | Or
  | And
  | BitOr
  | BitXor
  | BitAnd
  | Equality
  | Comparison
  | Range
  | Cons
  | Term
  | Factor
  | Exponent
  | Unary
  | Postfix

let of_token = function
  | Token.LtMinus -> Assign
  | Token.BarGt -> Pipe
  | Token.QuestionQuestion -> Coal
  | Token.KwOr -> Or
  | Token.KwAnd -> And
  | Token.Bar -> BitOr
  | Token.Caret -> BitXor
  | Token.Amp -> BitAnd
  | Token.Eq | Token.SlashEq -> Equality
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.KwIs | Token.KwAs ->
    Comparison
  | Token.DotDot | Token.DotDotLt -> Range
  | Token.ColonColon -> Cons
  | Token.Plus | Token.Minus -> Term
  | Token.Star | Token.Slash | Token.Percent | Token.KwMod | Token.LtLt
  | Token.GtGt ->
    Factor
  | Token.StarStar -> Exponent
  | Token.LParen | Token.LBrack | Token.Dot | Token.DotCaret | Token.Question ->
    Postfix
  | _ -> None
