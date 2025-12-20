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

val of_token : Token.t -> t
val is_right_assoc : Token.t -> bool
