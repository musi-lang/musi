module Token = Lex.Token
module Combinator = Combinator

type binding_power = { left : int; right : int }

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

let rec parse_expr bp nud led stream =
  match nud stream with
  | Error e -> Error e
  | Ok (left, stream') -> parse_expr_tail bp nud led left stream'

and parse_expr_tail bp nud led left stream =
  match Combinator.peek stream with
  | Ok (tok, _) when can_bind_infix tok ->
    let bp' = infix_bp (fst tok) in
    if bp'.left > bp then
      match led left stream with
      | Error e -> Error e
      | Ok (new_left, stream') ->
        parse_expr_tail bp'.right nud led new_left stream'
    else Ok (left, stream)
  | _ -> Ok (left, stream)
