open Basic
module Token = Lex.Token
open Ast

type state = {
    toks : (Token.t * Span.t) list
  ; mutable pos : int
  ; len : int
  ; file_id : Interner.file_id
  ; _interner : Interner.t
  ; mutable diags : Diagnostic.bag
}

type 'a t = state -> ('a * state, Diagnostic.t) Result.t

let mk_state toks file_id interner =
  {
    toks
  ; pos = 0
  ; len = List.length toks
  ; file_id
  ; _interner = interner
  ; diags = Diagnostic.empty_bag
  }

let return x = fun st -> Ok (x, st)

let bind m f =
 fun st -> match m st with Error diag -> Error diag | Ok (x, st') -> f x st'

let ( >>= ) = bind
let ( >>| ) m f = m >>= fun x -> return (f x)

let parse_error err_code span =
 fun _ ->
  let diag = Errors.parse_diag err_code span [] in
  Error diag

let catch_error m handler =
 fun st ->
  match m st with Ok result -> Ok result | Error diag -> handler diag st

let or_else m1 m2 =
 fun st -> match m1 st with Ok result -> Ok result | Error _ -> m2 st

let ( <|> ) = or_else

let advance =
 fun st ->
  st.pos <- st.pos + 1;
  Ok ((), st)

let advance_n n =
 fun st ->
  st.pos <- st.pos + n;
  Ok ((), st)

let token =
 fun st ->
  if st.pos >= st.len then
    parse_error
      (Errors.E1001 ("unexpected EOF", "expected token"))
      (Span.make st.file_id st.pos st.pos)
      st
  else
    let tok_span = List.nth st.toks st.pos in
    match advance st with
    | Ok ((), st') -> Ok (tok_span, st')
    | Error diag -> Error diag

let peek st =
  if st.pos >= st.len then Token.EOF else fst (List.nth st.toks st.pos)

let peek_span st =
  if st.pos >= st.len then Span.dummy else snd (List.nth st.toks st.pos)

let curr_opt st =
  if st.pos >= st.len then None else Some (List.nth st.toks st.pos)

let optional parser =
 fun st ->
  match parser st with
  | Ok (x, st') -> Ok (Some x, st')
  | Error _ -> Ok (None, st)

let many parser =
 fun st ->
  let rec collect acc st =
    match parser st with
    | Ok (x, st') -> collect (x :: acc) st'
    | Error _ -> Ok (List.rev acc, st)
  in
  collect [] st

let many1 parser =
  parser >>= fun fst ->
  many parser >>= fun rst -> return (fst :: rst)

let sep_by parser sep =
  parser
  >>= (fun fst ->
  many (sep >>= fun _ -> parser) >>= fun rst -> return (fst :: rst))
  <|> return []

let between opening closing content : 'c t =
  opening >>= fun _ ->
  content >>= fun content' ->
  closing >>= fun _ -> return content'

let token_match matcher =
  token >>= fun (tok, span) ->
  if matcher tok then return tok else parse_error Errors.E1004 span

let token_expect expected =
  token_match (fun tok -> tok = expected) >>= fun _ -> return ()

let report_error st err_code span =
  let diag = Errors.parse_diag err_code span [] in
  st.diags <- Diagnostic.add st.diags diag

let enclosed_list lhs rhs sep parser =
  token_expect lhs >>= fun () ->
  parser >>= fun fst ->
  many (token_expect sep >>= fun _ -> parser) >>= fun rst ->
  token_expect rhs >>= fun () ->
  return (fst :: rst)
  <|> ( token_expect lhs >>= fun () ->
        token_expect rhs >>= fun () -> return [] )

type bp = { lhs : int; rhs : int }

let infix_bp = function
  | Token.Eq -> { lhs = 2; rhs = 3 }
  | Token.BangEq -> { lhs = 2; rhs = 3 }
  | Token.Lt -> { lhs = 2; rhs = 3 }
  | Token.LtEq -> { lhs = 2; rhs = 3 }
  | Token.Gt -> { lhs = 2; rhs = 3 }
  | Token.GtEq -> { lhs = 2; rhs = 3 }
  | Token.LtMinus -> { lhs = 4; rhs = 5 }
  | Token.Plus -> { lhs = 6; rhs = 7 }
  | Token.Minus -> { lhs = 6; rhs = 7 }
  | Token.Star -> { lhs = 8; rhs = 9 }
  | Token.Slash -> { lhs = 8; rhs = 9 }
  | Token.StarStar -> { lhs = 10; rhs = 11 }
  | Token.Amp -> { lhs = 12; rhs = 13 }
  | Token.Pipe -> { lhs = 12; rhs = 13 }
  | Token.Caret -> { lhs = 12; rhs = 13 }
  | Token.LtLt -> { lhs = 14; rhs = 15 }
  | Token.GtGt -> { lhs = 14; rhs = 15 }
  | Token.KwAnd -> { lhs = 16; rhs = 17 }
  | Token.KwOr -> { lhs = 18; rhs = 19 }
  | _ -> raise Not_found

let prefix_bp = function
  | Token.Minus | Token.Tilde | Token.KwNot | Token.At -> 100
  | _ -> raise Not_found

let postfix_bp = function
  | Token.Bang | Token.Question -> 101
  | _ -> raise Not_found

let can_bind_infix tok =
  try
    ignore (infix_bp tok);
    true
  with Not_found -> false

let can_bind_prefix tok =
  try
    ignore (prefix_bp tok);
    true
  with Not_found -> false

let can_bind_postfix tok =
  try
    ignore (postfix_bp tok);
    true
  with Not_found -> false

let curr st =
  if st.pos >= st.len then Token.EOF else fst (List.nth st.toks st.pos)

let curr_span st =
  if st.pos >= st.len then Span.dummy else snd (List.nth st.toks st.pos)

let make_node span data = { Node.span; data }

let rec parse_expr_bp min_bp st =
  let tok = curr st in
  let span = curr_span st in
  match parse_prefix tok span st with
  | Error _ as e -> e
  | Ok (mut_lhs, st') ->
    let rec loop lhs st'' =
      match curr st'' with
      | Token.EOF -> Ok (lhs, st'')
      | infix_tok when can_bind_infix infix_tok -> (
        let { lhs = infix_lhs; rhs } = infix_bp infix_tok in
        if infix_lhs < min_bp then Ok (lhs, st'')
        else
          let st''' = { st'' with pos = st''.pos + 1 } in
          match parse_expr_bp rhs st''' with
          | Error _ as e -> e
          | Ok (rhs_expr, st'''') ->
            let span' = Span.merge (Node.span lhs) (Node.span rhs_expr) in
            let lhs' =
              make_node
                span'
                (Node.ExprCall
                   {
                     callee = lhs
                   ; typ_args = None
                   ; args = [ rhs_expr ]
                   ; optional = false
                   })
            in
            loop lhs' st''''
          | _ -> Ok (lhs, st''))
    in
    loop mut_lhs st'

and parse_prefix tok span st =
  match tok with
  | _ when can_bind_prefix tok -> (
    let r_bp = prefix_bp tok in
    let st' = { st with pos = st.pos + 1 } in
    match parse_expr_bp r_bp st' with
    | Error _ as e -> e
    | Ok (op, st'') ->
      let span' = Span.merge span (Node.span op) in
      Ok (make_node span' (Node.ExprUnary { op = tok; arg = op }), st'')
    | _ -> parse_expr_atom tok span st)

and parse_expr_atom tok span st =
  match tok with
  | Token.Ident name ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (make_node span (Node.ExprIdent name), st')
  | Token.LitNumber s ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (make_node span (Node.ExprLit (Node.LitWhole s)), st')
  | Token.LitString name ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (make_node span (Node.ExprLit (Node.LitString name)), st')
  | Token.LitRune c ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (make_node span (Node.ExprLit (Node.LitRune c)), st')
  | _ ->
    let diag = Errors.parse_diag Errors.E1003 span [] in
    Error diag

let parse_typ : Node.typ t = failwith "TODO: implement parse_typ"
let parse_pat : Node.pat t = failwith "TODO: implement parse_pat"
let parse_expr = parse_expr_bp 0

let parse_stmt =
  parse_expr >>= fun expr ->
  let span' = Node.span expr in
  return Node.{ span = span'; data = StmtExpr expr }

let parse_prog : Node.prog t = many parse_stmt
