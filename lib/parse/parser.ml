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

let choice parsers =
  List.fold_right
    or_else
    parsers
    (parse_error (Errors.E1002 "no matching parser") Span.dummy)

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

let ident_parser =
  token_match (function Token.Ident _ -> true | _ -> false) >>= fun tok ->
  match tok with Token.Ident name -> return name | _ -> assert false

let lit_parser =
  token >>= fun (tok, span) ->
  match tok with
  | Token.LitNumber s -> return (Node.LitWhole s)
  | Token.LitString name -> return (Node.LitString name)
  | Token.LitRune c -> return (Node.LitRune c)
  | _ -> parse_error Errors.E1003 span

let keyword_parser kw = token_match (fun tok -> tok = kw) >>= fun _ -> return ()

let report_error st err_code span =
  let diag = Errors.parse_diag err_code span [] in
  st.diags <- Diagnostic.add st.diags diag

let enclosed_list lhs rhs sep parser =
  token_expect lhs >>= fun () ->
  parser >>= fun first ->
  many (token_expect sep >>= fun _ -> parser) >>= fun rest ->
  token_expect rhs >>= fun () ->
  return (first :: rest)
  <|> ( token_expect lhs >>= fun () ->
        token_expect rhs >>= fun () -> return [] )

type bp = { lhs : int; rhs : int }

let infix_bp = function
  | Token.LtMinus -> { lhs = 5; rhs = 6 }
  | _ -> raise Not_found

let prefix_bp = function
  | Token.Minus | Token.Tilde | Token.KwNot | Token.Bang | Token.At -> 100
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

let parse_typ : Node.typ t = failwith "TODO: implement parse_typ"
let parse_pat : Node.pat t = failwith "TODO: implement parse_pat"

let parse_expr =
  token >>= fun (tok, span') ->
  match tok with
  | Token.Ident name -> return Node.{ span = span'; data = ExprIdent name }
  | Token.LitNumber s ->
    return Node.{ span = span'; data = ExprLit (Node.LitWhole s) }
  | Token.LitString name ->
    return Node.{ span = span'; data = ExprLit (Node.LitString name) }
  | Token.LitRune c ->
    return Node.{ span = span'; data = ExprLit (Node.LitRune c) }
  | _ -> parse_error Errors.E1003 span'

let parse_stmt : Node.stmt t = failwith "TODO: implement parse_stmt"
let parse_prog : Node.prog t = failwith "TODO: implement parse_prog"
