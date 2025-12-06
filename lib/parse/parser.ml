open Basic
module Token = Lex.Token
open Ast

type state = {
    tokens : (Token.t * Span.t) list
  ; mutable pos : int
  ; len : int
  ; file_id : Interner.file_id
  ; _interner : Interner.t
  ; mutable diags : Diagnostic.bag
}

type 'a t = state -> ('a * state, Diagnostic.t) Result.t

let mk_state tokens file_id interner =
  {
    tokens
  ; pos = 0
  ; len = List.length tokens
  ; file_id
  ; _interner = interner
  ; diags = Diagnostic.empty_bag
  }

let return (x : 'a) : 'a t = fun st -> Ok (x, st)

let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
 fun st -> match m st with Error diag -> Error diag | Ok (x, st') -> f x st'

let ( >>= ) = bind
let ( >>| ) m f = m >>= fun x -> return (f x)

let parse_error err_code span : 'a t =
 fun _ ->
  let diag = Errors.parse_diag err_code span [] in
  Error diag

let catch_error (m : 'a t) (handler : Diagnostic.t -> 'a t) : 'a t =
 fun st ->
  match m st with Ok result -> Ok result | Error diag -> handler diag st

let or_else (m1 : 'a t) (m2 : 'a t) : 'a t =
 fun st -> match m1 st with Ok result -> Ok result | Error _ -> m2 st

let ( <|> ) = or_else

let advance : unit t =
 fun st ->
  st.pos <- st.pos + 1;
  Ok ((), st)

let token : (Token.t * Span.t) t =
 fun st ->
  if st.pos >= st.len then
    parse_error
      (Errors.E1001 ("unexpected EOF", "expected token"))
      (Span.make st.file_id st.pos st.pos)
      st
  else
    let tok_span = List.nth st.tokens st.pos in
    match advance st with
    | Ok ((), st') -> Ok (tok_span, st')
    | Error diag -> Error diag

let peek st =
  if st.pos >= st.len then Token.EOF else fst (List.nth st.tokens st.pos)

let peek_span st =
  if st.pos >= st.len then Span.dummy else snd (List.nth st.tokens st.pos)

let curr_opt st =
  if st.pos >= st.len then None else Some (List.nth st.tokens st.pos)

let choice parsers =
  List.fold_right
    or_else
    parsers
    (parse_error (Errors.E1002 "no matching parser") Span.dummy)

let optional (parser : 'a t) : 'a option t =
 fun st ->
  match parser st with
  | Ok (x, st') -> Ok (Some x, st')
  | Error _ -> Ok (None, st)

let many (parser : 'a t) : 'a list t =
 fun st ->
  let rec collect acc st =
    match parser st with
    | Ok (x, st') -> collect (x :: acc) st'
    | Error _ -> Ok (List.rev acc, st)
  in
  collect [] st

let many1 (parser : 'a t) : 'a list t =
  parser >>= fun first ->
  many parser >>= fun rest -> return (first :: rest)

let sep_by (parser : 'a t) (sep : 'b t) : 'a list t =
  parser
  >>= (fun first ->
  many (sep >>= fun _ -> parser) >>= fun rest -> return (first :: rest))
  <|> return []

let between (open_parser : 'a t) (close_parser : 'b t) (content_parser : 'c t) :
  'c t =
  open_parser >>= fun _ ->
  content_parser >>= fun content ->
  close_parser >>= fun _ -> return content

let token_match (matcher : Token.t -> bool) : Token.t t =
  token >>= fun (tok, span) ->
  if matcher tok then return tok else parse_error Errors.E1004 span

let token_expect (expected : Token.t) : unit t =
  token_match (fun tok -> tok = expected) >>= fun _ -> return ()

let ident_parser : Node.ident t =
  token_match (function Token.Ident _ -> true | _ -> false) >>= fun tok ->
  match tok with Token.Ident name -> return name | _ -> assert false

let keyword_parser (kw : Token.t) : unit t =
  token_match (fun tok -> tok = kw) >>= fun _ -> return ()

let report_error st err_code span =
  let diag = Errors.parse_diag err_code span [] in
  st.diags <- Diagnostic.add st.diags diag

let delimited_list (left : Token.t) (right : Token.t) (sep : Token.t)
  (parser : 'a t) : 'a list t =
  token_expect left >>= fun () ->
  parser >>= fun first ->
  many (token_expect sep >>= fun _ -> parser) >>= fun rest ->
  token_expect right >>= fun () ->
  return (first :: rest)
  <|> ( token_expect left >>= fun () ->
        token_expect right >>= fun () -> return [] )

let parse_typ : Node.typ t = failwith "TODO: implement parse_typ"
let parse_pat : Node.pat t = failwith "TODO: implement parse_pat"
let parse_expr : Node.expr t = failwith "TODO: implement parse_expr"
let parse_stmt : Node.stmt t = failwith "TODO: implement parse_stmt"
let parse_prog : Node.prog t = failwith "TODO: implement parse_prog"
