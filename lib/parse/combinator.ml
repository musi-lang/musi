open Basic
module Token = Lex.Token

type 'a result = ('a, Diagnostic.bag) Result.t

type stream = {
    tokens : (Token.t * Span.t) list
  ; pos : int
  ; diags : Diagnostic.bag
}

let mk_stream tokens = { tokens; pos = 0; diags = Diagnostic.empty_bag }

type 'a parser = stream -> ('a * stream, Diagnostic.bag) Result.t

let ret_ok x stream = Ok (x, stream)
let ret_err bag _stream = Error bag

let bind p f stream =
  match p stream with Ok (x, stream') -> f x stream' | Error e -> Error e

let ( >>= ) = bind
let map f p = p >>= fun x -> ret_ok (f x)
let ( >>| ) p f = map f p
let ( >> ) p1 p2 = p1 >>= fun _ -> p2
let ( <$> ) f p = map f p
let ( <*> ) pf p = pf >>= fun f -> map f p
let ( <$>> ) p f = map f p

let fail span message stream =
  let diag = Diagnostic.error message span in
  let bag = Diagnostic.add stream.diags diag in
  Error bag

let parse_error err span args stream =
  let diag = Errors.parse_diag err span args in
  let bag = Diagnostic.add stream.diags diag in
  Error bag

let token stream =
  match stream.pos < List.length stream.tokens with
  | true ->
    let token, span = List.nth stream.tokens stream.pos in
    let stream' = { stream with pos = stream.pos + 1 } in
    Ok ((token, span), stream')
  | false -> fail Span.dummy "unexpected EOF" stream

let peek stream =
  match stream.pos < List.length stream.tokens with
  | true ->
    let token, span = List.nth stream.tokens stream.pos in
    Ok ((token, span), stream)
  | false -> fail Span.dummy "unexpected EOF" stream

let expect tok stream =
  match stream.pos < List.length stream.tokens with
  | true ->
    let actual, span = List.nth stream.tokens stream.pos in
    if actual = tok then
      let stream' = { stream with pos = stream.pos + 1 } in
      Ok ((tok, span), stream')
    else
      fail
        span
        ("expected " ^ Token.show tok ^ ", got " ^ Token.show actual)
        stream
  | false -> fail Span.dummy "unexpected EOF" stream

let optional p stream =
  match p stream with
  | Ok (x, stream') -> Ok (Some x, stream')
  | Error _ -> Ok (None, stream)

let choice ps stream =
  let rec try_choices = function
    | [] -> Error stream.diags
    | p :: rest -> (
      match p stream with
      | Ok _ as result -> result
      | Error _ -> try_choices rest)
  in
  try_choices ps

let many p =
  let rec collect acc stream =
    match p stream with
    | Ok (x, stream') -> collect (x :: acc) stream'
    | Error _ -> Ok (List.rev acc, stream)
  in
  collect []

let many1 p =
  p >>= fun first ->
  many p >>| fun rest -> first :: rest

let sep_by p sep =
  let rec collect acc stream =
    match p stream with
    | Ok (x, stream') -> (
      match sep stream' with
      | Ok (_, stream'') -> collect (x :: acc) stream''
      | Error _ -> Ok (List.rev (x :: acc), stream'))
    | Error _ -> Ok (List.rev acc, stream)
  in
  collect []

let sep_by1 p sep =
  p >>= fun first ->
  many (sep >> p) >>| fun rest -> first :: rest

let surrounded p1 p2 p3 =
  p1 >>= fun _ ->
  p2 >>= fun x ->
  p3 >>| fun _ -> x

let between p1 p2 p3 =
  p1 >>= fun _ ->
  p2 >>= fun x ->
  p3 >>| fun _ -> x

let with_span p stream =
  let start_span =
    match stream.pos < List.length stream.tokens with
    | true ->
      let _, span = List.nth stream.tokens stream.pos in
      span
    | false -> Span.dummy
  in
  match p stream with
  | Ok (x, stream') ->
    let end_span =
      match stream'.pos > 0 && stream'.pos <= List.length stream'.tokens with
      | true ->
        let _, span = List.nth stream'.tokens (stream'.pos - 1) in
        span
      | false -> start_span
    in
    let combined_span = Span.merge start_span end_span in
    Ok ((x, combined_span), stream')
  | Error e -> Error e
