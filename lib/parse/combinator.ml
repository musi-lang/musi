module Combinator = struct
  open Basic
  module Token = Lex.Token

  type 'a result = ('a, Diagnostic.bag) Result.t

  type stream = {
      toks : (Token.t * Span.t) list
    ; pos : int
    ; diags : Diagnostic.bag
  }

  let mk_stream toks = { toks; pos = 0; diags = Diagnostic.empty_bag }

  type 'a parser = stream -> ('a * stream, Diagnostic.bag) Result.t

  let ret_ok x st = Ok (x, st)
  let ret_err bag _st = Error bag

  let bind p f st =
    match p st with Ok (x, stream') -> f x stream' | Error e -> Error e

  let ( >>= ) = bind
  let map f p = p >>= fun x -> ret_ok (f x)
  let ( >>| ) p f = map f p
  let ( >> ) p1 p2 = p1 >>= fun _ -> p2
  let ( <$> ) f p = map f p
  let ( <*> ) pf p = pf >>= fun f -> map f p
  let ( <$>> ) p f = map f p

  let report_error err span args st =
    let diag = Errors.parse_diag err span args in
    let bag = Diagnostic.add st.diags diag in
    Error bag

  let unexpected_eof span st = report_error (Errors.E1002 "EOF") span [] st

  let token st =
    match st.pos < List.length st.toks with
    | true ->
      let tok, span = List.nth st.toks st.pos in
      let st' = { st with pos = st.pos + 1 } in
      Ok ((tok, span), st')
    | false -> unexpected_eof Span.dummy st

  let peek st =
    match st.pos < List.length st.toks with
    | true ->
      let tok, span = List.nth st.toks st.pos in
      Ok ((tok, span), st)
    | false -> unexpected_eof Span.dummy st

  let expect tok st =
    match st.pos < List.length st.toks with
    | true ->
      let actual, span = List.nth st.toks st.pos in
      if actual = tok then
        let st' = { st with pos = st.pos + 1 } in
        Ok ((tok, span), st')
      else
        report_error
          (Errors.E1001 (Token.show tok, Token.show actual))
          span
          []
          st
    | false -> unexpected_eof Span.dummy st

  let optional p st =
    match p st with Ok (x, st') -> Ok (Some x, st') | Error _ -> Ok (None, st)

  let choice ps st =
    let rec try_choices = function
      | [] -> Error st.diags
      | p :: rest -> (
        match p st with Ok _ as res -> res | Error _ -> try_choices rest)
    in
    try_choices ps

  let many p =
    let rec collect acc st =
      match p st with
      | Ok (x, st') -> collect (x :: acc) st'
      | Error _ -> Ok (List.rev acc, st)
    in
    collect []

  let many1 p =
    p >>= fun first ->
    many p >>| fun rest -> first :: rest

  let sep_by p sep =
    let rec collect acc st =
      match p st with
      | Ok (x, st') -> (
        match sep st' with
        | Ok (_, st'') -> collect (x :: acc) st''
        | Error _ -> Ok (List.rev (x :: acc), st'))
      | Error _ -> Ok (List.rev acc, st)
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

  let with_span p st =
    let start_span =
      match st.pos < List.length st.toks with
      | true ->
        let _, span = List.nth st.toks st.pos in
        span
      | false -> Span.dummy
    in
    match p st with
    | Ok (x, st') ->
      let end_span =
        match st'.pos > 0 && st'.pos <= List.length st'.toks with
        | true ->
          let _, span = List.nth st'.toks (st'.pos - 1) in
          span
        | false -> start_span
      in
      let merged_span = Span.merge start_span end_span in
      Ok ((x, merged_span), st')
    | Error e -> Error e
end
