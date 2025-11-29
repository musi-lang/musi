open Basic
open Lex

type state = {
    tokens : (Token.t * Span.t) array
  ; pos : int
  ; interner : Interner.t
  ; diags : Diagnostic.bag
}

let mk_state tokens interner =
  {
    tokens = Array.of_list tokens
  ; pos = 0
  ; interner
  ; diags = Diagnostic.empty_bag
  }

let peek st =
  if st.pos >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(st.pos)

let peek_nth st n =
  let idx = st.pos + n in
  if idx >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(idx)

let advance st = { st with pos = st.pos + 1 }

let curr_span st =
  if st.pos >= Array.length st.tokens then Span.dummy
  else snd st.tokens.(st.pos)

let add_error st msg span =
  { st with diags = Diagnostic.add st.diags (Diagnostic.error msg span) }

let add_error_code st code span args =
  { st with diags = Diagnostic.add st.diags (Parse_errors.diag code span args) }

let rec skip_trivia st =
  match fst (peek st) with
  | Token.Whitespace | Token.Newline | Token.Comment _ ->
    skip_trivia (advance st)
  | _ -> st

let expect st tok =
  let st = skip_trivia st in
  let curr, span = peek st in
  if curr = tok then (advance st, span)
  else
    let st' =
      add_error_code
        st
        Parse_errors.E1003
        span
        [ Token.to_string tok; Token.to_string curr ]
    in
    (st', span)

let match_token st tok =
  let st = skip_trivia st in
  let curr, _ = peek st in
  curr = tok

let consume_if st tok =
  let st = skip_trivia st in
  let curr, _ = peek st in
  if curr = tok then (advance st, true) else (st, false)

let parse_expr st =
  (st, Node.{ kind = ExprLiteral LitUnit; span = curr_span st })

let parse_pat st = (st, Node.{ kind = PatWild; span = curr_span st })

let parse_typ_expr st =
  ( st
  , Node.
      {
        kind = TypExprIdent (Interner.empty_name st.interner)
      ; span = curr_span st
      } )

let parse_params parse_typ st =
  let st, _ = expect st Token.LParen in
  let st = skip_trivia st in
  if match_token st Token.RParen then
    let st, _ = expect st Token.RParen in
    (st, [])
  else
    let rec loop st acc =
      let st = skip_trivia st in
      let curr, span = peek st in
      match curr with
      | Token.Ident name ->
        let st = advance st in
        let st, type_opt =
          if match_token st Token.Colon then
            let st, _ = expect st Token.Colon in
            let st, t = parse_typ st in
            (st, Some t)
          else (st, None)
        in
        let param = Node.{ name; typ = type_opt } in
        let st = skip_trivia st in
        if match_token st Token.Comma then
          let st, _ = expect st Token.Comma in
          loop st (param :: acc)
        else (st, List.rev (param :: acc))
      | _ ->
        let st = add_error_code st Parse_errors.E1105 span [] in
        (st, List.rev acc)
    in
    let st, params = loop st [] in
    let st, _ = expect st Token.RParen in
    (st, params)

let parse_field_inits parse_expr st =
  let rec loop st acc =
    let st = skip_trivia st in
    let curr, _span = peek st in
    match curr with
    | Token.Dot -> (
      let st = advance st in
      let st = skip_trivia st in
      let curr, span = peek st in
      match curr with
      | Token.Ident name ->
        let st = advance st in
        let st, _ = expect st Token.ColonEq in
        let st, value = parse_expr st in
        let field = Node.{ name; value } in
        let st = skip_trivia st in
        if match_token st Token.Comma then
          let st, _ = expect st Token.Comma in
          loop st (field :: acc)
        else (st, List.rev (field :: acc))
      | _ ->
        let st = add_error_code st Parse_errors.E1105 span [] in
        (st, List.rev acc))
    | _ -> (st, List.rev acc)
  in
  loop st []

let parse_match_arm parse_pat parse_expr st =
  let st, _ = expect st Token.KwCase in
  let st, pattern = parse_pat st in
  let st, _ = expect st Token.MinusGt in
  let st, body = parse_expr st in
  let st = skip_trivia st in
  let st, _ = consume_if st Token.Comma in
  (st, Node.{ pattern; body })

let parse_typ_fields parse_typ st =
  let rec loop st acc =
    let st = skip_trivia st in
    let curr, span = peek st in
    match curr with
    | Token.Ident name ->
      let st = advance st in
      let st, _ = expect st Token.Colon in
      let st, typ = parse_typ st in
      let field = Node.{ name; typ } in
      let st = skip_trivia st in
      if match_token st Token.Comma then
        let st, _ = expect st Token.Comma in
        loop st (field :: acc)
      else (st, List.rev (field :: acc))
    | _ ->
      let st = add_error_code st Parse_errors.E1105 span [] in
      (st, List.rev acc)
  in
  loop st []

let parse_typ_case parse_typ st =
  let st, _ = expect st Token.KwCase in
  let st = skip_trivia st in
  let curr, span = peek st in
  match curr with
  | Token.Ident name ->
    let st = advance st in
    let st = skip_trivia st in
    if match_token st Token.LParen then
      let st, _ = expect st Token.LParen in
      let rec loop st acc =
        let st, typ = parse_typ st in
        let st = skip_trivia st in
        if match_token st Token.Comma then
          let st, _ = expect st Token.Comma in
          loop st (typ :: acc)
        else (st, List.rev (typ :: acc))
      in
      let st, fields = loop st [] in
      let st, _ = expect st Token.RParen in
      (st, Node.{ name; fields })
    else (st, Node.{ name; fields = [] })
  | _ ->
    let st = add_error_code st Parse_errors.E1105 span [] in
    (st, Node.{ name = Interner.empty_name st.interner; fields = [] })

let parse tokens interner =
  let st = mk_state tokens interner in
  let st = skip_trivia st in
  ([], st.diags)
