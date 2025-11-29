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
  { st with diags = Diagnostic.add st.diags (Parse_error.diag code span args) }

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
        Parse_error.E1003
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

let peek_skip st =
  let st = skip_trivia st in
  let curr, span = peek st in
  (st, curr, span)

let parse_comma_sep parse_item st =
  let rec loop st acc =
    let st, curr, span = peek_skip st in
    match parse_item st curr span with
    | None -> (st, List.rev acc)
    | Some (st, item) ->
      let st, curr, _ = peek_skip st in
      if curr = Token.Comma then
        let st, _ = expect st Token.Comma in
        loop st (item :: acc)
      else (st, List.rev (item :: acc))
  in
  loop st []

let parse_expr st =
  (st, Node.{ kind = ExprLiteral LitUnit; span = curr_span st })

let parse_params parse_typ st =
  let st, _ = expect st Token.LParen in
  if match_token st Token.RParen then
    let st, _ = expect st Token.RParen in
    (st, [])
  else
    let parse_item st curr span =
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
        Some (st, Node.{ name; typ = type_opt })
      | _ ->
        let _ = add_error_code st Parse_error.E1105 span [] in
        None
    in
    let st, params = parse_comma_sep parse_item st in
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
        let st = add_error_code st Parse_error.E1105 span [] in
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

let rec parse_stmt st =
  let st, curr, start_span = peek_skip st in
  match curr with
  | Token.KwVal | Token.KwVar -> (
    let mutable_ = curr = Token.KwVar in
    let st = advance st in
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Ident name ->
      let st = advance st in
      let st, typ =
        if match_token st Token.Colon then
          let st, _ = expect st Token.Colon in
          let st, t = parse_typ_expr st in
          (st, Some t)
        else (st, None)
      in
      let st, _ = expect st Token.ColonEq in
      let st, value = parse_expr st in
      let st, _ = expect st Token.Semi in
      let kind = Node.StmtBinding { mutable_; name; typ; value } in
      (st, Node.{ attrs = []; kind; span = start_span })
    | _ ->
      let st = add_error_code st Parse_error.E1401 span [] in
      ( st
      , Node.
          {
            attrs = []
          ; kind = StmtExpr Node.{ kind = ExprLiteral LitUnit; span }
          ; span = start_span
          } ))
  | Token.Ident name ->
    let st = advance st in
    if match_token st Token.LtMinus then
      let st, _ = expect st Token.LtMinus in
      let st, value = parse_expr st in
      let st, _ = expect st Token.Semi in
      ( st
      , Node.{ attrs = []; kind = StmtAssign (name, value); span = start_span }
      )
    else
      let st = { st with pos = st.pos - 1 } in
      let st, expr = parse_expr st in
      let st, _ = expect st Token.Semi in
      (st, Node.{ attrs = []; kind = StmtExpr expr; span = start_span })
  | _ ->
    let st, expr = parse_expr st in
    let st, _ = expect st Token.Semi in
    (st, Node.{ attrs = []; kind = StmtExpr expr; span = start_span })

and parse_pat st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.KwVal | Token.KwVar -> (
    let mutable_ = curr = Token.KwVar in
    let st = advance st in
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Ident name ->
      let st = advance st in
      (st, Node.{ kind = PatBind { mutable_; name }; span })
    | _ ->
      let st = add_error_code st Parse_error.E1501 span [] in
      (st, Node.{ kind = PatWild; span }))
  | Token.Ident name -> (
    let st = advance st in
    let st, curr, _ = peek_skip st in
    match curr with
    | Token.LBrace ->
      let st, _ = expect st Token.LBrace in
      let parse_item st curr _span =
        match curr with
        | Token.Dot -> (
          let st = advance st in
          let st, curr, span = peek_skip st in
          match curr with
          | Token.Ident field_name ->
            let st = advance st in
            let st, pat_opt =
              if match_token st Token.ColonEq then
                let st, _ = expect st Token.ColonEq in
                let st, p = parse_pat st in
                (st, Some p)
              else (st, None)
            in
            Some (st, Node.{ name = field_name; pat = pat_opt })
          | _ ->
            let _ = add_error_code st Parse_error.E1105 span [] in
            None)
        | _ -> None
      in
      let st, fields = parse_comma_sep parse_item st in
      let st, _ = expect st Token.RBrace in
      (st, Node.{ kind = PatRecord (name, fields); span })
    | Token.LParen ->
      let st, _ = expect st Token.LParen in
      let parse_item st _curr _span =
        let st, p = parse_pat st in
        Some (st, p)
      in
      let st, pats = parse_comma_sep parse_item st in
      let st, _ = expect st Token.RParen in
      (st, Node.{ kind = PatCtor (name, pats); span })
    | _ -> (st, Node.{ kind = PatIdent name; span }))
  | Token.LParen ->
    let st = advance st in
    let st, first = parse_pat st in
    if match_token st Token.Comma then
      let st, _ = expect st Token.Comma in
      let parse_item st _curr _span =
        let st, p = parse_pat st in
        Some (st, p)
      in
      let st, rest = parse_comma_sep parse_item st in
      let st, _ = expect st Token.RParen in
      (st, Node.{ kind = PatTuple (first, rest); span })
    else
      let st, _ = expect st Token.RParen in
      (st, first)
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _ ->
    let st = advance st in
    let lit =
      match curr with
      | Token.LitNumber s -> Node.LitInt s
      | Token.LitString n -> Node.LitString n
      | Token.LitRune c -> Node.LitRune c
      | _ -> Node.LitUnit
    in
    (st, Node.{ kind = PatLiteral lit; span })
  | Token.Minus -> (
    let st = advance st in
    let st, curr, _ = peek_skip st in
    match curr with
    | Token.LitNumber s ->
      let st = advance st in
      (st, Node.{ kind = PatLiteral (LitInt ("-" ^ s)); span })
    | _ ->
      let st = add_error_code st Parse_error.E1103 span [] in
      (st, Node.{ kind = PatWild; span }))
  | _ ->
    let st = advance st in
    (st, Node.{ kind = PatWild; span })

and parse_typ_expr st =
  let parse_typ_expr_primary st =
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Caret ->
      let st = advance st in
      let st, inner = parse_typ_expr st in
      (st, Node.{ kind = TypExprPtr inner; span })
    | Token.LBrack ->
      let st = advance st in
      let st, size_opt =
        if match_token st Token.RBrack then (st, None)
        else
          let st, e = parse_expr st in
          (st, Some e)
      in
      let st, _ = expect st Token.RBrack in
      let st, elem = parse_typ_expr st in
      (st, Node.{ kind = TypExprArray (size_opt, elem); span })
    | Token.Ident name ->
      let st = advance st in
      (st, Node.{ kind = TypExprIdent name; span })
    | Token.LBrace ->
      let st, _ = expect st Token.LBrace in
      let parse_item st curr span =
        match curr with
        | Token.Ident name ->
          let st = advance st in
          let st, _ = expect st Token.Colon in
          let st, typ = parse_typ_expr st in
          let field : Node.typ_field = { name; typ } in
          Some (st, field)
        | _ ->
          let _ = add_error_code st Parse_error.E1105 span [] in
          None
      in
      let st, fields = parse_comma_sep parse_item st in
      let st, _ = expect st Token.RBrace in
      (st, Node.{ kind = TypExprRecord fields; span })
    | Token.LParen ->
      let st = advance st in
      let st, first = parse_typ_expr st in
      if match_token st Token.Comma then
        let st, _ = expect st Token.Comma in
        let parse_item st _curr _span =
          let st, t = parse_typ_expr st in
          Some (st, t)
        in
        let st, rest = parse_comma_sep parse_item st in
        let st, _ = expect st Token.RParen in
        (st, Node.{ kind = TypExprTuple (first, rest); span })
      else
        let st, _ = expect st Token.RParen in
        (st, first)
    | Token.KwDef ->
      let st = advance st in
      let st, _ = expect st Token.LParen in
      let parse_item st _curr _span =
        let st, t = parse_typ_expr st in
        Some (st, t)
      in
      let st, params = parse_comma_sep parse_item st in
      let st, _ = expect st Token.RParen in
      let st, ret =
        if match_token st Token.MinusGt then
          let st, _ = expect st Token.MinusGt in
          let st, t = parse_typ_expr st in
          (st, Some t)
        else (st, None)
      in
      (st, Node.{ kind = TypExprFunc (params, ret); span })
    | _ ->
      let st = add_error_code st Parse_error.E1104 span [] in
      (st, Node.{ kind = TypExprIdent (Interner.empty_name st.interner); span })
  in
  let st, typ = parse_typ_expr_primary st in
  if match_token st Token.Pipe then
    let rec loop st acc =
      let st, _ = expect st Token.Pipe in
      let st, _ = expect st Token.KwCase in
      let st, curr, span = peek_skip st in
      let st, case =
        match curr with
        | Token.Ident name ->
          let st = advance st in
          if match_token st Token.LParen then
            let st, _ = expect st Token.LParen in
            let parse_item st _curr _span =
              let st, typ = parse_typ_expr st in
              Some (st, typ)
            in
            let st, fields = parse_comma_sep parse_item st in
            let st, _ = expect st Token.RParen in
            (st, Node.{ name; fields })
          else (st, Node.{ name; fields = [] })
        | _ ->
          let st = add_error_code st Parse_error.E1105 span [] in
          (st, Node.{ name = Interner.empty_name st.interner; fields = [] })
      in
      let st, curr, _ = peek_skip st in
      if curr = Token.Pipe then loop st (case :: acc)
      else (st, List.rev (case :: acc))
    in
    let st, cases = loop st [] in
    (st, Node.{ kind = TypExprSum cases; span = typ.Node.span })
  else (st, typ)

let parse tokens interner =
  let st = mk_state tokens interner in
  let st = skip_trivia st in
  ([], st.diags)
