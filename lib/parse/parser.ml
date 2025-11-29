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
    ( add_error_code
        st
        Parse_error.E1003
        span
        [ Token.to_string tok; Token.to_string curr ]
    , span )

let match_token st tok =
  let st = skip_trivia st in
  fst (peek st) = tok

let consume_if st tok =
  let st = skip_trivia st in
  if fst (peek st) = tok then (advance st, true) else (st, false)

let peek_skip st =
  let st = skip_trivia st in
  let curr, span = peek st in
  (st, curr, span)

let parse_sep sep parse_item st =
  let rec loop st acc =
    let st, curr, span = peek_skip st in
    match parse_item st curr span with
    | None -> (st, List.rev acc)
    | Some (st, item) ->
      let st, curr, _ = peek_skip st in
      if curr = sep then loop (fst (expect st sep)) (item :: acc)
      else (st, List.rev (item :: acc))
  in
  loop st []

let parse_delim open_tok close_tok sep parse_item st =
  let st, _ = expect st open_tok in
  if match_token st close_tok then (fst (expect st close_tok), [])
  else
    let st, items = parse_sep sep parse_item st in
    let st, _ = expect st close_tok in
    (st, items)

let rec parse_expr_literal st curr span =
  let st = advance st in
  let kind =
    match curr with
    | Token.LitNumber s -> Node.ExprLiteral (LitInt s)
    | Token.LitString n -> Node.ExprLiteral (LitString n)
    | Token.LitRune c -> Node.ExprLiteral (LitRune c)
    | Token.LitTemplate n -> Node.ExprLiteral (LitTemplate n)
    | _ ->
      let _ = add_error_code st Parse_error.E1102 span [] in
      Node.ExprLiteral LitUnit
  in
  (st, Node.{ kind; span })

and parse_expr_tuple st first =
  let st, _ = expect st Token.Comma in
  let st, rest =
    parse_sep Token.Comma (fun st _ _ -> Some (parse_expr st)) st
  in
  let st, _ = expect st Token.RParen in
  ( st
  , Node.
      {
        kind = ExprTuple (first, rest)
      ; span = Span.merge first.Node.span (curr_span st)
      } )

and parse_expr_block st unsafeness start_span =
  let st, _ = expect st Token.LBrace in
  let rec loop st stmts =
    let st, curr, _ = peek_skip st in
    match curr with
    | Token.RBrace | Token.EOF -> (st, List.rev stmts, None)
    | _ ->
      let st, stmt = parse_stmt st in
      let st, curr, _ = peek_skip st in
      if curr = Token.RBrace then
        match stmt.Node.kind with
        | Node.StmtExpr e -> (st, List.rev stmts, Some e)
        | _ -> (st, List.rev (stmt :: stmts), None)
      else loop st (stmt :: stmts)
  in
  let st, stmts, ret = loop st [] in
  let st, _ = expect st Token.RBrace in
  ( st
  , Node.
      {
        kind = ExprBlock { unsafeness; stmts; ret }
      ; span = Span.merge start_span (curr_span st)
      } )

and parse_expr_primary st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _
  | Token.LitTemplate _ ->
    parse_expr_literal st curr span
  | Token.LParen ->
    let st = advance st in
    if match_token st Token.RParen then
      let st, _ = expect st Token.RParen in
      (st, Node.{ kind = ExprLiteral LitUnit; span })
    else
      let st, first = parse_expr st in
      if match_token st Token.Comma then parse_expr_tuple st first
      else
        let st, _ = expect st Token.RParen in
        (st, first)
  | Token.Ident name -> (advance st, Node.{ kind = ExprIdent name; span })
  | Token.LBrace -> parse_expr_block st false span
  | Token.KwUnsafe -> parse_expr_block (advance st) true span
  | _ ->
    ( add_error_code st Parse_error.E1102 span []
    , Node.{ kind = ExprLiteral LitUnit; span } )

and parse_expr_prefix st op =
  let st, right = parse_expr_bp (advance st) (Prec.prec_value Prec.Unary) in
  ( st
  , Node.
      {
        kind = ExprUnary (op, right)
      ; span = Span.merge (curr_span st) right.Node.span
      } )

and parse_expr_postfix st left =
  let st, curr, _ = peek_skip st in
  match curr with
  | Token.Dot -> (
    let st, curr, span = peek_skip (advance st) in
    match curr with
    | Token.Ident name ->
      ( advance st
      , Node.
          {
            kind = ExprField (left, name)
          ; span = Span.merge left.Node.span span
          } )
    | _ -> (add_error_code st Parse_error.E1105 span [], left))
  | Token.LBrack ->
    let st, index = parse_expr (advance st) in
    let st, _ = expect st Token.RBrack in
    ( st
    , Node.
        {
          kind = ExprIndex (left, index)
        ; span = Span.merge left.Node.span (curr_span st)
        } )
  | Token.LParen ->
    let st, args =
      parse_sep Token.Comma (fun st _ _ -> Some (parse_expr st)) (advance st)
    in
    let st, _ = expect st Token.RParen in
    ( st
    , Node.
        {
          kind = ExprCall (left, args)
        ; span = Span.merge left.Node.span (curr_span st)
        } )
  | _ -> (st, left)

and parse_expr_infix st left op min_bp =
  match Prec.token_prec op with
  | None -> (st, left)
  | Some (prec, assoc) ->
    let prec_val = Prec.prec_value prec in
    if prec_val < min_bp then (st, left)
    else
      let next_bp = if assoc = Prec.Right then prec_val else prec_val + 1 in
      let st, right = parse_expr_bp (advance st) next_bp in
      ( st
      , Node.
          {
            kind = ExprBinary (op, left, right)
          ; span = Span.merge left.Node.span right.Node.span
          } )

and parse_expr_bp st min_bp =
  let st, curr, _ = peek_skip st in
  let st, left =
    if Prec.is_prefix_op curr then parse_expr_prefix st curr
    else parse_expr_primary st
  in
  let rec loop st left =
    let st, curr, _ = peek_skip st in
    if Prec.is_postfix_op curr then
      loop (fst (parse_expr_postfix st left)) (snd (parse_expr_postfix st left))
    else
      match Prec.token_prec curr with
      | Some (prec, _) when Prec.prec_value prec >= min_bp ->
        let st, left = parse_expr_infix st left curr min_bp in
        loop st left
      | _ -> (st, left)
  in
  loop st left

and parse_expr st = parse_expr_bp st 0

and parse_stmt st =
  let st, curr, start_span = peek_skip st in
  let mk_stmt kind =
    Node.
      {
        modifiers =
          { attrs = []; export_ = false; extern_ = false; unsafe_ = false }
      ; kind
      ; span = start_span
      }
  in
  match curr with
  | Token.KwVal | Token.KwVar -> (
    let mutable_ = curr = Token.KwVar in
    let st, curr, span = peek_skip (advance st) in
    match curr with
    | Token.Ident name ->
      let st = advance st in
      let st, (typ : Node.typ_expr option) =
        if match_token st Token.Colon then
          let st, t = parse_typ_expr (fst (expect st Token.Colon)) in
          (st, Some t)
        else (st, None)
      in
      let st, value = parse_expr (fst (expect st Token.ColonEq)) in
      let st, _ = expect st Token.Semi in
      (st, mk_stmt (StmtBinding { mutable_; name; typ; value }))
    | _ ->
      ( add_error_code st Parse_error.E1401 span []
      , mk_stmt (StmtExpr Node.{ kind = ExprLiteral LitUnit; span }) ))
  | Token.Ident name ->
    let st = advance st in
    if match_token st Token.LtMinus then
      let st, value = parse_expr (fst (expect st Token.LtMinus)) in
      let st, _ = expect st Token.Semi in
      (st, mk_stmt (StmtAssign (name, value)))
    else
      let st = { st with pos = st.pos - 1 } in
      let st, expr = parse_expr st in
      let st, _ = expect st Token.Semi in
      (st, mk_stmt (StmtExpr expr))
  | _ ->
    let st, expr = parse_expr st in
    let st, _ = expect st Token.Semi in
    (st, mk_stmt (StmtExpr expr))

and parse_pat_record_field st curr _span =
  match curr with
  | Token.Dot -> (
    let st, curr, span = peek_skip (advance st) in
    match curr with
    | Token.Ident field_name ->
      let st = advance st in
      let st, pat_opt =
        if match_token st Token.ColonEq then
          let st, p = parse_pat (fst (expect st Token.ColonEq)) in
          (st, Some p)
        else (st, None)
      in
      Some (st, Node.{ name = field_name; pat = pat_opt })
    | _ ->
      let _ = add_error_code st Parse_error.E1105 span [] in
      None)
  | _ -> None

and parse_pat_ident st name span =
  let st, curr, _ = peek_skip (advance st) in
  match curr with
  | Token.LBrace ->
    let st, (fields : Node.pat_field list) =
      parse_delim
        Token.LBrace
        Token.RBrace
        Token.Comma
        parse_pat_record_field
        st
    in
    (st, Node.{ kind = PatRecord (name, fields); span })
  | Token.LParen ->
    let st, (pats : Node.pat list) =
      parse_delim
        Token.LParen
        Token.RParen
        Token.Comma
        (fun st _ _ -> Some (parse_pat st))
        st
    in
    (st, Node.{ kind = PatCtor (name, pats); span })
  | _ -> (st, Node.{ kind = PatIdent name; span })

and parse_pat st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.KwVal | Token.KwVar -> (
    let mutable_ = curr = Token.KwVar in
    let st, curr, span = peek_skip (advance st) in
    match curr with
    | Token.Ident name ->
      (advance st, Node.{ kind = PatBinding { mutable_; name }; span })
    | _ ->
      ( add_error_code st Parse_error.E1501 span []
      , Node.{ kind = PatWild; span } ))
  | Token.Ident name -> parse_pat_ident st name span
  | Token.LParen ->
    let st, first = parse_pat (advance st) in
    if match_token st Token.Comma then
      let st, (rest : Node.pat list) =
        parse_sep
          Token.Comma
          (fun st _ _ -> Some (parse_pat st))
          (fst (expect st Token.Comma))
      in
      let st, _ = expect st Token.RParen in
      (st, Node.{ kind = PatTuple (first, rest); span })
    else
      let st, _ = expect st Token.RParen in
      (st, first)
  | Token.LitNumber s ->
    (advance st, Node.{ kind = PatLiteral (LitInt s); span })
  | Token.LitString n ->
    (advance st, Node.{ kind = PatLiteral (LitString n); span })
  | Token.LitRune c -> (advance st, Node.{ kind = PatLiteral (LitRune c); span })
  | Token.Minus -> (
    let st, curr, _ = peek_skip (advance st) in
    match curr with
    | Token.LitNumber s ->
      (advance st, Node.{ kind = PatLiteral (LitInt ("-" ^ s)); span })
    | _ ->
      ( add_error_code st Parse_error.E1103 span []
      , Node.{ kind = PatWild; span } ))
  | _ -> (advance st, Node.{ kind = PatWild; span })

and parse_typ_field st curr span : (state * Node.typ_field) option =
  match curr with
  | Token.Ident name ->
    let st, typ = parse_typ_expr (fst (expect (advance st) Token.Colon)) in
    Some (st, (Node.{ name; typ } : Node.typ_field))
  | _ ->
    let _ = add_error_code st Parse_error.E1105 span [] in
    None

and parse_typ_case st =
  let st, curr, span =
    peek_skip (fst (expect (fst (expect st Token.Pipe)) Token.KwCase))
  in
  match curr with
  | Token.Ident name ->
    let st = advance st in
    if match_token st Token.LParen then
      let st, (fields : Node.typ_expr list) =
        parse_sep
          Token.Comma
          (fun st _ _ -> Some (parse_typ_expr st))
          (fst (expect st Token.LParen))
      in
      let st, _ = expect st Token.RParen in
      (st, (Node.{ name; fields } : Node.typ_case))
    else (st, (Node.{ name; fields = [] } : Node.typ_case))
  | _ ->
    ( add_error_code st Parse_error.E1105 span []
    , (Node.{ name = Interner.empty_name st.interner; fields = [] }
        : Node.typ_case) )

and parse_typ_expr_primary st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.Caret ->
    let st, inner = parse_typ_expr (advance st) in
    (st, Node.{ kind = TypExprPtr inner; span })
  | Token.LBrack ->
    let st = advance st in
    let st, size_opt =
      if match_token st Token.RBrack then (st, None)
      else
        let st, e = parse_expr st in
        (st, Some e)
    in
    let st, elem = parse_typ_expr (fst (expect st Token.RBrack)) in
    (st, Node.{ kind = TypExprArray (size_opt, elem); span })
  | Token.Ident name -> (advance st, Node.{ kind = TypExprIdent name; span })
  | Token.LBrace ->
    let st, (fields : Node.typ_field list) =
      parse_sep Token.Comma parse_typ_field (fst (expect st Token.LBrace))
    in
    let st, _ = expect st Token.RBrace in
    (st, Node.{ kind = TypExprRecord fields; span })
  | Token.LParen ->
    let st, first = parse_typ_expr (advance st) in
    if match_token st Token.Comma then
      let st, (rest : Node.typ_expr list) =
        parse_sep
          Token.Comma
          (fun st _ _ -> Some (parse_typ_expr st))
          (fst (expect st Token.Comma))
      in
      let st, _ = expect st Token.RParen in
      (st, Node.{ kind = TypExprTuple (first, rest); span })
    else
      let st, _ = expect st Token.RParen in
      (st, first)
  | Token.KwDef ->
    let st, (params : Node.typ_expr list) =
      parse_sep
        Token.Comma
        (fun st _ _ -> Some (parse_typ_expr st))
        (fst (expect (advance st) Token.LParen))
    in
    let st, _ = expect st Token.RParen in
    let st, (ret : Node.typ_expr option) =
      if match_token st Token.MinusGt then
        let st, t = parse_typ_expr (fst (expect st Token.MinusGt)) in
        (st, Some t)
      else (st, None)
    in
    (st, Node.{ kind = TypExprFunc (params, ret); span })
  | _ ->
    ( add_error_code st Parse_error.E1104 span []
    , Node.{ kind = TypExprIdent (Interner.empty_name st.interner); span } )

and parse_typ_expr st =
  let st, typ = parse_typ_expr_primary st in
  if match_token st Token.Pipe then
    let rec loop st acc =
      let st, case = parse_typ_case st in
      if match_token st Token.Pipe then loop st (case :: acc)
      else (st, List.rev (case :: acc))
    in
    let st, cases = loop st [] in
    (st, Node.{ kind = TypExprSum cases; span = typ.Node.span })
  else (st, typ)

let parse_params parse_typ st =
  let parse_item st curr span =
    match curr with
    | Token.Ident name ->
      let st = advance st in
      let st, typ =
        if match_token st Token.Colon then
          let st, t = parse_typ (fst (expect st Token.Colon)) in
          (st, Some t)
        else (st, None)
      in
      Some (st, Node.{ name; typ })
    | _ ->
      let _ = add_error_code st Parse_error.E1105 span [] in
      None
  in
  let st, (params : Node.param list) =
    parse_delim Token.LParen Token.RParen Token.Comma parse_item st
  in
  (st, params)

let parse_field_inits parse_expr st =
  let rec loop st acc =
    let st, curr, _span = peek_skip st in
    match curr with
    | Token.Dot -> (
      let st, curr, span = peek_skip (advance st) in
      match curr with
      | Token.Ident name ->
        let st, value = parse_expr (fst (expect (advance st) Token.ColonEq)) in
        let st, _ = consume_if st Token.Comma in
        loop st (Node.{ name; value } :: acc)
      | _ -> (add_error_code st Parse_error.E1105 span [], List.rev acc))
    | _ -> (st, List.rev acc)
  in
  loop st []

let parse_match_arm parse_pat parse_expr st =
  let st, pattern = parse_pat (fst (expect st Token.KwCase)) in
  let st, body = parse_expr (fst (expect st Token.MinusGt)) in
  let st, _ = consume_if st Token.Comma in
  (st, Node.{ pattern; body })

let parse_prog tokens interner =
  let st = mk_state tokens interner in
  let rec loop st stmts =
    let st, curr, _ = peek_skip st in
    match curr with
    | Token.EOF -> (st, List.rev stmts)
    | _ ->
      let st, stmt = parse_stmt st in
      loop st (stmt :: stmts)
  in
  let st, stmts = loop st [] in
  (stmts, st.diags)
