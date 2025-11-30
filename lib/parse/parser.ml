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

let peek_opt st =
  if st.pos >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(st.pos)

let peek_nth_opt st n =
  let idx = st.pos + n in
  if idx >= Array.length st.tokens then (Token.EOF, Span.dummy)
  else st.tokens.(idx)

let advance st = { st with pos = st.pos + 1 }

let current_span st =
  if st.pos >= Array.length st.tokens then Span.dummy
  else snd st.tokens.(st.pos)

let add_error st msg span =
  { st with diags = Diagnostic.add st.diags (Diagnostic.error msg span) }

let add_error_code st code span args =
  { st with diags = Diagnostic.add st.diags (Parse_error.diag code span args) }

let rec skip_trivia st =
  match fst (peek_opt st) with
  | Token.Whitespace | Token.Newline | Token.Comment _ ->
    skip_trivia (advance st)
  | _ -> st

let expect st tok =
  let st = skip_trivia st in
  let curr, span = peek_opt st in
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
  fst (peek_opt st) = tok

let consume_if st tok =
  let st = skip_trivia st in
  if fst (peek_opt st) = tok then (advance st, true) else (st, false)

let peek_skip st =
  let st = skip_trivia st in
  let curr, span = peek_opt st in
  (st, curr, span)

let parse_separated sep parse_item st =
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

let parse_delimited open_tok close_tok sep parse_item st =
  let st, _ = expect st open_tok in
  if match_token st close_tok then (fst (expect st close_tok), [])
  else
    let st, items = parse_separated sep parse_item st in
    let st, _ = expect st close_tok in
    (st, items)

let mk_stmt kind span =
  Node.
    {
      modifiers =
        { attrs = []; export_ = false; extern_ = false; unsafe_ = false }
    ; kind
    ; span
    }

let mk_literal_expr lit span = Node.{ kind = ExprLiteral lit; span }
let mk_literal_pat lit span = Node.{ kind = PatLiteral lit; span }

let parse_literal curr =
  match curr with
  | Token.LitNumber s -> Some (Node.LitInt s)
  | Token.LitString n -> Some (Node.LitString n)
  | Token.LitRune c -> Some (Node.LitRune c)
  | Token.LitTemplate n -> Some (Node.LitTemplate n)
  | _ -> None

let rec parse_cond st =
  let st, curr, _ = peek_skip st in
  if curr = Token.KwCase then
    let st, pat = parse_pat (fst (expect st Token.KwCase)) in
    let st, expr = parse_expr (fst (expect st Token.ColonEq)) in
    (st, Node.CondCase (pat, expr))
  else
    let st, expr = parse_expr st in
    (st, Node.CondExpr expr)

and parse_block st unsafeness span =
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
  (st, Node.{ kind = ExprBlock { unsafeness; stmts; ret }; span })

and parse_body st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.LBrace -> parse_block st false span
  | Token.KwUnsafe -> parse_block (advance st) true span
  | _ ->
    ( add_error_code st Parse_error.E1108 span []
    , Node.
        {
          kind = ExprBlock { unsafeness = false; stmts = []; ret = None }
        ; span
        } )

and parse_expr_literal st curr span =
  let st = advance st in
  match parse_literal curr with
  | Some lit -> (st, mk_literal_expr lit span)
  | None ->
    let _ = add_error_code st Parse_error.E1102 span [] in
    (st, mk_literal_expr LitUnit span)

and parse_expr_tuple st first =
  let st, _ = expect st Token.Comma in
  let st, rest =
    parse_separated Token.Comma (fun st _ _ -> Some (parse_expr st)) st
  in
  let st, _ = expect st Token.RParen in
  ( st
  , Node.
      {
        kind = ExprTuple (first, rest)
      ; span = Span.merge first.Node.span (current_span st)
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
      (st, mk_literal_expr LitUnit span)
    else
      let st, first = parse_expr st in
      if match_token st Token.Comma then parse_expr_tuple st first
      else
        let st, _ = expect st Token.RParen in
        (st, first)
  | Token.Ident name -> (advance st, Node.{ kind = ExprIdent name; span })
  | Token.LBrace -> parse_block st false span
  | Token.KwUnsafe -> parse_block (advance st) true span
  | Token.KwIf -> parse_expr_if st span
  | Token.KwMatch -> parse_expr_match st span
  | Token.KwWhile -> parse_expr_while st span
  | Token.KwFor -> parse_expr_for st span
  | _ ->
    (add_error_code st Parse_error.E1102 span [], mk_literal_expr LitUnit span)

and parse_expr_if st start_span =
  let st = advance st in
  let st, first = parse_cond st in
  let rec parse_rest st acc =
    if match_token st Token.Comma then
      let st = fst (expect st Token.Comma) in
      let st, c = parse_cond st in
      parse_rest st (c :: acc)
    else (st, List.rev acc)
  in
  let st, rest = parse_rest st [] in
  let conds = first :: rest in
  let st, then_block = parse_body st in
  let st, else_block =
    if match_token st Token.KwElse then
      let st, block = parse_body (advance st) in
      (st, Some block)
    else (st, None)
  in
  let then_blk =
    match then_block.Node.kind with
    | ExprBlock b -> b
    | _ -> { unsafeness = false; stmts = []; ret = None }
  in
  let else_blk =
    match else_block with
    | Some e -> ( match e.Node.kind with ExprBlock b -> Some b | _ -> None)
    | None -> None
  in
  (st, Node.{ kind = ExprIf (conds, then_blk, else_blk); span = start_span })

and parse_expr_match st start_span =
  let parse_arm st =
    let st, pattern = parse_pat (fst (expect st Token.KwCase)) in
    let st, guard =
      if match_token st Token.KwIf then
        let st, g = parse_expr (fst (expect st Token.KwIf)) in
        (st, Some g)
      else (st, None)
    in
    let st, body = parse_expr (fst (expect st Token.MinusGt)) in
    let st, _ = consume_if st Token.Comma in
    (st, Node.{ pattern; guard; body })
  in
  let st, scrutinee = parse_expr (advance st) in
  let st, _ = expect st Token.LBrace in
  let st, arms =
    parse_separated
      Token.Comma
      (fun st _ _ ->
        if match_token st Token.KwCase then Some (parse_arm st) else None)
      st
  in
  let st, _ = expect st Token.RBrace in
  (st, Node.{ kind = ExprMatch (scrutinee, arms); span = start_span })

and parse_expr_while st start_span =
  let st, cond = parse_cond (advance st) in
  let st, guard =
    if match_token st Token.KwIf then
      let st, g = parse_expr (fst (expect st Token.KwIf)) in
      (st, Some g)
    else (st, None)
  in
  let st, body_expr = parse_body st in
  let body =
    match body_expr.Node.kind with
    | ExprBlock b -> b
    | _ -> { unsafeness = false; stmts = []; ret = None }
  in
  (st, Node.{ kind = ExprWhile (cond, guard, body); span = start_span })

and parse_expr_for st start_span =
  let st, curr, span = peek_skip (advance st) in
  let st, binding =
    if curr = Token.KwCase then
      let st, pat = parse_pat (fst (expect st Token.KwCase)) in
      (st, Node.ForCase pat)
    else
      match curr with
      | Token.Ident name -> (advance st, Node.ForIdent name)
      | _ ->
        ( add_error_code st Parse_error.E1105 span []
        , Node.ForIdent (Interner.empty_name st.interner) )
  in
  let st, range = parse_expr (fst (expect st Token.KwIn)) in
  let st, step =
    if match_token st Token.KwStep then
      let st, s = parse_expr (fst (expect st Token.KwStep)) in
      (st, Some s)
    else (st, None)
  in
  let st, guard =
    if match_token st Token.KwIf then
      let st, g = parse_expr (fst (expect st Token.KwIf)) in
      (st, Some g)
    else (st, None)
  in
  let st, body_expr = parse_body st in
  let body =
    match body_expr.Node.kind with
    | ExprBlock b -> b
    | _ -> { unsafeness = false; stmts = []; ret = None }
  in
  ( st
  , Node.
      {
        kind = ExprFor { binding; range; step; guard; body }
      ; span = start_span
      } )

and parse_expr_prefix st op =
  let st, right = parse_expr_bp (advance st) (Prec.prec_value Prec.Unary) in
  ( st
  , Node.
      {
        kind = ExprUnary (op, right)
      ; span = Span.merge (current_span st) right.Node.span
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
        ; span = Span.merge left.Node.span (current_span st)
        } )
  | Token.LParen ->
    let st, args =
      parse_separated
        Token.Comma
        (fun st _ _ -> Some (parse_expr st))
        (advance st)
    in
    let st, _ = expect st Token.RParen in
    ( st
    , Node.
        {
          kind = ExprCall (left, args)
        ; span = Span.merge left.Node.span (current_span st)
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

and parse_import_clause st =
  let st, curr, span = peek_skip st in
  if curr = Token.Star then
    let st, _ = expect (advance st) Token.KwAs in
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Ident name -> (advance st, Node.ImportAll name)
    | _ ->
      ( add_error_code st Parse_error.E1105 span []
      , Node.ImportAll (Interner.empty_name st.interner) )
  else if curr = Token.LBrace then
    let st, items =
      parse_delimited
        Token.LBrace
        Token.RBrace
        Token.Comma
        (fun st curr span ->
          match curr with
          | Token.Ident name -> Some (advance st, name)
          | _ ->
            let _ = add_error_code st Parse_error.E1105 span [] in
            None)
        st
    in
    (st, Node.ImportNamed items)
  else (add_error_code st Parse_error.E1101 span [], Node.ImportNamed [])

and parse_export_clause st =
  let st, curr, span = peek_skip st in
  if curr = Token.Star then
    let st, _ = expect (advance st) Token.KwAs in
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Ident name -> (advance st, Node.ExportAll name)
    | _ ->
      ( add_error_code st Parse_error.E1105 span []
      , Node.ExportAll (Interner.empty_name st.interner) )
  else if curr = Token.LBrace then
    let st, items =
      parse_delimited
        Token.LBrace
        Token.RBrace
        Token.Comma
        (fun st curr span ->
          match curr with
          | Token.Ident name -> Some (advance st, name)
          | _ ->
            let _ = add_error_code st Parse_error.E1105 span [] in
            None)
        st
    in
    (st, Node.ExportNamed items)
  else (add_error_code st Parse_error.E1101 span [], Node.ExportNamed [])

and parse_stmt_import st start_span =
  let st, clause = parse_import_clause (advance st) in
  let st, curr, span = peek_skip st in
  let st, path =
    if curr = Token.KwFrom then
      let st, curr, span = peek_skip (advance st) in
      match curr with
      | Token.LitString n -> (advance st, n)
      | _ ->
        ( add_error_code st Parse_error.E1115 span []
        , Interner.empty_name st.interner )
    else
      ( add_error_code st Parse_error.E1115 span []
      , Interner.empty_name st.interner )
  in
  let st, _ = expect st Token.Semi in
  (st, mk_stmt (StmtImport (clause, path)) start_span)

and parse_stmt_export st start_span =
  let st, clause = parse_export_clause (advance st) in
  let st, path_opt =
    if match_token st Token.KwFrom then
      let st, curr, span = peek_skip (advance st) in
      match curr with
      | Token.LitString n -> (advance st, Some n)
      | _ -> (add_error_code st Parse_error.E1115 span [], None)
    else (st, None)
  in
  let st, _ = expect st Token.Semi in
  (st, mk_stmt (StmtExport (clause, path_opt)) start_span)

and parse_stmt_binding st start_span mutable_ =
  let st, curr, span = peek_skip (advance st) in
  match curr with
  | Token.Ident name ->
    let st = advance st in
    let st, typ =
      if match_token st Token.Colon then
        let st, t = parse_typ_expr (fst (expect st Token.Colon)) in
        (st, Some t)
      else (st, None)
    in
    let st, value = parse_expr (fst (expect st Token.ColonEq)) in
    let st, _ = expect st Token.Semi in
    (st, mk_stmt (StmtBinding { mutable_; name; typ; value }) start_span)
  | _ ->
    ( add_error_code st Parse_error.E1401 span []
    , mk_stmt (StmtExpr (mk_literal_expr LitUnit span)) start_span )

and parse_stmt_ident st start_span name =
  let st = advance st in
  if match_token st Token.LtMinus then
    let st, value = parse_expr (fst (expect st Token.LtMinus)) in
    let st, _ = expect st Token.Semi in
    (st, mk_stmt (StmtAssign (name, value)) start_span)
  else
    let st = { st with pos = st.pos - 1 } in
    let st, expr = parse_expr st in
    let st, _ = expect st Token.Semi in
    (st, mk_stmt (StmtExpr expr) start_span)

and parse_stmt st =
  let st, curr, start_span = peek_skip st in
  match curr with
  | Token.KwImport -> parse_stmt_import st start_span
  | Token.KwExport -> parse_stmt_export st start_span
  | Token.KwVal -> parse_stmt_binding st start_span false
  | Token.KwVar -> parse_stmt_binding st start_span true
  | Token.Ident name -> parse_stmt_ident st start_span name
  | _ ->
    let st, expr = parse_expr st in
    let st, _ = expect st Token.Semi in
    (st, mk_stmt (StmtExpr expr) start_span)

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
    let st, fields =
      parse_delimited
        Token.LBrace
        Token.RBrace
        Token.Comma
        parse_pat_record_field
        st
    in
    (st, Node.{ kind = PatRecord (name, fields); span })
  | Token.LParen ->
    let st, pats =
      parse_delimited
        Token.LParen
        Token.RParen
        Token.Comma
        (fun st _ _ -> Some (parse_pat st))
        st
    in
    (st, Node.{ kind = PatCtor (name, pats); span })
  | _ -> (st, Node.{ kind = PatIdent name; span })

and parse_pat_tuple st span =
  let st, first = parse_pat (advance st) in
  if match_token st Token.Comma then
    let st, rest =
      parse_separated
        Token.Comma
        (fun st _ _ -> Some (parse_pat st))
        (fst (expect st Token.Comma))
    in
    let st, _ = expect st Token.RParen in
    (st, Node.{ kind = PatTuple (first, rest); span })
  else
    let st, _ = expect st Token.RParen in
    (st, first)

and parse_pat_literal st curr span =
  match curr with
  | Token.Minus -> (
    let st, curr, _ = peek_skip (advance st) in
    match curr with
    | Token.LitNumber s -> (advance st, mk_literal_pat (LitInt ("-" ^ s)) span)
    | _ ->
      ( add_error_code st Parse_error.E1103 span []
      , Node.{ kind = PatWild; span } ))
  | _ -> (
    match parse_literal curr with
    | Some lit -> (advance st, mk_literal_pat lit span)
    | None -> (advance st, Node.{ kind = PatWild; span }))

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
  | Token.LParen -> parse_pat_tuple st span
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _ | Token.Minus ->
    parse_pat_literal st curr span
  | _ -> (advance st, Node.{ kind = PatWild; span })

and parse_typ_field st curr span =
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
      let st, fields =
        parse_separated
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

and parse_typ_expr_ptr st span =
  let st, inner = parse_typ_expr (advance st) in
  (st, Node.{ kind = TypExprPtr inner; span })

and parse_typ_expr_array st span =
  let st = advance st in
  let st, size_opt =
    if match_token st Token.RBrack then (st, None)
    else
      let st, e = parse_expr st in
      (st, Some e)
  in
  let st, elem = parse_typ_expr (fst (expect st Token.RBrack)) in
  (st, Node.{ kind = TypExprArray (size_opt, elem); span })

and parse_typ_expr_tuple st span =
  let st, first = parse_typ_expr (advance st) in
  if match_token st Token.Comma then
    let st, rest =
      parse_separated
        Token.Comma
        (fun st _ _ -> Some (parse_typ_expr st))
        (fst (expect st Token.Comma))
    in
    let st, _ = expect st Token.RParen in
    (st, Node.{ kind = TypExprTuple (first, rest); span })
  else
    let st, _ = expect st Token.RParen in
    (st, first)

and parse_typ_expr_func st span =
  let st, params =
    parse_separated
      Token.Comma
      (fun st _ _ -> Some (parse_typ_expr st))
      (fst (expect (advance st) Token.LParen))
  in
  let st, _ = expect st Token.RParen in
  let st, ret =
    if match_token st Token.MinusGt then
      let st, t = parse_typ_expr (fst (expect st Token.MinusGt)) in
      (st, Some t)
    else (st, None)
  in
  (st, Node.{ kind = TypExprFunc (params, ret); span })

and parse_typ_expr_record st span =
  let st, fields =
    parse_separated Token.Comma parse_typ_field (fst (expect st Token.LBrace))
  in
  let st, _ = expect st Token.RBrace in
  (st, Node.{ kind = TypExprRecord fields; span })

and parse_typ_expr_primary st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.Caret -> parse_typ_expr_ptr st span
  | Token.LBrack -> parse_typ_expr_array st span
  | Token.Ident name -> (advance st, Node.{ kind = TypExprIdent name; span })
  | Token.LBrace -> parse_typ_expr_record st span
  | Token.LParen -> parse_typ_expr_tuple st span
  | Token.KwDef -> parse_typ_expr_func st span
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
  let st, params =
    parse_delimited Token.LParen Token.RParen Token.Comma parse_item st
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
