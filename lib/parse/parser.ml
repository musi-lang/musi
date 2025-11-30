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

let mk_literal_expr lit span = Node.{ kind = ExprLiteral lit; span }
let mk_literal_pat lit span = Node.{ kind = PatLiteral lit; span }
let mk_expr kind span = Node.{ kind; span }
let mk_pat kind span = Node.{ kind; span }
let expect_tok st tok = fst (expect st tok)
let empty_name st = Interner.empty_name st.interner

let error_expr st code span =
  (add_error_code st code span [], mk_literal_expr LitUnit span)

let error_pat st code span =
  (add_error_code st code span [], mk_pat Node.PatWild span)

let error_typ st code span =
  ( add_error_code st code span []
  , Node.{ kind = TypExprIdent (empty_name st); span } )

let error_name st code span = (add_error_code st code span [], empty_name st)
let mk_node kind span = Node.{ kind; span }

let parse_sep sep parse_item st =
  let rec loop st acc =
    let st, curr, span = peek_skip st in
    match parse_item st curr span with
    | None -> (st, List.rev acc)
    | Some (st, item) ->
      let st, curr, _ = peek_skip st in
      if curr = sep then loop (expect_tok st sep) (item :: acc)
      else (st, List.rev (item :: acc))
  in
  loop st []

let parse_list sep f st = parse_sep sep (fun st _ _ -> Some (f st)) st

let parse_delim open_tok close_tok sep parse_item st =
  let st = expect_tok st open_tok in
  if match_token st close_tok then (expect_tok st close_tok, [])
  else
    let st, items = parse_sep sep parse_item st in
    (expect_tok st close_tok, items)

let unwrap_block = function
  | Node.ExprBlock b | ExprUnsafe b -> b
  | _ -> { stmts = []; ret = None }

let parse_opt_type st tok parse_fn =
  if match_token st tok then
    let st, t = parse_fn (expect_tok st tok) in
    (st, Some t)
  else (st, None)

let parse_name st code =
  let st, curr, span = peek_skip (advance st) in
  match curr with
  | Token.Ident n -> (advance st, n)
  | _ -> error_name st code span

let parse_tuple_or_single close_tok parse_fn mk_tuple st span =
  let st, first = parse_fn (advance st) in
  if match_token st Token.Comma then
    let st, rest =
      parse_list Token.Comma parse_fn (expect_tok st Token.Comma)
    in
    (expect_tok st close_tok, mk_tuple first rest span)
  else (expect_tok st close_tok, first)

let parse_sep_pipe parse_fn st =
  let rec loop st acc =
    let st, item = parse_fn st in
    if match_token st Token.Pipe then loop st (item :: acc)
    else (st, List.rev (item :: acc))
  in
  loop st []

let mk_stmt kind span =
  Node.
    {
      modifiers =
        { attrs = []; export_ = false; extern_ = false; unsafe_ = false }
    ; kind
    ; span
    }

let parse_literal curr =
  match curr with
  | Token.LitNumber s -> Some (Node.LitInt s)
  | Token.LitString n -> Some (Node.LitString n)
  | Token.LitRune c -> Some (Node.LitRune c)
  | Token.LitTemplate n -> Some (Node.LitTemplate n)
  | _ -> None

let parse_params parse_typ st =
  parse_delim
    Token.LParen
    Token.RParen
    Token.Comma
    (fun st curr span ->
      match curr with
      | Token.Ident name ->
        let st = advance st in
        let st, typ = parse_opt_type st Token.Colon parse_typ in
        Some (st, Node.{ name; typ })
      | _ ->
        let _ = add_error_code st Parse_error.E1105 span [] in
        None)
    st

let parse_field_inits parse_expr st =
  let rec loop st acc =
    let st, curr, _span = peek_skip st in
    match curr with
    | Token.Dot -> (
      let st, curr, span = peek_skip (advance st) in
      match curr with
      | Token.Ident name ->
        let st, value = parse_expr (expect_tok (advance st) Token.ColonEq) in
        let st, _ = consume_if st Token.Comma in
        loop st (Node.{ name; value } :: acc)
      | _ -> (add_error_code st Parse_error.E1105 span [], List.rev acc))
    | _ -> (st, List.rev acc)
  in
  loop st []

let rec parse_cond st =
  let st, curr, _ = peek_skip st in
  if curr = Token.KwCase then
    let st, pat = parse_pat (expect_tok st Token.KwCase) in
    let st, expr = parse_expr (expect_tok st Token.ColonEq) in
    (st, Node.CondCase (pat, expr))
  else
    let st, expr = parse_expr st in
    (st, Node.CondExpr expr)

and parse_block st span =
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
  (st, Node.{ kind = ExprBlock { stmts; ret }; span })

and parse_body st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.LBrace -> parse_block st span
  | Token.KwUnsafe ->
    let st, block_expr = parse_block (advance st) span in
    let block =
      match block_expr.Node.kind with
      | ExprBlock b -> b
      | _ -> { stmts = []; ret = None }
    in
    (st, Node.{ kind = ExprUnsafe block; span })
  | _ ->
    ( add_error_code st Parse_error.E1108 span []
    , Node.{ kind = ExprBlock { stmts = []; ret = None }; span } )

and parse_expr_literal st curr span =
  let st = advance st in
  match parse_literal curr with
  | Some lit -> (st, mk_literal_expr lit span)
  | None ->
    let _ = add_error_code st Parse_error.E1102 span [] in
    (st, mk_literal_expr LitUnit span)

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
      if match_token st Token.Comma then
        let st, rest =
          parse_list Token.Comma parse_expr (expect_tok st Token.Comma)
        in
        let st, _ = expect st Token.RParen in
        ( st
        , Node.
            {
              kind = ExprTuple (first, rest)
            ; span = Span.merge first.Node.span (current_span st)
            } )
      else
        let st, _ = expect st Token.RParen in
        (st, first)
  | Token.Ident name ->
    let st = advance st in
    let st, curr, _ = peek_skip st in
    if curr = Token.LBrace then
      let next_tok, _ = peek_nth_opt st 1 in
      if next_tok = Token.Dot then
        let st, _ = expect st Token.LBrace in
        let st, fields = parse_field_inits parse_expr st in
        let st, _ = expect st Token.RBrace in
        (st, Node.{ kind = ExprRecord (Some name, fields); span })
      else (st, Node.{ kind = ExprIdent name; span })
    else (st, Node.{ kind = ExprIdent name; span })
  | Token.LBrace ->
    let st = advance st in
    let st, curr, _ = peek_skip st in
    if curr = Token.Dot then
      let st = { st with pos = st.pos - 1 } in
      let st, _ = expect st Token.LBrace in
      let st, fields = parse_field_inits parse_expr st in
      let st, _ = expect st Token.RBrace in
      (st, Node.{ kind = ExprRecord (None, fields); span })
    else
      let st = { st with pos = st.pos - 1 } in
      parse_block st span
  | Token.KwUnsafe ->
    let st, block_expr = parse_block (advance st) span in
    (st, mk_expr (Node.ExprUnsafe (unwrap_block block_expr.Node.kind)) span)
  | Token.KwDefer ->
    let st, expr = parse_expr (advance st) in
    (st, mk_expr (Node.ExprDefer expr) span)
  | Token.KwExit ->
    let st = advance st in
    let st, curr, _ = peek_skip st in
    if curr = Token.Semi || curr = Token.EOF || curr = Token.RBrace then
      (st, mk_expr (Node.ExprExit None) span)
    else
      let st, expr = parse_expr st in
      (st, mk_expr (Node.ExprExit (Some expr)) span)
  | Token.KwSkip -> (advance st, mk_expr Node.ExprSkip span)
  | Token.KwIf -> parse_expr_if st span
  | Token.KwMatch -> parse_expr_match st span
  | Token.KwWhile -> parse_expr_while st span
  | Token.KwFor -> parse_expr_for st span
  | Token.KwDef -> parse_expr_func st span
  | Token.KwExtern ->
    let next_tok, _ = peek_nth_opt st 1 in
    if
      next_tok = Token.KwDef
      || match next_tok with Token.LitString _ -> true | _ -> false
    then parse_expr_func st span
    else
      (add_error_code st Parse_error.E1102 span [], mk_literal_expr LitUnit span)
  | _ ->
    (add_error_code st Parse_error.E1102 span [], mk_literal_expr LitUnit span)

and parse_expr_if st start_span =
  let st, first = parse_cond (advance st) in
  let rec parse_rest st acc =
    if match_token st Token.Comma then
      let st, c = parse_cond (expect_tok st Token.Comma) in
      parse_rest st (c :: acc)
    else (st, List.rev acc)
  in
  let st, rest = parse_rest st [] in
  let st, then_block = parse_body st in
  let st, else_block =
    if match_token st Token.KwElse then
      let st, b = parse_body (advance st) in
      (st, Some b)
    else (st, None)
  in
  let then_blk = unwrap_block then_block.Node.kind in
  let else_blk =
    match else_block with
    | Some e -> Some (unwrap_block e.Node.kind)
    | None -> None
  in
  (st, mk_expr (Node.ExprIf (first :: rest, then_blk, else_blk)) start_span)

and parse_expr_match st start_span =
  let st, scrutinee = parse_expr (advance st) in
  let st = expect_tok st Token.LBrace in
  let st, arms =
    parse_sep
      Token.Comma
      (fun st _ _ ->
        if match_token st Token.KwCase then
          let st, pattern = parse_pat (expect_tok st Token.KwCase) in
          let st, guard = parse_opt_type st Token.KwIf parse_expr in
          let st, body = parse_expr (expect_tok st Token.MinusGt) in
          let st, _ = consume_if st Token.Comma in
          Some (st, Node.{ pattern; guard; body })
        else None)
      st
  in
  ( expect_tok st Token.RBrace
  , mk_expr (Node.ExprMatch (scrutinee, arms)) start_span )

and parse_expr_while st start_span =
  let st, cond = parse_cond (advance st) in
  let st, guard = parse_opt_type st Token.KwIf parse_expr in
  let st, body_expr = parse_body st in
  ( st
  , mk_expr
      (Node.ExprWhile (cond, guard, unwrap_block body_expr.Node.kind))
      start_span )

and parse_expr_for st start_span =
  let st, curr, span = peek_skip (advance st) in
  let st, binding =
    if curr = Token.KwCase then
      let st, pat = parse_pat (expect_tok st Token.KwCase) in
      (st, Node.ForCase pat)
    else
      match curr with
      | Token.Ident name -> (advance st, Node.ForIdent name)
      | _ ->
        ( add_error_code st Parse_error.E1105 span []
        , Node.ForIdent (empty_name st) )
  in
  let st, range = parse_expr (expect_tok st Token.KwIn) in
  let st, guard = parse_opt_type st Token.KwIf parse_expr in
  let st, body_expr = parse_body st in
  ( st
  , mk_expr
      (Node.ExprFor
         { binding; range; guard; body = unwrap_block body_expr.Node.kind })
      start_span )

and parse_expr_func st start_span =
  let st, abi =
    if match_token st Token.KwExtern then
      let st, curr, _ = peek_skip (advance st) in
      match curr with
      | Token.LitString n -> (advance st, Some (Interner.lookup st.interner n))
      | _ -> (st, None)
    else (st, None)
  in
  let st, _ = expect st Token.KwDef in
  let st, name =
    let st, curr, _ = peek_skip st in
    match curr with Token.Ident n -> (advance st, Some n) | _ -> (st, None)
  in
  let st, params = parse_params parse_typ_expr st in
  let st, ret_type = parse_opt_type st Token.MinusGt parse_typ_expr in
  let st, body_expr = parse_body st in
  let body = unwrap_block body_expr.Node.kind in
  (st, mk_expr (Node.ExprFunc { abi; name; params; ret_type; body }) start_span)

and parse_expr_bp st min_bp =
  let st, curr, _ = peek_skip st in
  let st, left =
    if Prec.is_prefix_op curr then
      let st, right = parse_expr_bp (advance st) (Prec.prec_value Prec.Unary) in
      ( st
      , mk_expr
          (Node.ExprUnary (curr, right))
          (Span.merge (current_span st) right.Node.span) )
    else parse_expr_primary st
  in
  let rec loop st left =
    let st, curr, _ = peek_skip st in
    let st, left =
      if Prec.is_postfix_op curr then
        match curr with
        | Token.Dot -> (
          let st, curr, span = peek_skip (advance st) in
          match curr with
          | Token.Ident name ->
            ( advance st
            , mk_expr
                (Node.ExprField (left, name))
                (Span.merge left.Node.span span) )
          | _ -> (add_error_code st Parse_error.E1105 span [], left))
        | Token.LBrack ->
          let st, index = parse_expr (advance st) in
          ( expect_tok st Token.RBrack
          , mk_expr
              (Node.ExprIndex (left, index))
              (Span.merge left.Node.span (current_span st)) )
        | Token.LParen ->
          let st, args =
            parse_sep
              Token.Comma
              (fun st _ _ -> Some (parse_expr st))
              (advance st)
          in
          ( expect_tok st Token.RParen
          , mk_expr
              (Node.ExprCall (left, args))
              (Span.merge left.Node.span (current_span st)) )
        | _ -> (st, left)
      else (st, left)
    in
    if Prec.is_postfix_op curr then loop st left
    else
      match Prec.token_prec curr with
      | Some (prec, assoc) when Prec.prec_value prec >= min_bp ->
        let next_bp =
          if assoc = Prec.Right then Prec.prec_value prec
          else Prec.prec_value prec + 1
        in
        let st, right = parse_expr_bp (advance st) next_bp in
        loop
          st
          (mk_expr
             (Node.ExprBinary (curr, left, right))
             (Span.merge left.Node.span right.Node.span))
      | _ -> (st, left)
  in
  loop st left

and parse_expr st = parse_expr_bp st 0

and parse_clause :
  'a. state -> (Interner.name -> 'a) -> (Interner.name list -> 'a) -> state * 'a
    =
 fun st mk_all mk_named ->
  let st, curr, span = peek_skip st in
  if curr = Token.Star then
    let st = expect_tok (advance st) Token.KwAs in
    let st, curr, span = peek_skip st in
    match curr with
    | Token.Ident name -> (advance st, mk_all name)
    | _ -> (add_error_code st Parse_error.E1105 span [], mk_all (empty_name st))
  else if curr = Token.LBrace then
    let st, items =
      parse_delim
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
    (st, mk_named items)
  else (add_error_code st Parse_error.E1101 span [], mk_named [])

and parse_import_clause st =
  parse_clause st (fun n -> Node.ImportAll n) (fun i -> Node.ImportNamed i)

and parse_export_clause st =
  parse_clause st (fun n -> Node.ExportAll n) (fun i -> Node.ExportNamed i)

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
  (expect_tok st Token.Semi, mk_stmt (StmtExport (clause, path_opt)) start_span)

and parse_stmt_binding st start_span mutable_ =
  let st, curr, span = peek_skip (advance st) in
  match curr with
  | Token.Ident name ->
    let st, typ = parse_opt_type (advance st) Token.Colon parse_typ_expr in
    let st, value = parse_expr (expect_tok st Token.ColonEq) in
    ( expect_tok st Token.Semi
    , mk_stmt (StmtBinding { mutable_; name; typ; value }) start_span )
  | _ ->
    ( add_error_code st Parse_error.E1105 span []
    , mk_stmt (StmtExpr (mk_literal_expr LitUnit span)) start_span )

and parse_stmt_data st start_span =
  let st, name = parse_name st Parse_error.E1105 in
  let st, typ = parse_typ_expr (expect_tok st Token.ColonEq) in
  let data_kind =
    match typ.Node.kind with
    | TypExprRecord fields -> Node.DataRecord fields
    | TypExprSum cases -> Node.DataSum cases
    | _ -> Node.DataRecord []
  in
  (expect_tok st Token.Semi, mk_stmt (StmtData (name, data_kind)) start_span)

and parse_stmt_extern st start_span =
  let st, abi =
    let st, curr, _ = peek_skip (advance st) in
    match curr with
    | Token.LitString n -> (advance st, Some (Interner.lookup st.interner n))
    | _ -> (st, None)
  in
  let rec loop st sigs =
    let st, curr, _ = peek_skip st in
    match curr with
    | Token.RBrace | Token.EOF -> (st, List.rev sigs)
    | Token.KwDef ->
      let st, name = parse_name st Parse_error.E1105 in
      let st, params = parse_params parse_typ_expr st in
      let st, ret_type = parse_opt_type st Token.MinusGt parse_typ_expr in
      loop (expect_tok st Token.Semi) (Node.{ name; params; ret_type } :: sigs)
    | _ -> loop (advance st) sigs
  in
  let st, sigs = loop (expect_tok st Token.LBrace) [] in
  (expect_tok st Token.RBrace, mk_stmt (StmtExtern (abi, sigs)) start_span)

and parse_stmt_ident st start_span name =
  let st = advance st in
  if match_token st Token.LtMinus then
    let st, value = parse_expr (expect_tok st Token.LtMinus) in
    (expect_tok st Token.Semi, mk_stmt (StmtAssign (name, value)) start_span)
  else
    let st, expr = parse_expr { st with pos = st.pos - 1 } in
    (expect_tok st Token.Semi, mk_stmt (StmtExpr expr) start_span)

and parse_stmt st =
  let st, curr, start_span = peek_skip st in
  match curr with
  | Token.KwImport -> parse_stmt_import st start_span
  | Token.KwExport -> parse_stmt_export st start_span
  | Token.KwVal -> parse_stmt_binding st start_span false
  | Token.KwVar -> parse_stmt_binding st start_span true
  | Token.KwData -> parse_stmt_data st start_span
  | Token.KwExtern -> parse_stmt_extern st start_span
  | Token.Ident name -> parse_stmt_ident st start_span name
  | _ ->
    let st, expr = parse_expr st in
    (expect_tok st Token.Semi, mk_stmt (StmtExpr expr) start_span)

and parse_pat_ident st name span =
  let st, curr, _ = peek_skip (advance st) in
  match curr with
  | Token.LBrace ->
    let st, fields =
      parse_delim
        Token.LBrace
        Token.RBrace
        Token.Comma
        (fun st curr _span ->
          match curr with
          | Token.Dot -> (
            let st, curr, span = peek_skip (advance st) in
            match curr with
            | Token.Ident field_name ->
              let st = advance st in
              let st, pat_opt = parse_opt_type st Token.ColonEq parse_pat in
              Some (st, Node.{ name = field_name; pat = pat_opt })
            | _ ->
              let _ = add_error_code st Parse_error.E1105 span [] in
              None)
          | _ -> None)
        st
    in
    (st, mk_pat (Node.PatRecord (name, fields)) span)
  | Token.LParen ->
    let st, pats =
      parse_delim
        Token.LParen
        Token.RParen
        Token.Comma
        (fun st _ _ -> Some (parse_pat st))
        st
    in
    (st, mk_pat (Node.PatCtor (name, pats)) span)
  | _ -> (st, mk_pat (Node.PatIdent name) span)

and parse_pat_literal st curr span =
  match curr with
  | Token.Minus -> (
    let st, curr, _ = peek_skip (advance st) in
    match curr with
    | Token.LitNumber s -> (advance st, mk_literal_pat (LitInt ("-" ^ s)) span)
    | _ ->
      (add_error_code st Parse_error.E1103 span [], mk_pat Node.PatWild span))
  | _ -> (
    match parse_literal curr with
    | Some lit -> (advance st, mk_literal_pat lit span)
    | None -> (advance st, mk_pat Node.PatWild span))

and parse_pat st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.KwVal | Token.KwVar -> (
    let mutable_ = curr = Token.KwVar in
    let st, curr, span = peek_skip (advance st) in
    match curr with
    | Token.Ident name ->
      (advance st, mk_pat (Node.PatBinding { mutable_; name }) span)
    | _ ->
      (add_error_code st Parse_error.E1105 span [], mk_pat Node.PatWild span))
  | Token.Ident name -> parse_pat_ident st name span
  | Token.LParen ->
    parse_tuple_or_single
      Token.RParen
      parse_pat
      (fun f r s -> mk_pat (Node.PatTuple (f, r)) s)
      st
      span
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _ | Token.Minus ->
    parse_pat_literal st curr span
  | _ -> (advance st, mk_pat Node.PatWild span)

and parse_typ_field st curr span =
  match curr with
  | Token.Ident name ->
    let st, typ = parse_typ_expr (expect_tok (advance st) Token.Colon) in
    Some (st, (Node.{ name; typ } : Node.typ_field))
  | _ ->
    let _ = add_error_code st Parse_error.E1105 span [] in
    None

and parse_typ_case st =
  let st, curr, span =
    peek_skip (expect_tok (expect_tok st Token.Pipe) Token.KwCase)
  in
  match curr with
  | Token.Ident name ->
    let st = advance st in
    if match_token st Token.LParen then
      let st, fields =
        parse_list Token.Comma parse_typ_expr (expect_tok st Token.LParen)
      in
      (expect_tok st Token.RParen, (Node.{ name; fields } : Node.typ_case))
    else (st, (Node.{ name; fields = [] } : Node.typ_case))
  | _ ->
    let st, name = error_name st Parse_error.E1105 span in
    (st, (Node.{ name; fields = [] } : Node.typ_case))

and parse_typ_expr_primary st =
  let st, curr, span = peek_skip st in
  match curr with
  | Token.Caret ->
    let st, inner = parse_typ_expr (advance st) in
    (st, mk_node (Node.TypExprPtr inner) span)
  | Token.LBrack ->
    let st = advance st in
    let st, size_opt =
      if match_token st Token.RBrack then (st, None)
      else
        let st, e = parse_expr st in
        (st, Some e)
    in
    let st, elem = parse_typ_expr (expect_tok st Token.RBrack) in
    (st, mk_node (Node.TypExprArray (size_opt, elem)) span)
  | Token.Ident name -> (advance st, mk_node (Node.TypExprIdent name) span)
  | Token.LBrace ->
    let st, fields =
      parse_sep Token.Comma parse_typ_field (expect_tok st Token.LBrace)
    in
    (expect_tok st Token.RBrace, mk_node (Node.TypExprRecord fields) span)
  | Token.LParen ->
    parse_tuple_or_single
      Token.RParen
      parse_typ_expr
      (fun f r s -> mk_node (Node.TypExprTuple (f, r)) s)
      st
      span
  | Token.KwDef ->
    let st, params =
      parse_list
        Token.Comma
        parse_typ_expr
        (expect_tok (advance st) Token.LParen)
    in
    let st, ret =
      parse_opt_type (expect_tok st Token.RParen) Token.MinusGt parse_typ_expr
    in
    (st, mk_node (Node.TypExprFunc (params, ret)) span)
  | _ -> error_typ st Parse_error.E1104 span

and parse_typ_expr st =
  let st, typ = parse_typ_expr_primary st in
  if match_token st Token.Pipe then
    let st, cases = parse_sep_pipe parse_typ_case st in
    (st, Node.{ kind = TypExprSum cases; span = typ.Node.span })
  else (st, typ)

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
