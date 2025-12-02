open Basic
open Lex

type state = {
    tokens : (Token.t * Span.t) array
  ; pos : int
  ; interner : Interner.t
  ; diags : Diagnostic.bag
}

let mk_state tokens interner =
  { tokens; pos = 0; interner; diags = Diagnostic.empty_bag }

let peek st =
  if st.pos < Array.length st.tokens then fst st.tokens.(st.pos) else Token.EOF

let peek_span st =
  if st.pos < Array.length st.tokens then snd st.tokens.(st.pos) else Span.dummy

let advance st = { st with pos = st.pos + 1 }
let at st tok = peek st = tok

let mk_span st start =
  if start < Array.length st.tokens then
    Span.merge (snd st.tokens.(start)) (peek_span st)
  else peek_span st

let add_error_code st code span args =
  { st with diags = Diagnostic.add st.diags (Parse_error.diag code span args) }

let expect st tok =
  if at st tok then (advance st, peek_span st)
  else
    ( add_error_code
        st
        Parse_error.E1003
        (peek_span st)
        [ Token.to_string tok; Token.to_string (peek st) ]
    , peek_span st )

let parse_delimited st open_tok close_tok sep_tok parse_fn =
  let st, _ = expect st open_tok in
  if at st close_tok then
    let st, _ = expect st close_tok in
    (st, [])
  else
    let st, first = parse_fn st in
    let rec loop st acc =
      if at st sep_tok then
        let st = advance st in
        if at st close_tok then (st, List.rev acc)
        else
          let st, item = parse_fn st in
          loop st (item :: acc)
      else (st, List.rev acc)
    in
    let st, rest = loop st [ first ] in
    let st, _ = expect st close_tok in
    (st, rest)

let parse_braced_single st parse_fn =
  let st, _ = expect st Token.LBrace in
  let st, item = parse_fn st in
  let st, _ = expect st Token.RBrace in
  (st, item)

let advance_then st parse_fn = parse_fn (advance st)

let rec parse_expr st = parse_expr_prec st 0

and parse_expr_prec st min_prec =
  let start = st.pos in
  let st, left = parse_expr_prefix st in
  parse_expr_loop st start left min_prec

and parse_expr_loop st start left min_prec =
  match Prec.token_prec (peek st) with
  | Some (prec, assoc) when Prec.prec_value prec >= min_prec ->
    let op = peek st in
    let st = advance st in
    let next_prec =
      match assoc with
      | Prec.Right -> Prec.prec_value prec
      | _ -> Prec.prec_value prec + 1
    in
    let st, right = parse_expr_prec st next_prec in
    let kind = Node.ExprBinary { op; left; right } in
    let expr = { Node.kind; span = mk_span st start } in
    parse_expr_loop st start expr min_prec
  | _ -> parse_expr_postfix st start left

and parse_expr_postfix st start base =
  match peek st with
  | Token.Dot ->
    let st, field, _ = advance_then st parse_expr_ident in
    let kind = Node.ExprField { base; field; optional = false } in
    let expr = { Node.kind; span = mk_span st start } in
    parse_expr_postfix st start expr
  | Token.LBrack ->
    let st, index = advance_then st parse_expr in
    let st, _ = expect st Token.RBrack in
    let kind = Node.ExprIndex { base; index; optional = false } in
    let expr = { Node.kind; span = mk_span st start } in
    parse_expr_postfix st start expr
  | Token.LParen ->
    let st, args = advance_then st parse_expr_args in
    let st, _ = expect st Token.RParen in
    let kind =
      Node.ExprCall { callee = base; typ_args = []; args; optional = false }
    in
    let expr = { Node.kind; span = mk_span st start } in
    parse_expr_postfix st start expr
  | _ -> (st, base)

and parse_expr_prefix st =
  let start = st.pos in
  match peek st with
  | Token.LitNumber s ->
    let st = advance st in
    let kind = Node.ExprLit (Node.LitNumber s) in
    (st, { Node.kind; span = mk_span st start })
  | Token.LitString name ->
    let st = advance st in
    let kind = Node.ExprLit (Node.LitString name) in
    (st, { Node.kind; span = mk_span st start })
  | Token.LitRune c ->
    let st = advance st in
    let kind = Node.ExprLit (Node.LitRune c) in
    (st, { Node.kind; span = mk_span st start })
  | Token.LitTemplate name ->
    let st = advance st in
    let kind = Node.ExprTemplate name in
    (st, { Node.kind; span = mk_span st start })
  | Token.Ident name ->
    let st = advance st in
    if at st Token.LBrace then
      let st, fields =
        parse_delimited
          st
          Token.LBrace
          Token.RBrace
          Token.Comma
          parse_record_field_init
      in
      let kind = Node.ExprRecordLit { name = Some name; fields } in
      (st, { Node.kind; span = mk_span st start })
    else if at st Token.LtMinus then
      let st, value = advance_then st parse_expr in
      let kind = Node.ExprAssign { target = name; value } in
      (st, { Node.kind; span = mk_span st start })
    else
      let kind = Node.ExprIdent name in
      (st, { Node.kind; span = mk_span st start })
  | Token.LParen -> parse_expr_tuple st
  | Token.LBrace -> parse_expr_block_or_record_lit st start
  | Token.KwIf -> parse_expr_if st
  | Token.KwMatch -> parse_expr_match st
  | Token.KwFor -> parse_expr_for st
  | Token.KwWhile -> parse_expr_while st
  | Token.KwDefer -> parse_expr_defer st
  | Token.KwBreak -> parse_expr_break st
  | Token.KwCycle -> parse_expr_cycle st
  | Token.KwUnsafe -> parse_expr_unsafe st
  | Token.KwFn -> parse_expr_fn st
  | Token.KwRecord -> parse_expr_record st
  | Token.KwChoice -> parse_expr_choice st
  | Token.KwTrait -> parse_expr_trait st
  | tok when Prec.is_prefix_op tok ->
    let op = peek st in
    let st, operand =
      advance_then st (fun st ->
        parse_expr_prec st (Prec.prec_value Prec.Unary))
    in
    let kind = Node.ExprUnary { op; operand } in
    (st, { Node.kind; span = mk_span st start })
  | _ ->
    let st = add_error_code st Parse_error.E1101 (peek_span st) [] in
    let kind = Node.ExprLit (Node.LitNumber "0") in
    (st, { Node.kind; span = mk_span st start })

and parse_expr_args st =
  if at st Token.RParen then (st, [])
  else
    let st, first = parse_expr st in
    let rec loop st acc =
      if at st Token.Comma then
        let st = advance st in
        if at st Token.RParen then (st, List.rev acc)
        else
          let st, e = parse_expr st in
          loop st (e :: acc)
      else (st, List.rev acc)
    in
    loop st [ first ]

and parse_expr_ident st =
  match peek st with
  | Token.Ident name -> (advance st, name, peek_span st)
  | _ ->
    ( add_error_code st Parse_error.E1105 (peek_span st) []
    , Interner.empty_name st.interner
    , peek_span st )

and parse_expr_tuple st =
  let start = st.pos in
  let st, first = advance_then st parse_expr in
  if at st Token.Comma then
    let st = advance st in
    let rec loop st acc =
      if at st Token.RParen then (st, List.rev acc)
      else
        let st, e = parse_expr st in
        let st = if at st Token.Comma then advance st else st in
        loop st (e :: acc)
    in
    let st, rest = loop st [] in
    let st, _ = expect st Token.RParen in
    let kind = Node.ExprTuple (first, rest) in
    (st, { Node.kind; span = mk_span st start })
  else
    let st, _ = expect st Token.RParen in
    (st, first)

and parse_expr_block_or_record_lit st start =
  let st = advance st in
  if at st Token.Dot then
    let st, fields =
      parse_delimited_no_open
        st
        Token.RBrace
        Token.Comma
        parse_record_field_init
    in
    let kind = Node.ExprRecordLit { name = None; fields } in
    (st, { Node.kind; span = mk_span st start })
  else
    let rec loop st stmts =
      if at st Token.RBrace || at st Token.EOF then (st, List.rev stmts, None)
      else if at st Token.KwReturn then
        let st, e = advance_then st parse_expr in
        let st, _ = expect st Token.Semi in
        (st, List.rev stmts, Some e)
      else
        let st, s = parse_stmt st in
        loop st (s :: stmts)
    in
    let st, stmts, ret = loop st [] in
    let st, _ = expect st Token.RBrace in
    let block = { Node.stmts; ret } in
    let kind = Node.ExprBlock block in
    (st, { Node.kind; span = mk_span st start })

and parse_expr_block st =
  let start = st.pos in
  parse_expr_block_or_record_lit st start

and parse_delimited_no_open st close_tok sep_tok parse_fn =
  if at st close_tok then
    let st, _ = expect st close_tok in
    (st, [])
  else
    let st, first = parse_fn st in
    let rec loop st acc =
      if at st sep_tok then
        let st = advance st in
        if at st close_tok then (st, List.rev acc)
        else
          let st, item = parse_fn st in
          loop st (item :: acc)
      else (st, List.rev acc)
    in
    let st, rest = loop st [ first ] in
    let st, _ = expect st close_tok in
    (st, rest)

and parse_expr_if st =
  let start = st.pos in
  let st, first_cond = advance_then st parse_cond in
  let rec loop st acc =
    if at st Token.Comma then
      let st, c = advance_then st parse_cond in
      loop st (c :: acc)
    else (st, List.rev acc)
  in
  let st, rest_conds = loop st [] in
  let st, then_block = parse_block st in
  let st, else_block =
    if at st Token.KwElse then
      let st, b = advance_then st parse_block in
      (st, Some b)
    else (st, None)
  in
  let kind =
    Node.ExprIf { conds = (first_cond, rest_conds); then_block; else_block }
  in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_match st =
  let start = st.pos in
  let st, scrutinee = advance_then st parse_expr in
  let st, arms =
    parse_delimited st Token.LBrace Token.RBrace Token.Comma parse_match_arm
  in
  let kind = Node.ExprMatch { scrutinee; arms } in
  (st, { Node.kind; span = mk_span st start })

and parse_match_arm st =
  let st, _ = expect st Token.KwCase in
  let st, pat = parse_pat st in
  let st, guard =
    if at st Token.KwIf then
      let st, e = advance_then st parse_expr in
      (st, Some e)
    else (st, None)
  in
  let st, _ = expect st Token.Eq in
  let st, _ = expect st Token.Gt in
  let st, body = parse_expr st in
  (st, { Node.pat; guard; body })

and parse_expr_for st =
  let start = st.pos in
  let st, binding = advance_then st parse_for_binding in
  let st, _ = expect st Token.KwIn in
  let st, range = parse_expr st in
  let st, guard =
    if at st Token.KwIf then
      let st, e = advance_then st parse_expr in
      (st, Some e)
    else (st, None)
  in
  let st, body = parse_block st in
  let kind = Node.ExprFor { binding; range; guard; body } in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_while st =
  let start = st.pos in
  let st, cond = advance_then st parse_cond in
  let st, guard =
    if at st Token.KwIf then
      let st, e = advance_then st parse_expr in
      (st, Some e)
    else (st, None)
  in
  let st, body = parse_block st in
  let kind = Node.ExprWhile { cond; guard; body } in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_defer st =
  let start = st.pos in
  let st, e = advance_then st parse_expr in
  let kind = Node.ExprDefer e in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_break st =
  let start = st.pos in
  let st = advance st in
  let st, e =
    if at st Token.Semi then (st, None)
    else
      let st, e = parse_expr st in
      (st, Some e)
  in
  let kind = Node.ExprBreak e in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_cycle st =
  let start = st.pos in
  let st = advance st in
  let kind = Node.ExprCycle in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_unsafe st =
  let start = st.pos in
  let st, block = advance_then st parse_block in
  let kind = Node.ExprUnsafe block in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_fn st =
  let start = st.pos in
  let st, abi =
    if at st Token.KwExtern then
      let st = advance st in
      match peek st with
      | Token.LitString name ->
        (advance st, Some (Interner.lookup st.interner name))
      | _ -> (st, None)
    else (st, None)
  in
  let st = advance st in
  let st, name =
    match peek st with Token.Ident n -> (advance st, Some n) | _ -> (st, None)
  in
  let st, typ_params =
    if at st Token.Lt then
      parse_delimited st Token.Lt Token.Gt Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    else (st, [])
  in
  let st, params = parse_fn_params st in
  let st, ret_typ =
    if at st Token.MinusGt then
      let st, t = advance_then st parse_typ in
      (st, Some t)
    else (st, None)
  in
  let st, body = parse_block st in
  let kind = Node.ExprFn { abi; name; typ_params; params; ret_typ; body } in
  (st, { Node.kind; span = mk_span st start })

and parse_fn_params st : state * Node.fn_param list =
  parse_delimited st Token.LParen Token.RParen Token.Comma (fun st ->
    let st, name, _ = parse_expr_ident st in
    let st, (typ : Node.typ option) =
      if at st Token.Colon then
        let st, t = advance_then st parse_typ in
        (st, Some t)
      else (st, None)
    in
    (st, ({ Node.name; typ } : Node.fn_param)))

and parse_expr_record st =
  let start = st.pos in
  let st = advance st in
  let st, typ_params =
    if at st Token.Lt then
      parse_delimited st Token.Lt Token.Gt Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    else (st, [])
  in
  let st, trait_bound =
    if at st Token.Colon then
      let st, t = advance_then st parse_typ in
      (st, Some t)
    else (st, None)
  in
  let st, _ = expect st Token.LBrace in
  let rec loop st fields body =
    if at st Token.RBrace then (st, List.rev fields, List.rev body)
    else if
      at st Token.KwVal || at st Token.KwVar || at st Token.KwImport
      || at st Token.KwExport || at st Token.KwExtern
    then
      let st, s = parse_stmt st in
      loop st fields (s :: body)
    else
      let st, name, _ = parse_expr_ident st in
      if at st Token.Colon then
        let st, typ = advance_then st parse_typ in
        let st, _ = if at st Token.Comma then (advance st, ()) else (st, ()) in
        loop st (({ Node.name; typ } : Node.record_field) :: fields) body
      else if at st Token.ColonEq then
        let st, value = advance_then st parse_expr in
        let st, _ = expect st Token.Semi in
        let kind =
          Node.StmtBind { mutable_ = false; name; typ = None; value }
        in
        let s = { Node.attr = None; kind; span = mk_span st start } in
        loop st fields (s :: body)
      else loop st fields body
  in
  let st, fields, body = loop st [] [] in
  let st, _ = expect st Token.RBrace in
  let kind = Node.ExprRecord { typ_params; trait_bound; fields; body } in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_choice st =
  let start = st.pos in
  let st = advance st in
  let st, typ_params =
    if at st Token.Lt then
      parse_delimited st Token.Lt Token.Gt Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    else (st, [])
  in
  let st, _ = expect st Token.LBrace in
  let rec loop st cases body =
    if at st Token.RBrace then (st, List.rev cases, List.rev body)
    else if at st Token.KwCase then
      let st, name, _ = advance_then st parse_expr_ident in
      let st, fields =
        if at st Token.LParen then
          parse_delimited st Token.LParen Token.RParen Token.Comma parse_typ
        else (st, [])
      in
      let st, _ = if at st Token.Comma then (advance st, ()) else (st, ()) in
      loop st ({ Node.name; fields } :: cases) body
    else
      let st, s = parse_stmt st in
      loop st cases (s :: body)
  in
  let st, cases, body = loop st [] [] in
  let st, _ = expect st Token.RBrace in
  let kind = Node.ExprChoice { typ_params; cases; body } in
  (st, { Node.kind; span = mk_span st start })

and parse_expr_trait st =
  let start = st.pos in
  let st = advance st in
  let st, typ_params =
    if at st Token.Lt then
      parse_delimited st Token.Lt Token.Gt Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    else (st, [])
  in
  let st, trait_bound =
    if at st Token.Colon then
      let st, t = advance_then st parse_typ in
      (st, Some t)
    else (st, None)
  in
  let st, items =
    parse_delimited st Token.LBrace Token.RBrace Token.Comma (fun st ->
      let st, name, _ = parse_expr_ident st in
      let st, (typ : Node.typ option) =
        if at st Token.Colon then
          let st, t = advance_then st parse_typ in
          (st, Some t)
        else (st, None)
      in
      (st, ({ Node.name; typ } : Node.fn_sig)))
  in
  let kind = Node.ExprTrait { typ_params; trait_bound; items } in
  (st, { Node.kind; span = mk_span st start })

and parse_stmt st =
  let start = st.pos in
  match peek st with
  | Token.KwImport -> parse_stmt_import st
  | Token.KwExport -> parse_stmt_export st
  | Token.KwVal | Token.KwVar -> parse_stmt_bind st
  | Token.KwExtern -> parse_stmt_extern st
  | _ ->
    let st, e = parse_expr st in
    let st, _ = expect st Token.Semi in
    let kind = Node.StmtExpr e in
    (st, { Node.attr = None; kind; span = mk_span st start })

and parse_stmt_import st =
  let start = st.pos in
  let st, clause = advance_then st parse_import_clause in
  let st, _ = expect st Token.KwFrom in
  let st, source =
    match peek st with
    | Token.LitString name -> (advance st, name)
    | _ ->
      ( add_error_code
          st
          Parse_error.E1003
          (peek_span st)
          [ "string"; Token.to_string (peek st) ]
      , Interner.empty_name st.interner )
  in
  let st, _ = expect st Token.Semi in
  let kind = Node.StmtImport { clause; source } in
  (st, { Node.attr = None; kind; span = mk_span st start })

and parse_stmt_export st =
  let start = st.pos in
  let st, clause = advance_then st parse_export_clause in
  let st, source =
    if at st Token.KwFrom then
      let st = advance st in
      match peek st with
      | Token.LitString name -> (advance st, Some name)
      | _ ->
        ( add_error_code
            st
            Parse_error.E1003
            (peek_span st)
            [ "string"; Token.to_string (peek st) ]
        , None )
    else (st, None)
  in
  let st, _ = expect st Token.Semi in
  let kind = Node.StmtExport { clause; source } in
  (st, { Node.attr = None; kind; span = mk_span st start })

and parse_stmt_bind st =
  let start = st.pos in
  let mutable_ = at st Token.KwVar in
  let st, name, _ = advance_then st parse_expr_ident in
  let st, typ =
    if at st Token.Colon then
      let st, t = advance_then st parse_typ in
      (st, Some t)
    else (st, None)
  in
  let st, _ = expect st Token.ColonEq in
  let st, value = parse_expr st in
  let st, _ = expect st Token.Semi in
  let kind = Node.StmtBind { mutable_; name; typ; value } in
  (st, { Node.attr = None; kind; span = mk_span st start })

and parse_stmt_extern st =
  let start = st.pos in
  let st = advance st in
  let st, abi =
    match peek st with
    | Token.LitString name ->
      (advance st, Some (Interner.lookup st.interner name))
    | _ -> (st, None)
  in
  let st, _ = expect st Token.KwUnsafe in
  let st, decls =
    parse_delimited st Token.LBrace Token.RBrace Token.Semi (fun st ->
      let st, _ = expect st Token.KwFn in
      let st, name, _ = parse_expr_ident st in
      let st, (params : Node.fn_param list) = parse_fn_params st in
      let st, ret_typ =
        if at st Token.MinusGt then
          let st, t = advance_then st parse_typ in
          (st, Some t)
        else (st, None)
      in
      let has_any_typ =
        List.exists
          (fun (p : Node.fn_param) -> Option.is_some p.Node.typ)
          params
        || Option.is_some ret_typ
      in
      let (typ : Node.typ option) =
        if has_any_typ then
          let param_typs =
            List.filter_map (fun (p : Node.fn_param) -> p.Node.typ) params
          in
          let typ_kind = Node.TypFn { params = param_typs; ret = ret_typ } in
          Some { Node.kind = typ_kind; span = peek_span st }
        else None
      in
      (st, ({ Node.name; typ } : Node.fn_sig)))
  in
  let kind = Node.StmtExtern { abi; decls } in
  (st, { Node.attr = None; kind; span = mk_span st start })

and parse_pat st =
  let start = st.pos in
  match peek st with
  | Token.KwVal | Token.KwVar -> parse_pat_bind st
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _ -> parse_pat_lit st
  | Token.Underscore -> parse_pat_wild st
  | Token.Ident _ -> parse_pat_ident st
  | Token.LParen -> parse_pat_tuple st
  | _ ->
    let st = add_error_code st Parse_error.E1103 (peek_span st) [] in
    let kind = Node.PatWild in
    (st, { Node.kind; span = mk_span st start })

and parse_pat_bind st =
  let start = st.pos in
  let mutable_ = at st Token.KwVar in
  let st, name, _ = advance_then st parse_expr_ident in
  let kind = Node.PatBind { mutable_; name } in
  (st, { Node.kind; span = mk_span st start })

and parse_pat_lit st =
  let start = st.pos in
  let lit =
    match peek st with
    | Token.LitNumber s -> Node.LitNumber s
    | Token.LitString name -> Node.LitString name
    | Token.LitRune c -> Node.LitRune c
    | _ -> Node.LitNumber "0"
  in
  let st = advance st in
  let kind = Node.PatLit lit in
  (st, { Node.kind; span = mk_span st start })

and parse_pat_wild st =
  let start = st.pos in
  let st = advance st in
  let kind = Node.PatWild in
  (st, { Node.kind; span = mk_span st start })

and parse_pat_ident st =
  let start = st.pos in
  let st, name, _ = parse_expr_ident st in
  if at st Token.LBrace then
    let st, fields =
      parse_delimited st Token.LBrace Token.RBrace Token.Comma parse_pat_field
    in
    let kind = Node.PatRecord { name; fields } in
    (st, { Node.kind; span = mk_span st start })
  else if at st Token.LParen then
    let st, args =
      parse_delimited st Token.LParen Token.RParen Token.Comma parse_pat
    in
    let kind = Node.PatCtor { name; args } in
    (st, { Node.kind; span = mk_span st start })
  else
    let kind = Node.PatIdent name in
    (st, { Node.kind; span = mk_span st start })

and parse_pat_tuple st =
  let start = st.pos in
  let st, pats =
    parse_delimited st Token.LParen Token.RParen Token.Comma parse_pat
  in
  match pats with
  | [ single ] -> (st, single)
  | first :: rest ->
    let kind = Node.PatTuple (first, rest) in
    (st, { Node.kind; span = mk_span st start })
  | [] ->
    let kind = Node.PatWild in
    (st, { Node.kind; span = mk_span st start })

and parse_pat_field st =
  let st, _ = expect st Token.Dot in
  let st, name, _ = parse_expr_ident st in
  let st, pat =
    if at st Token.ColonEq then
      let st, pat = advance_then st parse_pat in
      (st, Some pat)
    else (st, None)
  in
  (st, { Node.name; pat })

and parse_typ st =
  let start = st.pos in
  match peek st with
  | Token.Caret -> parse_typ_ptr st
  | Token.LBrack -> parse_typ_arr st
  | Token.Ident _ -> parse_typ_ident st
  | Token.LParen -> parse_typ_tuple st
  | Token.KwFn -> parse_typ_fn st
  | Token.LBrace -> parse_typ_record st
  | _ ->
    let st = add_error_code st Parse_error.E1104 (peek_span st) [] in
    let kind = Node.TypIdent (Interner.empty_name st.interner) in
    (st, { Node.kind; span = mk_span st start })

and parse_typ_ptr st =
  let start = st.pos in
  let st, t = advance_then st parse_typ in
  let kind = Node.TypPtr t in
  (st, { Node.kind; span = mk_span st start })

and parse_typ_arr st =
  let start = st.pos in
  let st = advance st in
  let st, size =
    if at st Token.RBrack then (st, None)
    else
      let st, e = parse_expr st in
      (st, Some e)
  in
  let st, _ = expect st Token.RBrack in
  let st, elem = parse_typ st in
  let kind = Node.TypArr { size; elem } in
  (st, { Node.kind; span = mk_span st start })

and parse_typ_ident st =
  let start = st.pos in
  let st, name, _ = parse_expr_ident st in
  let st, kind =
    if at st Token.Lt then
      let st, args =
        parse_delimited st Token.Lt Token.Gt Token.Comma parse_typ
      in
      (st, Node.TypApp { base = name; args })
    else (st, Node.TypIdent name)
  in
  let st, kind =
    if at st Token.Question then
      let st = advance st in
      (st, Node.TypOptional { Node.kind; span = mk_span st start })
    else (st, kind)
  in
  (st, { Node.kind; span = mk_span st start })

and parse_typ_tuple st =
  let start = st.pos in
  let st, typs =
    parse_delimited st Token.LParen Token.RParen Token.Comma parse_typ
  in
  match typs with
  | [ single ] -> (st, single)
  | first :: rest ->
    let kind = Node.TypTuple (first, rest) in
    (st, { Node.kind; span = mk_span st start })
  | [] ->
    let kind = Node.TypIdent (Interner.empty_name st.interner) in
    (st, { Node.kind; span = mk_span st start })

and parse_typ_fn st =
  let start = st.pos in
  let st = advance st in
  let st, params =
    parse_delimited st Token.LParen Token.RParen Token.Comma parse_typ
  in
  let st, ret =
    if at st Token.MinusGt then
      let st, t = advance_then st parse_typ in
      (st, Some t)
    else (st, None)
  in
  let kind = Node.TypFn { params; ret } in
  (st, { Node.kind; span = mk_span st start })

and parse_typ_record st =
  let start = st.pos in
  let st, fields =
    parse_delimited
      st
      Token.LBrace
      Token.RBrace
      Token.Comma
      parse_typ_record_field
  in
  let kind = Node.TypRecord fields in
  (st, { Node.kind; span = mk_span st start })

and parse_typ_record_field st =
  let st, name, _ = parse_expr_ident st in
  let st, _ = expect st Token.Colon in
  let st, typ = parse_typ st in
  (st, { Node.name; typ })

and parse_block st =
  match parse_expr_block st with
  | st, { Node.kind = Node.ExprBlock b; _ } -> (st, b)
  | st, _ -> (st, { Node.stmts = []; ret = None })

and parse_cond st =
  if at st Token.KwCase then
    let st, pat = advance_then st parse_pat in
    let st, _ = expect st Token.ColonEq in
    let st, value = parse_expr st in
    (st, Node.CondCase { pat; value })
  else
    let st, e = parse_expr st in
    (st, Node.CondExpr e)

and parse_for_binding st =
  if at st Token.KwCase then
    let st, pat = advance_then st parse_pat in
    (st, Node.ForCase pat)
  else
    let st, name, _ = parse_expr_ident st in
    (st, Node.ForIdent name)

and parse_import_clause st =
  if at st Token.Star then
    let st = advance st in
    let st, _ = expect st Token.KwAs in
    let st, name, _ = parse_expr_ident st in
    (st, Node.ImportAll name)
  else
    let st, names =
      parse_delimited st Token.LBrace Token.RBrace Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    in
    (st, Node.ImportNamed names)

and parse_export_clause st =
  if at st Token.Star then
    let st = advance st in
    let st, _ = expect st Token.KwAs in
    let st, name, _ = parse_expr_ident st in
    (st, Node.ExportAll name)
  else
    let st, names =
      parse_delimited st Token.LBrace Token.RBrace Token.Comma (fun st ->
        let st, n, _ = parse_expr_ident st in
        (st, n))
    in
    (st, Node.ExportNamed names)

and parse_record_field_init st =
  let shorthand = at st Token.Dot in
  let st = if shorthand then advance st else st in
  let st, name, _ = parse_expr_ident st in
  let st, _ = expect st Token.ColonEq in
  let st, value = parse_expr st in
  (st, { Node.shorthand; name; value })

let parse tokens interner =
  let st = mk_state tokens interner in
  let rec loop st stmts =
    if at st Token.EOF then (List.rev stmts, st.diags)
    else
      let st, s = parse_stmt st in
      loop st (s :: stmts)
  in
  loop st []
