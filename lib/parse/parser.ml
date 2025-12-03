open Basic
open Node
module Token = Lex.Token

module type S = sig
  type prog = Node.prog

  type state = {
      source : string
    ; file_id : Span.file_id
    ; tokens : (Token.t * Span.t) list
    ; pos : int
    ; interner : Interner.t
    ; diags : Diagnostic.bag
  }

  val mk_state :
    string -> Span.file_id -> (Token.t * Span.t) list -> Interner.t -> state

  val parse :
       string
    -> Span.file_id
    -> Interner.t
    -> (Token.t * Span.t) list
    -> prog * Diagnostic.bag

  val peek : state -> (Token.t * Span.t) option
  val parse_expr : state -> state * Node.expr
  val parse_pat : state -> state * Node.pat
  val parse_typ : state -> state * Node.typ
  val parse_stmt : state -> state * Node.stmt

  val parse :
       string
    -> Span.file_id
    -> Interner.t
    -> (Token.t * Span.t) list
    -> prog * Diagnostic.bag
end

module Make () : S = struct
  type prog = Node.prog

  type state = {
      source : string
    ; file_id : Span.file_id
    ; tokens : (Token.t * Span.t) list
    ; pos : int
    ; interner : Interner.t
    ; diags : Diagnostic.bag
  }

  let mk_state src fid toks int =
    {
      source = src
    ; file_id = fid
    ; tokens = toks
    ; pos = 0
    ; interner = int
    ; diags = Diagnostic.empty_bag
    }

  let peek st =
    if st.pos >= List.length st.tokens then None
    else Some (List.nth st.tokens st.pos)

  let adv st = { st with pos = st.pos + 1 }

  let span st =
    if List.length st.tokens = 0 then Span.dummy
    else snd (List.nth st.tokens (min st.pos (List.length st.tokens - 1)))

  let err st c s a =
    { st with diags = Diagnostic.add st.diags (Error.diag c s a) }

  let expect st tok name =
    match peek st with
    | Some (t, _) when t = tok -> adv st
    | Some (t, s) -> err st Error.E1003 s [ name; Token.to_string t ]
    | None -> err st Error.E1003 (span st) [ name; "EOF" ]

  let ident st =
    match peek st with
    | Some (Token.Ident n, s) -> (adv st, n, s)
    | Some (t, s) ->
      ( err st Error.E1105 s [ Token.to_string t ]
      , Interner.empty_name st.interner
      , s )
    | None ->
      let s = span st in
      (err st Error.E1105 s [ "EOF" ], Interner.empty_name st.interner, s)

  let adv_then p st = p (adv st)

  let satisfy pred st =
    match peek st with
    | Some (t, _) when pred t -> adv st
    | _ -> failwith "satisfy"

  let tok t = satisfy (fun x -> x = t)

  let rec many p st =
    try
      let s, x = p st in
      let s', xs = many p s in
      (s', x :: xs)
    with _ -> (st, [])

  let sep_by p sep st =
    let rec loop s acc =
      match peek s with
      | Some (t, _) when t = sep -> (
        try
          let s', x = adv_then p s in
          loop s' (x :: acc)
        with _ -> (s, List.rev acc))
      | _ -> (s, List.rev acc)
    in
    try
      let s, x = p st in
      loop s [ x ]
    with _ -> (st, [])

  let opt p st =
    try
      let s, r = p st in
      (s, Some r)
    with _ -> (st, None)

  let btwn l r p st =
    let s = l st in
    let s', x = p s in
    (r s', x)

  let parens p = btwn (tok Token.LParen) (tok Token.RParen) p
  let braces p = btwn (tok Token.LBrace) (tok Token.RBrace) p
  let angles p = btwn (tok Token.Lt) (tok Token.Gt) p
  let mk_node k st = { Node.kind = k; span = span st }

  let ident_name st =
    let s, n, _ = ident st in
    (s, n)

  let rec attr st =
    match peek st with
    | Some (Token.At, _) ->
      let s = expect (adv st) Token.LBrack "[" in
      let s', n, _ = ident s in
      let s'', args = opt (parens (sep_by attr_arg Token.Comma)) s' in
      let s''' = expect s'' Token.RBrack "]" in
      (s''', Some { Node.name = n; args = Option.value args ~default:[] })
    | _ -> (st, None)

  and attr_arg st =
    match peek st with
    | Some (Token.Ident n, _) -> (adv st, AttrIdent n)
    | Some (Token.LitNumber n, _) -> (adv st, AttrNumber n)
    | Some (Token.LitString s, _) -> (adv st, AttrString s)
    | _ ->
      ( err st Error.E1003 (span st) [ "attribute argument"; "" ]
      , AttrIdent (Interner.empty_name st.interner) )

  let rec parse_expr st = expr_prec st 0

  and expr_prec st min =
    let s, l = expr_prefix st in
    let s', l' = expr_postfix_chain s l in
    expr_infix_chain s' l' min

  and expr_postfix_chain st l =
    match peek st with
    | Some (t, _) when Prec.is_postfix_op t ->
      let s, l' = expr_postfix l st in
      expr_postfix_chain s l'
    | _ -> (st, l)

  and expr_prefix st =
    match peek st with
    | Some (t, _) when Prec.is_prefix_op t ->
      let s, op =
        adv_then (fun s -> expr_prec s (Prec.prec_value Prec.Unary)) st
      in
      (s, mk_node (Node.ExprUnary { op = t; operand = op }) s)
    | _ -> expr_atom st

  and expr_infix_chain st l min =
    match peek st with
    | Some (Token.LtMinus, _) ->
      let s, r = adv_then parse_expr st in
      let tgt =
        match l.Node.kind with
        | ExprIdent n -> n
        | _ -> Interner.empty_name s.interner
      in
      (s, mk_node (Node.ExprAssign { target = tgt; value = r }) s)
    | Some (t, _) when Prec.is_infix_op t -> (
      match Prec.token_prec t with
      | Some (p, _) when Prec.prec_value p >= min ->
        let s, r = adv_then (fun s -> expr_prec s (Prec.prec_value p + 1)) st in
        expr_infix_chain
          s
          (mk_node (Node.ExprBinary { op = t; left = l; right = r }) s)
          min
      | _ -> (st, l))
    | _ -> (st, l)

  and expr_atom st =
    match peek st with
    | Some (Token.LitNumber n, s) ->
      (adv st, { Node.kind = ExprLit (LitNumber n); span = s })
    | Some (Token.LitString n, s) ->
      (adv st, { Node.kind = ExprLit (LitString n); span = s })
    | Some (Token.LitRune c, s) ->
      (adv st, { Node.kind = ExprLit (LitRune c); span = s })
    | Some (Token.LitTemplate t, s) ->
      (adv st, { Node.kind = ExprTemplate t; span = s })
    | Some (Token.Ident _, _) ->
      let s, n, sp = ident st in
      (s, { Node.kind = ExprIdent n; span = sp })
    | Some (Token.LBrace, _) -> (
      match peek (adv st) with
      | Some (Token.Dot, _) -> expr_rec_lit st
      | _ ->
        let s, b = adv_then block st in
        (s, mk_node (ExprBlock b) s))
    | Some (Token.LParen, _) -> (
      let s, es = parens (sep_by parse_expr Token.Comma) st in
      match es with
      | [] -> (s, mk_node ExprError s)
      | [ x ] -> (s, x)
      | f :: r -> (s, mk_node (ExprTuple (f, r)) s))
    | Some (Token.KwIf, _) -> expr_if st
    | Some (Token.KwMatch, _) -> expr_match st
    | Some (Token.KwFor, _) -> expr_for st
    | Some (Token.KwWhile, _) -> expr_while st
    | Some (Token.KwDefer, _) ->
      let s, e = adv_then parse_expr st in
      (s, mk_node (ExprDefer e) s)
    | Some (Token.KwBreak, _) -> (
      let s = adv st in
      match peek s with
      | Some (Token.Semi, _) | Some (Token.RBrace, _) | None ->
        (s, mk_node (ExprBreak None) s)
      | _ ->
        let s', e = parse_expr s in
        (s', mk_node (ExprBreak (Some e)) s'))
    | Some (Token.KwCycle, _) -> (adv st, mk_node ExprCycle st)
    | Some (Token.KwUnsafe, _) ->
      let s, b = adv_then block st in
      (s, mk_node (ExprUnsafe b) s)
    | Some (Token.KwFn, _) -> expr_fn st
    | Some (Token.KwRecord, _) -> expr_record st
    | Some (Token.KwChoice, _) -> expr_choice st
    | Some (Token.KwTrait, _) -> expr_trait st
    | _ -> (err st Error.E1101 (span st) [], mk_node ExprError st)

  and expr_postfix l st =
    match peek st with
    | Some (Token.Dot, _) ->
      let s, n, sp = adv_then ident st in
      ( s
      , {
          Node.kind = ExprField { base = l; field = n; optional = false }
        ; span = sp
        } )
    | Some (Token.LBrack, _) ->
      let s, i = adv_then parse_expr st in
      let s' = expect s Token.RBrack "]" in
      (s', mk_node (ExprIndex { base = l; index = i; optional = false }) s')
    | Some (Token.LParen, _) ->
      let s, a = adv_then (sep_by parse_expr Token.Comma) st in
      let s' = expect s Token.RParen ")" in
      ( s'
      , mk_node
          (ExprCall { callee = l; typ_args = []; args = a; optional = false })
          s' )
    | _ -> (st, l)

  and block st =
    braces
      (fun s ->
        let s', stmts = many parse_stmt s in
        match peek s' with
        | Some (Token.KwReturn, _) ->
          let s'', r = adv_then parse_expr s' in
          let s''' = expect s'' Token.Semi ";" in
          (s''', { Node.stmts; ret = Some r })
        | _ -> (s', { Node.stmts; ret = None }))
      st

  and expr_if st =
    let s = expect st Token.KwIf "if" in
    let s', c = cond s in
    let s'', cs = sep_by cond Token.Comma s' in
    let s''', tb = block s'' in
    let s'''', eb = opt (adv_then block) s''' in
    ( s''''
    , mk_node
        (ExprIf { conds = (c, cs); then_block = tb; else_block = eb })
        s'''' )

  and cond st =
    match peek st with
    | Some (Token.KwCase, _) ->
      let s, p = adv_then parse_pat st in
      let s' = expect s Token.ColonEq ":=" in
      let s'', v = parse_expr s' in
      (s'', CondCase { pat = p; value = v })
    | _ ->
      let s, e = parse_expr st in
      (s, CondExpr e)

  and expr_match st =
    let s = expect st Token.KwMatch "match" in
    let s', sc = parse_expr s in
    let s'', arms = braces (many match_arm) s' in
    (s'', mk_node (ExprMatch { scrutinee = sc; arms }) s'')

  and match_arm st =
    let s = expect st Token.KwCase "case" in
    let s', p = parse_pat s in
    let s'', g = opt (adv_then parse_expr) s' in
    let s''' = expect s'' Token.EqGt "=>" in
    let s'''', b = parse_expr s''' in
    let s''''' =
      match peek s'''' with Some (Token.Comma, _) -> adv s'''' | _ -> s''''
    in
    (s''''', { Node.pat = p; guard = g; body = b })

  and expr_for st =
    let s = expect st Token.KwFor "for" in
    let s', b = for_bind s in
    let s'' = expect s' Token.KwIn "in" in
    let s''', r = parse_expr s'' in
    let s'''', g = opt (adv_then parse_expr) s''' in
    let s''''', bd = block s'''' in
    ( s'''''
    , mk_node (ExprFor { binding = b; range = r; guard = g; body = bd }) s'''''
    )

  and for_bind st =
    match peek st with
    | Some (Token.KwCase, _) ->
      let s, p = adv_then parse_pat st in
      (s, ForCase p)
    | Some (Token.Ident _, _) ->
      let s, n, _ = ident st in
      (s, ForIdent n)
    | _ ->
      ( err st Error.E1105 (span st) []
      , ForIdent (Interner.empty_name st.interner) )

  and expr_while st =
    let s = expect st Token.KwWhile "while" in
    let s', c = cond s in
    let s'', g = opt (adv_then parse_expr) s' in
    let s''', bd = block s'' in
    (s''', mk_node (ExprWhile { cond = c; guard = g; body = bd }) s''')

  and expr_rec_lit st =
    let s, fs = braces (sep_by rec_field_init Token.Comma) st in
    (s, mk_node (ExprRecordLit { name = None; fields = fs }) s)

  and rec_field_init st =
    match peek st with
    | Some (Token.Dot, _) ->
      let s, n, _ = adv_then ident st in
      let s' = expect s Token.ColonEq ":=" in
      let s'', v = parse_expr s' in
      (s'', { Node.shorthand = false; name = n; value = v })
    | Some (Token.Ident n, _) ->
      let s = expect (adv st) Token.ColonEq ":=" in
      let s', v = parse_expr s in
      (s', { Node.shorthand = true; name = n; value = v })
    | _ ->
      let _ = span st in
      ( st
      , {
          Node.shorthand = false
        ; name = Interner.empty_name st.interner
        ; value = mk_node ExprError st
        } )

  and expr_fn st =
    let s, abi =
      match peek st with
      | Some (Token.KwExtern, _) -> (
        match peek (adv st) with
        | Some (Token.LitString str, _) ->
          let s = adv (adv st) in
          (s, Some (Interner.lookup s.interner str))
        | _ -> (adv st, None))
      | _ -> (st, None)
    in
    let s' = expect s Token.KwFn "fn" in
    let s'', nm = opt ident_name s' in
    let s''', tp = opt (angles (sep_by ident_name Token.Comma)) s'' in
    let s'''', prm = fn_params s''' in
    let s''''', rt = opt (adv_then parse_typ) s'''' in
    let s'''''', bd = block s''''' in
    ( s''''''
    , mk_node
        (ExprFn
           {
             abi
           ; name = nm
           ; typ_params = Option.value tp ~default:[]
           ; params = prm
           ; ret_typ = rt
           ; body = bd
           })
        s'''''' )

  and expr_record st =
    let s = expect st Token.KwRecord "record" in
    let s', tp = opt (angles (sep_by ident_name Token.Comma)) s in
    let s'', tb = opt (adv_then parse_typ) s' in
    let s''', (fs, bd) =
      braces
        (fun st ->
          let s, its = sep_by rec_item Token.Comma st in
          let flds =
            List.filter_map (function `F f -> Some f | _ -> None) its
          in
          let mths =
            List.filter_map (function `M m -> Some m | _ -> None) its
          in
          (s, (flds, mths)))
        s''
    in
    ( s'''
    , mk_node
        (ExprRecord
           {
             typ_params = Option.value tp ~default:[]
           ; trait_bound = tb
           ; fields = fs
           ; body = bd
           })
        s''' )

  and rec_item st =
    let s, n, _ = ident st in
    match peek s with
    | Some (Token.Colon, _) ->
      let s', t = adv_then parse_typ s in
      (s', `F { Node.name = n; typ = t })
    | Some (Token.ColonEq, _) ->
      let s', e = adv_then parse_expr s in
      ( s'
      , `M
          {
            Node.attr = None
          ; kind =
              StmtBind { mutable_ = false; name = n; typ = None; value = e }
          ; span = span s'
          } )
    | _ ->
      let sp = span s in
      ( err s Error.E1003 sp [ "':' or ':='"; "" ]
      , `F { Node.name = n; typ = mk_node TypError s } )

  and expr_choice st =
    let s = expect st Token.KwChoice "choice" in
    let s', tp = opt (angles (sep_by ident_name Token.Comma)) s in
    let s'', cs = braces (sep_by choice_case Token.Comma) s' in
    ( s''
    , mk_node
        (ExprChoice
           { typ_params = Option.value tp ~default:[]; cases = cs; body = [] })
        s'' )

  and choice_case st =
    let s = expect st Token.KwCase "case" in
    let s', n, _ = ident s in
    let s'', fs = opt (parens (sep_by parse_typ Token.Comma)) s' in
    (s'', { Node.name = n; fields = Option.value fs ~default:[] })

  and expr_trait st =
    let s = expect st Token.KwTrait "trait" in
    let s', tp = opt (angles (sep_by ident_name Token.Comma)) s in
    let s'', tb = opt (adv_then parse_typ) s' in
    let s''', its = braces (sep_by trait_item Token.Comma) s'' in
    ( s'''
    , mk_node
        (ExprTrait
           {
             typ_params = Option.value tp ~default:[]
           ; trait_bound = tb
           ; items = its
           })
        s''' )

  and trait_item st =
    let s, n, _ = ident st in
    let s' = expect s Token.Colon ":" in
    let s'', t = parse_typ s' in
    (s'', { Node.name = n; typ = Some t })

  and parse_pat st =
    match peek st with
    | Some (Token.KwVal, sp) ->
      let s, n, _ = adv_then ident st in
      (s, { Node.kind = PatBind { mutable_ = false; name = n }; span = sp })
    | Some (Token.KwVar, sp) ->
      let s, n, _ = adv_then ident st in
      (s, { Node.kind = PatBind { mutable_ = true; name = n }; span = sp })
    | Some (Token.LitNumber n, sp) ->
      (adv st, { Node.kind = PatLit (LitNumber n); span = sp })
    | Some (Token.LitString s, sp) ->
      (adv st, { Node.kind = PatLit (LitString s); span = sp })
    | Some (Token.LitRune c, sp) ->
      (adv st, { Node.kind = PatLit (LitRune c); span = sp })
    | Some (Token.Underscore, sp) -> (adv st, { Node.kind = PatWild; span = sp })
    | Some (Token.Ident _, _) -> (
      let s, n, sp = ident st in
      match peek s with
      | Some (Token.LBrace, _) ->
        let s', fs = braces (sep_by pat_field Token.Comma) s in
        (s', { Node.kind = PatRecord { name = n; fields = fs }; span = span s' })
      | Some (Token.LParen, _) ->
        let s', args = parens (sep_by parse_pat Token.Comma) s in
        (s', { Node.kind = PatCtor { name = n; args }; span = span s' })
      | _ -> (s, { Node.kind = PatIdent n; span = sp }))
    | Some (Token.LParen, _) -> (
      let s, ps = parens (sep_by parse_pat Token.Comma) st in
      match ps with
      | [] -> (err s Error.E1103 (span s) [], mk_node PatError s)
      | [ x ] -> (s, x)
      | f :: r -> (s, mk_node (PatTuple (f, r)) s))
    | _ -> (err st Error.E1103 (span st) [], mk_node PatError st)

  and pat_field st =
    let s = expect st Token.Dot "." in
    let s', n, _ = ident s in
    let s'', p = opt (adv_then parse_pat) s' in
    (s'', { Node.name = n; pat = p })

  and parse_typ st =
    match peek st with
    | Some (Token.Caret, _) ->
      let s, t = adv_then parse_typ st in
      (s, mk_node (TypPtr t) s)
    | Some (Token.LBrack, _) ->
      let s = adv st in
      let s', sz =
        match peek s with
        | Some (Token.RBrack, _) -> (s, None)
        | _ ->
          let s', e = parse_expr s in
          (s', Some e)
      in
      let s'' = expect s' Token.RBrack "]" in
      let s''', el = parse_typ s'' in
      (s''', mk_node (TypArr { size = sz; elem = el }) s''')
    | Some (Token.Ident _, _) -> (
      let s, n, sp = ident st in
      match peek s with
      | Some (Token.Lt, _) ->
        let s', args = angles (sep_by parse_typ Token.Comma) s in
        (s', mk_node (TypApp { base = n; args }) s')
      | _ -> (s, { Node.kind = TypIdent n; span = sp }))
    | Some (Token.LParen, _) -> (
      let s, ts = parens (sep_by parse_typ Token.Comma) st in
      match ts with
      | [] -> (err s Error.E1104 (span s) [], mk_node TypError s)
      | [ x ] -> (s, x)
      | f :: r -> (s, mk_node (TypTuple (f, r)) s))
    | Some (Token.KwFn, _) ->
      let s = adv st in
      let s', ps = parens (sep_by parse_typ Token.Comma) s in
      let s'', rt = opt (adv_then parse_typ) s' in
      (s'', mk_node (TypFn { params = ps; ret = rt }) s'')
    | Some (Token.LBrace, _) ->
      let s, fs = braces (sep_by typ_rec_field Token.Comma) st in
      (s, mk_node (TypRecord fs) s)
    | _ ->
      ( err st Error.E1104 (span st) []
      , mk_node (TypIdent (Interner.empty_name st.interner)) st )

  and typ_rec_field st =
    let s, n, _ = ident st in
    let s' = expect s Token.Colon ":" in
    let s'', t = parse_typ s' in
    (s'', { Node.name = n; typ = t })

  and fn_params st = parens (sep_by fn_param Token.Comma) st

  and fn_param st =
    let s, n, _ = ident st in
    let s', t = opt (adv_then parse_typ) s in
    (s', { Node.name = n; typ = t })

  and parse_stmt st =
    let s, a = attr st in
    let s', kind =
      match peek s with
      | Some (Token.KwImport, _) -> stmt_import s
      | Some (Token.KwExport, _) -> stmt_export s
      | Some (Token.KwVal, _) | Some (Token.KwVar, _) -> stmt_bind s
      | Some (Token.KwExtern, _) -> stmt_extern s
      | _ ->
        let s'', e = parse_expr s in
        (s'', StmtExpr e)
    in
    (s', { Node.attr = a; kind; span = span s' })

  and stmt_import st =
    let s = expect st Token.KwImport "import" in
    let s', cl = import_clause s in
    let s'' = expect s' Token.KwFrom "from" in
    let s''', src =
      match peek s'' with
      | Some (Token.LitString s, _) -> (adv s'', s)
      | _ ->
        ( err s'' Error.E1003 (span s'') [ "string"; "" ]
        , Interner.empty_name s''.interner )
    in
    let s'''' = expect s''' Token.Semi ";" in
    (s'''', StmtImport { clause = cl; source = src })

  and import_clause st =
    match peek st with
    | Some (Token.Star, _) ->
      let s = expect (adv st) Token.KwAs "as" in
      let s', n, _ = ident s in
      (s', ImportAll n)
    | Some (Token.LBrace, _) ->
      let s, ns = braces (sep_by ident_name Token.Comma) st in
      (s, ImportNamed ns)
    | _ -> (err st Error.E1003 (span st) [ "import clause"; "" ], ImportNamed [])

  and stmt_export st =
    let s = expect st Token.KwExport "export" in
    let s', cl = export_clause s in
    let s'', src =
      match peek s' with
      | Some (Token.KwFrom, _) -> (
        match peek (adv s') with
        | Some (Token.LitString str, _) -> (adv (adv s'), Some str)
        | _ -> (err (adv s') Error.E1003 (span s') [ "string"; "" ], None))
      | _ -> (s', None)
    in
    let s''' = expect s'' Token.Semi ";" in
    (s''', StmtExport { clause = cl; source = src })

  and export_clause st =
    match peek st with
    | Some (Token.Star, _) ->
      let s = expect (adv st) Token.KwAs "as" in
      let s', n, _ = ident s in
      (s', ExportAll n)
    | Some (Token.LBrace, _) ->
      let s, ns = braces (sep_by ident_name Token.Comma) st in
      (s, ExportNamed ns)
    | _ -> (err st Error.E1003 (span st) [ "export clause"; "" ], ExportNamed [])

  and stmt_bind st =
    let s, mut, _ =
      match peek st with
      | Some (Token.KwVal, sp) -> (adv st, false, sp)
      | Some (Token.KwVar, sp) -> (adv st, true, sp)
      | _ ->
        let sp = span st in
        (err st Error.E1003 sp [ "val or var"; "" ], false, sp)
    in
    let s', n, _ = ident s in
    let s'', t = opt (adv_then parse_typ) s' in
    let s''' = expect s'' Token.ColonEq ":=" in
    let s'''', v = parse_expr s''' in
    let s''''' = expect s'''' Token.Semi ";" in
    (s''''', StmtBind { mutable_ = mut; name = n; typ = t; value = v })

  and stmt_extern st =
    let s = expect st Token.KwExtern "extern" in
    let s', abi =
      match peek s with
      | Some (Token.LitString str, _) ->
        (adv s, Some (Interner.lookup s.interner str))
      | _ -> (s, None)
    in
    let s'' = expect s' Token.KwUnsafe "unsafe" in
    let s''', ds = braces (many fn_sig) s'' in
    (s''', StmtExtern { abi; decls = ds })

  and fn_sig st =
    let s = expect st Token.KwFn "fn" in
    let s', n, _ = ident s in
    let s'', _ = fn_params s' in
    let s''', t = opt (adv_then parse_typ) s'' in
    (s''', { Node.name = n; typ = t })

  let prog st =
    let rec loop s acc =
      match peek s with
      | Some (Token.EOF, _) -> (s, List.rev acc)
      | Some (Token.Newline, _) -> loop (adv s) acc
      | _ ->
        let s', st = parse_stmt s in
        loop s' (st :: acc)
    in
    loop st []

  let parse src fid int toks =
    let st = mk_state src fid toks int in
    let st', p = prog st in
    (p, st'.diags)
end

include Make ()
