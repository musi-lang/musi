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

  val peek_opt : state -> (Token.t * Span.t) option
  val advance : state -> state
  val curr_span : state -> Span.t
  val add_error : state -> string -> Span.t -> state
  val add_error_code : state -> Error.code -> Span.t -> string list -> state

  val parse :
       string
    -> Span.file_id
    -> Interner.t
    -> (Token.t * Span.t) list
    -> prog * Diagnostic.bag

  val parse_prog : state -> state * prog
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

  let mk_state source file_id tokens interner =
    { source; file_id; tokens; pos = 0; interner; diags = Diagnostic.empty_bag }

  let peek_opt st =
    if st.pos >= List.length st.tokens then None
    else Some (List.nth st.tokens st.pos)

  let advance st = { st with pos = st.pos + 1 }

  let advance_then parser st =
    let st' = advance st in
    parser st'

  let curr_span st =
    if List.length st.tokens = 0 then Span.dummy
    else snd (List.nth st.tokens (min st.pos (List.length st.tokens - 1)))

  let add_error st msg span =
    { st with diags = Diagnostic.add st.diags (Diagnostic.error msg span) }

  let add_error_code st code span args =
    { st with diags = Diagnostic.add st.diags (Error.diag code span args) }

  let expect st token_type expected_name =
    match peek_opt st with
    | Some (token, _) when token = token_type -> advance st
    | Some (token, span) ->
      let found = Token.to_string token in
      add_error_code st Error.E1003 span [ expected_name; found ]
    | None ->
      let span = curr_span st in
      add_error_code st Error.E1003 span [ expected_name; "EOF" ]

  let expect_ident st =
    match peek_opt st with
    | Some (Token.Ident name, span) ->
      let st' = advance st in
      (st', name, span)
    | Some (token, span) ->
      let found = Token.to_string token in
      let st' = add_error_code st Error.E1105 span [ found ] in
      (st', Interner.empty_name st.interner, span)
    | None ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1105 span [ "EOF" ] in
      (st', Interner.empty_name st.interner, span)

  let expect_ident_name st =
    let st', name, _ = expect_ident st in
    (st', name)

  let parse_sep_list st sep parser =
    let rec loop st acc =
      match peek_opt st with
      | Some (sep_token, _) when sep_token = sep ->
        let st' = advance st in
        begin match parser st' with
        | st'', item -> loop st'' (item :: acc)
        | exception _ -> (st', List.rev acc)
        end
      | _ -> (st, List.rev acc)
    in
    try match parser st with st', first_item -> loop st' [ first_item ]
    with Failure _ -> (st, [])

  let parse_many_opts st parser =
    let rec loop st acc =
      try
        let st', item = parser st in
        loop st' (item :: acc)
      with Failure _ -> (st, List.rev acc)
    in
    loop st []

  let satisfy pred st =
    match peek_opt st with
    | Some (token, _) when pred token -> advance st
    | _ -> failwith "satisfy failed"

  let token t = satisfy (fun x -> x = t)

  let many parser st =
    let rec loop st acc =
      try
        let st', item = parser st in
        loop st' (item :: acc)
      with Failure _ -> (st, List.rev acc)
    in
    loop st []

  let many1 parser st =
    let st', first = parser st in
    let st'', rest = many parser st' in
    (st'', first :: rest)

  let sep_by parser sep st =
    let rec loop st acc =
      match peek_opt st with
      | Some (sep_token, _) when sep_token = sep ->
        let st' = advance st in
        begin try
          let st'', item = parser st' in
          loop st'' (item :: acc)
        with Failure _ -> (st', List.rev acc)
        end
      | _ -> (st, List.rev acc)
    in
    try
      let st', first = parser st in
      loop st' [ first ]
    with Failure _ -> (st, [])

  let opt parser st =
    try
      let st', result = parser st in
      (st', Some result)
    with Failure _ -> (st, None)

  let btwn left right parser st =
    let st' = left st in
    let st'', result = parser st' in
    let st''' = right st'' in
    (st''', result)

  let btwn_parens parser = btwn (token Token.LParen) (token Token.RParen) parser
  let btwn_braces parser = btwn (token Token.LBrace) (token Token.RBrace) parser

  let rec parse_expr st = parse_expr_with_prec st 0

  and parse_expr_with_prec st min_prec =
    let st', left = parse_expr_prefix st in
    let st'', left_with_postfix = parse_expr_postfix_chain st' left in
    parse_expr_infix_chain st'' left_with_postfix min_prec

  and parse_expr_postfix_chain st left =
    match peek_opt st with
    | Some (token, _) when Prec.is_postfix_op token ->
      let st', left' = parse_expr_postfix left st in
      parse_expr_postfix_chain st' left'
    | _ -> (st, left)

  and parse_expr_prefix st =
    match peek_opt st with
    | Some (token, _) when Prec.is_prefix_op token ->
      let st', operand =
        advance_then
          (fun s -> parse_expr_with_prec s (Prec.prec_value Prec.Unary))
          st
      in
      let span = curr_span st' in
      (st', { Node.kind = Node.ExprUnary { op = token; operand }; span })
    | _ -> parse_expr_atom st

  and parse_expr_infix_chain st left min_prec =
    match peek_opt st with
    | Some (Token.MinusGt, _) ->
      let st', right = advance_then parse_expr st in
      let span = curr_span st' in
      ( st'
      , {
          Node.kind =
            Node.ExprAssign
              {
                target =
                  (match left.Node.kind with
                  | Node.ExprIdent name -> name
                  | _ -> Interner.empty_name st'.interner)
              ; value = right
              }
        ; span
        } )
    | Some (token, _) when Prec.is_infix_op token -> (
      match Prec.token_prec token with
      | Some (prec_typ, _) when Prec.prec_value prec_typ >= min_prec ->
        let st', right =
          advance_then
            (fun s -> parse_expr_with_prec s (Prec.prec_value prec_typ + 1))
            st
        in
        let span = curr_span st' in
        let binary_expr =
          { Node.kind = Node.ExprBinary { op = token; left; right }; span }
        in
        parse_expr_infix_chain st' binary_expr min_prec
      | _ -> (st, left))
    | _ -> (st, left)

  and parse_expr_lit st =
    match peek_opt st with
    | Some (Token.LitNumber n, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.ExprLit (Node.LitNumber n); span })
    | Some (Token.LitString s, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.ExprLit (Node.LitString s); span })
    | Some (Token.LitRune c, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.ExprLit (Node.LitRune c); span })
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1101 span [] in
      (st', { Node.kind = Node.ExprError; span })

  and parse_expr_template st =
    match peek_opt st with
    | Some (Token.LitTemplate t, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.ExprTemplate t; span })
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1101 span [] in
      (st', { Node.kind = Node.ExprError; span })

  and parse_expr_block_or_record_lit st =
    match peek_opt st with
    | Some (Token.LBrace, _) -> (
      let st' = advance st in
      match peek_opt st' with
      | Some (Token.Dot, _) -> parse_expr_record_lit st'
      | _ -> parse_expr_block st')
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1108 span [] in
      (st', { Node.kind = Node.ExprError; span })

  and parse_expr_tuple_or_grouping st =
    let st', expr = btwn_parens parse_expr st in
    let span = curr_span st' in
    (st', { Node.kind = Node.ExprTuple (expr, []); span })

  and parse_expr_ident st =
    let st', name, span = expect_ident st in
    (st', { Node.kind = Node.ExprIdent name; span })

  and parse_expr_tuple st =
    let st', exprs = btwn_parens (sep_by parse_expr Token.Comma) st in
    let span = curr_span st' in
    match exprs with
    | [] ->
      let error_expr =
        { Node.kind = Node.ExprLit (Node.LitNumber "0"); span }
      in
      (st', error_expr)
    | [ single ] -> (st', single)
    | first :: rest -> (st', { Node.kind = Node.ExprTuple (first, rest); span })

  and parse_expr_block st =
    let st', stmts_and_ret =
      btwn_braces
        (fun s ->
          let s', stmts = many parse_stmt s in
          match peek_opt s' with
          | Some (Token.KwReturn, _) ->
            let s'' = advance s' in
            let s''', ret = parse_expr s'' in
            (s''', (stmts, Some ret))
          | _ -> (s', (stmts, None)))
        st
    in
    let span = curr_span st' in
    let stmts, ret = stmts_and_ret in
    (st', { Node.kind = Node.ExprBlock { stmts; ret }; span })

  and parse_expr_if st =
    let st' = expect st Token.KwIf "if" in
    let st'', first_cond = parse_cond st' in
    let st''', rest_conds = parse_sep_list st'' Token.Comma parse_cond in
    let st'''', then_block = parse_block st''' in
    let st''''', else_block =
      match peek_opt st'''' with
      | Some (Token.KwElse, _) ->
        let s = advance st'''' in
        let s', blk = parse_block s in
        (s', Some blk)
      | _ -> (st'''', None)
    in
    let span = curr_span st''''' in
    ( st'''''
    , {
        Node.kind =
          Node.ExprIf
            { conds = (first_cond, rest_conds); then_block; else_block }
      ; span
      } )

  and parse_block st =
    let st', stmts_and_ret =
      btwn_braces
        (fun s ->
          let s', stmts = many parse_stmt s in
          match peek_opt s' with
          | Some (Token.KwReturn, _) ->
            let s'' = advance s' in
            let s''', ret = parse_expr s'' in
            let s'''' = expect s''' Token.Semi ";" in
            (s'''', (stmts, Some ret))
          | _ -> (s', (stmts, None)))
        st
    in
    let stmts, ret = stmts_and_ret in
    (st', { Node.stmts; ret })

  and parse_cond st =
    match peek_opt st with
    | Some (Token.KwCase, _) ->
      let st' = advance st in
      let st'', pat = parse_pat st' in
      let st''' = expect st'' Token.ColonEq ":=" in
      let st'''', value = parse_expr st''' in
      (st'''', Node.CondCase { pat; value })
    | _ ->
      let st', expr = parse_expr st in
      (st', Node.CondExpr expr)

  and parse_expr_match st =
    let st' = expect st Token.KwMatch "match" in
    let st'', scrutinee = parse_expr st' in
    let st''' = expect st'' Token.LBrace "{" in
    let st'''', arms = many parse_match_arm st''' in
    let st''''' = expect st'''' Token.RBrace "}" in
    let span = curr_span st''''' in
    (st''''', { Node.kind = Node.ExprMatch { scrutinee; arms }; span })

  and parse_match_arm st =
    let st' = expect st Token.KwCase "case" in
    let st'', pat = parse_pat st' in
    let st''', guard =
      match peek_opt st'' with
      | Some (Token.KwIf, _) ->
        let s = advance st'' in
        let s', g = parse_expr s in
        (s', Some g)
      | _ -> (st'', None)
    in
    let st'''' = expect st''' Token.EqGt "=>" in
    let st''''', body = parse_expr st'''' in
    let st'''''' =
      match peek_opt st''''' with
      | Some (Token.Comma, _) -> advance st'''''
      | _ -> st'''''
    in
    (st'''''', { Node.pat; guard; body })

  and parse_expr_for st =
    let st' = expect st Token.KwFor "for" in
    let st'', binding = parse_for_binding st' in
    let st''' = expect st'' Token.KwIn "in" in
    let st'''', range = parse_expr st''' in
    let st''''', guard =
      match peek_opt st'''' with
      | Some (Token.KwIf, _) ->
        let s = advance st'''' in
        let s', g = parse_expr s in
        (s', Some g)
      | _ -> (st'''', None)
    in
    let st'''''', body = parse_block st''''' in
    let span = curr_span st'''''' in
    ( st''''''
    , { Node.kind = Node.ExprFor { binding; range; guard; body }; span } )

  and parse_for_binding st =
    match peek_opt st with
    | Some (Token.KwCase, _) ->
      let st' = advance st in
      let st'', pat = parse_pat st' in
      (st'', Node.ForCase pat)
    | Some (Token.Ident _, _) ->
      let st', name, _ = expect_ident st in
      (st', Node.ForIdent name)
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1105 span [] in
      (st', Node.ForIdent (Interner.empty_name st.interner))

  and parse_expr_while st =
    let st' = expect st Token.KwWhile "while" in
    let st'', cond = parse_cond st' in
    let st''', guard =
      match peek_opt st'' with
      | Some (Token.KwIf, _) ->
        let s = advance st'' in
        let s', g = parse_expr s in
        (s', Some g)
      | _ -> (st'', None)
    in
    let st'''', body = parse_block st''' in
    let span = curr_span st'''' in
    (st'''', { Node.kind = Node.ExprWhile { cond; guard; body }; span })

  and parse_expr_defer st =
    let st' = expect st Token.KwDefer "defer" in
    let st'', expr = parse_expr st' in
    let span = curr_span st'' in
    (st'', { Node.kind = Node.ExprDefer expr; span })

  and parse_expr_break st =
    let st' = expect st Token.KwBreak "break" in
    match peek_opt st' with
    | Some (Token.Semi, _) | Some (Token.RBrace, _) | None ->
      let span = curr_span st' in
      (st', { Node.kind = Node.ExprBreak None; span })
    | _ ->
      let st'', expr = parse_expr st' in
      let span = curr_span st'' in
      (st'', { Node.kind = Node.ExprBreak (Some expr); span })

  and parse_expr_cycle st =
    let span = curr_span st in
    let st' = expect st Token.KwCycle "cycle" in
    (st', { Node.kind = Node.ExprCycle; span })

  and parse_expr_unsafe st =
    let st' = expect st Token.KwUnsafe "unsafe" in
    let st'', block = parse_block st' in
    let span = curr_span st'' in
    (st'', { Node.kind = Node.ExprUnsafe block; span })

  and parse_expr_record_lit st =
    let st', fields =
      btwn_braces (sep_by parse_record_field_init Token.Comma) st
    in
    let span = curr_span st' in
    (st', { Node.kind = Node.ExprRecordLit { name = None; fields }; span })

  and parse_record_field_init st =
    match peek_opt st with
    | Some (Token.Dot, _) ->
      let st' = advance st in
      let st'', name, _ = expect_ident st' in
      let st''' = expect st'' Token.Colon ":=" in
      let st'''', value = parse_expr st''' in
      (st'''', { Node.shorthand = false; name; value })
    | Some (Token.Ident name, _) ->
      let st' = advance st in
      let st'' = expect st' Token.Colon ":=" in
      let st''', value = parse_expr st'' in
      (st''', { Node.shorthand = true; name; value })
    | _ ->
      let span = curr_span st in
      let error_field =
        {
          Node.shorthand = false
        ; name = Interner.empty_name st.interner
        ; value = { Node.kind = Node.ExprLit (Node.LitNumber "0"); span }
        }
      in
      (st, error_field)

  and parse_expr_fn st =
    let st', abi =
      match peek_opt st with
      | Some (Token.KwExtern, _) -> (
        let s = advance st in
        match peek_opt s with
        | Some (Token.LitString str, _) ->
          let s' = advance s in
          let abi_str = Interner.lookup s'.interner str in
          (s', Some abi_str)
        | _ -> (s, None))
      | _ -> (st, None)
    in
    let st'' = expect st' Token.KwFn "fn" in
    let st''', name =
      match peek_opt st'' with
      | Some (Token.Ident _, _) ->
        let s, n, _ = expect_ident st'' in
        (s, Some n)
      | _ -> (st'', None)
    in
    let st'''', typ_params =
      match peek_opt st''' with
      | Some (Token.Lt, _) ->
        btwn
          (token Token.Lt)
          (token Token.Gt)
          (sep_by expect_ident_name Token.Comma)
          st'''
      | _ -> (st''', [])
    in
    let st''''', params = parse_fn_params st'''' in
    let st'''''', ret_typ =
      match peek_opt st''''' with
      | Some (Token.MinusGt, _) ->
        let s = advance st''''' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st''''', None)
    in
    let st''''''', body = parse_block st'''''' in
    let span = curr_span st''''''' in
    ( st'''''''
    , {
        Node.kind = Node.ExprFn { abi; name; typ_params; params; ret_typ; body }
      ; span
      } )

  and parse_expr_record st =
    let st' = expect st Token.KwRecord "record" in
    let st'', typ_params =
      match peek_opt st' with
      | Some (Token.Lt, _) ->
        btwn
          (token Token.Lt)
          (token Token.Gt)
          (sep_by expect_ident_name Token.Comma)
          st'
      | _ -> (st', [])
    in
    let st''', trait_bound =
      match peek_opt st'' with
      | Some (Token.Colon, _) ->
        let s = advance st'' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st'', None)
    in
    let st'''', (fields, body) =
      btwn_braces
        (fun s ->
          let fields_and_methods = sep_by parse_record_item Token.Comma s in
          let s', items = fields_and_methods in
          let fields =
            List.filter_map
              (function `Field f -> Some f | `Method _ -> None)
              items
          in
          let methods =
            List.filter_map
              (function `Field _ -> None | `Method m -> Some m)
              items
          in
          (s', (fields, methods)))
        st'''
    in
    let span = curr_span st'''' in
    ( st''''
    , {
        Node.kind = Node.ExprRecord { typ_params; trait_bound; fields; body }
      ; span
      } )

  and parse_record_item st =
    let st', name, _ = expect_ident st in
    match peek_opt st' with
    | Some (Token.Colon, _) ->
      let st'' = advance st' in
      let st''', typ = parse_typ st'' in
      (st''', `Field ({ Node.name; typ } : Node.record_field))
    | Some (Token.ColonEq, _) ->
      let st'' = advance st' in
      let st''', expr = parse_expr st'' in
      let span = curr_span st''' in
      ( st'''
      , `Method
          ({
             Node.attr = None
           ; kind =
               Node.StmtBind
                 { mutable_ = false; name; typ = None; value = expr }
           ; span
           }
            : Node.stmt) )
    | _ ->
      let span = curr_span st' in
      let st'' = add_error_code st' Error.E1003 span [ "':' or ':='"; "" ] in
      let error_typ = { Node.kind = Node.TypError; span } in
      (st'', `Field ({ Node.name; typ = error_typ } : Node.record_field))

  and parse_expr_choice st =
    let st' = expect st Token.KwChoice "choice" in
    let st'', typ_params =
      match peek_opt st' with
      | Some (Token.Lt, _) ->
        btwn
          (token Token.Lt)
          (token Token.Gt)
          (sep_by expect_ident_name Token.Comma)
          st'
      | _ -> (st', [])
    in
    let st''', cases =
      btwn_braces (sep_by parse_choice_case Token.Comma) st''
    in
    let span = curr_span st''' in
    ( st'''
    , { Node.kind = Node.ExprChoice { typ_params; cases; body = [] }; span } )

  and parse_choice_case st =
    let st' = expect st Token.KwCase "case" in
    let st'', name, _ = expect_ident st' in
    let st''', fields =
      match peek_opt st'' with
      | Some (Token.LParen, _) ->
        btwn_parens (sep_by parse_typ Token.Comma) st''
      | _ -> (st'', [])
    in
    (st''', { Node.name; fields })

  and parse_expr_trait st =
    let st' = expect st Token.KwTrait "trait" in
    let st'', typ_params =
      match peek_opt st' with
      | Some (Token.Lt, _) ->
        btwn
          (token Token.Lt)
          (token Token.Gt)
          (sep_by expect_ident_name Token.Comma)
          st'
      | _ -> (st', [])
    in
    let st''', trait_bound =
      match peek_opt st'' with
      | Some (Token.Colon, _) ->
        let s = advance st'' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st'', None)
    in
    let st'''', items =
      btwn_braces (sep_by parse_trait_item Token.Comma) st'''
    in
    let span = curr_span st'''' in
    ( st''''
    , { Node.kind = Node.ExprTrait { typ_params; trait_bound; items }; span } )

  and parse_trait_item st =
    let st', name, _ = expect_ident st in
    let st'' = expect st' Token.Colon ":" in
    let st''', typ = parse_typ st'' in
    (st''', { Node.name; typ = Some typ })

  and parse_expr_atom st =
    match peek_opt st with
    | Some (Token.Ident _, _) -> parse_expr_ident st
    | Some (Token.LBrace, _) -> parse_expr_block_or_record_lit st
    | Some (Token.LParen, _) -> parse_expr_tuple_or_grouping st
    | _ ->
      let span = curr_span st in
      let error_expr =
        { Node.kind = Node.ExprLit (Node.LitNumber "0"); span }
      in
      (st, error_expr)

  and parse_expr_postfix left st =
    match peek_opt st with
    | Some (Token.Dot, _) ->
      let st', name, span = advance_then expect_ident st in
      ( st'
      , {
          Node.kind =
            Node.ExprField { base = left; field = name; optional = false }
        ; span
        } )
    | Some (Token.LBrack, _) ->
      let st', index = advance_then parse_expr st in
      let st'' = expect st' Token.RBrack "]" in
      let span = curr_span st'' in
      ( st''
      , {
          Node.kind = Node.ExprIndex { base = left; index; optional = false }
        ; span
        } )
    | Some (Token.LParen, _) ->
      let st', args = advance_then (sep_by parse_expr Token.Comma) st in
      let st'' = expect st' Token.RParen ")" in
      let span = curr_span st'' in
      ( st''
      , {
          Node.kind =
            Node.ExprCall
              { callee = left; typ_args = []; args; optional = false }
        ; span
        } )
    | _ -> (st, left)

  and parse_pat st =
    match peek_opt st with
    | Some (Token.Ident _, _) -> parse_pat_ident st
    | _ ->
      let span = curr_span st in
      let error_pat = { Node.kind = Node.PatWild; span } in
      (st, error_pat)

  and parse_pat_bind st =
    match peek_opt st with
    | Some (Token.KwVal, span) ->
      let st' = advance st in
      let st'', name, _ = expect_ident st' in
      (st'', { Node.kind = Node.PatBind { mutable_ = false; name }; span })
    | Some (Token.KwVar, span) ->
      let st' = advance st in
      let st'', name, _ = expect_ident st' in
      (st'', { Node.kind = Node.PatBind { mutable_ = true; name }; span })
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1103 span [] in
      (st', { Node.kind = Node.PatError; span })

  and parse_pat_lit st =
    match peek_opt st with
    | Some (Token.LitNumber n, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.PatLit (Node.LitNumber n); span })
    | Some (Token.LitString s, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.PatLit (Node.LitString s); span })
    | Some (Token.LitRune c, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.PatLit (Node.LitRune c); span })
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1103 span [] in
      (st', { Node.kind = Node.PatError; span })

  and parse_pat_wild st =
    match peek_opt st with
    | Some (Token.Underscore, span) ->
      let st' = advance st in
      (st', { Node.kind = Node.PatWild; span })
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1103 span [] in
      (st', { Node.kind = Node.PatError; span })

  and parse_pat_ident st =
    let st', name, span = expect_ident st in
    (st', { Node.kind = Node.PatIdent name; span })

  and parse_pat_record st =
    let st', name, _ = expect_ident st in
    let st'', fields = btwn_braces (sep_by parse_pat_field Token.Comma) st' in
    let span' = curr_span st'' in
    (st'', { Node.kind = Node.PatRecord { name; fields }; span = span' })

  and parse_pat_field st =
    let st' = expect st Token.Dot "." in
    let st'', name, _ = expect_ident st' in
    match peek_opt st'' with
    | Some (Token.ColonEq, _) ->
      let st''' = advance st'' in
      let st'''', pat = parse_pat st''' in
      (st'''', { Node.name; pat = Some pat })
    | _ -> (st'', { Node.name; pat = None })

  and parse_pat_ctor st =
    let st', name, span = expect_ident st in
    match peek_opt st' with
    | Some (Token.LParen, _) ->
      let st'', args = btwn_parens (sep_by parse_pat Token.Comma) st' in
      let span' = curr_span st'' in
      (st'', { Node.kind = Node.PatCtor { name; args }; span = span' })
    | _ -> (st', { Node.kind = Node.PatCtor { name; args = [] }; span })

  and parse_pat_tuple st =
    let st', pats = btwn_parens (sep_by parse_pat Token.Comma) st in
    let span = curr_span st' in
    match pats with
    | [] ->
      let st'' = add_error_code st' Error.E1103 span [] in
      (st'', { Node.kind = Node.PatError; span })
    | [ single ] -> (st', single)
    | first :: rest -> (st', { Node.kind = Node.PatTuple (first, rest); span })

  and parse_typ st =
    match peek_opt st with
    | Some (Token.Ident _, _) -> parse_typ_ident st
    | Some (Token.LBrack, _) -> parse_typ_array st
    | Some (Token.LParen, _) -> parse_typ_tuple st
    | Some (Token.LBrace, _) -> parse_typ_record st
    | _ ->
      let span = curr_span st in
      let error_typ =
        { Node.kind = Node.TypIdent (Interner.empty_name st.interner); span }
      in
      (st, error_typ)

  and parse_typ_ptr st =
    let st' = expect st Token.Caret "^" in
    let st'', typ = parse_typ st' in
    let span = curr_span st'' in
    (st'', { Node.kind = Node.TypPtr typ; span })

  and parse_typ_array st =
    let st' = expect st Token.LBrack "[" in
    let st'', size =
      match peek_opt st' with
      | Some (Token.RBrack, _) -> (st', None)
      | _ ->
        let s, e = parse_expr st' in
        (s, Some e)
    in
    let st''' = expect st'' Token.RBrack "]" in
    let st'''', elem = parse_typ st''' in
    let span = curr_span st'''' in
    (st'''', { Node.kind = Node.TypArr { size; elem }; span })

  and parse_typ_ident st =
    let st', name, span = expect_ident st in
    (st', { Node.kind = Node.TypIdent name; span })

  and parse_typ_app st =
    let st', base, span = expect_ident st in
    match peek_opt st' with
    | Some (Token.Lt, _) ->
      let st'', args =
        btwn
          (token Token.Lt)
          (token Token.Gt)
          (sep_by parse_typ Token.Comma)
          st'
      in
      let span' = curr_span st'' in
      (st'', { Node.kind = Node.TypApp { base; args }; span = span' })
    | _ -> (st', { Node.kind = Node.TypIdent base; span })

  and parse_typ_tuple st =
    let st', typs = btwn_parens (sep_by parse_typ Token.Comma) st in
    let span = curr_span st' in
    match typs with
    | [] ->
      let st'' = add_error_code st' Error.E1104 span [] in
      (st'', { Node.kind = Node.TypError; span })
    | [ single ] -> (st', single)
    | first :: rest -> (st', { Node.kind = Node.TypTuple (first, rest); span })

  and parse_typ_fn st =
    let st' = expect st Token.KwFn "fn" in
    let st'', params = btwn_parens (sep_by parse_typ Token.Comma) st' in
    let st''', ret =
      match peek_opt st'' with
      | Some (Token.MinusGt, _) ->
        let s = advance st'' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st'', None)
    in
    let span = curr_span st''' in
    (st''', { Node.kind = Node.TypFn { params; ret }; span })

  and parse_typ_record st =
    let st', fields =
      btwn_braces (sep_by parse_typ_record_field Token.Comma) st
    in
    let span = curr_span st' in
    (st', { Node.kind = Node.TypRecord fields; span })

  and parse_typ_record_field st =
    let st', name, _ = expect_ident st in
    let st'' = expect st' Token.Colon ":" in
    let st''', typ = parse_typ st'' in
    (st''', { Node.name; typ })

  and parse_typ_optional st =
    let st', base = parse_typ st in
    match peek_opt st' with
    | Some (Token.Question, _) ->
      let st'' = advance st' in
      let span = curr_span st'' in
      (st'', { Node.kind = Node.TypOptional base; span })
    | _ -> (st', base)

  and parse_stmt st =
    match peek_opt st with
    | Some (Token.Ident _, _) -> parse_stmt_bind st
    | Some (Token.KwImport, _) -> parse_stmt_import st
    | Some (Token.KwExport, _) -> parse_stmt_export st
    | Some (Token.KwExtern, _) -> parse_stmt_extern st
    | _ -> parse_stmt_expr st

  and parse_stmt_import st =
    let st' = expect st Token.KwImport "import" in
    let st'', clause = parse_import_clause st' in
    let st''' = expect st'' Token.KwFrom "from" in
    let st'''', source =
      match peek_opt st''' with
      | Some (Token.LitString s, _) ->
        let s' = advance st''' in
        (s', s)
      | _ ->
        let span = curr_span st''' in
        let s' = add_error_code st''' Error.E1003 span [ "string"; "" ] in
        (s', Interner.empty_name st'''.interner)
    in
    let st''''' = expect st'''' Token.Semi ";" in
    let span = curr_span st''''' in
    ( st'''''
    , { Node.attr = None; kind = Node.StmtImport { clause; source }; span } )

  and parse_stmt_export st =
    let st' = expect st Token.KwExport "export" in
    let st'', clause = parse_export_clause st' in
    let st''', source =
      match peek_opt st'' with
      | Some (Token.KwFrom, _) -> (
        let s = advance st'' in
        match peek_opt s with
        | Some (Token.LitString str, _) ->
          let s' = advance s in
          (s', Some str)
        | _ ->
          let span = curr_span s in
          let s' = add_error_code s Error.E1003 span [ "string"; "" ] in
          (s', None))
      | _ -> (st'', None)
    in
    let st'''' = expect st''' Token.Semi ";" in
    let span = curr_span st'''' in
    ( st''''
    , { Node.attr = None; kind = Node.StmtExport { clause; source }; span } )

  and parse_import_clause st =
    match peek_opt st with
    | Some (Token.Star, _) ->
      let st' = advance st in
      let st'' = expect st' Token.KwAs "as" in
      let st''', name, _ = expect_ident st'' in
      (st''', Node.ImportAll name)
    | Some (Token.LBrace, _) ->
      let st', names = btwn_braces (sep_by expect_ident_name Token.Comma) st in
      (st', Node.ImportNamed names)
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1003 span [ "import clause"; "" ] in
      (st', Node.ImportNamed [])

  and parse_export_clause st =
    match peek_opt st with
    | Some (Token.Star, _) ->
      let st' = advance st in
      let st'' = expect st' Token.KwAs "as" in
      let st''', name, _ = expect_ident st'' in
      (st''', Node.ExportAll name)
    | Some (Token.LBrace, _) ->
      let st', names = btwn_braces (sep_by expect_ident_name Token.Comma) st in
      (st', Node.ExportNamed names)
    | _ ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1003 span [ "export clause"; "" ] in
      (st', Node.ExportNamed [])

  and expect_ident_name st =
    let st', name, _ = expect_ident st in
    (st', name)

  and parse_stmt_bind st =
    let st', mutable_, _ =
      match peek_opt st with
      | Some (Token.KwVal, span) ->
        let s = advance st in
        (s, false, span)
      | Some (Token.KwVar, span) ->
        let s = advance st in
        (s, true, span)
      | _ ->
        let span = curr_span st in
        let s = add_error_code st Error.E1003 span [ "val or var"; "" ] in
        (s, false, span)
    in
    let st'', name, _ = expect_ident st' in
    let st''', typ =
      match peek_opt st'' with
      | Some (Token.Colon, _) ->
        let s = advance st'' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st'', None)
    in
    let st'''' = expect st''' Token.ColonEq ":=" in
    let st''''', value = parse_expr st'''' in
    let st'''''' = expect st''''' Token.Semi ";" in
    let span' = curr_span st'''''' in
    ( st''''''
    , {
        Node.attr = None
      ; kind = Node.StmtBind { mutable_; name; typ; value }
      ; span = span'
      } )

  and parse_stmt_extern st =
    let st' = expect st Token.KwExtern "extern" in
    let st'', abi =
      match peek_opt st' with
      | Some (Token.LitString s, _) ->
        let s' = advance st' in
        let abi_str = Interner.lookup st'.interner s in
        (s', Some abi_str)
      | _ -> (st', None)
    in
    let st''' = expect st'' Token.KwUnsafe "unsafe" in
    let st'''', decls = btwn_braces (many parse_fn_sig) st''' in
    let span = curr_span st'''' in
    (st'''', { Node.attr = None; kind = Node.StmtExtern { abi; decls }; span })

  and parse_fn_sig st =
    let st' = expect st Token.KwFn "fn" in
    let st'', name, _ = expect_ident st' in
    let st''', _ = parse_fn_params st'' in
    let st'''', typ =
      match peek_opt st''' with
      | Some (Token.MinusGt, _) ->
        let s = advance st''' in
        let s', t = parse_typ s in
        (s', Some t)
      | _ -> (st''', None)
    in
    (st'''', { Node.name; typ })

  and parse_fn_params st = btwn_parens (sep_by parse_fn_param Token.Comma) st

  and parse_fn_param st =
    let st', name, _ = expect_ident st in
    match peek_opt st' with
    | Some (Token.Colon, _) ->
      let st'' = advance st' in
      let st''', typ = parse_typ st'' in
      (st''', { Node.name; typ = Some typ })
    | _ -> (st', { Node.name; typ = None })

  and parse_stmt_expr st =
    let st', expr = parse_expr st in
    let span = curr_span st in
    (st', { Node.attr = None; kind = Node.StmtExpr expr; span })

  and parse_prog st =
    let rec loop st acc =
      match peek_opt st with
      | Some (Token.EOF, _) -> (st, List.rev acc)
      | Some (Token.Newline, _) ->
        let st' = advance st in
        loop st' acc
      | _ ->
        let st', stmt = parse_stmt st in
        loop st' (stmt :: acc)
    in
    loop st []

  let parse source file_id interner tokens =
    let st = mk_state source file_id tokens interner in
    let prog_st, prog = parse_prog st in
    (prog, prog_st.diags)
end

include Make ()
