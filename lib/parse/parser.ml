open Basic
open Node

module type S = sig
  type prog = Node.prog

  type state = {
      source : string
    ; file_id : Span.file_id
    ; tokens : (Lex.Token.t * Span.t) list
    ; pos : int
    ; interner : Interner.t
    ; diags : Diagnostic.bag
  }

  val mk_state :
    string -> Span.file_id -> (Lex.Token.t * Span.t) list -> Interner.t -> state

  val peek_token_opt : state -> (Lex.Token.t * Span.t) option
  val advance : state -> state
  val curr_span : state -> Span.t
  val add_error : state -> string -> Span.t -> state
  val add_error_code : state -> Error.code -> Span.t -> string list -> state

  val parse :
       string
    -> Span.file_id
    -> Interner.t
    -> (Lex.Token.t * Span.t) list
    -> prog * Diagnostic.bag

  val parse_program : state -> state * prog
end

module Make () : S = struct
  type prog = Node.prog

  type state = {
      source : string
    ; file_id : Span.file_id
    ; tokens : (Lex.Token.t * Span.t) list
    ; pos : int
    ; interner : Interner.t
    ; diags : Diagnostic.bag
  }

  let mk_state source file_id tokens interner =
    { source; file_id; tokens; pos = 0; interner; diags = Diagnostic.empty_bag }

  let peek_token_opt st =
    if st.pos >= List.length st.tokens then None
    else Some (List.nth st.tokens st.pos)

  let advance st = { st with pos = st.pos + 1 }

  let curr_span st =
    if List.length st.tokens = 0 then Span.dummy
    else snd (List.nth st.tokens (min st.pos (List.length st.tokens - 1)))

  let add_error st msg span =
    { st with diags = Diagnostic.add st.diags (Diagnostic.error msg span) }

  let add_error_code st code span args =
    { st with diags = Diagnostic.add st.diags (Error.diag code span args) }

  let expect_token st token_type expected_name =
    match peek_token_opt st with
    | Some (token, _) when token = token_type -> advance st
    | Some (token, span) ->
      let found = Lex.Token.to_string token in
      add_error_code st Error.E1003 span [ expected_name; found ]
    | None ->
      let span = curr_span st in
      add_error_code st Error.E1003 span [ expected_name; "EOF" ]

  let expect_ident st =
    match peek_token_opt st with
    | Some (Lex.Token.Ident name, span) ->
      let st' = advance st in
      (st', name, span)
    | Some (token, span) ->
      let found = Lex.Token.to_string token in
      let st' = add_error_code st Error.E1105 span [ found ] in
      (st', Interner.empty_name st.interner, span)
    | None ->
      let span = curr_span st in
      let st' = add_error_code st Error.E1105 span [ "EOF" ] in
      (st', Interner.empty_name st.interner, span)

  let parse_list_sep st sep parser =
    let rec loop st acc =
      match peek_token_opt st with
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

  let parse_zero_or_more st parser =
    let rec loop st acc =
      try
        let st', item = parser st in
        loop st' (item :: acc)
      with Failure _ -> (st, List.rev acc)
    in
    loop st []

  (* Parser combinators with proper terminology *)
  let satisfy pred st =
    match peek_token_opt st with
    | Some (token, span) when pred token -> advance st
    | _ -> failwith "satisfy failed"

  let token t = satisfy (fun x -> x = t)

  let choice parsers st =
    let rec try_parser = function
      | [] -> failwith "choice failed"
      | p :: ps -> ( try p st with Failure _ -> try_parser ps)
    in
    try_parser parsers st

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
      match peek_token_opt st with
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

  let optional parser st =
    try
      let st', result = parser st in
      (st', Some result)
    with Failure _ -> (st, None)

  let btwn left right parser st =
    let st' = left st in
    let st'', result = parser st' in
    let st''' = right st'' in
    (st''', result)

  let btwn_parens parser =
    btwn (token Lex.Token.LParen) (token Lex.Token.RParen) parser

  let btwn_braces parser =
    btwn (token Lex.Token.LBrace) (token Lex.Token.RBrace) parser

  let rec parse_expr st : state * Node.expr =
    let rec parse_bp (bp : int) st : state * Node.expr =
      (* parse at binding precedence *)
      let st', left = nud st in
      parse_led bp left st
    and nud st : state * Node.expr =
      (* null denotation - prefix parsing *)
      match peek_token_opt st with
      | Some (Lex.Token.Ident name, span) ->
        let st' = advance st in
        (st', { Node.kind = Node.ExprIdent name; span })
      | Some (Lex.Token.LitNumber s, span) ->
        let st' = advance st in
        (st', { Node.kind = Node.ExprLit (Node.LitNumber s); span })
      | Some (Lex.Token.LitString name, span) ->
        let st' = advance st in
        (st', { Node.kind = Node.ExprLit (Node.LitString name); span })
      | Some (Lex.Token.LitRune c, span) ->
        let st' = advance st in
        (st', { Node.kind = Node.ExprLit (Node.LitRune c); span })
      | Some (Lex.Token.LitTemplate name, span) ->
        let st' = advance st in
        (st', { Node.kind = Node.ExprTemplate name; span })
      | Some (Lex.Token.LParen, span) ->
        let st', expr = btwn_parens parse_expr st in
        (st', expr)
      | Some (token, span) when Prec.is_prefix_op token ->
        let st' = advance st in
        let st'', operand = parse_bp (Prec.prec_value Prec.Unary) st' in
        let span' = Span.merge span operand.Node.span in
        ( st''
        , { Node.kind = Node.ExprUnary { op = token; operand }; span = span' }
        )
      | _ -> failwith "parse_expr not impl yet"
    and parse_led bp left st =
      (* left denotation - infix parsing *)
      match peek_token_opt st with
      | Some (op, _) when Prec.is_infix_op op -> (
        match Prec.token_prec op with
        | Some (prec, assoc) when Prec.prec_value prec > bp ->
          let st' = advance st in
          let right_bp =
            if assoc = Prec.Right then Prec.prec_value prec
            else Prec.prec_value prec + 1
          in
          let st'', right = parse_bp right_bp st' in
          let span = Span.merge left.Node.span right.Node.span in
          parse_led
            bp
            { Node.kind = Node.ExprBinary { op; left; right }; span }
            st'
        | _ -> (st, left))
      | _ -> (st, left)
    in
    parse_bp 0 st

  and parse_expr_block st =
    (* parse_expr_block not impl yet *)
    failwith "parse_expr_block not impl yet"

  and parse_stmt st =
    (* parse_stmt not impl yet *)
    failwith "parse_stmt not impl yet"

  and parse_stmt_import st =
    (* parse_stmt_import not impl yet *)
    failwith "parse_stmt_import not impl yet"

  and parse_stmt_export st =
    (* parse_stmt_export not impl yet *)
    failwith "parse_stmt_export not impl yet"

  and parse_stmt_bind st =
    (* parse_stmt_bind not impl yet *)
    failwith "parse_stmt_bind not impl yet"

  and parse_stmt_extern st =
    (* parse_stmt_extern not impl yet *)
    failwith "parse_stmt_extern not impl yet"

  and parse_pat st =
    (* parse_pat not impl yet *)
    failwith "parse_pat not impl yet"

  and parse_pat_lit st =
    (* parse_pat_lit not impl yet *)
    failwith "parse_pat_lit not impl yet"

  and parse_pat_ident st =
    (* parse_pat_ident not impl yet *)
    failwith "parse_pat_ident not impl yet"

  and parse_pat_ctor st =
    (* parse_pat_ctor not impl yet *)
    failwith "parse_pat_ctor not impl yet"

  and parse_typ st =
    (* parse_typ not impl yet *)
    failwith "parse_typ not impl yet"

  and parse_typ_ident st =
    (* parse_typ_ident not impl yet *)
    failwith "parse_typ_ident not impl yet"

  and parse_typ_ptr st =
    (* parse_typ_ptr not impl yet *)
    failwith "parse_typ_ptr not impl yet"

  and parse_typ_arr st =
    (* parse_typ_arr not impl yet *)
    failwith "parse_typ_arr not impl yet"

  and parse_typ_app st =
    (* parse_typ_app not impl yet *)
    failwith "parse_typ_app not impl yet"

  and parse_typ_tuple st =
    (* parse_typ_tuple not impl yet *)
    failwith "parse_typ_tuple not impl yet"

  and parse_typ_fn st =
    (* parse_typ_fn not impl yet *)
    failwith "parse_typ_fn not impl yet"

  and parse_program st =
    (* parse_program not impl yet *)
    failwith "parse_program not impl yet"

  let parse source file_id interner tokens =
    let st = mk_state source file_id tokens interner in
    let final_st, prog = parse_program st in
    (prog, final_st.diags)
end

include Make ()
