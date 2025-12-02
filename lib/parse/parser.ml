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

  val peek_opt : state -> (Lex.Token.t * Span.t) option
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

  val parse_prog : state -> state * prog
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

  let peek_opt st =
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

  let expect st token_type expected_name =
    match peek_opt st with
    | Some (token, _) when token = token_type -> advance st
    | Some (token, span) ->
      let found = Lex.Token.to_string token in
      add_error_code st Error.E1003 span [ expected_name; found ]
    | None ->
      let span = curr_span st in
      add_error_code st Error.E1003 span [ expected_name; "EOF" ]

  let expect_ident st =
    match peek_opt st with
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

  let parse_opt_many st parser =
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

  let btwn_parens parser =
    btwn (token Lex.Token.LParen) (token Lex.Token.RParen) parser

  let btwn_braces parser =
    btwn (token Lex.Token.LBrace) (token Lex.Token.RBrace) parser

  let rec parse_stmt st =
    (* For now, create a simple error statement *)
    let span = curr_span st in
    let error_stmt =
      {
        Node.attr = None
      ; kind =
          Node.StmtExpr { Node.kind = Node.ExprLit (Node.LitNumber "0"); span }
      ; span
      }
    in
    (st, error_stmt)

  and parse_prog st =
    let rec loop st acc =
      match peek_opt st with
      | Some (Lex.Token.EOF, _) -> (st, List.rev acc)
      | Some (Lex.Token.Newline, _) ->
        let st' = advance st in
        loop st' acc
      | _ ->
        let st', stmt = parse_stmt st in
        loop st' (stmt :: acc)
    in
    loop st []

  let parse source file_id interner tokens =
    let st = mk_state source file_id tokens interner in
    let final_st, prog = parse_prog st in
    (prog, final_st.diags)
end

include Make ()
