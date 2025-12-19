open Basic
open Lex
open Ast
open Nodes

type t = {
    tokens : (Token.t * Span.t) array
  ; mutable token_idx : int
  ; mutable diag : Reporter.bag
  ; source : Source.t
  ; file_id : int
  ; interner : Interner.t
}

type 'a result = ('a, Reporter.bag) Stdlib.result

let create tokens source file_id interner =
  {
    tokens = Array.of_seq (Seq.memoize tokens)
  ; token_idx = 0
  ; diag = Reporter.empty_bag
  ; source
  ; file_id
  ; interner
  }

let is_at_end p = p.token_idx >= Array.length p.tokens

let peek_at p offset =
  let idx = p.token_idx + offset in
  if idx >= Array.length p.tokens then (Token.EOF, Span.dummy)
  else p.tokens.(idx)

let peek p = peek_at p 0

let prev p =
  if p.token_idx = 0 then (Token.EOF, Span.dummy)
  else p.tokens.(p.token_idx - 1)

let advance p =
  if not (is_at_end p) then p.token_idx <- p.token_idx + 1;
  prev p

let check p kind =
  if is_at_end p then false
  else
    let tok, _ = peek p in
    tok = kind

let match_token p kinds =
  if List.exists (check p) kinds then (
    ignore (advance p);
    true)
  else false

let error p msg span =
  let err = Reporter.error msg span in
  p.diag <- Reporter.add p.diag err

let consume p kind message =
  if check p kind then advance p
  else
    let tok, span = peek p in
    error
      p
      (Printf.sprintf "%s, got '%s'" message (Token.show p.interner tok))
      span;
    (kind, span)

let resolve p id =
  match Interner.lookup_opt p.interner id with Some s -> s | None -> "?"

let sync p =
  ignore (advance p);
  while not (is_at_end p) do
    if fst (prev p) = Token.Semicolon then ()
    else
      match fst (peek p) with
      | Token.KwFn | Token.KwVal | Token.KwVar | Token.KwFor | Token.KwIf
      | Token.KwWhile | Token.KwReturn | Token.KwRecord | Token.KwSum ->
        ()
      | _ -> ignore (advance p)
  done

type prec =
  | PrecNone
  | PrecAssign
  | PrecPipe
  | PrecCoal
  | PrecOr
  | PrecAnd
  | PrecBitOr
  | PrecBitXor
  | PrecBitAnd
  | PrecEquality
  | PrecComparison
  | PrecRange
  | PrecCons
  | PrecTerm
  | PecFactor
  | PrecExponent
  | PrecUnary
  | PrecPostfix

let token_prec = function
  | Token.LtMinus -> PrecAssign
  | Token.BarGt -> PrecPipe
  | Token.QuestionQuestion -> PrecCoal
  | Token.KwOr -> PrecOr
  | Token.KwAnd -> PrecAnd
  | Token.Bar -> PrecBitOr
  | Token.Caret -> PrecBitXor
  | Token.Amp -> PrecBitAnd
  | Token.Eq | Token.SlashEq -> PrecEquality
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.KwIs | Token.KwAs ->
    PrecComparison
  | Token.DotDot | Token.DotDotLt -> PrecRange
  | Token.ColonColon -> PrecCons
  | Token.Plus | Token.Minus -> PrecTerm
  | Token.Star | Token.Slash | Token.Percent | Token.KwMod | Token.LtLt
  | Token.GtGt ->
    PecFactor
  | Token.StarStar -> PrecExponent
  | Token.LParen | Token.LBrack | Token.Dot | Token.DotCaret | Token.Question ->
    PrecPostfix
  | _ -> PrecNone

let parse_comma_list p parse_fn delimiter =
  let elems = ref [] in
  (if not (check p delimiter) then
     let rec loop () =
       elems := parse_fn p :: !elems;
       if match_token p [ Token.Comma ] then loop ()
     in
     loop ());
  List.rev !elems

let rec parse_ty p =
  let ps_token, ps_span = peek p in
  match ps_token with
  | Token.Ident _ -> parse_ty_app_or_ident p
  | Token.Question ->
    let _, start = advance p in
    let inner = parse_ty p in
    { kind = TyOptional inner; span = Span.merge start inner.span }
  | Token.Caret ->
    let _, start = advance p in
    let inner = parse_ty p in
    { kind = TyPtr inner; span = Span.merge start inner.span }
  | Token.LBrack -> parse_ty_array p ps_span
  | Token.LParen -> parse_ty_tuple_or_group p ps_span
  | _ ->
    error p "expected type" ps_span;
    { kind = TyError; span = ps_span }

and parse_ty_app_or_ident p =
  let id_token, start = advance p in
  let name = match id_token with Token.Ident id -> resolve p id | _ -> "?" in
  if match_token p [ Token.Lt ] then
    let args = parse_comma_list p parse_ty Token.Gt in
    let _, end_span = consume p Token.Gt "expected '>' after type arguments" in
    { kind = TyApp (name, args); span = Span.merge start end_span }
  else parse_ty_infix p { kind = TyIdent name; span = start }

and parse_ty_infix p left =
  if match_token p [ Token.MinusGt ] then
    let right = parse_ty p in
    { kind = TyFn (left, right); span = Span.merge left.span right.span }
  else left

and parse_ty_array p start =
  ignore (advance p);
  let size =
    match fst (peek p) with
    | Token.LitInt id ->
      ignore (advance p);
      Some (int_of_string (resolve p id))
    | _ -> None
  in
  ignore (consume p Token.RBrack "expected ']' after array size");
  let inner = parse_ty p in
  { kind = TyArray (size, inner); span = Span.merge start inner.span }

and parse_ty_tuple_or_group p start =
  ignore (advance p);
  let first = parse_ty p in
  if match_token p [ Token.Comma ] then
    let rest = parse_comma_list p parse_ty Token.RParen in
    let _, end_span = consume p Token.RParen "expected ')' after tuple types" in
    { kind = TyTuple (first :: rest); span = Span.merge start end_span }
  else (
    ignore (consume p Token.RParen "expected ')' after type grouping");
    first)

let rec parse_pat p =
  let left = parse_pat_atom p in
  if match_token p [ Token.ColonColon ] then
    let right = parse_pat p in
    { kind = PatCons (left, right); span = Span.merge left.span right.span }
  else left

and parse_pat_atom p =
  let tok, span = peek p in
  match tok with
  | Token.Underscore ->
    ignore (advance p);
    { kind = PatWild; span }
  | Token.LitInt id ->
    ignore (advance p);
    { kind = PatLit (LitInt (resolve p id)); span }
  | Token.LitString id ->
    ignore (advance p);
    { kind = PatLit (LitString (resolve p id)); span }
  | Token.KwTrue ->
    ignore (advance p);
    { kind = PatLit (LitBool true); span }
  | Token.KwFalse ->
    ignore (advance p);
    { kind = PatLit (LitBool false); span }
  | Token.Ident id ->
    ignore (advance p);
    let name = resolve p id in
    parse_pat_ident_rest p name span
  | Token.LParen -> parse_pat_tuple p span
  | Token.LBrack -> parse_pat_array p span
  | _ ->
    error p "expected pattern" span;
    ignore (advance p);
    { kind = PatError; span }

and parse_pat_ident_rest p name start =
  if match_token p [ Token.Dot ] then (
    ignore (consume p Token.LBrace "expected '{' for record pattern");
    let fields = parse_comma_list p parse_pat_field Token.RBrace in
    let _, end_span = consume p Token.RBrace "expected '}'" in
    { kind = PatLitRecord (name, fields); span = Span.merge start end_span })
  else if check p Token.Lt then (
    let _, _ = advance p in
    let args = parse_comma_list p parse_ty Token.Gt in
    ignore (consume p Token.Gt "expected '>'");
    let sub =
      if match_token p [ Token.LParen ] then (
        let s =
          if not (check p Token.RParen) then Some (parse_pat p) else None
        in
        ignore (consume p Token.RParen "expected ')'");
        s)
      else None
    in
    {
      kind = PatVariant (name, args, sub)
    ; span = Span.merge start (snd (prev p))
    })
  else if match_token p [ Token.LParen ] then
    let sub = if not (check p Token.RParen) then Some (parse_pat p) else None in
    let _, end_span = consume p Token.RParen "expected ')'" in
    { kind = PatVariant (name, [], sub); span = Span.merge start end_span }
  else { kind = PatIdent name; span = start }

and parse_pat_field p =
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name" s;
      "?"
  in
  { field_name = name }

and parse_pat_tuple p start =
  ignore (advance p);
  let elems = parse_comma_list p parse_pat Token.RParen in
  let _, end_span = consume p Token.RParen "expected ')' after pattern tuple" in
  { kind = PatLitTuple elems; span = Span.merge start end_span }

and parse_pat_array p start =
  ignore (advance p);
  let elems = parse_comma_list p parse_pat Token.RBrack in
  let _, end_span = consume p Token.RBrack "expected ']' after pattern array" in
  { kind = PatLitArray elems; span = Span.merge start end_span }

let parse_attr_arg p =
  let name_tok, _ = advance p in
  let name = match name_tok with Token.Ident id -> resolve p id | _ -> "?" in
  if match_token p [ Token.ColonEq ] then
    let val_tok, span = advance p in
    let lit =
      match val_tok with
      | Token.LitInt id -> LitInt (resolve p id)
      | Token.LitString id -> LitString (resolve p id)
      | Token.KwTrue -> LitBool true
      | Token.KwFalse -> LitBool false
      | _ ->
        error p "expected literal value" span;
        LitInt "0"
    in
    AttrArgNamed (name, name, lit)
  else AttrArgPos (name, LitBool true)

let parse_attr p =
  let name_tok, _ = advance p in
  let name = match name_tok with Token.Ident id -> resolve p id | _ -> "?" in
  let args =
    if match_token p [ Token.LParen ] then (
      let a = parse_comma_list p parse_attr_arg Token.RParen in
      ignore (consume p Token.RParen "expected ')'");
      a)
    else []
  in
  { attr_name = name; attr_args = args }

let parse_attrs p =
  let attrs = ref [] in
  if match_token p [ Token.LBrackLt ] then (
    attrs := parse_comma_list p parse_attr Token.GtRBrack;
    ignore (consume p Token.GtRBrack "expected '>]'"));
  !attrs

let parse_modifiers p =
  let is_export = ref false in
  let is_extern = ref None in
  let is_unsafe = ref false in
  let rec loop () =
    let tok, _ = peek p in
    match tok with
    | Token.Ident id when resolve p id = "export" ->
      ignore (advance p);
      is_export := true;
      loop ()
    | Token.KwExtern ->
      let offset = ref 1 in
      let next_is_str =
        match fst (peek_at p !offset) with
        | Token.LitString _ ->
          offset := !offset + 1;
          true
        | _ -> false
      in
      if fst (peek_at p !offset) = Token.LBrace then ()
      else (
        ignore (advance p);
        let abi =
          if next_is_str then
            match advance p with
            | Token.LitString id, _ -> Some (resolve p id)
            | _ -> None
          else None
        in
        let unsafe = match_token p [ Token.KwUnsafe ] in
        is_extern := Some (abi, unsafe);
        loop ())
    | Token.KwUnsafe ->
      if fst (peek_at p 1) = Token.LBrace then ()
      else (
        ignore (advance p);
        is_unsafe := true;
        loop ())
    | _ -> ()
  in
  loop ();
  { is_export = !is_export; is_extern = !is_extern; is_unsafe = !is_unsafe }

let rec parse_expr p prec =
  let left = parse_prefix p in
  let rec loop left =
    let p_tok, _ = peek p in
    let p_prec = token_prec p_tok in
    if p_prec > prec then
      let left' = parse_infix p left p_tok in
      loop left'
    else left
  in
  loop left

and parse_prefix p =
  let attrs = parse_attrs p in
  let mods = parse_modifiers p in
  let tok, span = peek p in
  match tok with
  | Token.LitInt id ->
    ignore (advance p);
    { kind = ExprLit (LitInt (resolve p id)); span }
  | Token.LitReal id ->
    ignore (advance p);
    { kind = ExprLit (LitFloat (resolve p id)); span }
  | Token.LitString id ->
    ignore (advance p);
    { kind = ExprLit (LitString (resolve p id)); span }
  | Token.LitRune c ->
    ignore (advance p);
    { kind = ExprLit (LitRune c); span }
  | Token.KwTrue ->
    ignore (advance p);
    { kind = ExprLit (LitBool true); span }
  | Token.KwFalse ->
    ignore (advance p);
    { kind = ExprLit (LitBool false); span }
  | Token.Ident id ->
    ignore (advance p);
    { kind = ExprIdent (resolve p id); span }
  | Token.LParen ->
    ignore (advance p);
    parse_expr_group_or_tuple p span
  | Token.LBrack ->
    ignore (advance p);
    parse_expr_lit_array p span
  | Token.LBrace ->
    ignore (advance p);
    parse_expr_block p span
  | Token.KwIf ->
    ignore (advance p);
    parse_expr_if p span
  | Token.KwWhile ->
    ignore (advance p);
    parse_expr_while p span
  | Token.KwFor ->
    ignore (advance p);
    parse_expr_for p span
  | Token.KwMatch ->
    ignore (advance p);
    parse_expr_match p span
  | Token.KwReturn ->
    ignore (advance p);
    parse_expr_return p span
  | Token.KwBreak ->
    ignore (advance p);
    parse_expr_break p span
  | Token.KwDefer ->
    ignore (advance p);
    let body = parse_expr p PrecNone in
    { kind = ExprDefer body; span = Span.merge span body.span }
  | Token.KwUnsafe ->
    ignore (advance p);
    let body = parse_expr p PrecNone in
    { kind = ExprUnsafe body; span = Span.merge span body.span }
  | Token.KwImport ->
    ignore (advance p);
    parse_expr_import p span
  | Token.KwFn ->
    ignore (advance p);
    parse_expr_fn p span attrs mods
  | Token.KwVal | Token.KwVar ->
    ignore (advance p);
    parse_expr_bind p span tok mods
  | Token.KwRecord ->
    ignore (advance p);
    parse_expr_record p span attrs mods
  | Token.KwSum ->
    ignore (advance p);
    parse_expr_sum p span attrs mods
  | Token.Minus | Token.KwNot | Token.Tilde | Token.At ->
    ignore (advance p);
    let operand = parse_expr p PrecUnary in
    {
      kind = ExprUnaryPrefix (tok, operand)
    ; span = Span.merge span operand.span
    }
  | Token.KwExtern ->
    if attrs <> [] then error p "attributes not allowed on 'extern' block" span;
    ignore (advance p);
    parse_expr_extern p span
  | _ ->
    error p "expected expression" span;
    ignore (advance p);
    { kind = ExprError; span }

and parse_infix p left op =
  let _, op_span = advance p in
  match op with
  | Token.LParen -> parse_expr_call p left
  | Token.LBrack -> parse_expr_index p left
  | Token.Dot -> parse_dot p left op_span
  | Token.DotCaret ->
    {
      kind = ExprUnaryPostfix (left, Token.DotCaret)
    ; span = Span.merge left.span op_span
    }
  | Token.Question ->
    {
      kind = ExprUnaryPostfix (left, Token.Question)
    ; span = Span.merge left.span op_span
    }
  | _ ->
    let right = parse_expr p (token_prec op) in
    {
      kind = ExprBinary (left, op, right)
    ; span = Span.merge left.span right.span
    }

and parse_expr_group_or_tuple p start =
  if match_token p [ Token.RParen ] then
    { kind = ExprLitTuple []; span = Span.merge start (snd (prev p)) }
  else
    let first = parse_expr p PrecNone in
    if match_token p [ Token.Comma ] then
      let rest =
        parse_comma_list p (fun p -> parse_expr p PrecNone) Token.RParen
      in
      let _, end_span =
        consume p Token.RParen "expected ')' after tuple literal"
      in
      { kind = ExprLitTuple (first :: rest); span = Span.merge start end_span }
    else (
      ignore (consume p Token.RParen "expected ')' in grouping");
      first)

and parse_expr_lit_array p start =
  let elems =
    parse_comma_list p (fun p -> parse_expr p PrecNone) Token.RBrack
  in
  let _, end_span = consume p Token.RBrack "expected ']' after array literal" in
  { kind = ExprLitArray elems; span = Span.merge start end_span }

and parse_expr_block p start =
  let stmts = ref [] in
  let expr_opt = ref None in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    let e = parse_expr p PrecNone in
    if match_token p [ Token.Semicolon ] then
      stmts := { kind = StmtExpr e; span = e.span } :: !stmts
    else if check p Token.RBrace then expr_opt := Some e
    else (
      error p "expected ';' after statement" (snd (peek p));
      sync p)
  done;
  let _, end_span = consume p Token.RBrace "expected '}' after block" in
  {
    kind = ExprBlock (List.rev !stmts, !expr_opt)
  ; span = Span.merge start end_span
  }

and parse_expr_if p start =
  let cond = parse_expr p PrecNone in
  let then_ = parse_expr p PrecNone in
  let else_ =
    if match_token p [ Token.KwElse ] then parse_expr p PrecNone
    else { kind = ExprBlock ([], None); span = Span.dummy }
  in
  { kind = ExprIf (cond, then_, else_); span = Span.merge start else_.span }

and parse_expr_while p start =
  let cond = parse_expr p PrecNone in
  let body = parse_expr p PrecNone in
  { kind = ExprWhile (cond, body); span = Span.merge start body.span }

and parse_expr_for p start =
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected identifier" s;
      "?"
  in
  ignore (consume p Token.KwIn "expected 'in' after 'for' name");
  let iter = parse_expr p PrecNone in
  let body = parse_expr p PrecNone in
  { kind = ExprFor (name, iter, body); span = Span.merge start body.span }

and parse_expr_match p start =
  let target = parse_expr p PrecNone in
  ignore (consume p Token.LBrace "expected '{' after 'match' target");
  let cases = ref [] in
  while match_token p [ Token.KwCase ] do
    let pat = parse_pat p in
    ignore (consume p Token.EqGt "expected '=>' after case pattern");
    let expr = parse_expr p PrecNone in
    ignore (match_token p [ Token.Comma ]);
    cases := { case_pat = pat; case_expr = expr } :: !cases
  done;
  let _, end_span = consume p Token.RBrace "expected '}' after 'match' cases" in
  {
    kind = ExprMatch (target, List.rev !cases)
  ; span = Span.merge start end_span
  }

and parse_expr_return p start =
  let expr_opt =
    if (not (check p Token.Semicolon)) && not (check p Token.RBrace) then
      Some (parse_expr p PrecNone)
    else None
  in
  { kind = ExprReturn expr_opt; span = start }

and parse_expr_break p start =
  let expr_opt =
    if (not (check p Token.Semicolon)) && not (check p Token.RBrace) then
      Some (parse_expr p PrecNone)
    else None
  in
  { kind = ExprBreak expr_opt; span = start }

and parse_expr_import p start =
  let path =
    match advance p with
    | Token.LitString id, _ -> resolve p id
    | _, s ->
      error p "expected import path string" s;
      ""
  in
  { kind = ExprImport path; span = Span.merge start (snd (prev p)) }

and parse_expr_bind p start tok mods =
  let mut = tok = Token.KwVar in
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected binding name" s;
      "?"
  in
  let ty_opt =
    if match_token p [ Token.Colon ] then Some (parse_ty p) else None
  in
  ignore (consume p Token.ColonEq "expected ':=' after binding name");
  let init = parse_expr p PrecNone in
  {
    kind =
      ExprBind
        ( mods
        , mut
        , name
        , ty_opt
        , init
        , { kind = ExprLit (LitInt "0"); span = Span.dummy } )
  ; span = Span.merge start init.span
  }

and parse_expr_fn p start attrs mods =
  let name =
    match peek p with
    | Token.Ident id, _ ->
      ignore (advance p);
      Some (resolve p id)
    | _ -> None
  in
  let sig_ = parse_fn_sig p name in
  let body = parse_expr_block p (snd (peek p)) in
  { kind = ExprFn (attrs, mods, sig_, body); span = Span.merge start body.span }

and parse_fn_sig p name =
  ignore (consume p Token.LParen "expected '('");
  let params = parse_comma_list p parse_param Token.RParen in
  ignore (consume p Token.RParen "expected ')'");
  let ret_ty =
    if match_token p [ Token.Colon ] then Some (parse_ty p) else None
  in
  { fn_name = name; fn_ty_params = []; fn_params = params; fn_ret_ty = ret_ty }

and parse_param p =
  let p_name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected parameter name" s;
      "?"
  in
  ignore (consume p Token.Colon "expected ':'");
  let p_ty = parse_ty p in
  {
    param_mutable = false
  ; param_name = p_name
  ; param_ty = Some p_ty
  ; param_default = None
  }

and parse_expr_record p start attrs mods =
  let name =
    match advance p with Token.Ident id, _ -> Some (resolve p id) | _ -> None
  in
  ignore (consume p Token.LBrace "expected '{' after 'record' name");
  let fields = parse_comma_list p parse_record_field Token.RBrace in
  let _, end_span = consume p Token.RBrace "expected '}'" in
  {
    kind = ExprRecord (attrs, mods, name, [], fields)
  ; span = Span.merge start end_span
  }

and parse_record_field p =
  let fname =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name" s;
      "?"
  in
  ignore (consume p Token.Colon "expected ':'");
  let fty = parse_ty p in
  let def =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p PrecNone)
    else None
  in
  {
    field_mutable = false
  ; field_name = fname
  ; field_ty = Some fty
  ; field_default = def
  }

and parse_expr_sum p start attrs mods =
  let name =
    match advance p with Token.Ident id, _ -> Some (resolve p id) | _ -> None
  in
  ignore (consume p Token.LBrace "expected '{' after 'sum' name");
  let cases = parse_comma_list p parse_sum_case Token.RBrace in
  let _, end_span = consume p Token.RBrace "expected '}'" in
  {
    kind = ExprSum (attrs, mods, name, [], cases)
  ; span = Span.merge start end_span
  }

and parse_sum_case p =
  let cname =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected case name" s;
      "?"
  in
  let types =
    if match_token p [ Token.LParen ] then (
      let t = parse_comma_list p parse_ty Token.RParen in
      ignore (consume p Token.RParen "expected ')'");
      t)
    else []
  in
  { case_name = cname; case_tys = types; case_params = [] }

and parse_expr_call p callee =
  let args =
    if not (check p Token.RParen) then
      parse_comma_list p (fun p -> parse_expr p PrecNone) Token.RParen
    else []
  in
  let _, end_span = consume p Token.RParen "expected ')'" in
  { kind = ExprCall (callee, args); span = Span.merge callee.span end_span }

and parse_expr_index p callee =
  let index = parse_expr p PrecNone in
  let _, end_span = consume p Token.RBrack "expected ']'" in
  { kind = ExprIndex (callee, index); span = Span.merge callee.span end_span }

and parse_dot p callee op_span =
  let field =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name" s;
      "?"
  in
  { kind = ExprField (callee, field); span = Span.merge callee.span op_span }

and parse_expr_extern p start =
  let next_is_str =
    match fst (peek p) with Token.LitString _ -> true | _ -> false
  in
  let abi =
    if next_is_str then
      match advance p with
      | Token.LitString id, _ -> Some (resolve p id)
      | _ -> None
    else None
  in
  let is_unsafe = match_token p [ Token.KwUnsafe ] in
  ignore (consume p Token.LBrace "expected '{' after extern header");
  let sigs = ref [] in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    ignore (consume p Token.KwFn "expected 'fn' in extern block");
    let name =
      match peek p with
      | Token.Ident id, _ ->
        ignore (advance p);
        Some (resolve p id)
      | _ -> None
    in
    sigs := parse_fn_sig p name :: !sigs;
    ignore (match_token p [ Token.Semicolon ])
  done;
  let _, end_span = consume p Token.RBrace "expected '}' after extern block" in
  {
    kind = ExprExtern (abi, is_unsafe, List.rev !sigs)
  ; span = Span.merge start end_span
  }

and parse_stmt p =
  let expr = parse_expr p PrecNone in
  ignore (consume p Token.Semicolon "expected ';' after statement");
  { kind = StmtExpr expr; span = expr.span }

let parse tokens source file_id interner =
  let p = create tokens source file_id interner in
  let stmts = ref [] in
  while not (is_at_end p) do
    stmts := parse_stmt p :: !stmts
  done;
  if Reporter.has_errors p.diag then Error p.diag else Ok (List.rev !stmts)
