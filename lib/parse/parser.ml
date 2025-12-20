open Basic
open Lex
open Ast
open Nodes

type t = {
    tokens : (Token.t * Span.t) array
  ; mutable token_idx : int
  ; mutable diag : Reporter.bag
  ; interner : Interner.t
}

let create tokens _ _ interner =
  {
    tokens = Array.of_seq (Seq.memoize tokens)
  ; token_idx = 0
  ; diag = Reporter.empty_bag
  ; interner
  }

let has_errors p = Reporter.has_errors p.diag
let diag p = p.diag
let is_at_end p = p.token_idx >= Array.length p.tokens

let peek p =
  if is_at_end p then (Token.EOF, Span.dummy) else p.tokens.(p.token_idx)

let peek_at p offset =
  let i = p.token_idx + offset in
  if i >= Array.length p.tokens then (Token.EOF, Span.dummy) else p.tokens.(i)

let advance p =
  if not (is_at_end p) then p.token_idx <- p.token_idx + 1;
  p.tokens.(p.token_idx - 1)

let prev p =
  if p.token_idx = 0 then (Token.EOF, Span.dummy)
  else p.tokens.(p.token_idx - 1)

let check p kind = if is_at_end p then false else fst (peek p) = kind

let match_token p kinds =
  if List.exists (check p) kinds then (
    ignore (advance p);
    true)
  else false

let error p msg span = p.diag <- Reporter.add p.diag (Reporter.error msg span)

let consume p kind message =
  if fst (peek p) = kind then advance p
  else
    let tok, span = peek p in
    error
      p
      (Printf.sprintf "%s, got '%s'" message (Token.show p.interner tok))
      span;
    (kind, span)

let expect p k m = ignore (consume p k m)

let resolve p id =
  match Interner.lookup_opt p.interner id with Some s -> s | None -> "?"

let expect_id p msg =
  match advance p with
  | Token.Ident id, _ -> resolve p id
  | _, s ->
    error p msg s;
    "?"

let parse_ident_opt p =
  match peek p with
  | Token.Ident id, _ ->
    ignore (advance p);
    Some (resolve p id)
  | _ -> None

let parse_list p f seps end_tok =
  let rec loop acc =
    let x = f p in
    if match_token p seps then loop (x :: acc) else x :: acc
  in
  if check p end_tok then [] else List.rev (loop [])

let sync p =
  let rec loop () =
    if is_at_end p then ()
    else if fst (prev p) = Token.Semicolon then ()
    else
      match fst (peek p) with
      | Token.KwFn | Token.KwVal | Token.KwVar | Token.KwFor | Token.KwIf
      | Token.KwWhile | Token.KwReturn | Token.KwRecord | Token.KwSum ->
        ()
      | _ ->
        ignore (advance p);
        loop ()
  in
  if not (is_at_end p) then (
    ignore (advance p);
    loop ())

let parse_lit_opt p =
  let tok, span = peek p in
  match tok with
  | Token.LitInt id | Token.LitReal id | Token.LitString id ->
    ignore (advance p);
    Some
      ( span
      , match tok with
        | Token.LitInt _ -> LitInt (resolve p id)
        | Token.LitReal _ -> LitFloat (resolve p id)
        | _ -> LitString (resolve p id) )
  | Token.LitRune c ->
    ignore (advance p);
    Some (span, LitRune c)
  | Token.KwTrue | Token.KwFalse ->
    ignore (advance p);
    Some (span, LitBool (tok = Token.KwTrue))
  | _ -> None

let rec parse_ty p =
  let l = parse_ty_primary p in
  if match_token p [ Token.MinusGt ] then
    let r = parse_ty p in
    make_ty (TyFn (l, r)) (Span.merge l.span r.span)
  else l

and parse_ty_primary p =
  match fst (peek p) with
  | Token.Ident _ -> parse_ty_app_or_ident p
  | (Token.Question | Token.Caret) as t ->
    let _, s = advance p in
    let i = parse_ty_primary p in
    make_ty
      (if t = Token.Question then TyOptional i else TyPtr i)
      (Span.merge s i.span)
  | Token.LBrack ->
    ignore (advance p);
    let sz =
      if check p Token.RBrack then None
      else
        match advance p with
        | Token.LitInt id, _ -> Some (int_of_string (resolve p id))
        | _, s ->
          error p "expected integer literal for array size" s;
          None
    in
    expect p Token.RBrack "expected ']' to close array type";
    let i = parse_ty_primary p in
    make_ty (TyArray (sz, i)) (Span.merge (snd (prev p)) i.span)
  | Token.LParen ->
    let _, s = advance p in
    let ts = parse_list p parse_ty [ Token.Comma ] Token.RParen in
    let _, es = consume p Token.RParen "expected ')' to close tuple type" in
    make_ty (TyTuple ts) (Span.merge s es)
  | _ ->
    let _, s = advance p in
    error p "expected type" s;
    make_ty TyError s

and parse_ty_app_or_ident p =
  match advance p with
  | Token.Ident id, s ->
    let n = resolve p id in
    if match_token p [ Token.Lt ] then
      let as_ = parse_list p parse_ty [ Token.Comma ] Token.Gt in
      let _, es =
        consume p Token.Gt "expected '>' to close type argument list"
      in
      make_ty (TyApp (n, as_)) (Span.merge s es)
    else make_ty (TyIdent n) s
  | _ ->
    let _, s = peek p in
    error p "expected identifier" s;
    make_ty TyError s

let parse_attrs p =
  if match_token p [ Token.LBrackLt ] then (
    let as_ =
      parse_list
        p
        (fun p ->
          { attr_name = expect_id p "expected attribute name"; attr_args = [] })
        [ Token.Comma ]
        Token.GtRBrack
    in
    expect p Token.GtRBrack "expected '>]' to close attribute list";
    as_)
  else []

let parse_mods p =
  let ex, ext, u = (ref false, ref (None, false), ref false) in
  let rec loop () =
    match fst (peek p) with
    | Token.Ident id when resolve p id = "export" ->
      ignore (advance p);
      ex := true;
      loop ()
    | Token.KwExtern ->
      let off = ref 1 in
      let has =
        match fst (peek_at p !off) with Token.LitString _ -> true | _ -> false
      in
      if has then off := !off + 1;
      if fst (peek_at p !off) = Token.KwUnsafe then off := !off + 1;
      if fst (peek_at p !off) = Token.LBrace then ()
      else (
        ignore (advance p);
        let abi =
          if has then
            match advance p with
            | Token.LitString id, _ -> Some (resolve p id)
            | _ -> None
          else None
        in
        ext := (abi, true);
        loop ())
    | Token.KwUnsafe ->
      if fst (peek_at p 1) = Token.LBrace then ()
      else (
        ignore (advance p);
        u := true;
        loop ())
    | _ -> ()
  in
  loop ();
  { is_export = !ex; is_extern = !ext; is_unsafe = !u }

let can_start_expr = function
  | Token.Ident _ | Token.LitInt _ | Token.LitReal _ | Token.LitString _
  | Token.LitRune _ | Token.KwTrue | Token.KwFalse | Token.LParen | Token.LBrack
  | Token.LBrace | Token.Minus | Token.KwNot | Token.Tilde | Token.At
  | Token.KwIf | Token.KwWhile | Token.KwFor | Token.KwMatch | Token.KwReturn
  | Token.KwBreak | Token.KwDefer | Token.KwUnsafe | Token.KwImport
  | Token.KwExtern | Token.KwFn | Token.KwVal | Token.KwVar | Token.KwRecord
  | Token.KwSum | Token.Dot ->
    true
  | _ -> false

let rec parse_expr p prec =
  let l = parse_prefix p in
  let rec loop l =
    let t, _ = peek p in
    let t_prec = Prec.of_token t in
    if
      t_prec > prec
      || (t_prec <> Prec.None && t_prec = prec && Prec.is_right_assoc t)
    then loop (parse_infix p l t)
    else l
  in
  loop l

and parse_prefix p =
  let attrs, mods = (parse_attrs p, parse_mods p) in
  let tok, span = peek p in
  match parse_lit_opt p with
  | Some (s, l) -> make_expr (ExprLit l) s
  | None -> (
    match tok with
    | Token.Minus | Token.KwNot | Token.Tilde | Token.At ->
      ignore (advance p);
      let r = parse_expr p Prec.Unary in
      make_expr (ExprUnaryPrefix (tok, r)) (Span.merge span r.span)
    | Token.Ident id ->
      ignore (advance p);
      let n = resolve p id in
      if check p Token.Dot && fst (peek_at p 1) = Token.LBrace then (
        ignore (advance p);
        parse_expr_lit_record p (Some n) span)
      else make_expr (ExprIdent n) span
    | Token.Dot ->
      if fst (peek_at p 1) = Token.LBrace then (
        ignore (advance p);
        parse_expr_lit_record p None span)
      else (
        error p "expected '{' after '.' to start record literal" span;
        ignore (advance p);
        make_expr ExprError span)
    | Token.LitTemplateNoSubst id ->
      ignore (advance p);
      make_expr (ExprLit (LitString (resolve p id))) span
    | Token.TemplateHead id ->
      ignore (advance p);
      let head = resolve p id in
      let rec loop acc =
        let e = parse_expr p Prec.None in
        match advance p with
        | Token.TemplateMiddle mid, _ -> loop ((resolve p mid, e) :: acc)
        | Token.TemplateTail tid, s ->
          make_expr
            (ExprTemplate (List.rev ((head, e) :: acc), resolve p tid))
            (Span.merge span s)
        | tok, s ->
          error
            p
            (Printf.sprintf
               "expected template middle or tail, found %s"
               (Token.show p.interner tok))
            s;
          make_expr ExprError span
      in
      loop []
    | Token.LParen ->
      ignore (advance p);
      parse_expr_group_or_tuple p span
    | Token.LBrack ->
      ignore (advance p);
      let es =
        parse_list
          p
          (fun p -> parse_expr p Prec.None)
          [ Token.Comma ]
          Token.RBrack
      in
      let _, es_s = consume p Token.RBrack "expected ']'" in
      make_expr (ExprLitArray es) (Span.merge span es_s)
    | Token.LBrace -> parse_expr_block p span
    | Token.KwIf ->
      ignore (advance p);
      let cs = parse_cond_list p in
      let t = parse_expr p Prec.None in
      let e =
        if match_token p [ Token.KwElse ] then Some (parse_expr p Prec.None)
        else None
      in
      make_expr (ExprIf (cs, t, e)) (Span.merge span (prev p |> snd))
    | Token.KwWhile ->
      ignore (advance p);
      let c = parse_cond p in
      let g =
        if match_token p [ Token.KwIf ] then Some (parse_expr p Prec.None)
        else None
      in
      let b = parse_expr p Prec.None in
      make_expr (ExprWhile (c, g, b)) (Span.merge span b.span)
    | Token.KwFor ->
      ignore (advance p);
      let is_case = match_token p [ Token.KwCase ] in
      let pat = parse_pat p in
      expect p Token.KwIn "expected 'in'";
      let it = parse_expr p Prec.None in
      let guard =
        if match_token p [ Token.KwIf ] then Some (parse_expr p Prec.None)
        else None
      in
      let b = parse_expr p Prec.None in
      make_expr (ExprFor (is_case, pat, it, guard, b)) (Span.merge span b.span)
    | Token.KwMatch ->
      ignore (advance p);
      parse_expr_match p span
    | Token.KwReturn | Token.KwBreak ->
      ignore (advance p);
      let e =
        if not (check p Token.Semicolon || check p Token.RBrace) then
          Some (parse_expr p Prec.None)
        else None
      in
      make_expr
        (if tok = Token.KwReturn then ExprReturn e else ExprBreak e)
        span
    | Token.KwDefer | Token.KwUnsafe ->
      ignore (advance p);
      let b = parse_expr p Prec.None in
      make_expr
        (if tok = Token.KwDefer then ExprDefer b else ExprUnsafe b)
        (Span.merge span b.span)
    | Token.KwImport ->
      ignore (advance p);
      let pth =
        match advance p with
        | Token.LitString id, _ -> resolve p id
        | _, s ->
          error p "expected string literal after 'import'" s;
          "?"
      in
      make_expr (ExprImport pth) span
    | Token.KwExtern ->
      ignore (advance p);
      parse_expr_extern p span
    | Token.KwFn ->
      ignore (advance p);
      parse_expr_fn p span attrs mods
    | Token.KwVal | Token.KwVar ->
      ignore (advance p);
      parse_expr_bind p span tok mods
    | Token.KwRecord | Token.KwSum ->
      let k = if tok = Token.KwRecord then "record" else "sum" in
      ignore (advance p);
      let n = parse_ident_opt p in
      let tp = parse_ty_params p in
      expect p Token.LBrace (Printf.sprintf "expected '{' after '%s' name" k);
      if k = "record" then
        let fs =
          parse_list p parse_record_field [ Token.Semicolon ] Token.RBrace
        in
        let _, es =
          consume p Token.RBrace "expected '}' to close 'record' definition"
        in
        make_expr (ExprRecord (attrs, mods, n, tp, fs)) (Span.merge span es)
      else
        let cs = parse_list p parse_sum_case [ Token.Comma ] Token.RBrace in
        let _, es =
          consume p Token.RBrace "expected '}' to close 'sum' type definition"
        in
        make_expr (ExprSum (attrs, mods, n, tp, cs)) (Span.merge span es)
    | _ ->
      error
        p
        (Printf.sprintf "unexpected token '%s'" (Token.show p.interner tok))
        span;
      ignore (advance p);
      make_expr ExprError span)

and parse_infix p l op =
  let _, s = advance p in
  match op with
  | Token.LParen ->
    let as_ =
      parse_list
        p
        (fun p -> parse_expr p Prec.None)
        [ Token.Comma ]
        Token.RParen
    in
    let _, es = consume p Token.RParen "expected ')' to close argument list" in
    make_expr (ExprCall (l, as_)) (Span.merge l.span es)
  | Token.LBrack ->
    let i = parse_expr p Prec.None in
    let _, es = consume p Token.RBrack "expected ']'" in
    make_expr (ExprIndex (l, i)) (Span.merge l.span es)
  | Token.Dot ->
    let f = expect_id p "expected field name after '.'" in
    make_expr (ExprField (l, f)) (Span.merge l.span s)
  | Token.DotCaret | Token.Question ->
    make_expr (ExprUnaryPostfix (l, op)) (Span.merge l.span s)
  | Token.LtMinus ->
    let r = parse_expr p Prec.Assign in
    make_expr (ExprAssign (l, r)) (Span.merge l.span r.span)
  | Token.DotDot | Token.DotDotLt ->
    let prec = Prec.of_token op in
    let r =
      if can_start_expr (fst (peek p)) then Some (parse_expr p prec) else None
    in
    let end_s = match r with Some e -> e.span | None -> s in
    make_expr (ExprRange (l, op, r)) (Span.merge l.span end_s)
  | Token.KwNot when check p Token.KwIn ->
    ignore (advance p);
    (* consume 'in' *)
    let r = parse_expr p (Prec.of_token Token.KwIn) in
    (* We use a special representation for 'not in' or just compose it in AST? *)
    (* Let's use ExprUnaryPrefix(KwNot, ExprBinary(l, KwIn, r)) *)
    let bin =
      make_expr (ExprBinary (l, Token.KwIn, r)) (Span.merge l.span r.span)
    in
    make_expr (ExprUnaryPrefix (Token.KwNot, bin)) (Span.merge l.span r.span)
  | _ ->
    let r = parse_expr p (Prec.of_token op) in
    make_expr (ExprBinary (l, op, r)) (Span.merge l.span r.span)

and parse_cond p =
  if match_token p [ Token.KwCase ] then (
    let pat = parse_pat p in
    expect p Token.ColonEq "expected ':=' in condition";
    let expr = parse_expr p Prec.None in
    CondPat (pat, expr))
  else CondExpr (parse_expr p Prec.None)

and parse_cond_list p =
  let c = parse_cond p in
  if match_token p [ Token.Comma ] then c :: parse_cond_list p else [ c ]

and parse_field_like p msg =
  let m = match_token p [ Token.KwVar ] in
  let n = expect_id p msg in
  let ty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  let d =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p Prec.None)
    else None
  in
  (m, n, ty, d)

and parse_record_field p =
  let m, n, ty, d = parse_field_like p "expected field name" in
  { field_mutable = m; field_name = n; field_ty = ty; field_default = d }

and parse_param p =
  let m, n, ty, d = parse_field_like p "expected parameter name" in
  { param_mutable = m; param_name = n; param_ty = ty; param_default = d }

and parse_expr_lit_record p n s =
  expect p Token.LBrace "expected '{' to start record literal";
  let seps = [ Token.Comma; Token.Semicolon ] in
  let fs = ref [] in
  let b = ref None in
  if not (check p Token.RBrace) then
    if match_token p [ Token.KwVar ] then
      fs := parse_list p parse_record_field seps Token.RBrace
    else
      let e = parse_expr p Prec.None in
      if match_token p [ Token.KwWith ] then (
        b := Some e;
        fs := parse_list p parse_record_field seps Token.RBrace)
      else
        match e.kind with
        | ExprIdent id ->
          let ty =
            if match_token p [ Token.Colon ] then Some (parse_ty p) else None
          in
          let def =
            if match_token p [ Token.ColonEq ] then
              Some (parse_expr p Prec.None)
            else None
          in
          let first =
            {
              field_mutable = false
            ; field_name = id
            ; field_ty = ty
            ; field_default = def
            }
          in
          let rest =
            if match_token p seps then
              parse_list p parse_record_field seps Token.RBrace
            else []
          in
          fs := first :: rest
        | _ -> error p "expected field name or 'with' base" e.span
  else ();
  let _, es = consume p Token.RBrace "expected '}' to close record literal" in
  make_expr (ExprLitRecord (n, !fs, !b)) (Span.merge s es)

and parse_expr_block p s =
  expect p Token.LBrace "expected '{' to start block";
  let st, e = (ref [], ref None) in
  while not (check p Token.RBrace || is_at_end p) do
    let ex = parse_expr p Prec.None in
    if match_token p [ Token.Semicolon ] then
      st := make_stmt (StmtExpr ex) ex.span :: !st
    else if check p Token.RBrace then e := Some ex
    else (
      error p "expected ';' after statement" (snd (peek p));
      sync p)
  done;
  let _, es = consume p Token.RBrace "expected '}' to close block" in
  make_expr (ExprBlock (List.rev !st, !e)) (Span.merge s es)

and parse_expr_group_or_tuple p s =
  if match_token p [ Token.RParen ] then
    make_expr (ExprLitTuple []) (Span.merge s (snd (prev p)))
  else
    let f = parse_expr p Prec.None in
    if match_token p [ Token.Comma ] then
      let rs =
        parse_list
          p
          (fun p -> parse_expr p Prec.None)
          [ Token.Comma ]
          Token.RParen
      in
      let _, es =
        consume p Token.RParen "expected ')' to close tuple literal"
      in
      make_expr (ExprLitTuple (f :: rs)) (Span.merge s es)
    else (
      expect p Token.RParen "expected ')' to close grouped expression";
      f)

and parse_expr_match p s =
  let t = parse_expr p Prec.None in
  expect p Token.LBrace "expected '{' to start 'match' expression";
  let cs = ref [] in
  while match_token p [ Token.KwCase ] do
    let pat = parse_pat p in
    let guard =
      if match_token p [ Token.KwIf ] then Some (parse_expr p Prec.None)
      else None
    in
    expect p Token.EqGt "expected '=>' after 'match' pattern";
    let e = parse_expr p Prec.None in
    ignore (match_token p [ Token.Comma; Token.Semicolon ]);
    cs := { case_pat = pat; case_guard = guard; case_expr = e } :: !cs
  done;
  let _, es =
    consume p Token.RBrace "expected '}' to close 'match' expression"
  in
  make_expr (ExprMatch (t, List.rev !cs)) (Span.merge s es)

and parse_pat p =
  let l = parse_pat_primary p in
  if match_token p [ Token.ColonColon ] then
    let r = parse_pat p in
    make_pat (PatCons (l, r)) (Span.merge l.span r.span)
  else l

and parse_pat_primary p =
  let tok, span = peek p in
  match parse_lit_opt p with
  | Some (s, l) -> make_pat (PatLit l) s
  | None -> (
    match tok with
    | Token.Ident id ->
      ignore (advance p);
      let n = resolve p id in
      if fst (peek p) = Token.Dot then (
        ignore (advance p);
        parse_pat_lit_record p n span)
      else if match_token p [ Token.Lt; Token.LParen ] then (
        p.token_idx <- p.token_idx - 1;
        parse_pat_variant p n span)
      else make_pat (PatIdent n) span
    | Token.Underscore ->
      ignore (advance p);
      make_pat PatWild span
    | Token.LParen ->
      ignore (advance p);
      let ps = parse_list p parse_pat [ Token.Comma ] Token.RParen in
      let _, es =
        consume p Token.RParen "expected ')' to match '(' in tuple pattern"
      in
      make_pat (PatLitTuple ps) (Span.merge span es)
    | Token.LBrack ->
      ignore (advance p);
      let ps = parse_list p parse_pat [ Token.Comma ] Token.RBrack in
      let _, es =
        consume p Token.RBrack "expected ']' to match '[' in array pattern"
      in
      make_pat (PatLitArray ps) (Span.merge span es)
    | _ ->
      error p "expected pattern" span;
      ignore (advance p);
      make_pat PatError span)

and parse_pat_lit_record p n s =
  expect p Token.LBrace "expected '{' to start record pattern";
  let fs =
    parse_list
      p
      (fun p -> { field_name = expect_id p "expected field name" })
      [ Token.Comma ]
      Token.RBrace
  in
  let _, es = consume p Token.RBrace "expected '}' to close record pattern" in
  make_pat (PatLitRecord (n, fs)) (Span.merge s es)

and parse_pat_variant p n s =
  let as_ =
    if match_token p [ Token.Lt ] then (
      let as_ = parse_list p parse_ty [ Token.Comma ] Token.Gt in
      expect p Token.Gt "expected '>'";
      as_)
    else []
  in
  let pt =
    if match_token p [ Token.LParen ] then (
      let ps = parse_list p parse_pat [ Token.Comma ] Token.RParen in
      expect p Token.RParen "expected ')'";
      ps)
    else []
  in
  make_pat (PatVariant (n, as_, pt)) (Span.merge s (snd (prev p)))

and parse_ty_params p =
  if match_token p [ Token.Lt ] then (
    let ps =
      parse_list
        p
        (fun p -> expect_id p "expected identifier")
        [ Token.Comma ]
        Token.Gt
    in
    expect p Token.Gt "expected '>' to close type parameter list";
    ps)
  else []

and parse_fn_sig p n =
  let tp = parse_ty_params p in
  expect p Token.LParen "expected '(' to start parameter list";
  let ps = parse_list p parse_param [ Token.Comma ] Token.RParen in
  expect p Token.RParen "expected ')' to close parameter list";
  let rt = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  { fn_name = n; fn_ty_params = tp; fn_params = ps; fn_ret_ty = rt }

and parse_expr_fn p s a m =
  let n = parse_ident_opt p in
  let sig_ = parse_fn_sig p n in
  let b = parse_expr_block p (snd (peek p)) in
  make_expr (ExprFn (a, m, sig_, b)) (Span.merge s b.span)

and parse_expr_bind p s t m =
  let pat = parse_pat p in
  let ty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  expect p Token.ColonEq "expected ':=' after binding";
  let i = parse_expr p Prec.None in
  make_expr
    (ExprBind
       ( m
       , t = Token.KwVar
       , pat
       , ty
       , i
       , make_expr (ExprLit (LitInt "0")) Span.dummy ))
    (Span.merge s i.span)

and parse_sum_case p =
  expect p Token.KwCase "expected 'case' keyword";
  let n = expect_id p "expected case name" in
  let ts, ps =
    if match_token p [ Token.LParen ] then (
      let items =
        parse_list
          p
          (fun p ->
            if
              check p Token.KwVar
              ||
              match peek p with
              | Token.Ident _, _ -> (
                match fst (peek_at p 1) with
                | Token.Colon | Token.ColonEq -> true
                | _ -> false)
              | _ -> false
            then (None, Some (parse_param p))
            else (Some (parse_ty p), None))
          [ Token.Comma ]
          Token.RParen
      in
      expect p Token.RParen "expected ')' to close case payload";
      (List.filter_map fst items, List.filter_map snd items))
    else ([], [])
  in
  { case_name = n; case_tys = ts; case_params = ps }

and parse_expr_extern p s =
  let a =
    match fst (peek p) with
    | Token.LitString id ->
      ignore (advance p);
      Some (resolve p id)
    | _ -> None
  in
  let u = match_token p [ Token.KwUnsafe ] in
  expect p Token.LBrace "expected '{' to start 'extern' block";
  let sigs = ref [] in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    if not (match_token p [ Token.KwFn ]) then (
      error p "expected 'fn' inside 'extern' block" (snd (peek p));
      sync p)
    else (
      sigs := parse_fn_sig p (parse_ident_opt p) :: !sigs;
      ignore (match_token p [ Token.Semicolon ]))
  done;
  let _, es = consume p Token.RBrace "expected '}' to close 'extern' block" in
  make_expr (ExprExtern (a, u, List.rev !sigs)) (Span.merge s es)

and parse_stmt p =
  let e = parse_expr p Prec.None in
  expect p Token.Semicolon "expected ';' after statement";
  make_stmt (StmtExpr e) e.span

let try_parse tokens source file_id interner =
  let p = create tokens source file_id interner in
  let ss = ref [] in
  while not (is_at_end p) do
    ss := parse_stmt p :: !ss
  done;
  if has_errors p then Error p.diag else Ok (List.rev !ss)
