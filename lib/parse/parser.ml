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

let parse_ident_opt p =
  match peek p with
  | Token.Ident id, _ ->
    ignore (advance p);
    Some (resolve p id)
  | _ -> None

let parse_list p f seps end_tok =
  let res = ref [] in
  (if not (check p end_tok) then
     let rec loop () =
       res := f p :: !res;
       if match_token p seps then loop ()
     in
     loop ());
  List.rev !res

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

let rec parse_ty p =
  match fst (peek p) with
  | Token.Ident _ -> parse_ty_app_or_ident p
  | Token.Question ->
    let _, start = advance p in
    let inner = parse_ty p in
    make_ty (TyOptional inner) (Span.merge start inner.span)
  | Token.Caret ->
    let _, start = advance p in
    let inner = parse_ty p in
    make_ty (TyPtr inner) (Span.merge start inner.span)
  | Token.LBrack -> parse_ty_array p (snd (peek p))
  | Token.LParen -> parse_ty_tuple_or_group p (snd (peek p))
  | _ ->
    let _, span = advance p in
    error p "expected type" span;
    make_ty TyError span

and parse_ty_app_or_ident p =
  let tok, span = advance p in
  let name = match tok with Token.Ident id -> resolve p id | _ -> "?" in
  if match_token p [ Token.Lt ] then
    let args = parse_list p parse_ty [ Token.Comma ] Token.Gt in
    let _, end_span =
      consume p Token.Gt "expected '>' closing type arguments"
    in
    make_ty (TyApp (name, args)) (Span.merge span end_span)
  else make_ty (TyIdent name) span

and parse_ty_array p start =
  ignore (advance p);
  let sz =
    match advance p with
    | Token.LitInt id, _ -> Some (int_of_string (resolve p id))
    | _ -> None
  in
  ignore (consume p Token.RBrack "expected ']' closing array type size");
  let inner = parse_ty p in
  make_ty (TyArray (sz, inner)) (Span.merge start inner.span)

and parse_ty_tuple_or_group p start =
  ignore (advance p);
  let types = parse_list p parse_ty [ Token.Comma ] Token.RParen in
  let _, end_span = consume p Token.RParen "expected ')' closing tuple type" in
  make_ty (TyTuple types) (Span.merge start end_span)

let parse_attrs p =
  if match_token p [ Token.LBrackLt ] then (
    let attrs =
      parse_list
        p
        (fun p ->
          let t, _ = advance p in
          {
            attr_name =
              (match t with Token.Ident id -> resolve p id | _ -> "?")
          ; attr_args = []
          })
        [ Token.Comma ]
        Token.GtRBrack
    in
    ignore (consume p Token.GtRBrack "expected '>]' closing attributes");
    attrs)
  else []

let parse_modifiers p =
  let export, extern, unsafe = (ref false, ref (None, false), ref false) in
  let rec loop () =
    match fst (peek p) with
    | Token.Ident id when resolve p id = "export" ->
      ignore (advance p);
      export := true;
      loop ()
    | Token.KwExtern ->
      let offset = ref 1 in
      let has_str =
        match fst (peek_at p !offset) with
        | Token.LitString _ ->
          offset := !offset + 1;
          true
        | _ -> false
      in
      if fst (peek_at p !offset) = Token.KwUnsafe then offset := !offset + 1;
      if fst (peek_at p !offset) = Token.LBrace then ()
      else (
        ignore (advance p);
        let abi =
          if has_str then
            match advance p with
            | Token.LitString id, _ -> Some (resolve p id)
            | _ -> None
          else None
        in
        extern := (abi, true);
        loop ())
    | Token.KwUnsafe ->
      if fst (peek_at p 1) = Token.LBrace then ()
      else (
        ignore (advance p);
        unsafe := true;
        loop ())
    | _ -> ()
  in
  loop ();
  { is_export = !export; is_extern = !extern; is_unsafe = !unsafe }

let rec parse_expr p prec =
  let left = parse_prefix p in
  let rec loop left =
    let tok, _ = peek p in
    if token_prec tok > prec then loop (parse_infix p left tok) else left
  in
  loop left

and parse_prefix p =
  let attrs = parse_attrs p in
  let mods = parse_modifiers p in
  let tok, span = peek p in
  match tok with
  | Token.LitInt id | Token.LitReal id | Token.LitString id ->
    ignore (advance p);
    let lit =
      match tok with
      | Token.LitInt _ -> LitInt (resolve p id)
      | Token.LitReal _ -> LitFloat (resolve p id)
      | _ -> LitString (resolve p id)
    in
    make_expr (ExprLit lit) span
  | Token.LitRune c ->
    ignore (advance p);
    make_expr (ExprLit (LitRune c)) span
  | Token.Minus | Token.KwNot | Token.Tilde | Token.At ->
    ignore (advance p);
    let right = parse_expr p PrecUnary in
    make_expr (ExprUnaryPrefix (tok, right)) (Span.merge span right.span)
  | Token.KwTrue | Token.KwFalse ->
    ignore (advance p);
    make_expr (ExprLit (LitBool (tok = Token.KwTrue))) span
  | Token.Ident id ->
    ignore (advance p);
    let name = resolve p id in
    if check p Token.Dot && fst (peek_at p 1) = Token.LBrace then (
      ignore (advance p);
      parse_expr_lit_record p (Some name) span)
    else make_expr (ExprIdent name) span
  | Token.Dot ->
    if fst (peek_at p 1) = Token.LBrace then (
      ignore (advance p);
      parse_expr_lit_record p None span)
    else (
      error p "expected '{' after '.' for record literal" span;
      ignore (advance p);
      make_expr ExprError span)
  | Token.LParen ->
    ignore (advance p);
    parse_expr_group_or_tuple p span
  | Token.LBrack ->
    ignore (advance p);
    let es =
      parse_list p (fun p -> parse_expr p PrecNone) [ Token.Comma ] Token.RBrack
    in
    let _, end_s = consume p Token.RBrack "expected ']'" in
    make_expr (ExprLitArray es) (Span.merge span end_s)
  | Token.LBrace -> parse_expr_block p span
  | Token.KwIf ->
    ignore (advance p);
    let c = parse_expr p PrecNone in
    let t = parse_expr p PrecNone in
    let e =
      if match_token p [ Token.KwElse ] then parse_expr p PrecNone
      else make_expr ExprError span
    in
    make_expr (ExprIf (c, t, e)) (Span.merge span (prev p |> snd))
  | Token.KwWhile ->
    ignore (advance p);
    let c, b = (parse_expr p PrecNone, parse_expr p PrecNone) in
    make_expr (ExprWhile (c, b)) (Span.merge span b.span)
  | Token.KwFor ->
    ignore (advance p);
    let n =
      match advance p with
      | Token.Ident id, _ -> resolve p id
      | _, s ->
        error p "expected identifier for iterator" s;
        "?"
    in
    ignore (consume p Token.KwIn "expected 'in'");
    let it, b = (parse_expr p PrecNone, parse_expr p PrecNone) in
    make_expr (ExprFor (n, it, b)) (Span.merge span b.span)
  | Token.KwMatch ->
    ignore (advance p);
    parse_expr_match p span
  | Token.KwReturn | Token.KwBreak ->
    ignore (advance p);
    let e =
      if not (check p Token.Semicolon || check p Token.RBrace) then
        Some (parse_expr p PrecNone)
      else None
    in
    make_expr (if tok = Token.KwReturn then ExprReturn e else ExprBreak e) span
  | Token.KwDefer | Token.KwUnsafe ->
    ignore (advance p);
    let b = parse_expr p PrecNone in
    make_expr
      (if tok = Token.KwDefer then ExprDefer b else ExprUnsafe b)
      (Span.merge span b.span)
  | Token.KwImport ->
    ignore (advance p);
    let pth =
      match advance p with
      | Token.LitString id, _ -> resolve p id
      | _, s ->
        error p "expected string after 'import'" s;
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
  | Token.KwRecord ->
    ignore (advance p);
    parse_expr_record p span attrs mods
  | Token.KwSum ->
    ignore (advance p);
    parse_expr_sum p span attrs mods
  | _ ->
    error
      p
      (Printf.sprintf "unexpected token '%s'" (Token.show p.interner tok))
      span;
    ignore (advance p);
    make_expr ExprError span

and parse_infix p left op =
  let _, span = advance p in
  match op with
  | Token.LParen ->
    let args =
      parse_list p (fun p -> parse_expr p PrecNone) [ Token.Comma ] Token.RParen
    in
    let _, end_s =
      consume p Token.RParen "expected ')' closing function arguments"
    in
    make_expr (ExprCall (left, args)) (Span.merge left.span end_s)
  | Token.LBrack ->
    let idx = parse_expr p PrecNone in
    let _, end_s = consume p Token.RBrack "expected ']'" in
    make_expr (ExprIndex (left, idx)) (Span.merge left.span end_s)
  | Token.Dot ->
    let fld =
      match advance p with
      | Token.Ident id, _ -> resolve p id
      | _, s ->
        error p "expected field name after '.'" s;
        "?"
    in
    make_expr (ExprField (left, fld)) (Span.merge left.span span)
  | Token.DotCaret | Token.Question ->
    make_expr (ExprUnaryPostfix (left, op)) (Span.merge left.span span)
  | Token.LtMinus ->
    let right = parse_expr p PrecAssign in
    make_expr (ExprAssign (left, right)) (Span.merge left.span right.span)
  | _ ->
    let right = parse_expr p (token_prec op) in
    make_expr (ExprBinary (left, op, right)) (Span.merge left.span right.span)

and parse_expr_lit_record p name start =
  ignore (consume p Token.LBrace "expected '{' after '.' for record literal");
  let base, fields = (ref None, ref []) in
  (if not (check p Token.RBrace) then
     let e = parse_expr p PrecNone in
     if match_token p [ Token.KwWith ] then (
       base := Some e;
       fields :=
         parse_list
           p
           parse_record_field
           [ Token.Comma; Token.Semicolon ]
           Token.RBrace)
     else
       fields :=
         match e.kind with
         | ExprIdent id ->
           {
             field_mutable = false
           ; field_name = id
           ; field_ty = None
           ; field_default = None
           }
           :: parse_list
                p
                parse_record_field
                [ Token.Comma; Token.Semicolon ]
                Token.RBrace
         | _ ->
           parse_list
             p
             parse_record_field
             [ Token.Comma; Token.Semicolon ]
             Token.RBrace);
  let _, end_s = consume p Token.RBrace "expected '}' closing record literal" in
  make_expr (ExprLitRecord (name, !fields, !base)) (Span.merge start end_s)

and parse_record_field p =
  let m, n =
    ( match_token p [ Token.KwVar ]
    , match advance p with
      | Token.Ident id, _ -> resolve p id
      | _, s ->
        error p "expected field name" s;
        "?" )
  in
  let ty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  let def =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p PrecNone)
    else None
  in
  { field_mutable = m; field_name = n; field_ty = ty; field_default = def }

and parse_expr_block p start =
  ignore (consume p Token.LBrace "expected '{' starting block");
  let stmts, expr = (ref [], ref None) in
  while not (check p Token.RBrace || is_at_end p) do
    let e = parse_expr p PrecNone in
    if match_token p [ Token.Semicolon ] then
      stmts := make_stmt (StmtExpr e) e.span :: !stmts
    else if check p Token.RBrace then expr := Some e
    else (
      error p "expected ';' after statement" (snd (peek p));
      sync p)
  done;
  let _, end_s = consume p Token.RBrace "expected '}' closing block" in
  make_expr (ExprBlock (List.rev !stmts, !expr)) (Span.merge start end_s)

and parse_expr_group_or_tuple p start =
  if match_token p [ Token.RParen ] then
    make_expr (ExprLitTuple []) (Span.merge start (prev p |> snd))
  else
    let first = parse_expr p PrecNone in
    if match_token p [ Token.Comma ] then
      let rest =
        parse_list
          p
          (fun p -> parse_expr p PrecNone)
          [ Token.Comma ]
          Token.RParen
      in
      let _, end_s =
        consume p Token.RParen "expected ')' closing tuple literal"
      in
      make_expr (ExprLitTuple (first :: rest)) (Span.merge start end_s)
    else (
      ignore (consume p Token.RParen "expected ')' closing expression grouping");
      first)

and parse_expr_match p start =
  let target = parse_expr p PrecNone in
  ignore (consume p Token.LBrace "expected '{' opening 'match' body");
  let cases = ref [] in
  while match_token p [ Token.KwCase ] do
    let pat = parse_pat p in
    ignore (consume p Token.EqGt "expected '=>' after 'match' pattern");
    let e = parse_expr p PrecNone in
    ignore (match_token p [ Token.Comma; Token.Semicolon ]);
    cases := { case_pat = pat; case_expr = e } :: !cases
  done;
  let _, end_s = consume p Token.RBrace "expected '}' closing 'match' body" in
  make_expr (ExprMatch (target, List.rev !cases)) (Span.merge start end_s)

and parse_pat p = parse_pat_cons p

and parse_pat_cons p =
  let left = parse_pat_primary p in
  if match_token p [ Token.ColonColon ] then
    let right = parse_pat_cons p in
    make_pat (PatCons (left, right)) (Span.merge left.span right.span)
  else left

and parse_pat_primary p =
  let tok, span = peek p in
  match tok with
  | Token.Ident id -> (
    ignore (advance p);
    let n = resolve p id in
    match fst (peek p) with
    | Token.Dot ->
      ignore (advance p);
      parse_pat_lit_record p n span
    | Token.Lt | Token.LParen -> parse_pat_variant p n span
    | _ -> make_pat (PatIdent n) span)
  | Token.Underscore ->
    ignore (advance p);
    make_pat PatWild span
  | Token.LitInt id | Token.LitReal id | Token.LitString id ->
    ignore (advance p);
    let lit =
      match tok with
      | Token.LitInt _ -> LitInt (resolve p id)
      | Token.LitReal _ -> LitFloat (resolve p id)
      | _ -> LitString (resolve p id)
    in
    make_pat (PatLit lit) span
  | Token.LitRune c ->
    ignore (advance p);
    make_pat (PatLit (LitRune c)) span
  | Token.KwTrue | Token.KwFalse ->
    ignore (advance p);
    make_pat (PatLit (LitBool (tok = Token.KwTrue))) span
  | Token.LParen ->
    ignore (advance p);
    let ps = parse_list p parse_pat [ Token.Comma ] Token.RParen in
    let _, end_s =
      consume p Token.RParen "expected ')' closing tuple pattern"
    in
    make_pat (PatLitTuple ps) (Span.merge span end_s)
  | Token.LBrack ->
    ignore (advance p);
    let ps = parse_list p parse_pat [ Token.Comma ] Token.RBrack in
    let _, end_s =
      consume p Token.RBrack "expected ']' closing array pattern"
    in
    make_pat (PatLitArray ps) (Span.merge span end_s)
  | _ ->
    error p "expected pattern" span;
    ignore (advance p);
    make_pat PatError span

and parse_pat_lit_record p name start =
  ignore (consume p Token.LBrace "expected '{' after '.' for record pattern");
  let fields =
    parse_list
      p
      (fun p ->
        let t, _ = advance p in
        {
          field_name =
            (match t with Token.Ident id -> resolve p id | _ -> "?")
        })
      [ Token.Comma ]
      Token.RBrace
  in
  let _, end_s = consume p Token.RBrace "expected '}' closing record pattern" in
  make_pat (PatLitRecord (name, fields)) (Span.merge start end_s)

and parse_pat_variant p name start =
  let args =
    if match_token p [ Token.Lt ] then (
      let as_ = parse_list p parse_ty [ Token.Comma ] Token.Gt in
      ignore (consume p Token.Gt "expected '>'");
      as_)
    else []
  in
  let pat =
    if match_token p [ Token.LParen ] then (
      let p' = parse_pat p in
      ignore (consume p Token.RParen "expected ')'");
      Some p')
    else None
  in
  make_pat (PatVariant (name, args, pat)) (Span.merge start (prev p |> snd))

and parse_ty_params p =
  if match_token p [ Token.Lt ] then (
    let ps =
      parse_list
        p
        (fun p ->
          match advance p with
          | Token.Ident id, _ -> resolve p id
          | _, s ->
            error p "expected identifier" s;
            "?")
        [ Token.Comma ]
        Token.Gt
    in
    ignore (consume p Token.Gt "expected '>' closing type parameters");
    ps)
  else []

and parse_fn_sig p name =
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LParen "expected '(' starting function parameters");
  let params = parse_list p parse_param [ Token.Comma ] Token.RParen in
  ignore (consume p Token.RParen "expected ')' closing function parameters");
  let ret_ty =
    if match_token p [ Token.Colon ] then Some (parse_ty p) else None
  in
  {
    fn_name = name
  ; fn_ty_params = ty_params
  ; fn_params = params
  ; fn_ret_ty = ret_ty
  }

and parse_param p =
  let mut = match_token p [ Token.KwVar ] in
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected parameter name" s;
      "?"
  in
  let ty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  let def =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p PrecNone)
    else None
  in
  { param_mutable = mut; param_name = name; param_ty = ty; param_default = def }

and parse_expr_fn p start attrs mods =
  let name = parse_ident_opt p in
  let sig_ = parse_fn_sig p name in
  let body = parse_expr_block p (snd (peek p)) in
  make_expr (ExprFn (attrs, mods, sig_, body)) (Span.merge start body.span)

and parse_expr_bind p start tok mods =
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected identifier" s;
      "?"
  in
  let ty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  ignore (consume p Token.ColonEq "expected ':=' after binding name and type");
  let init = parse_expr p PrecNone in
  make_expr
    (ExprBind
       ( mods
       , tok = Token.KwVar
       , name
       , ty
       , init
       , make_expr (ExprLit (LitInt "0")) Span.dummy ))
    (Span.merge start init.span)

and parse_expr_record p start attrs mods =
  let name = parse_ident_opt p in
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LBrace "expected '{' after 'record' name");
  let fields =
    parse_list
      p
      parse_record_field
      [ Token.Comma; Token.Semicolon ]
      Token.RBrace
  in
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'record' expression"
  in
  make_expr
    (ExprRecord (attrs, mods, name, ty_params, fields))
    (Span.merge start end_span)

and parse_expr_sum p start attrs mods =
  let name = parse_ident_opt p in
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LBrace "expected '{' after 'sum' type name");
  let cases =
    parse_list p parse_sum_case [ Token.Comma; Token.Semicolon ] Token.RBrace
  in
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'sum' type expression"
  in
  make_expr
    (ExprSum (attrs, mods, name, ty_params, cases))
    (Span.merge start end_span)

and parse_sum_case p =
  ignore (consume p Token.KwCase "expected 'case' starting sum case");
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected sum case name" s;
      "?"
  in
  let tys =
    if match_token p [ Token.LParen ] then (
      let ts = parse_list p parse_ty [ Token.Comma ] Token.RParen in
      ignore (consume p Token.RParen "expected ')' closing sum case payload");
      ts)
    else []
  in
  { case_name = name; case_tys = tys; case_params = [] }

and parse_expr_extern p start =
  let abi =
    match fst (peek p) with
    | Token.LitString _ -> (
      let tok, _ = advance p in
      match tok with Token.LitString id -> Some (resolve p id) | _ -> None)
    | _ -> None
  in
  let is_unsafe = match_token p [ Token.KwUnsafe ] in
  ignore (consume p Token.LBrace "expected '{' starting 'extern' block");
  let sigs = ref [] in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    if not (match_token p [ Token.KwFn ]) then (
      error p "expected 'fn' inside 'extern' block" (snd (peek p));
      sync p)
    else
      let name = parse_ident_opt p in
      sigs := parse_fn_sig p name :: !sigs;
      ignore (match_token p [ Token.Semicolon ])
  done;
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'extern' block"
  in
  make_expr
    (ExprExtern (abi, is_unsafe, List.rev !sigs))
    (Span.merge start end_span)

and parse_stmt p =
  let expr = parse_expr p PrecNone in
  ignore (consume p Token.Semicolon "expected ';' after statement-expression");
  make_stmt (StmtExpr expr) expr.span

let parse tokens source file_id interner =
  let p = create tokens source file_id interner in
  let stmts = ref [] in
  while not (is_at_end p) do
    stmts := parse_stmt p :: !stmts
  done;
  if has_errors p then Error p.diag else Ok (List.rev !stmts)
