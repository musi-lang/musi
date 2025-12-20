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
    make_ty (TyOptional inner) (Span.merge start inner.span)
  | Token.Caret ->
    let _, start = advance p in
    let inner = parse_ty p in
    make_ty (TyPtr inner) (Span.merge start inner.span)
  | Token.LBrack -> parse_ty_array p ps_span
  | Token.LParen -> parse_ty_tuple_or_group p ps_span
  | _ ->
    error p "expected type" ps_span;
    make_ty TyError ps_span

and parse_ty_app_or_ident p =
  let id_token, start = advance p in
  let name = match id_token with Token.Ident id -> resolve p id | _ -> "?" in
  if match_token p [ Token.Lt ] then
    let args = parse_comma_list p parse_ty Token.Gt in
    let _, end_span =
      consume p Token.Gt "expected '>' closing type arguments"
    in
    make_ty (TyApp (name, args)) (Span.merge start end_span)
  else parse_ty_infix p (make_ty (TyIdent name) start)

and parse_ty_infix p left =
  if match_token p [ Token.MinusGt ] then
    let right = parse_ty p in
    make_ty (TyFn (left, right)) (Span.merge left.span right.span)
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
  ignore (consume p Token.RBrack "expected ']' closing array type size");
  let inner = parse_ty p in
  make_ty (TyArray (size, inner)) (Span.merge start inner.span)

and parse_ty_tuple_or_group p start =
  ignore (advance p);
  let first = parse_ty p in
  if match_token p [ Token.Comma ] then
    let rest = parse_comma_list p parse_ty Token.RParen in
    let _, end_span =
      consume p Token.RParen "expected ')' closing tuple type"
    in
    make_ty (TyTuple (first :: rest)) (Span.merge start end_span)
  else (
    ignore (consume p Token.RParen "expected ')' closing type grouping");
    first)

let rec parse_pat p =
  let left = parse_pat_atom p in
  if match_token p [ Token.ColonColon ] then
    let right = parse_pat p in
    make_pat (PatCons (left, right)) (Span.merge left.span right.span)
  else left

and parse_pat_atom p =
  let tok, span = peek p in
  match tok with
  | Token.Underscore ->
    ignore (advance p);
    make_pat PatWild span
  | Token.LitInt id ->
    ignore (advance p);
    make_pat (PatLit (LitInt (resolve p id))) span
  | Token.LitString id ->
    ignore (advance p);
    make_pat (PatLit (LitString (resolve p id))) span
  | Token.KwTrue ->
    ignore (advance p);
    make_pat (PatLit (LitBool true)) span
  | Token.KwFalse ->
    ignore (advance p);
    make_pat (PatLit (LitBool false)) span
  | Token.Ident id ->
    ignore (advance p);
    let name = resolve p id in
    parse_pat_ident_rest p name span
  | Token.LParen -> parse_pat_tuple p span
  | Token.LBrack -> parse_pat_array p span
  | _ ->
    error
      p
      "expected pattern starting with identifier, literal, or grouping"
      span;
    ignore (advance p);
    make_pat PatError span

and parse_pat_ident_rest p name start =
  if match_token p [ Token.Dot ] then (
    ignore
      (consume p Token.LBrace "expected '{' starting record pattern fields");
    let fields = parse_comma_list p parse_pat_field Token.RBrace in
    let _, end_span =
      consume p Token.RBrace "expected '}' closing record pattern"
    in
    make_pat (PatLitRecord (name, fields)) (Span.merge start end_span))
  else if check p Token.Lt then (
    let _, _ = advance p in
    let args = parse_comma_list p parse_ty Token.Gt in
    ignore (consume p Token.Gt "expected '>' closing pattern type arguments");
    let sub =
      if match_token p [ Token.LParen ] then (
        let s =
          if not (check p Token.RParen) then Some (parse_pat p) else None
        in
        ignore (consume p Token.RParen "expected ')' closing variant payload");
        s)
      else None
    in
    make_pat (PatVariant (name, args, sub)) (Span.merge start (snd (prev p))))
  else if match_token p [ Token.LParen ] then
    let sub = if not (check p Token.RParen) then Some (parse_pat p) else None in
    let _, end_span =
      consume p Token.RParen "expected ')' closing variant payload"
    in
    make_pat (PatVariant (name, [], sub)) (Span.merge start end_span)
  else make_pat (PatIdent name) start

and parse_pat_field p =
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name in record pattern" s;
      "?"
  in
  { field_name = name }

and parse_pat_tuple p start =
  ignore (advance p);
  let elems = parse_comma_list p parse_pat Token.RParen in
  let _, end_span =
    consume p Token.RParen "expected ')' closing tuple pattern"
  in
  make_pat (PatLitTuple elems) (Span.merge start end_span)

and parse_pat_array p start =
  ignore (advance p);
  let elems = parse_comma_list p parse_pat Token.RBrack in
  let _, end_span =
    consume p Token.RBrack "expected ']' closing array pattern"
  in
  make_pat (PatLitArray elems) (Span.merge start end_span)

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
        error p "expected literal value in attribute argument" span;
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
      ignore (consume p Token.RParen "expected ')' closing attribute arguments");
      a)
    else []
  in
  { attr_name = name; attr_args = args }

let parse_attrs p =
  let attrs = ref [] in
  if match_token p [ Token.LBrackLt ] then (
    attrs := parse_comma_list p parse_attr Token.GtRBrack;
    ignore (consume p Token.GtRBrack "expected '>]' closing attributes"));
  !attrs

let parse_modifiers p =
  let is_export = ref false in
  let is_extern = ref (None, false) in
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
      if fst (peek_at p !offset) = Token.KwUnsafe then offset := !offset + 1;
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

        is_extern := (abi, true);
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
    make_expr (ExprLit (LitInt (resolve p id))) span
  | Token.LitReal id ->
    ignore (advance p);
    make_expr (ExprLit (LitFloat (resolve p id))) span
  | Token.LitString id ->
    ignore (advance p);
    make_expr (ExprLit (LitString (resolve p id))) span
  | Token.LitRune c ->
    ignore (advance p);
    make_expr (ExprLit (LitRune c)) span
  | Token.KwTrue ->
    ignore (advance p);
    make_expr (ExprLit (LitBool true)) span
  | Token.KwFalse ->
    ignore (advance p);
    make_expr (ExprLit (LitBool false)) span
  | Token.Ident id ->
    ignore (advance p);
    let name = resolve p id in
    let is_lit_rec =
      match fst (peek p) with
      | Token.Dot -> fst (peek_at p 1) = Token.LBrace
      | _ -> false
    in
    if is_lit_rec then (
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
    parse_expr_lit_array p span
  | Token.LBrace -> parse_expr_block p span
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
    make_expr (ExprDefer body) (Span.merge span body.span)
  | Token.KwUnsafe ->
    ignore (advance p);
    let body = parse_expr p PrecNone in
    make_expr (ExprUnsafe body) (Span.merge span body.span)
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
    make_expr (ExprUnaryPrefix (tok, operand)) (Span.merge span operand.span)
  | Token.KwExtern ->
    if attrs <> [] then error p "attributes not allowed on 'extern' block" span;
    ignore (advance p);
    parse_expr_extern p span
  | _ ->
    error p "expected expression" span;
    ignore (advance p);
    make_expr ExprError span

and parse_infix p left op =
  let _, op_span = advance p in
  match op with
  | Token.LParen -> parse_expr_call p left
  | Token.LBrack -> parse_expr_index p left
  | Token.Dot -> parse_dot p left op_span
  | Token.DotCaret ->
    make_expr
      (ExprUnaryPostfix (left, Token.DotCaret))
      (Span.merge left.span op_span)
  | Token.Question ->
    make_expr
      (ExprUnaryPostfix (left, Token.Question))
      (Span.merge left.span op_span)
  | _ ->
    let right = parse_expr p (token_prec op) in
    make_expr (ExprBinary (left, op, right)) (Span.merge left.span right.span)

and parse_expr_lit_record p name_opt start =
  ignore (consume p Token.LBrace "expected '{' after '.' for record literal");
  let fields = ref [] in
  let base_val = ref None in

  (if match_token p [ Token.KwWith ] then (
     error p "expected expression before 'with' in record update" (snd (peek p));
     base_val := Some (parse_expr p PrecNone))
   else if check p Token.RBrace then ()
   else
     let is_field_start () =
       match fst (peek p) with
       | Token.KwVar -> true
       | Token.Ident _ ->
         let next = fst (peek_at p 1) in
         next = Token.ColonEq || next = Token.Colon || next = Token.Comma
         || next = Token.RBrace
       | _ -> false
     in
     if not (is_field_start ()) then
       let e = parse_expr p PrecNone in
       if match_token p [ Token.KwWith ] then (
         base_val := Some e;
         if not (check p Token.RBrace) then
           fields := parse_comma_list p parse_record_field Token.RBrace)
       else
         match e.kind with
         | ExprIdent id ->
           fields :=
             {
               field_mutable = false
             ; field_name = id
             ; field_ty = None
             ; field_default = None
             }
             :: !fields;
           if match_token p [ Token.Comma ] then
             fields :=
               !fields @ parse_comma_list p parse_record_field Token.RBrace
         | _ ->
           error p "expected 'with' after record base expression" (snd (peek p))
     else fields := parse_comma_list p parse_record_field Token.RBrace);
  let _, end_span =
    consume p Token.RBrace "expected '}' closing record literal"
  in
  make_expr
    (ExprLitRecord (name_opt, !fields, !base_val))
    (Span.merge start end_span)

and parse_record_field p =
  let mut = match_token p [ Token.KwVar ] in
  let fname =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name" s;
      "?"
  in
  let fty = if match_token p [ Token.Colon ] then Some (parse_ty p) else None in
  let def =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p PrecNone)
    else None
  in
  {
    field_mutable = mut
  ; field_name = fname
  ; field_ty = fty
  ; field_default = def
  }

and parse_expr_group_or_tuple p start =
  if match_token p [ Token.RParen ] then
    make_expr (ExprLitTuple []) (Span.merge start (snd (prev p)))
  else
    let first = parse_expr p PrecNone in
    if match_token p [ Token.Comma ] then
      let rest =
        parse_comma_list p (fun p -> parse_expr p PrecNone) Token.RParen
      in
      let _, end_span =
        consume p Token.RParen "expected ')' closing tuple literal"
      in
      make_expr (ExprLitTuple (first :: rest)) (Span.merge start end_span)
    else (
      ignore (consume p Token.RParen "expected ')' closing expression grouping");
      first)

and parse_expr_lit_array p start =
  let elems =
    parse_comma_list p (fun p -> parse_expr p PrecNone) Token.RBrack
  in
  let _, end_span =
    consume p Token.RBrack "expected ']' closing array literal"
  in
  make_expr (ExprLitArray elems) (Span.merge start end_span)

and parse_expr_block p start =
  ignore (consume p Token.LBrace "expected '{' starting block");
  let stmts = ref [] in
  let expr_opt = ref None in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    let e = parse_expr p PrecNone in
    if match_token p [ Token.Semicolon ] then
      stmts := make_stmt (StmtExpr e) e.span :: !stmts
    else if check p Token.RBrace then expr_opt := Some e
    else (
      error p "expected ';' after statement" (snd (peek p));
      sync p)
  done;
  let _, end_span = consume p Token.RBrace "expected '}' closing block" in
  make_expr (ExprBlock (List.rev !stmts, !expr_opt)) (Span.merge start end_span)

and parse_expr_if p start =
  let cond = parse_expr p PrecNone in
  let then_ = parse_expr p PrecNone in
  let else_ =
    if match_token p [ Token.KwElse ] then parse_expr p PrecNone
    else make_expr (ExprBlock ([], None)) Span.dummy
  in
  make_expr (ExprIf (cond, then_, else_)) (Span.merge start else_.span)

and parse_expr_while p start =
  let cond = parse_expr p PrecNone in
  let body = parse_expr p PrecNone in
  make_expr (ExprWhile (cond, body)) (Span.merge start body.span)

and parse_expr_for p start =
  let name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected identifier after 'for'" s;
      "?"
  in
  ignore (consume p Token.KwIn "expected 'in' after loop binding");
  let iter = parse_expr p PrecNone in
  let body = parse_expr p PrecNone in
  make_expr (ExprFor (name, iter, body)) (Span.merge start body.span)

and parse_expr_match p start =
  let target = parse_expr p PrecNone in
  ignore (consume p Token.LBrace "expected '{' opening 'match' body");
  let cases = ref [] in
  while match_token p [ Token.KwCase ] do
    let pat = parse_pat p in
    ignore (consume p Token.EqGt "expected '=>' after 'match' pattern");
    let expr = parse_expr p PrecNone in
    ignore (match_token p [ Token.Comma ]);
    cases := { case_pat = pat; case_expr = expr } :: !cases
  done;
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'match' body"
  in
  make_expr (ExprMatch (target, List.rev !cases)) (Span.merge start end_span)

and parse_expr_return p start =
  let expr_opt =
    if (not (check p Token.Semicolon)) && not (check p Token.RBrace) then
      Some (parse_expr p PrecNone)
    else None
  in
  make_expr (ExprReturn expr_opt) start

and parse_expr_break p start =
  let expr_opt =
    if (not (check p Token.Semicolon)) && not (check p Token.RBrace) then
      Some (parse_expr p PrecNone)
    else None
  in
  make_expr (ExprBreak expr_opt) start

and parse_expr_import p start =
  let path =
    match advance p with
    | Token.LitString id, _ -> resolve p id
    | _, s ->
      error p "expected import path string" s;
      ""
  in
  make_expr (ExprImport path) (Span.merge start (snd (prev p)))

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
  ignore (consume p Token.ColonEq "expected ':=' after binding name and type");
  let init = parse_expr p PrecNone in
  make_expr
    (ExprBind
       ( mods
       , mut
       , name
       , ty_opt
       , init
       , make_expr (ExprLit (LitInt "0")) Span.dummy ))
    (Span.merge start init.span)

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
  make_expr (ExprFn (attrs, mods, sig_, body)) (Span.merge start body.span)

and parse_ty_params p =
  if match_token p [ Token.Lt ] then (
    let params =
      parse_comma_list
        p
        (fun p ->
          match advance p with
          | Token.Ident id, _ -> resolve p id
          | _, s ->
            error p "expected type parameter name" s;
            "?")
        Token.Gt
    in
    ignore (consume p Token.Gt "expected '>' closing type parameters");
    params)
  else []

and parse_fn_sig p name =
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LParen "expected '(' starting function parameters");
  let params = parse_comma_list p parse_param Token.RParen in
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
  let p_name =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected parameter name" s;
      "?"
  in
  let p_ty =
    if match_token p [ Token.Colon ] then Some (parse_ty p) else None
  in
  let p_init =
    if match_token p [ Token.ColonEq ] then Some (parse_expr p PrecNone)
    else None
  in
  {
    param_mutable = mut
  ; param_name = p_name
  ; param_ty = p_ty
  ; param_default = p_init
  }

and parse_expr_record p start attrs mods =
  let name =
    match peek p with
    | Token.Ident id, _ ->
      ignore (advance p);
      Some (resolve p id)
    | _ -> None
  in
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LBrace "expected '{' after 'record' name");
  let fields = parse_comma_list p parse_record_field Token.RBrace in
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'record' expression"
  in
  make_expr
    (ExprRecord (attrs, mods, name, ty_params, fields))
    (Span.merge start end_span)

and parse_expr_sum p start attrs mods =
  let name =
    match peek p with
    | Token.Ident id, _ ->
      ignore (advance p);
      Some (resolve p id)
    | _ -> None
  in
  let ty_params = parse_ty_params p in
  ignore (consume p Token.LBrace "expected '{' after 'sum' type name");
  let cases = parse_comma_list p parse_sum_case Token.RBrace in
  let _, end_span =
    consume p Token.RBrace "expected '}' closing 'sum' type expression"
  in
  make_expr
    (ExprSum (attrs, mods, name, ty_params, cases))
    (Span.merge start end_span)

and parse_sum_case p =
  ignore (consume p Token.KwCase "expected 'case' starting sum case");
  let cname =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected sum case name" s;
      "?"
  in
  let types =
    if match_token p [ Token.LParen ] then (
      let t = parse_comma_list p parse_ty Token.RParen in
      ignore (consume p Token.RParen "expected ')' closing sum case payload");
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
  let _, end_span =
    consume p Token.RParen "expected ')' closing function arguments"
  in
  make_expr (ExprCall (callee, args)) (Span.merge callee.span end_span)

and parse_expr_index p callee =
  let index = parse_expr p PrecNone in
  let _, end_span = consume p Token.RBrack "expected ']' closing index" in
  make_expr (ExprIndex (callee, index)) (Span.merge callee.span end_span)

and parse_dot p callee op_span =
  let field =
    match advance p with
    | Token.Ident id, _ -> resolve p id
    | _, s ->
      error p "expected field name after '.'" s;
      "?"
  in
  make_expr (ExprField (callee, field)) (Span.merge callee.span op_span)

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
  ignore (consume p Token.LBrace "expected '{' starting 'extern' block");
  let sigs = ref [] in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    if not (check p Token.KwFn) then (
      error p "expected 'fn' inside 'extern' block" (snd (peek p));
      sync p)
    else (
      ignore (advance p);
      let name =
        match peek p with
        | Token.Ident id, _ ->
          ignore (advance p);
          Some (resolve p id)
        | _ -> None
      in
      sigs := parse_fn_sig p name :: !sigs;
      ignore (match_token p [ Token.Semicolon ]))
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
  if Reporter.has_errors p.diag then Error p.diag else Ok (List.rev !stmts)
