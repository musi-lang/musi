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
let is_at_end p = p.token_idx >= Array.length p.tokens

let peek p =
  if is_at_end p then (Token.EOF, Span.dummy) else p.tokens.(p.token_idx)

let peek_at p i =
  let k = p.token_idx + i in
  if k >= Array.length p.tokens then (Token.EOF, Span.dummy) else p.tokens.(k)

let advance p =
  if not (is_at_end p) then p.token_idx <- p.token_idx + 1;
  if p.token_idx = 0 then (Token.EOF, Span.dummy)
  else p.tokens.(p.token_idx - 1)

let prev p =
  if p.token_idx = 0 then (Token.EOF, Span.dummy)
  else p.tokens.(p.token_idx - 1)

let check p k = if is_at_end p then false else fst (peek p) = k
let check_fn p f = if is_at_end p then false else f (fst (peek p))

let match_token p ks =
  if List.exists (check p) ks then (
    ignore (advance p);
    true)
  else false

let error p m s = p.diag <- Reporter.add p.diag (Reporter.error m s)

let resolve p id =
  match Interner.lookup_opt p.interner id with Some s -> s | None -> "?"

let expect_spanned p k m =
  match advance p with
  | t, s when t = k -> { kind = t; span = s }
  | _, s ->
    error p m s;
    { kind = k; span = s }

let expect_id p m =
  match advance p with
  | Token.Ident id, s -> { kind = id; span = s }
  | _, s ->
    error p m s;
    { kind = Interner.intern p.interner "error"; span = s }

let parse_preceded p k f m =
  let leader = expect_spanned p k m in
  { leader; value = f p }

let parse_followed p f k m =
  let value = f p in
  { value; trailer = expect_spanned p k m }

let opt p k f = if check p k then Some (parse_preceded p k f "") else None

let parse_delimited_list p open_tok parse_item seps close_tok msg =
  let ldelim =
    expect_spanned
      p
      open_tok
      (Printf.sprintf "expected '%s'" (Token.show p.interner open_tok))
  in
  let items, s_list = (ref [], ref []) in
  while (not (check p close_tok)) && not (is_at_end p) do
    items := parse_item p :: !items;
    if not (check p close_tok) then (
      let matched = ref false in
      List.iter
        (fun sep ->
          if check p sep then (
            let t, s = advance p in
            s_list := { kind = t; span = s } :: !s_list;
            matched := true))
        seps;
      if (not !matched) && not (check p close_tok) then
        error p (Printf.sprintf "expected separator in %s" msg) (snd (peek p)))
  done;
  {
    ldelim
  ; value = { elems = List.rev !items; seps = List.rev !s_list }
  ; rdelim =
      expect_spanned
        p
        close_tok
        (Printf.sprintf "missing '%s'" (Token.show p.interner close_tok))
  }

let parse_list p k f c m = parse_delimited_list p k f [ Token.Comma ] c m

let make_stmt_expr value trailer =
  make_stmt (StmtExpr { value; trailer }) (Span.merge value.span trailer.span)

let parse_lit_opt p =
  let tok, span = peek p in
  match tok with
  | Token.LitInt id ->
    ignore (advance p);
    Some (span, LitInt id)
  | Token.LitReal id ->
    ignore (advance p);
    Some (span, LitFloat id)
  | Token.LitString id ->
    ignore (advance p);
    Some (span, LitString id)
  | Token.LitRune c ->
    ignore (advance p);
    Some (span, LitRune c)
  | Token.KwTrue | Token.KwFalse ->
    ignore (advance p);
    Some (span, LitBool (tok = Token.KwTrue))
  | _ -> None

let rec parse_ty p =
  let l = parse_ty_primary p in
  if check p Token.MinusGt then
    let op = expect_spanned p Token.MinusGt "expected '->'" in
    let r = parse_ty p in
    make_ty (TyFn (l, op, r)) (Span.merge l.span r.span)
  else l

and parse_ty_primary p =
  match fst (peek p) with
  | Token.Ident _ ->
    let n = expect_id p "expected identifier" in
    if check p Token.Lt then
      let args = parse_list p Token.Lt parse_ty Token.Gt "generics" in
      make_ty (TyApp { name = n; args }) (Span.merge n.span args.rdelim.span)
    else make_ty (TyIdent n) n.span
  | (Token.Question | Token.Caret) as t ->
    let op = expect_spanned p t "expected type operator" in
    let i = parse_ty_primary p in
    make_ty
      (if t = Token.Question then TyOptional (op, i) else TyPtr (op, i))
      (Span.merge op.span i.span)
  | Token.LBrack ->
    let l = expect_spanned p Token.LBrack "expected '['" in
    let len =
      if check_fn p (function Token.LitInt _ -> true | _ -> false) then
        match advance p with
        | Token.LitInt id, _ -> Some (int_of_string (resolve p id))
        | _ -> None
      else None
    in
    let r = expect_spanned p Token.RBrack "expected ']'" in
    let ty = parse_ty_primary p in
    make_ty
      (TyArray { ldelim = l; len; rdelim = r; ty })
      (Span.merge l.span ty.span)
  | Token.LParen ->
    let d = parse_list p Token.LParen parse_ty Token.RParen "tuple" in
    make_ty (TyTuple d) (Span.merge d.ldelim.span d.rdelim.span)
  | _ ->
    let _, s = advance p in
    error p "expected type" s;
    make_ty TyError s

let rec parse_pat p =
  let cons = parse_pat_cons p in
  if check p Token.Bar then (
    let seps, rest = (ref [], ref []) in
    while check p Token.Bar do
      let op = expect_spanned p Token.Bar "expected '|'" in
      seps := op :: !seps;
      rest := parse_pat_cons p :: !rest
    done;
    let elems = cons :: List.rev !rest in
    let last = List.hd !rest in
    make_pat
      (PatOr
         {
           kind = { elems; seps = List.rev !seps }
         ; span = Span.merge cons.span last.span
         })
      (Span.merge cons.span last.span))
  else cons

and parse_pat_cons p =
  let l = parse_pat_primary p in
  if check p Token.ColonColon then
    let op = expect_spanned p Token.ColonColon "expected '::'" in
    let r = parse_pat p in
    make_pat (PatCons (l, op, r)) (Span.merge l.span r.span)
  else l

and parse_pat_primary p =
  let tok, _ = peek p in
  match parse_lit_opt p with
  | Some (s, l) -> make_pat (PatLit { kind = l; span = s }) s
  | None -> (
    match tok with
    | Token.Ident _ ->
      let n = expect_id p "expected identifier" in
      if check p Token.Dot && fst (peek_at p 1) = Token.LBrace then
        let dot = expect_spanned p Token.Dot "expected '.'" in
        let fs =
          parse_list
            p
            Token.LBrace
            (fun p -> { name = expect_id p "expected field name" })
            Token.RBrace
            "pat fields"
        in
        make_pat
          (PatLitRecord { name = Some n; dot; fields = fs })
          (Span.merge n.span fs.rdelim.span)
      else if check p Token.LParen || check p Token.Lt then
        let ta =
          if check p Token.Lt then
            Some (parse_list p Token.Lt parse_ty Token.Gt "variants")
          else None
        in
        let a =
          if check p Token.LParen then
            Some (parse_list p Token.LParen parse_pat Token.RParen "variants")
          else None
        in
        let s2 =
          match (a, ta) with
          | Some d, _ -> d.rdelim.span
          | None, Some d -> d.rdelim.span
          | _ -> n.span
        in
        make_pat
          (PatVariant { name = n; ty_args = ta; args = a })
          (Span.merge n.span s2)
      else make_pat (PatIdent n) n.span
    | Token.Underscore ->
      let op = expect_spanned p Token.Underscore "expected '_'" in
      make_pat (PatWild op) op.span
    | Token.LParen ->
      let d = parse_list p Token.LParen parse_pat Token.RParen "tuple" in
      make_pat (PatLitTuple d) (Span.merge d.ldelim.span d.rdelim.span)
    | Token.LBrack ->
      let d = parse_list p Token.LBrack parse_pat Token.RBrack "array" in
      make_pat (PatLitArray d) (Span.merge d.ldelim.span d.rdelim.span)
    | Token.Dot when fst (peek_at p 1) = Token.LBrace ->
      let dot = expect_spanned p Token.Dot "expected '.'" in
      let fs =
        parse_list
          p
          Token.LBrace
          (fun p -> { name = expect_id p "expected field name" })
          Token.RBrace
          "record"
      in
      make_pat
        (PatLitRecord { name = None; dot; fields = fs })
        (Span.merge dot.span fs.rdelim.span)
    | _ ->
      let _, s = advance p in
      error p "expected pattern" s;
      make_pat PatError s)

let parse_attrs p =
  let rec loop acc =
    if check p Token.LBrackLt then
      let l = expect_spanned p Token.LBrackLt "expected '[<'" in
      let n = expect_id p "expected attribute name" in
      let a =
        if check p Token.LParen then
          Some
            (parse_list
               p
               Token.LParen
               (fun p ->
                 let is_named =
                   match (peek p, peek_at p 1) with
                   | (Token.Ident _, _), (Token.ColonEq, _) -> true
                   | _ -> false
                 in
                 if is_named then
                   AttrArgNamed
                     ( expect_id p "expected argument name"
                     , expect_spanned p Token.ColonEq "expected ':='"
                     , match parse_lit_opt p with
                       | Some (s, l) -> { kind = l; span = s }
                       | _ ->
                         {
                           kind = LitInt (Interner.intern p.interner "0")
                         ; span = snd (peek p)
                         } )
                 else
                   AttrArgPos
                     (match parse_lit_opt p with
                     | Some (s, l) -> { kind = l; span = s }
                     | _ ->
                       {
                         kind = LitInt (Interner.intern p.interner "0")
                       ; span = snd (peek p)
                       }))
               Token.RParen
               "args")
        else None
      in
      loop
        ({
           ldelim = l
         ; name = n
         ; args = a
         ; rdelim = expect_spanned p Token.GtRBrack "expected '>]'"
         }
        :: acc)
    else List.rev acc
  in
  loop []

let parse_mods p =
  let ex, ext, un = (ref None, ref None, ref None) in
  let rec loop () =
    match fst (peek p) with
    | Token.Ident id when resolve p id = "export" ->
      ex := Some (expect_spanned p (fst (peek p)) "expected 'export'");
      loop ()
    | Token.KwExtern ->
      let kw = expect_spanned p Token.KwExtern "expected 'extern'" in
      if
        check_fn p (function Token.LitString _ -> true | _ -> false)
        && fst (peek_at p 1) <> Token.LBrace
      then (
        let abi =
          Some
            (let l =
               expect_spanned p (fst (peek p)) "expected abi string literal"
             in
             let v =
               match l.kind with
               | Token.LitString id -> id
               | _ -> Interner.intern p.interner "error"
             in
             { leader = l; value = { kind = v; span = l.span } })
        in
        ext := Some (kw, abi, match_token p [ Token.KwUnsafe ]);
        loop ())
      else (
        ext := Some (kw, None, false);
        if not (check p Token.LBrace) then loop ())
    | Token.KwUnsafe ->
      if fst (peek_at p 1) <> Token.LBrace then (
        un := Some (expect_spanned p Token.KwUnsafe "expected 'unsafe'");
        loop ())
    | _ -> ()
  in
  loop ();
  { is_export = !ex; is_extern = !ext; is_unsafe = !un }

let can_start_expr = function
  | Token.Ident _ | Token.LitInt _ | Token.LitReal _ | Token.LitString _
  | Token.LitRune _ | Token.KwTrue | Token.KwFalse | Token.LParen | Token.LBrack
  | Token.LBrace | Token.Minus | Token.KwNot | Token.Tilde | Token.At
  | Token.KwIf | Token.KwWhile | Token.KwFor | Token.KwMatch | Token.KwReturn
  | Token.KwBreak | Token.KwDefer | Token.KwUnsafe | Token.KwImport
  | Token.KwExtern | Token.KwFn | Token.KwVal | Token.KwVar | Token.KwRecord
  | Token.KwSum | Token.KwAlias | Token.Dot ->
    true
  | _ -> false

let rec parse_expr_with_prec p prec =
  let l = parse_prefix p in
  let rec loop l' =
    let t, _ = peek p in
    let t_prec = Prec.of_token t in
    if
      t_prec > prec
      || (t_prec <> Prec.None && t_prec = prec && Prec.is_right_assoc t)
    then loop (parse_infix p l' t)
    else l'
  in
  loop l

and parse_expr p = parse_expr_with_prec p Prec.None

and parse_prefix p =
  let attrs, m = (parse_attrs p, parse_mods p) in
  let tok, span = peek p in
  match parse_lit_opt p with
  | Some (s, l) -> make_expr (ExprLit { kind = l; span = s }) s
  | None -> (
    match tok with
    | Token.Minus | Token.KwNot | Token.Tilde | Token.At ->
      let op = expect_spanned p tok "expected operator" in
      let r = parse_expr_with_prec p Prec.Unary in
      make_expr (ExprUnaryPrefix (op, r)) (Span.merge span r.span)
    | Token.Ident _ ->
      let n = expect_id p "expected identifier" in
      if check p Token.Dot && fst (peek_at p 1) = Token.LBrace then
        let dot = expect_spanned p Token.Dot "expected '.'" in
        let fs = parse_record_lit_content p in
        make_expr
          (ExprLitRecord { name = Some n; dot; fields = fs })
          (Span.merge n.span fs.rdelim.span)
      else make_expr (ExprIdent n) n.span
    | Token.Dot when fst (peek_at p 1) = Token.LBrace ->
      let dot = expect_spanned p Token.Dot "expected '.'" in
      let fs = parse_record_lit_content p in
      make_expr
        (ExprLitRecord { name = None; dot; fields = fs })
        (Span.merge dot.span fs.rdelim.span)
    | Token.KwIf ->
      let kw = expect_spanned p Token.KwIf "expected 'if'" in
      let c, t = (parse_expr p, parse_expr p) in
      let rec loop () =
        if check p Token.KwElse && fst (peek_at p 1) = Token.KwIf then
          let el = expect_spanned p Token.KwElse "expected 'else'" in
          let if_ = expect_spanned p Token.KwIf "expected 'if'" in
          let co, br = (parse_expr p, parse_expr p) in
          { else_kw = el; if_kw = if_; cond = co; branch = br } :: loop ()
        else []
      in
      let eifs = loop () in
      let els =
        if match_token p [ Token.KwElse ] then
          Some (expect_spanned p Token.KwElse "expected 'else'", parse_expr p)
        else None
      in
      let end_s =
        match els with
        | Some (_, b) -> b.span
        | None -> (
          match List.rev eifs with h :: _ -> h.branch.span | [] -> t.span)
      in
      make_expr
        (ExprIf
           {
             if_kw = kw
           ; cond = c
           ; then_branch = t
           ; else_if_branches = eifs
           ; else_branch = els
           })
        (Span.merge kw.span end_s)
    | Token.KwWhile ->
      let kw = expect_spanned p Token.KwWhile "expected 'while'" in
      let c, b = (parse_expr p, parse_expr p) in
      make_expr
        (ExprWhile { while_kw = kw; cond = c; body = b })
        (Span.merge kw.span b.span)
    | Token.KwFor ->
      let kw = expect_spanned p Token.KwFor "expected 'for'" in
      let pat = parse_pat p in
      let in_kw = expect_spanned p Token.KwIn "expected 'in'" in
      let tgt, b = (parse_expr p, parse_expr p) in
      make_expr
        (ExprFor { for_kw = kw; pat; in_kw; target = tgt; body = b })
        (Span.merge kw.span b.span)
    | Token.KwMatch ->
      let kw = expect_spanned p Token.KwMatch "expected 'match'" in
      let tgt = parse_expr p in
      let cases =
        parse_delimited_list
          p
          Token.LBrace
          (fun p ->
            {
              case_kw = expect_spanned p Token.KwCase "expected 'case'"
            ; pat = parse_pat p
            ; guard =
                (if match_token p [ Token.KwIf ] then
                   Some (parse_preceded p Token.KwIf parse_expr "if")
                 else None)
            ; arrow = expect_spanned p Token.EqGt "expected '=>'"
            ; expr = parse_expr p
            })
          [ Token.Semicolon; Token.Comma ]
          Token.RBrace
          "cases"
      in
      make_expr
        (ExprMatch { match_kw = kw; target = tgt; cases })
        (Span.merge kw.span cases.rdelim.span)
    | Token.LParen ->
      let d = parse_list p Token.LParen parse_expr Token.RParen "tuple" in
      if List.length d.value.elems = 1 && d.value.seps = [] then
        List.hd d.value.elems
      else make_expr (ExprLitTuple d) (Span.merge d.ldelim.span d.rdelim.span)
    | Token.LBrack ->
      let d = parse_list p Token.LBrack parse_expr Token.RBrack "array" in
      make_expr (ExprLitArray d) (Span.merge d.ldelim.span d.rdelim.span)
    | Token.LBrace ->
      let d = parse_block p in
      make_expr (ExprBlock d) (Span.merge d.ldelim.span d.rdelim.span)
    | Token.KwReturn | Token.KwBreak ->
      let kw = expect_spanned p tok "expected keyword" in
      let e =
        if can_start_expr (fst (peek p)) then Some (parse_expr p) else None
      in
      let s =
        match e with Some r -> Span.merge kw.span r.span | None -> kw.span
      in
      make_expr
        (if tok = Token.KwReturn then ExprReturn (kw, e) else ExprBreak (kw, e))
        s
    | Token.KwDefer | Token.KwUnsafe ->
      let kw = expect_spanned p tok "expected keyword" in
      let e = parse_expr p in
      make_expr
        (if tok = Token.KwDefer then ExprDefer (kw, e) else ExprUnsafe (kw, e))
        (Span.merge kw.span e.span)
    | Token.KwImport ->
      let kw = expect_spanned p Token.KwImport "expected 'import'" in
      let path =
        match advance p with
        | Token.Ident id, s -> { kind = id; span = s }
        | _, s ->
          error p "expected identifier" s;
          { kind = Interner.intern p.interner "error"; span = s }
      in
      make_expr (ExprImport (kw, path)) (Span.merge kw.span path.span)
    | Token.KwVal | Token.KwVar ->
      let kw = expect_spanned p tok "expected binding keyword" in
      let pat = parse_pat p in
      let ty_annot = opt p Token.Colon parse_ty in
      let init = parse_preceded p Token.ColonEq parse_expr "init" in
      make_expr
        (ExprBind { modifier = m; kind_kw = kw; pat; ty_annot; init })
        (Span.merge kw.span init.value.span)
    | Token.KwFn ->
      let kw = expect_spanned p Token.KwFn "expected 'fn'" in
      let sig_ = parse_fn_sig p in
      let body = parse_expr p in
      make_expr
        (ExprFn { attrs; modifier = m; fn_kw = kw; sig_; body })
        (Span.merge kw.span body.span)
    | Token.KwRecord | Token.KwSum | Token.KwAlias ->
      let kw = expect_spanned p tok "expected type decl keyword" in
      parse_type_decl p attrs m kw
    | Token.KwExtern ->
      let e = parse_expr_extern p m in
      make_expr e.kind e.span
    | _ ->
      let _, s = advance p in
      error p "expected expression" s;
      make_expr ExprError s)

and parse_expr_extern p modifier =
  let extern_kw = expect_spanned p Token.KwExtern "expected 'extern'" in
  let abi =
    if check_fn p (function Token.LitString _ -> true | _ -> false) then
      Some
        (let l =
           expect_spanned p (fst (peek p)) "expected abi string literal"
         in
         let v =
           match l.kind with
           | Token.LitString id -> id
           | _ -> Interner.intern p.interner "error"
         in
         { leader = l; value = { kind = v; span = l.span } })
    else None
  in
  let sigs =
    parse_delimited_list
      p
      Token.LBrace
      (fun p ->
        {
          fn_kw = expect_spanned p Token.KwFn "expected 'fn'"
        ; sig_ = parse_fn_sig p
        ; semi = expect_spanned p Token.Semicolon "expected ';'"
        })
      [ Token.Semicolon ]
      Token.RBrace
      "sigs"
  in
  {
    kind = ExprExtern { modifier; extern_kw; abi; sigs }
  ; span = Span.merge extern_kw.span sigs.rdelim.span
  }

and parse_record_lit_content p =
  let l = expect_spanned p Token.LBrace "expected '{'" in
  let with_expr =
    if match_token p [ Token.KwWith ] then
      Some (parse_preceded p Token.KwWith parse_expr "with")
    else None
  in
  let fs, sl = (ref [], ref []) in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    fs :=
      {
        name = expect_id p "expected field name"
      ; ty_annot = opt p Token.Colon parse_ty
      ; init = opt p Token.ColonEq parse_expr
      }
      :: !fs;
    if check p Token.Comma then
      let t, s = advance p in
      sl := { kind = t; span = s } :: !sl
  done;
  let r = expect_spanned p Token.RBrace "expected '}'" in
  {
    ldelim = l
  ; value =
      { with_expr; fields = { elems = List.rev !fs; seps = List.rev !sl } }
  ; rdelim = r
  }

and parse_fn_sig p =
  {
    name =
      (if check_fn p (function Token.Ident _ -> true | _ -> false) then
         Some (expect_id p "expected function name")
       else None)
  ; ty_params =
      (if check p Token.Lt then
         Some
           (parse_list
              p
              Token.Lt
              (fun p -> expect_id p "expected type parameter")
              Token.Gt
              "ty_params")
       else None)
  ; params =
      parse_list
        p
        Token.LParen
        (fun p ->
          ({
             is_var =
               (if match_token p [ Token.KwVar ] then
                  Some { kind = Token.KwVar; span = snd (prev p) }
                else None)
           ; name = expect_id p "expected parameter name"
           ; ty_annot = opt p Token.Colon parse_ty
           ; init = opt p Token.ColonEq parse_expr
           }
            : param))
        Token.RParen
        "params"
  ; ret_ty = opt p Token.MinusGt parse_ty
  }

and parse_type_decl p attrs modifier kw =
  let n =
    if check_fn p (function Token.Ident _ -> true | _ -> false) then
      Some (expect_id p "expected type name")
    else None
  in
  let tp =
    if check p Token.Lt then
      Some
        (parse_list
           p
           Token.Lt
           (fun p -> expect_id p "expected type parameter")
           Token.Gt
           "ty_params")
    else None
  in
  match kw.kind with
  | Token.KwRecord ->
    let fs =
      parse_delimited_list
        p
        Token.LBrace
        (fun p ->
          ({
             is_var =
               (if match_token p [ Token.KwVar ] then
                  Some { kind = Token.KwVar; span = snd (prev p) }
                else None)
           ; name = expect_id p "expected field name"
           ; ty_annot = opt p Token.Colon parse_ty
           ; init = opt p Token.ColonEq parse_expr
           }
            : record_field_def))
        [ Token.Semicolon; Token.Comma ]
        Token.RBrace
        "fields"
    in
    make_expr
      (ExprRecord
         {
           attrs
         ; modifier
         ; record_kw = kw
         ; name = n
         ; ty_params = tp
         ; fields = fs
         })
      (Span.merge kw.span fs.rdelim.span)
  | Token.KwSum ->
    let cs =
      parse_delimited_list
        p
        Token.LBrace
        (fun p ->
          {
            case_kw = expect_spanned p Token.KwCase "expected 'case'"
          ; name = expect_id p "expected case name"
          ; ty_args =
              (if check p Token.Lt then
                 Some (parse_list p Token.Lt parse_ty Token.Gt "ty_args")
               else None)
          ; args =
              (if check p Token.LParen then
                 Some
                   (parse_list
                      p
                      Token.LParen
                      (fun p ->
                        if match_token p [ Token.KwVar ] then
                          SumCaseArgParam
                            {
                              is_var =
                                Some { kind = Token.KwVar; span = snd (prev p) }
                            ; name = expect_id p "expected parameter name"
                            ; ty_annot =
                                Some
                                  (parse_preceded p Token.Colon parse_ty "ty")
                            ; init = opt p Token.ColonEq parse_expr
                            }
                        else SumCaseArgTy (parse_ty p))
                      Token.RParen
                      "args")
               else None)
          })
        [ Token.Comma; Token.Semicolon ]
        Token.RBrace
        "cases"
    in
    make_expr
      (ExprSum
         { attrs; modifier; sum_kw = kw; name = n; ty_params = tp; cases = cs })
      (Span.merge kw.span cs.rdelim.span)
  | _ ->
    let init = parse_preceded p Token.ColonEq parse_ty "init" in
    make_expr
      (ExprAlias
         {
           attrs
         ; modifier
         ; alias_kw = kw
         ; name =
             (match n with
             | Some id -> id
             | None -> expect_id p "expected type name")
         ; ty_params = tp
         ; init
         })
      (Span.merge kw.span init.value.span)

and parse_infix p l op =
  let t_sp = expect_spanned p op "expected operator" in
  match op with
  | Token.LParen ->
    let a = parse_list p Token.LParen parse_expr Token.RParen "args" in
    make_expr
      (ExprCall { callee = l; args = a })
      (Span.merge l.span a.rdelim.span)
  | Token.LBrack ->
    let i = expect_spanned p Token.LBrack "expected '['" in
    let e = parse_expr p in
    let r = expect_spanned p Token.RBrack "expected ']'" in
    make_expr
      (ExprIndex { target = l; index = { ldelim = i; value = e; rdelim = r } })
      (Span.merge l.span r.span)
  | Token.Dot ->
    let f = expect_id p "expected field name" in
    make_expr
      (ExprField { target = l; dot = t_sp; field = f })
      (Span.merge l.span f.span)
  | Token.DotCaret | Token.Question ->
    make_expr (ExprUnaryPostfix (l, t_sp)) (Span.merge l.span t_sp.span)
  | Token.LtMinus ->
    let r = parse_expr_with_prec p Prec.Assign in
    make_expr
      (ExprAssign { left = l; op = t_sp; right = r })
      (Span.merge l.span r.span)
  | Token.DotDot | Token.DotDotLt ->
    let r =
      if can_start_expr (fst (peek p)) then
        Some (parse_expr_with_prec p (Prec.of_token op))
      else None
    in
    make_expr
      (ExprRange { left = l; op = t_sp; right = r })
      (Span.merge l.span (match r with Some e -> e.span | _ -> t_sp.span))
  | _ ->
    let r = parse_expr_with_prec p (Prec.of_token op) in
    make_expr
      (ExprBinary { left = l; op = t_sp; right = r })
      (Span.merge l.span r.span)

and parse_block p =
  let l = expect_spanned p Token.LBrace "expected '{'" in
  let st, res = (ref [], ref None) in
  while (not (check p Token.RBrace)) && not (is_at_end p) do
    let e = parse_expr p in
    if check p Token.Semicolon then
      let semi = expect_spanned p Token.Semicolon "expected ';'" in
      st := make_stmt_expr e semi :: !st
    else if check p Token.RBrace then res := Some e
    else
      let semi = expect_spanned p Token.Semicolon "expected ';'" in
      st := make_stmt_expr e semi :: !st
  done;
  {
    ldelim = l
  ; value = { stmts = List.rev !st; result_expr = !res }
  ; rdelim = expect_spanned p Token.RBrace "expected '}'"
  }

and parse_stmt p =
  let e = parse_expr p in
  let s = expect_spanned p Token.Semicolon "expected ';'" in
  make_stmt_expr e s

let try_parse tokens _ _ interner =
  let p = create tokens [||] 0 interner in
  let ss = ref [] in
  while not (is_at_end p) do
    ss := parse_stmt p :: !ss
  done;
  if has_errors p then Error p.diag else Ok (List.rev !ss)
