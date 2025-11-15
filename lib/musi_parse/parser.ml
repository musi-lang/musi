open Musi_basic
open Musi_lex

type t = {
    stream : Token.stream
  ; interner : Interner.t
  ; diags : Diagnostic.bag ref
}

(* === PRECEDENCE === *)

type prec =
  | PrecNone
  | PrecAssign
  | PrecOr
  | PrecXor
  | PrecAnd
  | PrecEquality
  | PrecComparison
  | PrecRange
  | PrecBitShift
  | PrecAdditive
  | PrecMultiply
  | PrecUnary
  | PrecPostfix

let prec_to_int = function
  | PrecNone -> 0
  | PrecAssign -> 1
  | PrecOr -> 2
  | PrecXor -> 3
  | PrecAnd -> 4
  | PrecEquality -> 5
  | PrecComparison -> 6
  | PrecRange -> 7
  | PrecBitShift -> 8
  | PrecAdditive -> 9
  | PrecMultiply -> 10
  | PrecUnary -> 11
  | PrecPostfix -> 12

let token_prec = function
  | Token.LtMinus -> PrecAssign
  | Token.KwOr -> PrecOr
  | Token.KwXor -> PrecXor
  | Token.KwAnd -> PrecAnd
  | Token.Eq | Token.EqSlashEq -> PrecEquality
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.KwAs | Token.KwIs ->
    PrecComparison
  | Token.DotDot | Token.DotDotLt -> PrecRange
  | Token.KwShl | Token.KwShr -> PrecBitShift
  | Token.Plus | Token.Minus -> PrecAdditive
  | Token.Star | Token.Slash | Token.KwMod -> PrecMultiply
  | Token.Dot | Token.LBrack | Token.LBrace | Token.LParen | Token.Question
  | Token.Bang ->
    PrecPostfix
  | _ -> PrecNone

(* === HELPERS === *)

let make tokens interner =
  let stream = Token.make_stream tokens in
  { stream; interner; diags = ref Diagnostic.empty_bag }

let curr t = Token.curr t.stream
let advance t = Token.advance t.stream
let at_end t = Token.at_end t.stream || Token.(curr t.stream).kind = Eof

let skip_trivia t =
  let rec loop () =
    match (curr t).kind with
    | Token.Whitespace | Token.Newline | Token.LineComment _
    | Token.BlockComment _ ->
      advance t;
      loop ()
    | _ -> ()
  in
  loop ()

let error t msg =
  let tok = curr t in
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg tok.span)

(* === COMBINATORS === *)

let expect kind t =
  skip_trivia t;
  let tok = curr t in
  if tok.kind = kind then (
    advance t;
    tok)
  else
    let expected = Token.show_kind t.interner kind in
    let found = Token.show_kind t.interner tok.kind in
    error t (Printf.sprintf "expected '%s', found '%s'" expected found);
    tok

let is_closing_delim t =
  let k = (curr t).kind in
  k = Token.RParen || k = Token.RBrack || k = Token.RBrace

let sep_by_opt sep p t =
  skip_trivia t;
  if is_closing_delim t then []
  else
    let rec loop acc =
      let item = p t in
      let acc' = item :: acc in
      skip_trivia t;
      if (curr t).kind = sep then (
        advance t;
        skip_trivia t;
        if is_closing_delim t then List.rev acc' else loop acc')
      else List.rev acc'
    in
    loop []

let is_empty_delim t delim =
  skip_trivia t;
  (curr t).kind = delim

(* === PRATT PARSING === *)

let parse_expr_ref = ref (fun _ _ -> failwith "Parser.parse_expr not ready")

let parse_expr_infix_ref =
  ref (fun _ _ _ -> failwith "Parser.parse_expr_infix not ready")

let parse_ty_ref = ref (fun _ -> failwith "Parser.parse_ty not ready")
let parse_pat_ref = ref (fun _ -> failwith "Parser.parse_pat not ready")
let parse_expr t bp = !parse_expr_ref t bp
let parse_expr_infix t left bp = !parse_expr_infix_ref t left bp
let parse_ty t = !parse_ty_ref t
let parse_pat t = !parse_pat_ref t

let expect_ident t err_msg =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident name ->
    advance t;
    name
  | _ ->
    error t err_msg;
    Interner.empty_name t.interner

let parse_typed_field ~allow_modifiers t =
  skip_trivia t;
  let is_var =
    if allow_modifiers && (curr t).kind = Token.KwVar then (
      advance t;
      true)
    else false
  in
  let fname = expect_ident t "expected field name" in
  let _ = expect Token.Colon t in
  { Node.fname; fty = parse_ty t; is_var }

let field_name t =
  skip_trivia t;
  let tok = curr t in
  match tok.kind with
  | Token.Ident name ->
    advance t;
    Some (name, tok.span)
  | Token.LitNumber s ->
    advance t;
    Some (Interner.intern t.interner s, tok.span)
  | _ -> None

let field_name_after_dot t err_msg =
  advance t;
  let name_tok = curr t in
  match name_tok.kind with
  | Token.Ident name ->
    advance t;
    (name, name_tok.span)
  | _ ->
    error t err_msg;
    (Interner.empty_name t.interner, name_tok.span)

let parse_postfix t left bp optional op_kind =
  match op_kind with
  | Token.Dot -> (
    match field_name t with
    | Some (name, span) ->
      let field = Node.make_expr (Node.ExprField (left, name, optional)) span in
      parse_expr_infix t field bp
    | None ->
      error
        t
        (if optional then "expected field name after '?.'"
         else "expected field name or number after '.'");
      parse_expr_infix t left bp)
  | Token.LBrack ->
    let index = parse_expr t PrecNone in
    let _ = expect Token.RBrack t in
    let idx =
      Node.make_expr (Node.ExprIndex (left, index, optional)) left.Node.span
    in
    parse_expr_infix t idx bp
  | Token.LParen ->
    let args = sep_by_opt Token.Comma (fun t -> parse_expr t PrecNone) t in
    let _ = expect Token.RParen t in
    let call =
      Node.make_expr (Node.ExprCall (left, args, optional)) left.Node.span
    in
    parse_expr_infix t call bp
  | _ -> parse_expr_infix t left bp

let parse_ty_array t (tok : Token.t) =
  let elem_ty = parse_ty t in
  let _ = expect Token.RBrack t in
  Node.make_ty (Node.TyArray elem_ty) tok.span

let parse_ty_tuple_or_fn t (tok : Token.t) =
  let types =
    if is_empty_delim t Token.RParen then (
      advance t;
      [])
    else
      let first = parse_ty t in
      skip_trivia t;
      match (curr t).kind with
      | Token.Comma ->
        advance t;
        let rest = sep_by_opt Token.Comma parse_ty t in
        let _ = expect Token.RParen t in
        first :: rest
      | Token.RParen ->
        advance t;
        [ first ]
      | _ ->
        error t "expected ',' or ')' in tuple or fn type";
        let _ = expect Token.RParen t in
        [ first ]
  in
  skip_trivia t;
  if (curr t).kind = Token.MinusGt then (
    advance t;
    Node.make_ty (Node.TyProc (types, Some (parse_ty t))) tok.span)
  else
    match types with
    | [] -> Node.make_ty (Node.TyTuple []) tok.span
    | [ single ] -> single
    | _ -> Node.make_ty (Node.TyTuple types) tok.span

let parse_ty_record t (tok : Token.t) =
  let fields =
    sep_by_opt
      Token.Comma
      (fun t -> parse_typed_field ~allow_modifiers:false t)
      t
  in
  let _ = expect Token.RBrace t in
  Node.make_ty (Node.TyRecord fields) tok.span

let parse_ty_base t =
  skip_trivia t;
  let tok = curr t in
  advance t;
  match tok.kind with
  | Token.Ident name ->
    let ty = Node.make_ty (Node.TyNamed name) tok.span in
    skip_trivia t;
    if (curr t).kind = Token.Lt then (
      advance t;
      let args = sep_by_opt Token.Comma parse_ty t in
      let _ = expect Token.Gt t in
      Node.make_ty (Node.TyApp (ty, args)) tok.span)
    else ty
  | Token.LBrack -> parse_ty_array t tok
  | Token.LParen -> parse_ty_tuple_or_fn t tok
  | Token.LBrace -> parse_ty_record t tok
  | _ ->
    let found = Token.show_kind t.interner tok.kind in
    error t (Printf.sprintf "expected type, found '%s'" found);
    Node.make_ty Node.TyError tok.span

let parse_ty_impl t =
  let base_ty = parse_ty_base t in
  skip_trivia t;
  if (curr t).kind = Token.Question then (
    advance t;
    Node.make_ty (Node.TyOptional base_ty) base_ty.Node.span)
  else base_ty

let parse_optional_expr t =
  skip_trivia t;
  match (curr t).kind with
  | Token.Semi | Token.RBrace | Token.Eof -> None
  | _ -> Some (parse_expr t PrecNone)

let parse_optional_ty_annot t =
  skip_trivia t;
  if (curr t).kind = Token.Colon then (
    advance t;
    Some (parse_ty t))
  else None

let parse_ty_params t =
  skip_trivia t;
  if (curr t).kind = Token.Lt then (
    advance t;
    let params =
      sep_by_opt
        Token.Comma
        (fun t -> expect_ident t "expected type parameter name")
        t
    in
    let _ = expect Token.Gt t in
    params)
  else []

let parse_attrib_arg t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LitString name ->
    advance t;
    name
  | _ ->
    error t "expected string argument";
    Interner.empty_name t.interner

let parse_attrib_param t =
  let key = expect_ident t "expected parameter name" in
  let _ = expect Token.ColonEq t in
  let value = parse_attrib_arg t in
  (key, value)

let parse_attrib t =
  let _ = expect Token.At t in
  let aname = expect_ident t "expected attribute name" in
  skip_trivia t;
  if (curr t).kind <> Token.LParen then { Node.aname; aargs = []; aparams = [] }
  else (
    advance t;
    skip_trivia t;
    let aargs, aparams =
      if (curr t).kind = Token.RParen then ([], [])
      else if
        (match (curr t).kind with Token.Ident _ -> true | _ -> false)
        && (Token.peek t.stream).kind = Token.ColonEq
      then ([], sep_by_opt Token.Comma parse_attrib_param t)
      else (sep_by_opt Token.Comma parse_attrib_arg t, [])
    in
    let _ = expect Token.RParen t in
    { Node.aname; aargs; aparams })

let parse_modifiers t =
  let rec loop (mods : Node.modifiers) =
    skip_trivia t;
    match (curr t).kind with
    | Token.At ->
      let attr = parse_attrib t in
      loop { mods with attribs = attr :: mods.attribs }
    | Token.KwExport ->
      advance t;
      loop { mods with is_exported = true }
    | Token.KwExtern ->
      advance t;
      skip_trivia t;
      let abi =
        match (curr t).kind with
        | Token.LitString name ->
          advance t;
          Some name
        | _ -> None
      in
      loop { mods with is_extern = true; abi }
    | _ -> { mods with attribs = List.rev mods.attribs }
  in
  loop Node.empty_modifiers

let parse_expr_if t (tok : Token.t) =
  let cond = parse_expr t PrecNone in
  let _ = expect Token.KwThen t in
  let then_branch = parse_expr t PrecNone in
  skip_trivia t;
  let else_branch =
    if (curr t).kind = Token.KwElse then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  Node.make_expr (Node.ExprIf (cond, then_branch, else_branch)) tok.span

let parse_expr_block t (tok : Token.t) =
  let rec loop acc =
    skip_trivia t;
    if (curr t).kind = Token.RBrace then List.rev acc
    else
      let e = parse_expr t PrecNone in
      skip_trivia t;
      if (curr t).kind = Token.Semi then (
        advance t;
        loop (e :: acc))
      else if (curr t).kind = Token.RBrace then List.rev (e :: acc)
      else (
        error t "expected ';' or '}' in block expression";
        List.rev (e :: acc))
  in
  let _ = expect Token.RBrace t in
  Node.make_expr (Node.ExprBlock (loop [])) tok.span

let parse_expr_lit_record t (tok : Token.t) =
  let parse_field t =
    skip_trivia t;
    match (curr t).kind with
    | Token.Dot ->
      let name, _ = field_name_after_dot t "expected field name after '.'" in
      let _ = expect Token.ColonEq t in
      (name, parse_expr t PrecNone)
    | Token.Ident name ->
      advance t;
      skip_trivia t;
      if (curr t).kind = Token.ColonEq then (
        advance t;
        (name, parse_expr t PrecNone))
      else (name, Node.make_expr (Node.ExprIdent name) tok.span)
    | _ ->
      error t "expected field name or '.' in record literal";
      (Interner.empty_name t.interner, Node.make_expr Node.ExprError tok.span)
  in
  let fields = sep_by_opt Token.Comma parse_field t in
  let _ = expect Token.RBrace t in
  Node.make_expr (Node.ExprLiteral (Node.LitRecord fields)) tok.span

let parse_expr_while t (tok : Token.t) =
  let cond = parse_expr t PrecNone in
  let _ = expect Token.KwDo t in
  Node.make_expr (Node.ExprWhile (cond, parse_expr t PrecNone)) tok.span

let parse_expr_binding t (tok : Token.t) is_var mods =
  let pat = parse_pat t in
  let ty_params = parse_ty_params t in
  let ty_opt = parse_optional_ty_annot t in
  let _ = expect Token.ColonEq t in
  Node.make_expr
    (Node.ExprBinding
       (is_var, ty_params, pat, ty_opt, parse_expr t PrecNone, mods))
    tok.span

let parse_expr_for t (tok : Token.t) =
  let pat = parse_pat t in
  let _ = expect Token.KwIn t in
  let iterable = parse_expr t PrecNone in
  let _ = expect Token.KwDo t in
  Node.make_expr (Node.ExprFor (pat, iterable, parse_expr t PrecNone)) tok.span

let parse_expr_do t (tok : Token.t) =
  let body = parse_expr t PrecNone in
  skip_trivia t;
  let cond =
    if (curr t).kind = Token.KwWhile then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  Node.make_expr (Node.ExprDo (body, cond)) tok.span

let parse_delimited_exprs t close_delim =
  if is_empty_delim t close_delim then (
    advance t;
    [])
  else
    let first = parse_expr t PrecNone in
    skip_trivia t;
    let rest =
      if (curr t).kind = Token.Comma then (
        advance t;
        sep_by_opt Token.Comma (fun t -> parse_expr t PrecNone) t)
      else []
    in
    let _ = expect close_delim t in
    first :: rest

let parse_expr_array t (tok : Token.t) =
  let exprs = parse_delimited_exprs t Token.RBrack in
  Node.make_expr (Node.ExprArray exprs) tok.span

let parse_expr_tuple t (tok : Token.t) =
  let exprs = parse_delimited_exprs t Token.RParen in
  match exprs with
  | [ single ] -> single
  | _ -> Node.make_expr (Node.ExprTuple exprs) tok.span

let variant_data t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LParen ->
    advance t;
    let types = sep_by_opt Token.Comma parse_ty t in
    let _ = expect Token.RParen t in
    Node.VTuple types
  | Token.LBrace ->
    advance t;
    let fields =
      sep_by_opt
        Token.Comma
        (fun t -> parse_typed_field ~allow_modifiers:false t)
        t
    in
    let _ = expect Token.RBrace t in
    Node.VRecord fields
  | _ -> Node.VUnit

let parse_variant t =
  let _ = expect Token.KwCase t in
  let vname = expect_ident t "expected variant name after 'case'" in
  let vdata = variant_data t in
  { Node.vname; vdata }

let parse_expr_choice t (tok : Token.t) mods ty_params =
  let _ = expect Token.LBrace t in
  let variants = sep_by_opt Token.Comma parse_variant t in
  let _ = expect Token.RBrace t in
  Node.make_expr (Node.ExprChoice (ty_params, variants, mods)) tok.span

let parse_match_case t =
  let _ = expect Token.KwCase t in
  let cpat = parse_pat t in
  skip_trivia t;
  let guard =
    if (curr t).kind = Token.KwIf then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  let _ = expect Token.MinusGt t in
  { Node.cpat; guard; body = parse_expr t PrecNone }

let parse_expr_match t (tok : Token.t) =
  let scrutinee = parse_expr t PrecNone in
  let _ = expect Token.KwWith t in
  let _ = expect Token.LBrace t in
  let cases = sep_by_opt Token.Comma parse_match_case t in
  let _ = expect Token.RBrace t in
  Node.make_expr (Node.ExprMatch (scrutinee, cases)) tok.span

let parse_param t =
  skip_trivia t;
  let is_var = (curr t).kind = Token.KwVar in
  if is_var then advance t;
  let pname = expect_ident t "expected parameter name" in
  { Node.pname; pty = parse_optional_ty_annot t; is_var }

let parse_expr_record t (tok : Token.t) mods ty_params =
  let _ = expect Token.LBrace t in
  let fields =
    sep_by_opt
      Token.Comma
      (fun t -> parse_typed_field ~allow_modifiers:true t)
      t
  in
  let _ = expect Token.RBrace t in
  Node.make_expr (Node.ExprRecord (ty_params, fields, mods)) tok.span

let parse_expr_fn t (tok : Token.t) mods ty_params =
  let _ = expect Token.LParen t in
  let params = sep_by_opt Token.Comma parse_param t in
  let _ = expect Token.RParen t in
  skip_trivia t;
  let ret_ty =
    if (curr t).kind = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  skip_trivia t;
  let body =
    if (curr t).kind = Token.LBrace then (
      let brace_tok = curr t in
      advance t;
      Some (parse_expr_block t brace_tok))
    else None
  in
  Node.make_expr (Node.ExprFn (ty_params, params, ret_ty, body, mods)) tok.span

let parse_expr_block_or_lit_record t tok =
  skip_trivia t;
  match (curr t).kind with
  | Token.Dot -> parse_expr_lit_record t tok
  | Token.RBrace ->
    advance t;
    Node.make_expr (Node.ExprBlock []) tok.span
  | Token.Ident _ -> (
    let next_tok = Token.peek t.stream in
    match next_tok.kind with
    | Token.ColonEq | Token.Comma -> parse_expr_lit_record t tok
    | _ -> parse_expr_block t tok)
  | _ -> parse_expr_block t tok

let parse_expr_decl t (tok : Token.t) mods =
  skip_trivia t;
  let name_opt =
    match (curr t).kind with
    | Token.Ident name ->
      advance t;
      Some name
    | _ -> None
  in
  let ty_params = parse_ty_params t in
  let construct =
    match tok.kind with
    | Token.KwChoice -> parse_expr_choice t tok mods ty_params
    | Token.KwRecord -> parse_expr_record t tok mods ty_params
    | Token.KwFn -> parse_expr_fn t tok mods ty_params
    | _ -> failwith "unreachable"
  in
  match name_opt with
  | Some name ->
    let pat = Node.make_pat (Node.PatIdent (name, false)) tok.span in
    Node.make_expr
      (Node.ExprBinding (false, [], pat, None, construct, Node.empty_modifiers))
      tok.span
  | None -> construct

let parse_expr_prefix t =
  let mods = parse_modifiers t in
  skip_trivia t;
  if mods.is_extern && (curr t).kind = Token.LBrace then (
    (* `extern "ABI" { ... }` - block w/ 'extern' applied to each decl *)
    let tok = curr t in
    advance t;
    parse_expr_block t tok)
  else
    let tok = curr t in
    advance t;
    match tok.kind with
    | Token.LitNumber s ->
      let lit =
        if
          String.contains s '.' || String.contains s 'e'
          || String.contains s 'E'
        then Node.LitBin s
        else Node.LitInt s
      in
      Node.make_expr (Node.ExprLiteral lit) tok.span
    | Token.LitString name ->
      Node.make_expr (Node.ExprLiteral (Node.LitStr name)) tok.span
    | Token.LitRune code ->
      Node.make_expr (Node.ExprLiteral (Node.LitRune code)) tok.span
    | Token.KwTrue | Token.KwFalse ->
      Node.make_expr
        (Node.ExprLiteral (Node.LitBool (tok.kind = Token.KwTrue)))
        tok.span
    | Token.Ident name -> Node.make_expr (Node.ExprIdent name) tok.span
    | Token.KwReturn ->
      Node.make_expr (Node.ExprReturn (parse_optional_expr t)) tok.span
    | Token.KwBreak ->
      Node.make_expr (Node.ExprBreak (parse_optional_expr t)) tok.span
    | Token.KwContinue -> Node.make_expr Node.ExprContinue tok.span
    | Token.KwDefer ->
      Node.make_expr (Node.ExprDefer (parse_expr t PrecUnary)) tok.span
    | Token.KwTry ->
      Node.make_expr (Node.ExprTry (parse_expr t PrecUnary)) tok.span
    | Token.KwIf -> parse_expr_if t tok
    | Token.KwMatch -> parse_expr_match t tok
    | Token.KwWhile -> parse_expr_while t tok
    | Token.KwDo -> parse_expr_do t tok
    | Token.KwVal -> parse_expr_binding t tok false mods
    | Token.KwVar -> parse_expr_binding t tok true mods
    | Token.KwFor -> parse_expr_for t tok
    | Token.KwChoice | Token.KwRecord | Token.KwFn -> parse_expr_decl t tok mods
    | Token.LBrace -> parse_expr_block_or_lit_record t tok
    | Token.LBrack -> parse_expr_array t tok
    | Token.LParen -> parse_expr_tuple t tok
    | Token.Minus | Token.KwNot ->
      let operand = parse_expr t PrecUnary in
      Node.make_expr (Node.ExprUnary (tok.kind, operand)) tok.span
    | _ ->
      let found = Token.show_kind t.interner tok.kind in
      error t (Printf.sprintf "unexpected token '%s' in expression" found);
      Node.make_expr Node.ExprError tok.span

let parse_expr_infix_impl t left bp =
  skip_trivia t;
  let op_tok = curr t in
  let op_prec = token_prec op_tok.kind in
  if prec_to_int op_prec <= prec_to_int bp then left
  else (
    advance t;
    let cont_with ekind =
      parse_expr_infix t (Node.make_expr ekind op_tok.span) bp
    in
    match op_tok.kind with
    | Token.Dot | Token.LBrack | Token.LParen ->
      parse_postfix t left bp false op_tok.kind
    | Token.LBrace ->
      let lit = parse_expr_lit_record t op_tok in
      parse_expr_infix t lit bp
    | Token.Bang -> cont_with (Node.ExprUnwrap left)
    | Token.Question ->
      skip_trivia t;
      let next = (curr t).kind in
      if next = Token.Dot || next = Token.LBrack || next = Token.LParen then (
        advance t;
        parse_postfix t left bp true next)
      else (
        error t "expected '.', '[', or '(' after '?'";
        parse_expr_infix t left bp)
    | Token.KwAs -> cont_with (Node.ExprCast (left, parse_ty t))
    | Token.KwIs -> cont_with (Node.ExprTest (left, parse_ty t))
    | Token.DotDot | Token.DotDotLt ->
      let right = parse_expr t op_prec in
      cont_with (Node.ExprRange (left, right, op_tok.kind = Token.DotDot))
    | Token.LtMinus | _ ->
      let right = parse_expr t op_prec in
      let ekind =
        if op_tok.kind = Token.LtMinus then Node.ExprAssign (left, right)
        else Node.ExprBinary (op_tok.kind, left, right)
      in
      cont_with ekind)

let parse_expr_impl t bp =
  let left = parse_expr_prefix t in
  parse_expr_infix t left bp

let parse_pat_choice t (tok : Token.t) =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident name ->
    advance t;
    skip_trivia t;
    let inner =
      if (curr t).kind = Token.LParen then (
        advance t;
        let pat = parse_pat t in
        let _ = expect Token.RParen t in
        Some pat)
      else None
    in
    Node.make_pat (Node.PatChoice (name, inner)) tok.span
  | _ ->
    error t "expected variant name after '.' in choice pattern";
    Node.make_pat Node.PatError tok.span

let parse_pat_array t (tok : Token.t) =
  let items, rest =
    if is_empty_delim t Token.RBrack then (
      advance t;
      ([], None))
    else
      let items = sep_by_opt Token.Comma parse_pat t in
      skip_trivia t;
      let rest =
        if (curr t).kind = Token.DotDot then (
          advance t;
          let name = expect_ident t "expected identifier after '..'" in
          Some (Node.make_pat (Node.PatRest name) tok.span))
        else None
      in
      let _ = expect Token.RBrack t in
      (items, rest)
  in
  Node.make_pat (Node.PatArray (items, rest)) tok.span

let parse_pat_record t (tok : Token.t) =
  let parse_field t =
    skip_trivia t;
    match (curr t).kind with
    | Token.Dot ->
      let name, _ =
        field_name_after_dot t "expected field name after '.' in record pattern"
      in
      let _ = expect Token.ColonEq t in
      (name, parse_pat t)
    | _ ->
      error t "expected '.' in record pattern";
      (Interner.empty_name t.interner, Node.make_pat Node.PatError tok.span)
  in
  let fields = sep_by_opt Token.Comma parse_field t in
  let _ = expect Token.RBrace t in
  Node.make_pat (Node.PatRecord fields) tok.span

let parse_pat_tuple t (tok : Token.t) =
  let pats =
    if is_empty_delim t Token.RParen then (
      advance t;
      [])
    else
      let first = parse_pat t in
      skip_trivia t;
      match (curr t).kind with
      | Token.Comma ->
        advance t;
        let rest = sep_by_opt Token.Comma parse_pat t in
        let _ = expect Token.RParen t in
        first :: rest
      | Token.RParen ->
        advance t;
        [ first ]
      | _ ->
        error t "expected ',' or ')' in tuple pattern";
        let _ = expect Token.RParen t in
        [ first ]
  in
  match pats with
  | [ single ] -> single
  | _ -> Node.make_pat (Node.PatTuple pats) tok.span

let parse_pat_impl t =
  skip_trivia t;
  let tok = curr t in
  let is_var = tok.kind = Token.KwVar in
  if is_var then advance t;
  let tok = if is_var then curr t else tok in
  advance t;
  match tok.kind with
  | Token.KwVar ->
    let name = expect_ident t "expected identifier after 'var' in pattern" in
    Node.make_pat (Node.PatBinding name) tok.span
  | Token.Underscore -> Node.make_pat Node.PatWild tok.span
  | Token.Ident name -> Node.make_pat (Node.PatIdent (name, is_var)) tok.span
  | Token.Dot -> parse_pat_choice t tok
  | Token.LParen -> parse_pat_tuple t tok
  | Token.LBrack -> parse_pat_array t tok
  | Token.LBrace -> parse_pat_record t tok
  | Token.LitNumber _ | Token.LitString _ | Token.LitRune _ | Token.KwTrue
  | Token.KwFalse ->
    Node.make_pat (Node.PatLiteral tok.kind) tok.span
  | _ ->
    let found = Token.show_kind t.interner tok.kind in
    error t (Printf.sprintf "expected pattern, found '%s'" found);
    Node.make_pat Node.PatError tok.span

let () =
  parse_expr_ref := parse_expr_impl;
  parse_expr_infix_ref := parse_expr_infix_impl;
  parse_ty_ref := parse_ty_impl;
  parse_pat_ref := parse_pat_impl

(* === REC-DESC PARSING === *)

let parse_ident_list t =
  sep_by_opt
    Token.Comma
    (fun t ->
      skip_trivia t;
      let tok = curr t in
      let name = expect_ident t "expected identifier" in
      skip_trivia t;
      if (curr t).kind = Token.KwAs then (
        advance t;
        ignore (expect_ident t "expected identifier after 'as'"));
      (name, tok.span))
    t

let parse_named_or_namespace_spec ~kw ~named_ctor ~namespace_ctor t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LBrace ->
    advance t;
    let names = parse_ident_list t in
    let _ = expect Token.RBrace t in
    named_ctor names
  | Token.Star ->
    advance t;
    let name =
      if kw = "import" then
        let _ = expect Token.KwAs t in
        expect_ident t "expected identifier after 'as'"
      else Interner.intern t.interner "*"
    in
    namespace_ctor name
  | _ ->
    error t (Printf.sprintf "expected '{' or '*' after '%s'" kw);
    named_ctor []

let parse_stmt_import t (tok : Token.t) =
  let mods = parse_modifiers t in
  let spec =
    parse_named_or_namespace_spec
      ~kw:"import"
      ~named_ctor:(fun names -> Node.ImportNamed names)
      ~namespace_ctor:(fun name -> Node.ImportNamespace name)
      t
  in
  let _ = expect Token.KwFrom t in
  skip_trivia t;
  match (curr t).kind with
  | Token.LitString path ->
    advance t;
    Node.make_stmt (Node.StmtImport (spec, path, mods)) tok.span
  | _ ->
    error t "expected string path after 'from'";
    Node.make_stmt Node.StmtError tok.span

let parse_stmt_export t (tok : Token.t) =
  let spec =
    parse_named_or_namespace_spec
      ~kw:"export"
      ~named_ctor:(fun names -> Node.ExportNamed names)
      ~namespace_ctor:(fun name -> Node.ExportNamespace name)
      t
  in
  skip_trivia t;
  let path =
    if (curr t).kind = Token.KwFrom then (
      advance t;
      skip_trivia t;
      match (curr t).kind with
      | Token.LitString p ->
        advance t;
        Some p
      | _ ->
        error t "expected string path after 'from' keyword";
        None)
    else None
  in
  Node.make_stmt (Node.StmtExport (spec, path)) tok.span

let parse_stmt t =
  skip_trivia t;
  let tok = curr t in
  match tok.kind with
  | Token.KwImport ->
    advance t;
    parse_stmt_import t tok
  | Token.KwExport -> (
    advance t;
    skip_trivia t;
    let next_tok = curr t in
    match next_tok.kind with
    | Token.LBrace | Token.Star -> parse_stmt_export t tok
    | _ ->
      Node.make_stmt (Node.StmtExpr (parse_expr t PrecNone, false)) Span.dummy)
  | _ ->
    Node.make_stmt (Node.StmtExpr (parse_expr t PrecNone, false)) Span.dummy

let parse t =
  let rec loop acc =
    skip_trivia t;
    if at_end t then List.rev acc
    else
      let s = parse_stmt t in
      skip_trivia t;
      if at_end t then List.rev (s :: acc)
      else
        let _ = expect Token.Semi t in
        loop (s :: acc)
  in
  (loop [], !(t.diags))
