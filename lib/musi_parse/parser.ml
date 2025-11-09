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

let get_tok kind t =
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

let sep_by_opt sep p t =
  skip_trivia t;
  if
    (curr t).kind = Token.RParen
    || (curr t).kind = Token.RBrack
    || (curr t).kind = Token.RBrace
  then []
  else
    let rec loop acc =
      let item = p t in
      let acc' = item :: acc in
      skip_trivia t;
      if (curr t).kind = sep then (
        advance t;
        skip_trivia t;
        if
          (curr t).kind = Token.RParen
          || (curr t).kind = Token.RBrack
          || (curr t).kind = Token.RBrace
        then List.rev acc'
        else loop acc')
      else List.rev acc'
    in
    loop []

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

let field_name t =
  skip_trivia t;
  let tok = curr t in
  match tok.kind with
  | Token.Ident name ->
    advance t;
    Some (name, tok.span)
  | Token.LitNum s ->
    advance t;
    let name = Interner.intern t.interner s in
    Some (name, tok.span)
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

let expr_field_opt t left bp optional =
  match field_name t with
  | Some (name, span) ->
    let field = Node.make_expr (Node.ExprField (left, name, optional)) span in
    parse_expr_infix t field bp
  | None ->
    error
      t
      (if optional then "expected field name after '?.'"
       else "expected field name or number after '.'");
    parse_expr_infix t left bp

let expr_index_opt t left bp optional =
  let index = parse_expr t PrecNone in
  let _ = get_tok Token.RBrack t in
  let idx =
    Node.make_expr (Node.ExprIndex (left, index, optional)) left.Node.span
  in
  parse_expr_infix t idx bp

let expr_call_opt t left bp optional =
  let args = sep_by_opt Token.Comma (fun t -> parse_expr t PrecNone) t in
  let _ = get_tok Token.RParen t in
  let call =
    Node.make_expr (Node.ExprCall (left, args, optional)) left.Node.span
  in
  parse_expr_infix t call bp

let expr_optional t left bp =
  skip_trivia t;
  match (curr t).kind with
  | Token.Dot ->
    advance t;
    expr_field_opt t left bp true
  | Token.LBrack ->
    advance t;
    expr_index_opt t left bp true
  | Token.LParen ->
    advance t;
    expr_call_opt t left bp true
  | _ ->
    error t "expected '.', '[', or '(' after '?'";
    parse_expr_infix t left bp

let ty_infix t left op_tok make_node bp =
  let ty = parse_ty t in
  let node = Node.make_expr (make_node (left, ty)) op_tok.Token.span in
  parse_expr_infix t node bp

let ty_array t (tok : Token.t) =
  let elem_ty = parse_ty t in
  let _ = get_tok Token.RBrack t in
  Node.make_ty (Node.TyArray elem_ty) tok.span

let ty_tuple_or_proc t (tok : Token.t) =
  skip_trivia t;
  if (curr t).kind = Token.RParen then (
    advance t;
    skip_trivia t;
    if (curr t).kind = Token.MinusGt then (
      advance t;
      Node.make_ty (Node.TyProc ([], Some (parse_ty t))) tok.span)
    else Node.make_ty (Node.TyTuple []) tok.span)
  else
    let first = parse_ty t in
    skip_trivia t;
    match (curr t).kind with
    | Token.Comma ->
      advance t;
      let rest = sep_by_opt Token.Comma parse_ty t in
      let _ = get_tok Token.RParen t in
      skip_trivia t;
      if (curr t).kind = Token.MinusGt then (
        advance t;
        Node.make_ty (Node.TyProc (first :: rest, Some (parse_ty t))) tok.span)
      else Node.make_ty (Node.TyTuple (first :: rest)) tok.span
    | Token.RParen ->
      advance t;
      skip_trivia t;
      if (curr t).kind = Token.MinusGt then (
        advance t;
        Node.make_ty (Node.TyProc ([ first ], Some (parse_ty t))) tok.span)
      else first
    | _ ->
      error t "expected ',' or ')' in tuple/proc type";
      let _ = get_tok Token.RParen t in
      first

let ty_record t (tok : Token.t) =
  let parse_field t =
    skip_trivia t;
    match (curr t).kind with
    | Token.Ident fname ->
      advance t;
      let _ = get_tok Token.Colon t in
      { Node.fname; fty = parse_ty t; is_var = false; is_weak = false }
    | _ ->
      error t "expected field name in record type";
      {
        Node.fname = Interner.empty_name t.interner
      ; fty = Node.make_ty Node.TyError tok.span
      ; is_var = false
      ; is_weak = false
      }
  in
  let fields = sep_by_opt Token.Comma parse_field t in
  let _ = get_tok Token.RBrace t in
  Node.make_ty (Node.TyRecord fields) tok.span

let ty_impl t =
  skip_trivia t;
  let tok = curr t in
  advance t;
  let base_ty =
    match tok.kind with
    | Token.Ident name ->
      let ty = Node.make_ty (Node.TyNamed name) tok.span in
      skip_trivia t;
      if (curr t).kind = Token.Lt then (
        advance t;
        let args = sep_by_opt Token.Comma parse_ty t in
        let _ = get_tok Token.Gt t in
        Node.make_ty (Node.TyApp (ty, args)) tok.span)
      else ty
    | Token.LBrack -> ty_array t tok
    | Token.LParen -> ty_tuple_or_proc t tok
    | Token.LBrace -> ty_record t tok
    | _ ->
      let found = Token.show_kind t.interner tok.kind in
      error t (Printf.sprintf "expected type, found '%s'" found);
      Node.make_ty Node.TyError tok.span
  in
  skip_trivia t;
  if (curr t).kind = Token.Question then (
    advance t;
    Node.make_ty (Node.TyOptional base_ty) tok.span)
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
        (fun t ->
          skip_trivia t;
          match (curr t).kind with
          | Token.Ident name ->
            advance t;
            name
          | _ ->
            error t "expected type parameter name";
            Interner.empty_name t.interner)
        t
    in
    let _ = get_tok Token.Gt t in
    params)
  else []

let parse_modifiers t =
  let rec loop (mods : Node.modifiers) =
    skip_trivia t;
    match (curr t).kind with
    | Token.KwExport ->
      advance t;
      loop { mods with is_exported = true }
    | Token.KwAsync ->
      advance t;
      loop { mods with is_async = true }
    | Token.KwUnsafe ->
      advance t;
      loop { mods with is_unsafe = true }
    | Token.KwExtern -> (
      advance t;
      skip_trivia t;
      match (curr t).kind with
      | Token.LitStr abi_name ->
        advance t;
        loop { mods with abi = Some abi_name }
      | _ ->
        error t "expected ABI string after 'extern'";
        loop mods)
    | _ -> mods
  in
  loop Node.empty_modifiers

let expr_if t (tok : Token.t) =
  let cond = parse_expr t PrecNone in
  let _ = get_tok Token.KwThen t in
  let then_branch = parse_expr t PrecNone in
  skip_trivia t;
  let else_branch =
    if (curr t).kind = Token.KwElse then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  Node.make_expr (Node.ExprIf (cond, then_branch, else_branch)) tok.span

let expr_block t (tok : Token.t) =
  let rec loop acc =
    skip_trivia t;
    if (curr t).kind = Token.RBrace then List.rev acc
    else
      let e = parse_expr t PrecNone in
      skip_trivia t;
      match (curr t).kind with
      | Token.Semi ->
        advance t;
        loop (e :: acc)
      | Token.RBrace -> List.rev (e :: acc)
      | _ ->
        error t "expected ';' or '}' in block";
        List.rev (e :: acc)
  in
  let exprs = loop [] in
  let _ = get_tok Token.RBrace t in
  Node.make_expr (Node.ExprBlock exprs) tok.span

let expr_lit_record t (tok : Token.t) =
  let parse_field t =
    skip_trivia t;
    match (curr t).kind with
    | Token.Dot ->
      let name, _ = field_name_after_dot t "expected field name after '.'" in
      let _ = get_tok Token.ColonEq t in
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
  let _ = get_tok Token.RBrace t in
  Node.make_expr (Node.ExprLiteral (Node.LitRecord fields)) tok.span

let expr_while t (tok : Token.t) =
  let cond = parse_expr t PrecNone in
  let _ = get_tok Token.KwDo t in
  Node.make_expr (Node.ExprWhile (cond, parse_expr t PrecNone)) tok.span

let expr_binding t (tok : Token.t) is_const mods =
  let pat = parse_pat t in
  let ty_params = parse_ty_params t in
  let ty_opt = parse_optional_ty_annot t in
  let _ = get_tok Token.ColonEq t in
  Node.make_expr
    (Node.ExprBinding
       (is_const, ty_params, pat, ty_opt, parse_expr t PrecNone, mods))
    tok.span

let expr_for t (tok : Token.t) =
  let pat = parse_pat t in
  let _ = get_tok Token.KwIn t in
  let iterable = parse_expr t PrecNone in
  let _ = get_tok Token.KwDo t in
  Node.make_expr (Node.ExprFor (pat, iterable, parse_expr t PrecNone)) tok.span

let expr_do t (tok : Token.t) =
  let body = parse_expr t PrecNone in
  skip_trivia t;
  let cond =
    if (curr t).kind = Token.KwWhile then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  Node.make_expr (Node.ExprDo (body, cond)) tok.span

let expr_array t (tok : Token.t) =
  let exprs = sep_by_opt Token.Comma (fun t -> parse_expr t PrecNone) t in
  let _ = get_tok Token.RBrack t in
  Node.make_expr (Node.ExprArray exprs) tok.span

let expr_tuple t (tok : Token.t) =
  skip_trivia t;
  if (curr t).kind = Token.RParen then (
    advance t;
    Node.make_expr (Node.ExprTuple []) tok.span)
  else
    let first = parse_expr t PrecNone in
    skip_trivia t;
    match (curr t).kind with
    | Token.Comma ->
      advance t;
      let rest = sep_by_opt Token.Comma (fun t -> parse_expr t PrecNone) t in
      let _ = get_tok Token.RParen t in
      Node.make_expr (Node.ExprTuple (first :: rest)) tok.span
    | Token.RParen ->
      advance t;
      first
    | _ ->
      error t "expected ',' or ')' in tuple or grouped expression";
      let _ = get_tok Token.RParen t in
      first

let variant_field t (tok : Token.t) =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident fname ->
    advance t;
    let _ = get_tok Token.Colon t in
    let fty = parse_ty t in
    { Node.fname; fty; is_var = false; is_weak = false }
  | _ ->
    error t "expected field name in variant record";
    let fname = Interner.empty_name t.interner in
    let fty = Node.make_ty Node.TyError tok.span in
    { Node.fname; fty; is_var = false; is_weak = false }

let variant_data t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LParen ->
    advance t;
    let types = sep_by_opt Token.Comma parse_ty t in
    let _ = get_tok Token.RParen t in
    Node.VTuple types
  | Token.LBrace ->
    advance t;
    let fields = sep_by_opt Token.Comma (fun t -> variant_field t (curr t)) t in
    let _ = get_tok Token.RBrace t in
    Node.VRecord fields
  | _ -> Node.VUnit

let parse_variant t =
  let _ = get_tok Token.KwCase t in
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident vname ->
    advance t;
    let vdata = variant_data t in
    { Node.vname; vdata }
  | _ ->
    error t "expected variant name after 'case'";
    let vname = Interner.empty_name t.interner in
    { Node.vname; vdata = Node.VUnit }

let expr_choice t (tok : Token.t) mods ty_params =
  let _ = get_tok Token.LBrace t in
  let variants = sep_by_opt Token.Comma parse_variant t in
  let _ = get_tok Token.RBrace t in
  Node.make_expr (Node.ExprChoice (ty_params, variants, mods)) tok.span

let parse_case t =
  let _ = get_tok Token.KwCase t in
  let cpat = parse_pat t in
  skip_trivia t;
  let guard =
    if (curr t).kind = Token.KwIf then (
      advance t;
      Some (parse_expr t PrecNone))
    else None
  in
  let _ = get_tok Token.MinusGt t in
  let body = parse_expr t PrecNone in
  { Node.cpat; guard; body }

let expr_match t (tok : Token.t) =
  let scrutinee = parse_expr t PrecNone in
  let _ = get_tok Token.KwWith t in
  let _ = get_tok Token.LBrace t in
  let cases = sep_by_opt Token.Comma parse_case t in
  let _ = get_tok Token.RBrace t in
  Node.make_expr (Node.ExprMatch (scrutinee, cases)) tok.span

let parse_capture t =
  skip_trivia t;
  let is_weak =
    if (curr t).kind = Token.KwWeak then (
      advance t;
      true)
    else false
  in
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident cname ->
    advance t;
    { Node.cname; is_weak }
  | _ ->
    error t "expected capture name";
    let cname = Interner.empty_name t.interner in
    { Node.cname; is_weak }

let parse_param t =
  skip_trivia t;
  let is_inout =
    if (curr t).kind = Token.KwVar then (
      advance t;
      true)
    else false
  in
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident pname ->
    advance t;
    let pty = parse_optional_ty_annot t in
    { Node.pname; pty; is_inout }
  | _ ->
    error t "expected parameter name";
    let pname = Interner.empty_name t.interner in
    { Node.pname; pty = None; is_inout }

let parse_field t =
  skip_trivia t;
  let is_var, is_weak =
    if (curr t).kind = Token.KwWeak then (
      advance t;
      skip_trivia t;
      if (curr t).kind = Token.KwVar then (
        advance t;
        (true, true))
      else (false, true))
    else if (curr t).kind = Token.KwVar then (
      advance t;
      (true, false))
    else (false, false)
  in
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident fname ->
    advance t;
    let _ = get_tok Token.Colon t in
    let fty = parse_ty t in
    { Node.fname; fty; is_var; is_weak }
  | _ ->
    error t "expected field name";
    let fname = Interner.empty_name t.interner in
    let fty = Node.make_ty Node.TyError Span.dummy in
    { Node.fname; fty; is_var; is_weak }

let expr_record t (tok : Token.t) mods ty_params =
  let _ = get_tok Token.LBrace t in
  let fields = sep_by_opt Token.Comma parse_field t in
  let _ = get_tok Token.RBrace t in
  Node.make_expr (Node.ExprRecord (ty_params, fields, mods)) tok.span

let expr_proc t (tok : Token.t) mods ty_params =
  let captures =
    if (curr t).kind = Token.Pipe then (
      advance t;
      let caps = sep_by_opt Token.Comma parse_capture t in
      let _ = get_tok Token.Pipe t in
      caps)
    else []
  in
  let _ = get_tok Token.LParen t in
  let params = sep_by_opt Token.Comma parse_param t in
  let _ = get_tok Token.RParen t in
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
      Some (expr_block t brace_tok))
    else None
  in
  Node.make_expr
    (Node.ExprProc (ty_params, captures, params, ret_ty, body, mods))
    tok.span

let expr_block_or_lit_record t tok =
  skip_trivia t;
  match (curr t).kind with
  | Token.Dot -> expr_lit_record t tok
  | Token.RBrace ->
    advance t;
    Node.make_expr (Node.ExprBlock []) tok.span
  | Token.Ident _ -> (
    let next_tok = Token.peek t.stream in
    match next_tok.kind with
    | Token.ColonEq | Token.Comma -> expr_lit_record t tok
    | _ -> expr_block t tok)
  | _ -> expr_block t tok

let expr_unary_wrap make_kind t (tok : Token.t) =
  Node.make_expr (make_kind (parse_expr t PrecUnary)) tok.Token.span

let expr_prefix t =
  skip_trivia t;
  let mods = parse_modifiers t in
  let tok = curr t in
  advance t;
  match tok.kind with
  | Token.LitNum s ->
    let lit =
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then Node.LitBin s
      else Node.LitInt s
    in
    Node.make_expr (Node.ExprLiteral lit) tok.span
  | Token.LitStr name ->
    Node.make_expr (Node.ExprLiteral (Node.LitStr name)) tok.span
  | Token.LitRune code ->
    Node.make_expr (Node.ExprLiteral (Node.LitRune code)) tok.span
  | Token.KwTrue ->
    Node.make_expr (Node.ExprLiteral (Node.LitBool true)) tok.span
  | Token.KwFalse ->
    Node.make_expr (Node.ExprLiteral (Node.LitBool false)) tok.span
  | Token.Ident name -> Node.make_expr (Node.ExprIdent name) tok.span
  | Token.KwReturn ->
    let value = parse_optional_expr t in
    Node.make_expr (Node.ExprReturn value) tok.span
  | Token.KwBreak ->
    let value = parse_optional_expr t in
    Node.make_expr (Node.ExprBreak value) tok.span
  | Token.KwContinue -> Node.make_expr Node.ExprContinue tok.span
  | Token.KwAwait -> expr_unary_wrap (fun e -> Node.ExprAwait e) t tok
  | Token.KwYield ->
    Node.make_expr (Node.ExprYield (parse_optional_expr t)) tok.span
  | Token.KwDefer -> expr_unary_wrap (fun e -> Node.ExprDefer e) t tok
  | Token.KwTry -> expr_unary_wrap (fun e -> Node.ExprTry e) t tok
  | Token.KwAsync ->
    let brace_tok = get_tok Token.LBrace t in
    let block = expr_block t brace_tok in
    Node.make_expr (Node.ExprAsync block) tok.span
  | Token.KwUnsafe ->
    let brace_tok = get_tok Token.LBrace t in
    let block = expr_block t brace_tok in
    Node.make_expr (Node.ExprUnsafe block) tok.span
  | Token.KwIf -> expr_if t tok
  | Token.KwMatch -> expr_match t tok
  | Token.KwWhile -> expr_while t tok
  | Token.KwDo -> expr_do t tok
  | Token.KwConst -> expr_binding t tok true mods
  | Token.KwVar -> expr_binding t tok false mods
  | Token.KwFor -> expr_for t tok
  | Token.KwChoice | Token.KwRecord | Token.KwProc | Token.Pipe -> (
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
      | Token.KwChoice -> expr_choice t tok mods ty_params
      | Token.KwRecord -> expr_record t tok mods ty_params
      | Token.KwProc | Token.Pipe -> expr_proc t tok mods ty_params
      | _ -> failwith "unreachable"
    in
    match name_opt with
    | Some name ->
      let pat = Node.make_pat (Node.PatIdent name) tok.span in
      Node.make_expr
        (Node.ExprBinding (true, [], pat, None, construct, Node.empty_modifiers))
        tok.span
    | None -> construct)
  | Token.LBrace -> expr_block_or_lit_record t tok
  | Token.LBrack -> expr_array t tok
  | Token.LParen -> expr_tuple t tok
  | Token.Minus | Token.KwNot ->
    let operand = parse_expr t PrecUnary in
    Node.make_expr (Node.ExprUnary (tok.kind, operand)) tok.span
  | _ ->
    let found = Token.show_kind t.interner tok.kind in
    error t (Printf.sprintf "unexpected '%s' in expression" found);
    Node.make_expr Node.ExprError tok.span

let expr_infix_impl t left bp =
  skip_trivia t;
  let op_token = curr t in
  let op_prec = token_prec op_token.kind in
  if prec_to_int op_prec <= prec_to_int bp then left
  else (
    advance t;
    match op_token.kind with
    | Token.Dot -> expr_field_opt t left bp false
    | Token.LBrack -> expr_index_opt t left bp false
    | Token.LParen -> expr_call_opt t left bp false
    | Token.LBrace ->
      let lit = expr_lit_record t op_token in
      parse_expr_infix t lit bp
    | Token.Bang ->
      let unwrap = Node.make_expr (Node.ExprUnwrap left) op_token.span in
      parse_expr_infix t unwrap bp
    | Token.Question -> expr_optional t left bp
    | Token.KwAs ->
      ty_infix t left op_token (fun (e, ty) -> Node.ExprCast (e, ty)) bp
    | Token.KwIs ->
      ty_infix t left op_token (fun (e, ty) -> Node.ExprTest (e, ty)) bp
    | Token.DotDot | Token.DotDotLt ->
      let right = parse_expr t op_prec in
      let inclusive = op_token.kind = Token.DotDot in
      let range =
        Node.make_expr (Node.ExprRange (left, right, inclusive)) op_token.span
      in
      parse_expr_infix t range bp
    | Token.LtMinus ->
      let right = parse_expr t op_prec in
      let node = Node.make_expr (Node.ExprAssign (left, right)) op_token.span in
      parse_expr_infix t node bp
    | _ ->
      let right = parse_expr t op_prec in
      let node =
        Node.make_expr
          (Node.ExprBinary (op_token.kind, left, right))
          op_token.span
      in
      parse_expr_infix t node bp)

let expr_impl t bp =
  skip_trivia t;
  let left = expr_prefix t in
  parse_expr_infix t left bp

let pat_rest t (tok : Token.t) =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident name ->
    advance t;
    Node.make_pat (Node.PatRest name) tok.span
  | _ ->
    error t "expected identifier after '..'";
    Node.make_pat Node.PatError tok.span

let pat_choice t (tok : Token.t) =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident name ->
    advance t;
    skip_trivia t;
    let inner =
      if (curr t).kind = Token.LParen then (
        advance t;
        let pat = parse_pat t in
        let _ = get_tok Token.RParen t in
        Some pat)
      else None
    in
    Node.make_pat (Node.PatChoice (name, inner)) tok.span
  | _ ->
    error t "expected variant name after '.'";
    Node.make_pat Node.PatError tok.span

let pat_array t (tok : Token.t) =
  skip_trivia t;
  if (curr t).kind = Token.RBrack then (
    advance t;
    Node.make_pat (Node.PatArray ([], None)) tok.span)
  else
    let rec parse_items acc =
      skip_trivia t;
      if (curr t).kind = Token.DotDot then (
        advance t;
        let rest_pat = pat_rest t tok in
        let _ = get_tok Token.RBrack t in
        Node.make_pat (Node.PatArray (List.rev acc, Some rest_pat)) tok.span)
      else
        let pat = parse_pat t in
        skip_trivia t;
        match (curr t).kind with
        | Token.Comma ->
          advance t;
          parse_items (pat :: acc)
        | Token.RBrack ->
          advance t;
          Node.make_pat (Node.PatArray (List.rev (pat :: acc), None)) tok.span
        | _ ->
          error t "expected ',' or ']' in array pattern";
          let _ = get_tok Token.RBrack t in
          Node.make_pat (Node.PatArray (List.rev (pat :: acc), None)) tok.span
    in
    parse_items []

let pat_record t (tok : Token.t) =
  let parse_field t =
    skip_trivia t;
    match (curr t).kind with
    | Token.Dot ->
      let name, _ =
        field_name_after_dot t "expected field name after '.' in record pattern"
      in
      let _ = get_tok Token.ColonEq t in
      (name, parse_pat t)
    | _ ->
      error t "expected '.' in record pattern";
      (Interner.empty_name t.interner, Node.make_pat Node.PatError tok.span)
  in
  let fields = sep_by_opt Token.Comma parse_field t in
  let _ = get_tok Token.RBrace t in
  Node.make_pat (Node.PatRecord fields) tok.span

let pat_tuple t (tok : Token.t) =
  skip_trivia t;
  if (curr t).kind = Token.RParen then (
    advance t;
    Node.make_pat (Node.PatTuple []) tok.span)
  else
    let first = parse_pat t in
    skip_trivia t;
    match (curr t).kind with
    | Token.Comma ->
      advance t;
      let rest = sep_by_opt Token.Comma parse_pat t in
      let _ = get_tok Token.RParen t in
      Node.make_pat (Node.PatTuple (first :: rest)) tok.span
    | Token.RParen ->
      advance t;
      first
    | _ ->
      error t "expected ',' or ')' in tuple pattern";
      let _ = get_tok Token.RParen t in
      Node.make_pat (Node.PatTuple [ first ]) tok.span

let pat_binding t (tok : Token.t) =
  skip_trivia t;
  match (curr t).kind with
  | Token.Ident name ->
    advance t;
    Node.make_pat (Node.PatBinding name) tok.span
  | _ ->
    error t "expected identifier after 'const' in pattern";
    Node.make_pat Node.PatError tok.span

let pat_impl t =
  skip_trivia t;
  let tok = curr t in
  advance t;
  match tok.kind with
  | Token.KwConst -> pat_binding t tok
  | Token.Underscore -> Node.make_pat Node.PatWild tok.span
  | Token.Ident name -> Node.make_pat (Node.PatIdent name) tok.span
  | Token.Dot -> pat_choice t tok
  | Token.LParen -> pat_tuple t tok
  | Token.LBrack -> pat_array t tok
  | Token.LBrace -> pat_record t tok
  | Token.LitNum _ | Token.LitStr _ | Token.LitRune _ | Token.KwTrue
  | Token.KwFalse ->
    Node.make_pat (Node.PatLiteral tok.kind) tok.span
  | _ ->
    let found = Token.show_kind t.interner tok.kind in
    error t (Printf.sprintf "expected pattern, found '%s'" found);
    Node.make_pat Node.PatError tok.span

let () =
  parse_expr_ref := expr_impl;
  parse_expr_infix_ref := expr_infix_impl;
  parse_ty_ref := ty_impl;
  parse_pat_ref := pat_impl

(* === REC-DESC PARSING === *)

let parse_ident_list t =
  sep_by_opt
    Token.Comma
    (fun t ->
      skip_trivia t;
      match (curr t).kind with
      | Token.Ident name ->
        advance t;
        skip_trivia t;
        if (curr t).kind = Token.KwAs then (
          advance t;
          skip_trivia t;
          match (curr t).kind with
          | Token.Ident _ ->
            advance t;
            name
          | _ ->
            error t "expected identifier after 'as'";
            name)
        else name
      | _ ->
        error t "expected identifier";
        Interner.empty_name t.interner)
    t

let parse_import_spec t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LBrace ->
    advance t;
    let names = parse_ident_list t in
    let _ = get_tok Token.RBrace t in
    Node.ImportNamed names
  | Token.Star -> (
    advance t;
    let _ = get_tok Token.KwAs t in
    skip_trivia t;
    match (curr t).kind with
    | Token.Ident name ->
      advance t;
      Node.ImportNamespace name
    | _ ->
      error t "expected identifier after 'as'";
      Node.ImportNamespace (Interner.empty_name t.interner))
  | _ ->
    error t "expected '{' or '*' after 'import'";
    Node.ImportNamed []

let parse_export_spec t =
  skip_trivia t;
  match (curr t).kind with
  | Token.LBrace ->
    advance t;
    let names = parse_ident_list t in
    let _ = get_tok Token.RBrace t in
    Node.ExportNamed names
  | Token.Star ->
    advance t;
    Node.ExportNamespace (Interner.intern t.interner "*")
  | _ ->
    error t "expected '{' or '*' after 'export'";
    Node.ExportNamed []

let stmt_import t (tok : Token.t) =
  let spec = parse_import_spec t in
  let _ = get_tok Token.KwFrom t in
  skip_trivia t;
  match (curr t).kind with
  | Token.LitStr path ->
    advance t;
    Node.make_stmt (Node.StmtImport (spec, path)) tok.span
  | _ ->
    error t "expected text path after 'from'";
    Node.make_stmt Node.StmtError tok.span

let stmt_export t (tok : Token.t) =
  let spec = parse_export_spec t in
  skip_trivia t;
  let path =
    if (curr t).kind = Token.KwFrom then (
      advance t;
      skip_trivia t;
      match (curr t).kind with
      | Token.LitStr p ->
        advance t;
        Some p
      | _ ->
        error t "expected text path after 'from'";
        None)
    else None
  in
  Node.make_stmt (Node.StmtExport (spec, path)) tok.span

let stmt t =
  skip_trivia t;
  let tok = curr t in
  match tok.kind with
  | Token.KwImport ->
    advance t;
    stmt_import t tok
  | Token.KwExport -> (
    let next_tok = Token.peek t.stream in
    match next_tok.kind with
    | Token.LBrace | Token.Star ->
      advance t;
      stmt_export t tok
    | _ ->
      let e = parse_expr t PrecNone in
      Node.make_stmt (Node.StmtExpr (e, false)) Span.dummy)
  | _ ->
    let e = parse_expr t PrecNone in
    Node.make_stmt (Node.StmtExpr (e, false)) Span.dummy

let parse t =
  let rec loop acc =
    skip_trivia t;
    if at_end t then List.rev acc
    else
      let s = stmt t in
      skip_trivia t;
      if at_end t then List.rev (s :: acc)
      else
        let _ = get_tok Token.Semi t in
        loop (s :: acc)
  in
  let stmts = loop [] in
  let diags = !(t.diags) in
  (stmts, diags)
