(* ========================================
   PARSER STATE
   ======================================== *)

type t = {
    stream : Token.stream
  ; interner : Interner.t
  ; diags : Diagnostic.bag ref
}

let make tokens interner =
  let filtered =
    List.filter
      (fun t ->
        match t.Token.kind with
        | Token.Whitespace | Token.Newline | Token.LineComment _
        | Token.BlockComment _ ->
          false
        | _ -> true)
      tokens
  in
  {
    stream = Token.make_stream filtered
  ; interner
  ; diags = ref Diagnostic.empty_bag
  }

(* ========================================
   DIAGNOSTICS
   ======================================== *)

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

let error_with_fixit t msg span replacement =
  let diag = Diagnostic.error msg span in
  let diag = Diagnostic.with_fixit diag { Diagnostic.span; replacement } in
  t.diags := Diagnostic.add !(t.diags) diag

(* ========================================
   TOKEN HELPERS
   ======================================== *)

let curr_token t = Token.curr t.stream
let curr_token_kind t = (curr_token t).Token.kind
let advance t = Token.advance t.stream
let at_end_or_eof t = Token.at_end t.stream || curr_token_kind t = Token.Eof

let expect t kind =
  let tok = curr_token t in
  if tok.Token.kind = kind then (
    advance t;
    tok)
  else
    let msg =
      Printf.sprintf
        "expected '%s', found '%s'"
        (Token.show_kind t.interner kind)
        (Token.show_kind t.interner tok.Token.kind)
    in
    (match kind with
    | Token.Semi -> error_with_fixit t msg tok.Token.span ";"
    | Token.RBrace -> error_with_fixit t msg tok.Token.span "}"
    | Token.RParen -> error_with_fixit t msg tok.Token.span ")"
    | Token.RBrack -> error_with_fixit t msg tok.Token.span "]"
    | Token.ColonEq -> error_with_fixit t msg tok.Token.span ":="
    | _ -> error t msg tok.Token.span);
    tok

(* ========================================
   OPERATOR PRECEDENCE
   ======================================== *)

type assoc = Left | Right

type prec =
  | PrecAssign
  | PrecOr
  | PrecXor
  | PrecAnd
  | PrecEquality
  | PrecComparison
  | PrecRange
  | PrecAdditive
  | PrecMultiplicative
  | PrecShift
  | PrecExponent
  | PrecCast
  | PrecPrefix

let prec_value = function
  | PrecAssign -> 1
  | PrecOr -> 2
  | PrecXor -> 3
  | PrecAnd -> 4
  | PrecEquality -> 5
  | PrecComparison -> 6
  | PrecRange -> 7
  | PrecAdditive -> 8
  | PrecMultiplicative -> 9
  | PrecShift -> 10
  | PrecExponent -> 11
  | PrecCast -> 12
  | PrecPrefix -> 100

let precedence = function
  | Token.LtMinus -> Some (PrecAssign, Right)
  | Token.KwOr -> Some (PrecOr, Left)
  | Token.KwXor -> Some (PrecXor, Left)
  | Token.KwAnd -> Some (PrecAnd, Left)
  | Token.Eq | Token.EqSlashEq -> Some (PrecEquality, Left)
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq -> Some (PrecComparison, Left)
  | Token.DotDot | Token.DotDotLt -> Some (PrecRange, Left)
  | Token.Plus | Token.Minus -> Some (PrecAdditive, Left)
  | Token.Star | Token.Slash | Token.KwMod -> Some (PrecMultiplicative, Left)
  | Token.KwShl | Token.KwShr -> Some (PrecShift, Left)
  | Token.Caret -> Some (PrecExponent, Right)
  | Token.KwAs -> Some (PrecCast, Left)
  | _ -> None

let prefix_op = function
  | Token.Minus | Token.KwNot | Token.Bang -> true
  | _ -> false

(* ========================================
   HELPERS
   ======================================== *)

let empty_modifiers =
  {
    Node.decorators = []
  ; is_exported = false
  ; is_unsafe = false
  ; is_extern = (false, None)
  ; is_async = false
  }

let parse_separated t ~start_tok ~end_tok ~trailing_ok ~parse_item =
  let rec loop acc =
    if curr_token_kind t = end_tok || curr_token_kind t = Token.Eof then
      (List.rev acc, false)
    else
      let item = parse_item t in
      let tok = curr_token t in
      match tok.Token.kind with
      | k when k = end_tok -> (List.rev (item :: acc), false)
      | Token.Comma ->
        advance t;
        if trailing_ok && curr_token_kind t = end_tok then
          (List.rev (item :: acc), true)
        else loop (item :: acc)
      | Token.Eof -> (List.rev (item :: acc), false)
      | _ ->
        error
          t
          (Printf.sprintf
             "expected ',' or '%s'"
             (Token.show_kind t.interner end_tok))
          tok.Token.span;
        advance t;
        (List.rev (item :: acc), false)
  in
  let _ = expect t start_tok in
  if curr_token_kind t = end_tok then { Node.items = []; trailing = false }
  else
    let items, trailing = loop [] in
    { Node.items; trailing }

(* ========================================
   EXPRESSION PARSING (PRATT)
   ======================================== *)

let rec parse_expr_atom t =
  let tok = curr_token t in
  let start = tok.Token.span in
  match tok.Token.kind with
  | Token.LitNumeric (text, suffix) ->
    advance t;
    Node.make (Node.ExprLitNumeric (text, suffix)) start
  | Token.LitText name ->
    advance t;
    Node.make (Node.ExprLitText name) start
  | Token.KwTrue ->
    advance t;
    Node.make (Node.ExprLitBool true) start
  | Token.KwFalse ->
    advance t;
    Node.make (Node.ExprLitBool false) start
  | Token.Ident name ->
    advance t;
    if curr_token_kind t = Token.LBrace then
      let next = Token.peek t.stream in
      if next.Token.kind = Token.Dot then
        parse_expr_record_literal t start (Some name)
      else Node.make (Node.ExprIdent name) start
    else Node.make (Node.ExprIdent name) start
  | Token.LParen -> parse_expr_paren t start
  | Token.LBrack -> parse_expr_array t start
  | Token.LBrace ->
    let next = Token.peek t.stream in
    if next.Token.kind = Token.Dot then parse_expr_record_literal t start None
    else parse_expr_block t start false
  | Token.KwDo ->
    advance t;
    let body = parse_expr_block t (curr_token t).Token.span false in
    if curr_token_kind t = Token.KwWhile then (
      advance t;
      let pat = parse_expr t 0 in
      let span = Span.merge start pat.Node.span in
      Node.make (Node.ExprWhile { pat; body }) span)
    else Node.make (Node.ExprLoop body) (Span.merge start body.Node.span)
  | Token.KwUnsafe ->
    advance t;
    parse_expr_block t start true
  | Token.KwIf -> parse_expr_if t start
  | Token.KwWhile -> parse_expr_while t start
  | Token.KwFor -> parse_expr_for t start
  | Token.KwReturn -> parse_expr_return t start
  | Token.KwMatch -> parse_expr_match t start
  | Token.KwBreak -> parse_expr_break t start
  | Token.KwContinue -> parse_expr_continue t start
  | Token.KwProc -> parse_expr_proc t start
  | Token.KwAsync | Token.KwExtern ->
    let modifiers = parse_modifiers t in
    if curr_token_kind t = Token.KwProc then parse_expr_proc ~modifiers t start
    else (
      error
        t
        "expected 'proc' after modifiers in expression"
        (curr_token t).Token.span;
      Node.make (Node.ExprIdent (Interner.intern t.interner "<error>")) start)
  | _ ->
    error
      t
      (Printf.sprintf
         "unexpected '%s' in expression"
         (Token.show_kind t.interner tok.Token.kind))
      start;
    advance t;
    Node.make (Node.ExprIdent (Interner.intern t.interner "<error>")) start

and parse_expr_paren t start =
  advance t;
  if curr_token_kind t = Token.RParen then (
    let end_tok = curr_token t in
    advance t;
    Node.make
      (Node.ExprTuple { Node.items = []; trailing = false })
      (Span.merge start end_tok.Token.span))
  else
    let first = parse_expr t 0 in
    match curr_token_kind t with
    | Token.Comma ->
      advance t;
      let rec parse_rest acc =
        if curr_token_kind t = Token.RParen then (List.rev acc, true)
        else
          let item = parse_expr t 0 in
          match curr_token_kind t with
          | Token.Comma ->
            advance t;
            parse_rest (item :: acc)
          | Token.RParen -> (List.rev (item :: acc), false)
          | _ ->
            error
              t
              "expected ',' or ')' in tuple literal"
              (curr_token t).Token.span;
            (List.rev (item :: acc), false)
      in
      let rest, trailing = parse_rest [] in
      let end_tok = curr_token t in
      advance t;
      Node.make
        (Node.ExprTuple { Node.items = first :: rest; trailing })
        (Span.merge start end_tok.Token.span)
    | Token.RParen ->
      advance t;
      first
    | _ ->
      error
        t
        "expected ',' or ')' in parenthesized expression"
        (curr_token t).Token.span;
      first

and parse_expr_array t start =
  let items =
    parse_separated
      t
      ~start_tok:Token.LBrack
      ~end_tok:Token.RBrack
      ~trailing_ok:true
      ~parse_item:(fun t -> parse_expr t 0)
  in
  let close_brack = expect t Token.RBrack in
  Node.make (Node.ExprArray items) (Span.merge start close_brack.Token.span)

and parse_expr_record_literal t start name =
  let _ = expect t Token.LBrace in
  let rec parse_fields acc =
    if curr_token_kind t = Token.RBrace || curr_token_kind t = Token.Eof then
      List.rev acc
    else
      let _ = expect t Token.Dot in
      if curr_token_kind t = Token.RBrace || curr_token_kind t = Token.Eof then
        List.rev acc
      else
        let label_tok = curr_token t in
        let label =
          match label_tok.Token.kind with
          | Token.Ident n ->
            advance t;
            n
          | _ ->
            error
              t
              "expected field name after '.' in record literal"
              label_tok.Token.span;
            advance t;
            Interner.intern t.interner "<error>"
        in
        let _ = expect t Token.ColonEq in
        let value = parse_expr t 0 in
        let field = { Node.label; value } in
        let tok = curr_token t in
        if tok.Token.kind = Token.Comma then advance t;
        parse_fields (field :: acc)
  in
  let fields = parse_fields [] in
  let close_brace = expect t Token.RBrace in
  Node.make
    (Node.ExprLitRecord { name; fields })
    (Span.merge start close_brace.Token.span)

and parse_expr_block t start is_unsafe =
  let _ = expect t Token.LBrace in
  let rec loop acc =
    let tok = curr_token t in
    if tok.Token.kind = Token.RBrace || tok.Token.kind = Token.Eof then
      (List.rev acc, None)
    else
      let stmt = parse_stmt t in
      let next = curr_token t in
      if next.Token.kind = Token.Semi then (
        advance t;
        loop (stmt :: acc))
      else if next.Token.kind = Token.RBrace then
        let expr =
          match stmt.Node.kind with Node.StmtExpr e -> Some e | _ -> Some stmt
        in
        (List.rev acc, expr)
      else (
        error t "expected ';' or '}' after statement in block" next.Token.span;
        advance t;
        loop (stmt :: acc))
  in
  let stmts, expr = loop [] in
  let close_brace = expect t Token.RBrace in
  let kind =
    if is_unsafe then Node.ExprBlockUnsafe { stmts; expr }
    else Node.ExprBlock { stmts; expr }
  in
  Node.make kind (Span.merge start close_brace.Token.span)

and parse_expr_if t start =
  advance t;
  let pat = parse_expr t 0 in
  let _ = expect t Token.KwThen in
  let then_branch = parse_expr_block t (curr_token t).Token.span false in
  let else_branch =
    if curr_token_kind t = Token.KwElse then (
      advance t;
      let tok = curr_token t in
      if tok.Token.kind = Token.KwIf then Some (parse_expr_if t tok.Token.span)
      else Some (parse_expr_block t tok.Token.span false))
    else None
  in
  let span = Span.merge start (curr_token t).Token.span in
  Node.make (Node.ExprIf { pat; then_branch; else_branch }) span

and parse_expr_while t start =
  advance t;
  let pat = parse_expr t 0 in
  let _ = expect t Token.KwDo in
  let body = parse_expr_block t (curr_token t).Token.span false in
  let span = Span.merge start body.Node.span in
  Node.make (Node.ExprWhile { pat; body }) span

and parse_expr_for t start =
  advance t;
  let pat = parse_expr t 0 in
  let _ = expect t Token.KwIn in
  let iter = parse_expr t 0 in
  let _ = expect t Token.KwDo in
  let body = parse_expr_block t (curr_token t).Token.span false in
  let span = Span.merge start body.Node.span in
  Node.make (Node.ExprFor { pat; iter; body }) span

and parse_expr_return t start =
  advance t;
  let tok = curr_token t in
  let expr =
    if tok.Token.kind = Token.Semi || tok.Token.kind = Token.RBrace then None
    else Some (parse_expr t 0)
  in
  let span =
    match expr with Some e -> Span.merge start e.Node.span | None -> start
  in
  Node.make (Node.ExprReturn expr) span

and parse_expr_postfix t node =
  let tok = curr_token t in
  match tok.Token.kind with
  | Token.LParen ->
    let args =
      parse_separated
        t
        ~start_tok:Token.LParen
        ~end_tok:Token.RParen
        ~trailing_ok:true
        ~parse_item:(fun t -> parse_expr t 0)
    in
    let close_paren = expect t Token.RParen in
    let node' =
      Node.make
        (Node.ExprCall { callee = node; args })
        (Span.merge node.Node.span close_paren.Token.span)
    in
    parse_expr_postfix t node'
  | Token.Dot -> (
    advance t;
    let field_tok = curr_token t in
    match field_tok.Token.kind with
    | Token.Ident name ->
      advance t;
      let span = Span.merge node.Node.span field_tok.Token.span in
      let node' =
        Node.make (Node.ExprField { target = node; field = name }) span
      in
      parse_expr_postfix t node'
    | _ ->
      error
        t
        "expected field name after '.' in field access"
        field_tok.Token.span;
      node)
  | Token.LBrack ->
    advance t;
    let index = parse_expr t 0 in
    let close_brack = expect t Token.RBrack in
    let node' =
      Node.make
        (Node.ExprIndex { target = node; index })
        (Span.merge node.Node.span close_brack.Token.span)
    in
    parse_expr_postfix t node'
  | _ -> node

and parse_expr_primary t =
  let tok = curr_token t in
  let start = tok.Token.span in
  if prefix_op tok.Token.kind then (
    let op = tok.Token.kind in
    advance t;
    let operand = parse_expr t (prec_value PrecPrefix) in
    let span = Span.merge start operand.Node.span in
    Node.make (Node.ExprUnary { op; operand }) span)
  else
    let atom = parse_expr_atom t in
    parse_expr_postfix t atom

and parse_expr t min_prec =
  let left = parse_expr_primary t in
  parse_expr_bp t left min_prec

and parse_expr_bp t left min_prec =
  let tok = curr_token t in
  match precedence tok.Token.kind with
  | Some (prec, assoc) when prec_value prec >= min_prec ->
    let op = tok.Token.kind in
    advance t;
    let prec_val = prec_value prec in
    let next_min = if assoc = Left then prec_val + 1 else prec_val in
    let right = parse_expr t next_min in
    let span = Span.merge left.Node.span right.Node.span in
    let node =
      if op = Token.LtMinus then
        Node.make (Node.ExprAssign { target = left; value = right }) span
      else Node.make (Node.ExprBinary { op; left; right }) span
    in
    parse_expr_bp t node min_prec
  | _ -> left

(* ========================================
   STATEMENT PARSING
   ======================================== *)

and parse_decorators t =
  let rec loop acc =
    if curr_token_kind t = Token.At then (
      let start = (curr_token t).Token.span in
      advance t;
      let name_tok = curr_token t in
      let name =
        match name_tok.Token.kind with
        | Token.Ident n ->
          advance t;
          n
        | _ ->
          error
            t
            "expected identifier after '@' in decorator"
            name_tok.Token.span;
          advance t;
          Interner.intern t.interner "<error>"
      in
      let args, end_span =
        if curr_token_kind t = Token.LParen then
          let args_sep =
            parse_separated
              t
              ~start_tok:Token.LParen
              ~end_tok:Token.RParen
              ~trailing_ok:true
              ~parse_item:(fun t -> parse_expr t 0)
          in
          let rparen = expect t Token.RParen in
          (args_sep.Node.items, rparen.Token.span)
        else ([], name_tok.Token.span)
      in
      let span = Span.merge start end_span in
      loop ({ Node.name; args; span } :: acc))
    else List.rev acc
  in
  loop []

and parse_modifiers t =
  let decorators = parse_decorators t in
  let is_exported = curr_token_kind t = Token.KwExport in
  if is_exported then advance t;
  let is_async = curr_token_kind t = Token.KwAsync in
  if is_async then advance t;
  let is_unsafe = curr_token_kind t = Token.KwUnsafe in
  if is_unsafe then advance t;
  let is_extern =
    if curr_token_kind t = Token.KwExtern then (
      advance t;
      let abi =
        match curr_token_kind t with
        | Token.LitText name ->
          advance t;
          Some (Interner.lookup t.interner name)
        | _ -> None
      in
      (true, abi))
    else (false, None)
  in
  { Node.decorators; is_exported; is_async; is_unsafe; is_extern }

and parse_stmt t =
  let tok = curr_token t in
  let start = tok.Token.span in
  match tok.Token.kind with
  | Token.KwImport -> parse_stmt_import t start
  | Token.At ->
    let modifiers = parse_modifiers t in
    parse_stmt_with_modifiers t start modifiers
  | Token.KwExport ->
    advance t;
    if curr_token_kind t = Token.LBrace then parse_stmt_export t start
    else
      let modifiers = parse_modifiers t in
      parse_stmt_with_modifiers t start modifiers
  | Token.KwAlias -> parse_stmt_alias t start
  | Token.KwExtern ->
    let modifiers = parse_modifiers t in
    parse_stmt_with_modifiers t start modifiers
  | Token.KwUnsafe ->
    let next = Token.peek t.stream in
    if next.Token.kind = Token.LBrace then
      let expr = parse_expr t 0 in
      Node.make (Node.StmtExpr expr) expr.Node.span
    else
      let modifiers = parse_modifiers t in
      parse_stmt_with_modifiers t start modifiers
  | Token.KwVar | Token.KwConst | Token.KwProc ->
    parse_stmt_with_modifiers t start empty_modifiers
  | _ ->
    let expr = parse_expr t 0 in
    Node.make (Node.StmtExpr expr) expr.Node.span

and parse_stmt_with_modifiers t start modifiers =
  let tok = curr_token t in
  match tok.Token.kind with
  | Token.KwVar | Token.KwConst ->
    let binding = parse_expr_binding ~modifiers t start in
    Node.make (Node.StmtExpr binding) binding.Node.span
  | Token.KwProc ->
    let proc = parse_expr_proc ~modifiers t start in
    Node.make (Node.StmtExpr proc) proc.Node.span
  | _ ->
    let mod_str =
      let parts = [] in
      let parts =
        if modifiers.Node.is_exported then "export" :: parts else parts
      in
      let parts =
        if modifiers.Node.is_unsafe then "unsafe" :: parts else parts
      in
      let parts =
        if fst modifiers.Node.is_extern then "extern" :: parts else parts
      in
      String.concat " " (List.rev parts)
    in
    error
      t
      (Printf.sprintf "expected 'var', 'const', or 'proc' after '%s'" mod_str)
      tok.Token.span;
    let expr = parse_expr t 0 in
    Node.make (Node.StmtExpr expr) expr.Node.span

and parse_stmt_import t start =
  advance t;
  let parse_import_item t =
    let tok = curr_token t in
    match tok.Token.kind with
    | Token.Ident name ->
      advance t;
      let alias =
        if curr_token_kind t = Token.KwAs then (
          advance t;
          let alias_tok = curr_token t in
          match alias_tok.Token.kind with
          | Token.Ident alias_name ->
            advance t;
            Some alias_name
          | _ ->
            error
              t
              "expected identifier after 'as' in 'import' statement"
              alias_tok.Token.span;
            None)
        else None
      in
      { Node.name; alias }
    | _ ->
      error t "expected identifier in import list" tok.Token.span;
      advance t;
      { Node.name = Interner.intern t.interner "<error>"; alias = None }
  in
  let items =
    parse_separated
      t
      ~start_tok:Token.LBrace
      ~end_tok:Token.RBrace
      ~trailing_ok:true
      ~parse_item:parse_import_item
  in
  let _ = expect t Token.RBrace in
  let _ = expect t Token.KwFrom in
  let path_tok = curr_token t in
  let path =
    match path_tok.Token.kind with
    | Token.LitText name ->
      advance t;
      name
    | _ ->
      error
        t
        "expected text literal after 'from' in 'import' statement"
        path_tok.Token.span;
      Interner.intern t.interner "<error>"
  in
  let span = Span.merge start (curr_token t).Token.span in
  Node.make (Node.StmtImport { path; items }) span

and parse_stmt_export t start =
  let parse_name t =
    let tok = curr_token t in
    match tok.Token.kind with
    | Token.Ident name ->
      advance t;
      name
    | _ ->
      error t "expected identifier in 'export' list" tok.Token.span;
      advance t;
      Interner.intern t.interner "<error>"
  in
  let items =
    parse_separated
      t
      ~start_tok:Token.LBrace
      ~end_tok:Token.RBrace
      ~trailing_ok:true
      ~parse_item:parse_name
  in
  let close_brace = expect t Token.RBrace in
  Node.make
    (Node.StmtExport { items })
    (Span.merge start close_brace.Token.span)

and parse_stmt_alias t start =
  advance t;
  let name_tok = curr_token t in
  let name =
    match name_tok.Token.kind with
    | Token.Ident n ->
      advance t;
      n
    | _ ->
      error t "expected identifier after 'alias' keyword" name_tok.Token.span;
      Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.ColonEq in
  let ty_expr = parse_expr t 0 in
  let span = Span.merge start ty_expr.Node.span in
  Node.make (Node.StmtAlias { name; ty = ty_expr }) span

(* ========================================
   TYPE PARSING
   ======================================== *)

and parse_ty t =
  let tok = curr_token t in
  let start = tok.Token.span in
  match tok.Token.kind with
  | Token.LBrack -> parse_ty_array t start
  | Token.LParen -> parse_ty_tuple t start
  | Token.KwProc -> parse_ty_proc t start
  | Token.Ident name ->
    advance t;
    let base_ty = Node.make_ty (Node.TyNamed name) start in
    parse_ty_postfix t base_ty
  | _ ->
    error t "expected type expression" tok.Token.span;
    Node.make_ty (Node.TyNamed (Interner.intern t.interner "<error>")) start

and parse_ty_list t ~end_tok =
  let rec loop acc =
    if curr_token_kind t = end_tok || curr_token_kind t = Token.Eof then
      List.rev acc
    else
      let ty = parse_ty t in
      if curr_token_kind t = Token.Comma then advance t;
      loop (ty :: acc)
  in
  loop []

and parse_ty_array t start =
  advance t;
  let elem_ty = parse_ty t in
  match curr_token_kind t with
  | Token.Semi ->
    advance t;
    let size = parse_expr t 0 in
    let close_brack = expect t Token.RBrack in
    Node.make_ty
      (Node.TySizedArray (elem_ty, size))
      (Span.merge start close_brack.Token.span)
  | Token.RBrack ->
    let close_brack = curr_token t in
    advance t;
    Node.make_ty
      (Node.TyArray elem_ty)
      (Span.merge start close_brack.Token.span)
  | _ ->
    error t "expected ';' or ']' in array type" (curr_token t).Token.span;
    Node.make_ty (Node.TyArray elem_ty) start

and parse_ty_tuple t start =
  advance t;
  if curr_token_kind t = Token.RParen then (
    let close_paren = curr_token t in
    advance t;
    Node.make_ty (Node.TyTuple []) (Span.merge start close_paren.Token.span))
  else
    let first = parse_ty t in
    match curr_token_kind t with
    | Token.Comma ->
      advance t;
      let rest = parse_ty_list t ~end_tok:Token.RParen in
      let close_paren = expect t Token.RParen in
      Node.make_ty
        (Node.TyTuple (first :: rest))
        (Span.merge start close_paren.Token.span)
    | Token.RParen ->
      advance t;
      first
    | _ ->
      error t "expected ',' or ')' in tuple type" (curr_token t).Token.span;
      first

and parse_ty_proc t start =
  advance t;
  let _ = expect t Token.LParen in
  let params = parse_ty_list t ~end_tok:Token.RParen in
  let _ = expect t Token.RParen in
  let ret_ty =
    if curr_token_kind t = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let span =
    match ret_ty with
    | Some rt -> Span.merge start rt.Node.ty_span
    | None -> Span.merge start (curr_token t).Token.span
  in
  Node.make_ty (Node.TyProc (params, ret_ty)) span

and parse_ty_postfix t base_ty =
  let tok = curr_token t in
  match tok.Token.kind with
  | Token.Lt ->
    advance t;
    let args = parse_ty_list t ~end_tok:Token.Gt in
    let close_gt = expect t Token.Gt in
    let ty =
      Node.make_ty
        (Node.TyGeneric (base_ty, args))
        (Span.merge base_ty.Node.ty_span close_gt.Token.span)
    in
    parse_ty_postfix t ty
  | Token.Question ->
    advance t;
    let ty =
      Node.make_ty
        (Node.TyOptional base_ty)
        (Span.merge base_ty.Node.ty_span tok.Token.span)
    in
    parse_ty_postfix t ty
  | Token.Bang ->
    advance t;
    let error_ty =
      match curr_token_kind t with
      | Token.Ident _ ->
        let err_ty = parse_ty t in
        Some err_ty
      | _ -> None
    in
    let span =
      match error_ty with
      | Some et -> Span.merge base_ty.Node.ty_span et.Node.ty_span
      | None -> Span.merge base_ty.Node.ty_span tok.Token.span
    in
    let ty = Node.make_ty (Node.TyFallible (base_ty, error_ty)) span in
    parse_ty_postfix t ty
  | _ -> base_ty

and parse_expr_binding ?(modifiers = empty_modifiers) t start =
  let tok = curr_token t in
  if tok.Token.kind <> Token.KwVar && tok.Token.kind <> Token.KwConst then
    error t "expected 'var' or 'const' in binding" tok.Token.span;
  let is_mutable = tok.Token.kind = Token.KwVar in
  advance t;
  let pat = parse_expr t 0 in
  let ty =
    if curr_token_kind t = Token.Colon then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.ColonEq in
  let init = parse_expr t 0 in
  let span = Span.merge start init.Node.span in
  Node.make (Node.ExprBinding { modifiers; is_mutable; pat; ty; init }) span

and parse_expr_proc ?(modifiers = empty_modifiers) t start =
  let _ = expect t Token.KwProc in
  (match curr_token_kind t with Token.Ident _ -> advance t | _ -> ());
  let parse_param t =
    let is_mutable = curr_token_kind t = Token.KwVar in
    if is_mutable then advance t;
    let tok = curr_token t in
    match tok.Token.kind with
    | Token.Ident name ->
      advance t;
      let ty =
        if curr_token_kind t = Token.Colon then (
          advance t;
          Some (parse_ty t))
        else None
      in
      { Node.is_mutable; name; ty }
    | _ ->
      error t "expected parameter name in 'proc' declaration" tok.Token.span;
      advance t;
      {
        Node.is_mutable
      ; name = Interner.intern t.interner "<error>"
      ; ty = None
      }
  in
  let params_sep =
    parse_separated
      t
      ~start_tok:Token.LParen
      ~end_tok:Token.RParen
      ~trailing_ok:true
      ~parse_item:parse_param
  in
  let _ = expect t Token.RParen in
  let ret_ty =
    if curr_token_kind t = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let body, end_span =
    if curr_token_kind t = Token.LBrace then
      let b = parse_expr_block t (curr_token t).Token.span false in
      (Some b, b.Node.span)
    else (None, (curr_token t).Token.span)
  in
  let span = Span.merge start end_span in
  Node.make
    (Node.ExprProc { modifiers; params = params_sep.Node.items; ret_ty; body })
    span

(* ========================================
   PATTERN PARSING
   ======================================== *)

and parse_pattern t =
  let tok = curr_token t in
  let start = tok.Token.span in
  match tok.Token.kind with
  | Token.KwConst -> (
    advance t;
    let name_tok = curr_token t in
    match name_tok.Token.kind with
    | Token.Ident name ->
      advance t;
      Node.make (Node.PatBinding name) (Span.merge start name_tok.Token.span)
    | _ ->
      error t "expected identifier after 'const' in pattern" name_tok.Token.span;
      Node.make (Node.PatIdent (Interner.intern t.interner "<error>")) start)
  | Token.Underscore ->
    advance t;
    Node.make Node.PatWildcard start
  | _ -> parse_expr t 0

and parse_expr_match t start =
  advance t;
  let scrutinee = parse_expr t 0 in
  let _ = expect t Token.KwWith in
  let _ = expect t Token.LBrace in
  let rec parse_cases acc =
    if curr_token_kind t = Token.RBrace || curr_token_kind t = Token.Eof then
      List.rev acc
    else
      let _ = expect t Token.KwCase in
      let pattern = parse_pattern t in
      let guard =
        if curr_token_kind t = Token.KwIf then (
          advance t;
          Some (parse_expr t 0))
        else None
      in
      let _ = expect t Token.MinusGt in
      let body = parse_expr t 0 in
      let case = { Node.pattern; guard; body } in
      let tok = curr_token t in
      if tok.Token.kind = Token.Comma then advance t;
      parse_cases (case :: acc)
  in
  let cases = parse_cases [] in
  let close_brace = expect t Token.RBrace in
  Node.make
    (Node.ExprMatch { scrutinee; cases })
    (Span.merge start close_brace.Token.span)

and parse_expr_break t start =
  advance t;
  let tok = curr_token t in
  let value =
    if tok.Token.kind = Token.Semi || tok.Token.kind = Token.RBrace then None
    else Some (parse_expr t 0)
  in
  let span =
    match value with Some v -> Span.merge start v.Node.span | None -> start
  in
  Node.make (Node.ExprBreak value) span

and parse_expr_continue t start =
  advance t;
  Node.make Node.ExprContinue start

(* ========================================
   MODULE PARSING
   ======================================== *)

let parse t =
  let rec loop acc =
    if at_end_or_eof t then List.rev acc
    else
      let stmt = parse_stmt t in
      let _ = expect t Token.Semi in
      if at_end_or_eof t then List.rev (stmt :: acc) else loop (stmt :: acc)
  in
  let stmts = loop [] in
  let bag = !(t.diags) in
  (stmts, bag)
