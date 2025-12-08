open Basic
module Token = Lex.Token
open Ast

type state = {
    toks : (Token.t * Span.t) list
  ; mutable pos : int
  ; len : int
  ; file_id : Interner.file_id
  ; _interner : Interner.t
  ; mutable diags : Diagnostic.bag
}

type 'a t = state -> ('a * state, Diagnostic.t) Result.t

let mk_state toks file_id interner =
  {
    toks
  ; pos = 0
  ; len = List.length toks
  ; file_id
  ; _interner = interner
  ; diags = Diagnostic.empty_bag
  }

let return x = fun st -> Ok (x, st)

let bind m f =
 fun st -> match m st with Error diag -> Error diag | Ok (x, st') -> f x st'

let ( >>= ) = bind

let parse_error err_code span =
 fun _ ->
  let diag = Errors.parse_diag err_code span [] in
  Error diag

let or_else m1 m2 =
 fun st -> match m1 st with Ok result -> Ok result | Error _ -> m2 st

let ( <|> ) = or_else

let advance =
 fun st ->
  st.pos <- st.pos + 1;
  Ok ((), st)

let advance_n n =
 fun st ->
  st.pos <- st.pos + n;
  Ok ((), st)

let token =
 fun st ->
  if st.pos >= st.len then
    parse_error Errors.E1070 (Span.make st.file_id st.pos st.pos) st
  else
    let tok_span = List.nth st.toks st.pos in
    match advance st with
    | Ok ((), st') -> Ok (tok_span, st')
    | Error diag -> Error diag

let optional parser =
 fun st ->
  match parser st with
  | Ok (x, st') -> Ok (Some x, st')
  | Error _ -> Ok (None, st)

let many parser =
 fun st ->
  let rec collect acc st =
    match parser st with
    | Ok (x, st') -> collect (x :: acc) st'
    | Error _ -> Ok (List.rev acc, st)
  in
  collect [] st

let many1 parser =
  parser >>= fun fst ->
  many parser >>= fun rst -> return (fst :: rst)

let sep_by parser sep =
  parser
  >>= (fun fst ->
  many (sep >>= fun _ -> parser) >>= fun rst -> return (fst :: rst))
  <|> return []

let between opening closing content : 'c t =
  opening >>= fun _ ->
  content >>= fun content' ->
  closing >>= fun _ -> return content'

let token_match matcher =
  token >>= fun (tok, span) ->
  if matcher tok then return tok else parse_error Errors.E1004 span

let token_expect expected =
  token_match (fun tok -> tok = expected) >>= fun _ -> return ()

let report_error st err_code span =
  let diag = Errors.parse_diag err_code span [] in
  st.diags <- Diagnostic.add st.diags diag

let enclosed_list lhs rhs sep parser =
  token_expect lhs >>= fun () ->
  parser >>= fun fst ->
  many (token_expect sep >>= fun _ -> parser) >>= fun rst ->
  token_expect rhs >>= fun () ->
  return (fst :: rst)
  <|> ( token_expect lhs >>= fun () ->
        token_expect rhs >>= fun () -> return [] )

type bp = { lhs : int; rhs : int }

let infix_bp = function
  | Token.LtMinus -> { lhs = 1; rhs = 0 }
  | Token.Eq -> { lhs = 1; rhs = 0 }
  | Token.BangEq -> { lhs = 2; rhs = 3 }
  | Token.Lt -> { lhs = 2; rhs = 3 }
  | Token.LtEq -> { lhs = 2; rhs = 3 }
  | Token.Gt -> { lhs = 2; rhs = 3 }
  | Token.GtEq -> { lhs = 2; rhs = 3 }
  | Token.Plus -> { lhs = 6; rhs = 7 }
  | Token.Minus -> { lhs = 6; rhs = 7 }
  | Token.Star -> { lhs = 8; rhs = 9 }
  | Token.Slash -> { lhs = 8; rhs = 9 }
  | Token.StarStar -> { lhs = 10; rhs = 11 }
  | Token.Amp -> { lhs = 12; rhs = 13 }
  | Token.Pipe -> { lhs = 12; rhs = 13 }
  | Token.Caret -> { lhs = 12; rhs = 13 }
  | Token.LtLt -> { lhs = 14; rhs = 15 }
  | Token.GtGt -> { lhs = 14; rhs = 15 }
  | Token.KwAnd -> { lhs = 16; rhs = 17 }
  | Token.KwOr -> { lhs = 18; rhs = 19 }
  | _ -> raise Not_found

let prefix_bp = function
  | Token.Minus | Token.Tilde | Token.KwNot | Token.At -> 100
  | _ -> raise Not_found

let postfix_bp = function
  | Token.LParen | Token.Dot | Token.LBrack -> 1000
  | Token.Question -> 1001
  | Token.Bang -> 1002
  | _ -> raise Not_found

let can_bind_infix tok =
  try
    ignore (infix_bp tok);
    true
  with Not_found -> false

let can_bind_prefix tok =
  try
    ignore (prefix_bp tok);
    true
  with Not_found -> false

let can_bind_postfix tok =
  try
    ignore (postfix_bp tok);
    true
  with Not_found -> false

let curr st =
  if st.pos >= st.len then Token.EOF else fst (List.nth st.toks st.pos)

let curr_span st =
  if st.pos >= st.len then Span.dummy else snd (List.nth st.toks st.pos)

let mk_node span data = { Node.span; data }

let rec parse_expr_bp min_bp st =
  let tok = curr st in
  let span = curr_span st in
  match parse_prefix tok span st with
  | Error _ as e -> e
  | Ok (mut_lhs, st') ->
    let rec loop lhs st'' =
      match curr st'' with
      | Token.EOF -> Ok (lhs, st'')
      | infix_tok when can_bind_infix infix_tok -> (
        let { lhs = infix_lhs; rhs } = infix_bp infix_tok in
        if infix_lhs < min_bp then Ok (lhs, st'')
        else
          let st''' = { st'' with pos = st''.pos + 1 } in
          match parse_expr_bp rhs st''' with
          | Error _ as e -> e
          | Ok (rhs_expr, st'''') ->
            let span' = Span.merge (Node.span lhs) (Node.span rhs_expr) in
            let lhs' =
              mk_node
                span'
                (Node.ExprCall
                   {
                     callee = lhs
                   ; typ_args = None
                   ; args = [ rhs_expr ]
                   ; optional = false
                   })
            in
            loop lhs' st'''')
      | _ -> Ok (lhs, st'')
    in
    loop mut_lhs st'

and parse_prefix tok span st =
  match tok with
  | _ when can_bind_prefix tok -> (
    let r_bp = prefix_bp tok in
    let st' = { st with pos = st.pos + 1 } in
    match parse_expr_bp r_bp st' with
    | Error _ as e -> e
    | Ok (op, st'') ->
      let span' = Span.merge span (Node.span op) in
      Ok (mk_node span' (Node.ExprUnary { op = tok; arg = op }), st''))
  | _ -> parse_expr_atom tok span st

and parse_expr_atom tok span st =
  match tok with
  | Token.Ident name ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprIdent name), st')
  | Token.LitNumber s ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitWhole s)), st')
  | Token.LitString name ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitString name)), st')
  | Token.LitRune c ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitRune c)), st')
  | _ ->
    let diag = Errors.parse_diag Errors.E1003 span [] in
    Error diag

let rec parse_typ st = parse_typ_atom st

and parse_typ_atom st =
  (* Entry point for type parsing
   * Syntax: ^T, [N]T, []T, T<U>, (T, U), fn(T) -> R, { f: T }, T?
   * Dispatches to specific type parsers based on first token *)
  failwith "TODO: parse_typ_atom"

and parse_typ_ptr st =
  (* Parse pointer types
   * Syntax: ^T, ^^T
   * Example: ^Int32, ^^String *)
  failwith "TODO: parse_typ_ptr"

and parse_typ_array st =
  (* Parse array types
   * Syntax: [N]T, []T
   * Example: [10]Int32, []String *)
  failwith "TODO: parse_typ_array"

and parse_typ_ident st =
  (* Parse type identifiers
   * Syntax: Int32, String, Bool, Point, Option
   * Example: val x : Int32 := 42 *)
  failwith "TODO: parse_typ_ident"

and parse_typ_app st =
  (* Parse type applications
   * Syntax: T<U, V>, Vec<T>, Map<K, V>
   * Example: Option<Int32>, Result<String, Error> *)
  failwith "TODO: parse_typ_app"

and parse_typ_tuple st =
  (* Parse tuple types
   * Syntax: (T, U, V)
   * Example: (Int32, String, Bool) *)
  failwith "TODO: parse_typ_tuple"

and parse_typ_fn st =
  (* Parse function types
   * Syntax: fn(T, U) -> R, fn() -> Unit
   * Example: fn(Int32, String) -> Bool *)
  failwith "TODO: parse_typ_fn"

and parse_typ_record st =
  (* Parse record types
   * Syntax: { f: T, g: U }, { x: Int32; y: Bin64 }
   * Example: { x: Int32, y: Bin64 } *)
  failwith "TODO: parse_typ_record"

and parse_typ_optional st =
  (* Parse optional types
   * Syntax: T?
   * Example: Int32?, String?, Point? *)
  failwith "TODO: parse_typ_optional"

let rec parse_pat st = parse_pat_atom st

and parse_pat_atom st =
  (* Entry point for pattern parsing
   * Syntax: _, 42, "hello", x, val/var x: T, Point { .x := a }, Some(x), (a, b)
   * Dispatches to specific pattern parsers based on first token *)
  failwith "TODO: parse_pat_atom"

and parse_pat_bind st =
  (* Parse binding patterns
   * Syntax: val x: T, var y: U
   * Example: val name: String := "Bob" *)
  failwith "TODO: parse_pat_bind"

and parse_pat_lit st =
  (* Parse literal patterns
   * Syntax: 42, 3.14, "hello", 'c', true
   * Example: case 42 => "answer" *)
  failwith "TODO: parse_pat_lit"

and parse_pat_wild st =
  (* Parse wildcard pattern
   * Syntax: _
   * Example: case _ => "default" *)
  failwith "TODO: parse_pat_wild"

and parse_pat_ident st =
  (* Parse identifier patterns
   * Syntax: x, name, value
   * Example: case x => use(x) *)
  failwith "TODO: parse_pat_ident"

and parse_pat_record st =
  (* Parse record patterns (name is optional)
   * Syntax: Point { .x := a, .y := b }, User { .name }
   * Example: case Point { .x := x, .y := y } => x + y *)
  failwith "TODO: parse_pat_record"

and parse_pat_ctor st =
  (* Parse constructor patterns
   * Syntax: Some(x), Ok(value), Err(msg), Cons(head, tail)
   * Example: case Some(x) => x, case None => 0 *)
  failwith "TODO: parse_pat_ctor"

and parse_pat_tuple st =
  (* Parse tuple patterns
   * Syntax: (a, b, c), (x, y)
   * Example: case (x, y) => x + y *)
  failwith "TODO: parse_pat_tuple"

let rec parse_expr st = parse_expr_bp 0 st

and parse_expr_bp min_bp st =
  let tok = curr st in
  let span = curr_span st in
  match parse_expr_unary tok span st with
  | Error _ as e -> e
  | Ok (mut_lhs, st') ->
    let rec loop lhs st'' =
      match curr st'' with
      | Token.EOF -> Ok (lhs, st'')
      | Token.LtMinus when min_bp <= 1 -> (
        let st''' = { st'' with pos = st''.pos + 1 } in
        match parse_expr_bp 0 st''' with
        | Error _ as e -> e
        | Ok (rhs_expr, st'''') -> (
          let span' = Span.merge (Node.span lhs) (Node.span rhs_expr) in
          match lhs.data with
          | Node.ExprIdent target ->
            let assign_expr =
              mk_node span' (Node.ExprAssign { target; value = rhs_expr })
            in
            Ok (assign_expr, st'''')
          | _ ->
            let span = curr_span st''' in
            let diag = Errors.parse_diag Errors.E1007 span [] in
            Error diag))
      | infix_tok when can_bind_infix infix_tok -> (
        let { lhs = infix_lhs; rhs } = infix_bp infix_tok in
        if infix_lhs < min_bp then Ok (lhs, st'')
        else
          let st''' = { st'' with pos = st''.pos + 1 } in
          match parse_expr_bp rhs st''' with
          | Error _ as e -> e
          | Ok (rhs_expr, _st'''' (* TODO: what is `st''''` for? *)) ->
            let span' = Span.merge (Node.span lhs) (Node.span rhs_expr) in
            let lhs' =
              mk_node
                span'
                (Node.ExprCall
                   {
                     callee = lhs
                   ; typ_args = None
                   ; args = [ rhs_expr ]
                   ; optional = false
                   })
            in
            loop lhs' st''')
      | _ when can_bind_postfix (curr st'') -> (
        let bp = postfix_bp (curr st'') in
        if bp < min_bp then Ok (lhs, st'')
        else
          match curr st'' with
          | Token.LParen -> parse_expr_call lhs st''
          | Token.Dot -> parse_expr_member lhs st''
          | _ -> Ok (lhs, st''))
      | _ -> Ok (lhs, st'')
    in
    loop mut_lhs st'

and parse_expr_unary tok span st =
  match tok with
  | _ when can_bind_prefix tok -> (
    let r_bp = prefix_bp tok in
    let st' = { st with pos = st.pos + 1 } in
    match parse_expr_bp r_bp st' with
    | Error _ as e -> e
    | Ok (operand, st'') ->
      let span' = Span.merge span (Node.span operand) in
      Ok (mk_node span' (Node.ExprUnary { op = tok; arg = operand }), st''))
  | _ -> parse_expr_atom tok span st

and parse_expr_atom tok span st =
  match tok with
  | Token.Ident name ->
    let st' = { st with pos = st.pos + 1 } in
    let ident_expr = mk_node span (Node.ExprIdent name) in
    parse_expr_postfix ident_expr st'
  | Token.LitNumber s ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitWhole s)), st')
  | Token.LitString name ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitString name)), st')
  | Token.LitRune c ->
    let st' = { st with pos = st.pos + 1 } in
    Ok (mk_node span (Node.ExprLit (Node.LitRune c)), st')
  | Token.LParen -> parse_grouped_or_expr_tuple span st
  | _ ->
    let diag = Errors.parse_diag Errors.E1003 span [] in
    Error diag

and parse_expr_postfix expr st =
  match curr st with
  | tok when can_bind_postfix tok ->
    let bp = postfix_bp tok in
    parse_expr_postfix_bp bp expr st
  | _ -> Ok (expr, st)

and parse_expr_postfix_bp min_bp expr st =
  match curr st with
  | Token.LParen when min_bp <= 1000 -> parse_expr_call expr st
  | Token.Dot when min_bp <= 1000 -> parse_expr_field expr false st
  | Token.LBrack when min_bp <= 1000 -> parse_expr_index expr false st
  | Token.Question when min_bp <= 1001 ->
    (* expr?[thing] *)
    ( advance >>= fun () ->
      token >>= fun (tok, tok_span) ->
      match tok with
      | Token.LBrack -> parse_expr_index expr true
      | Token.Dot -> parse_expr_field expr true
      | _ -> parse_error Errors.E1005 tok_span )
      st
  | _ -> return expr st

and parse_expr_call callee =
  token_expect Token.LParen >>= fun () ->
  sep_by parse_expr (token_expect Token.Comma) >>= fun args ->
  token_expect Token.RParen >>= fun () ->
  let span = Node.span callee in
  let call_expr =
    mk_node
      span
      (Node.ExprCall { callee; typ_args = None; args; optional = false })
  in
  return call_expr

and parse_expr_field obj optional =
  advance >>= fun () ->
  token >>= fun (tok, tok_span) ->
  match tok with
  | Token.Ident field_name ->
    let full_span = Span.merge (Node.span obj) tok_span in
    let field_expr =
      mk_node full_span (Node.ExprField { obj; prop = field_name; optional })
    in
    return field_expr
  | _ -> parse_error Errors.E1005 tok_span

and parse_expr_index obj optional =
  advance >>= fun () ->
  parse_expr >>= fun index_expr ->
  token >>= fun (tok, tok_span) ->
  match tok with
  | Token.RBrack ->
    let full_span = Span.merge (Node.span obj) tok_span in
    let index_expr =
      mk_node full_span (Node.ExprIndex { obj; index = index_expr; optional })
    in
    return index_expr
  | _ -> parse_error Errors.E1006 tok_span

and parse_expr_member obj = parse_expr_field obj false

and parse_expr_deref obj =
  advance >>= fun () ->
  parse_expr >>= fun inner_expr ->
  token >>= fun (tok, tok_span) ->
  match tok with
  | Token.RBrack ->
    let full_span = Span.merge (Node.span obj) tok_span in
    let deref_expr = mk_node full_span (Node.ExprDeref inner_expr) in
    return deref_expr
  | _ -> parse_error Errors.E1006 tok_span

and parse_expr_args st =
  (* Parse comma-separated function arguments with type args
   * Syntax: <T, U>(x: T, y: U), (a, b), ()
   * Example: map<Int32, String>(func, list) *)
  failwith "TODO: parse_expr_args"

and parse_grouped_or_expr_tuple start_span st =
  (* Distinguish between grouped expressions and tuples
   * Syntax: (expr) vs (expr1, expr2, ...)
   * Example: (x + 1) vs (a, b, c) *)
  failwith "TODO: parse_grouped_or_expr_tuple"

and parse_expr_tuple st =
  (* Parse tuple expressions
   * Syntax: (expr1, expr2, expr3)
   * Example: (1, "hello", true) *)
  failwith "TODO: parse_expr_tuple"

and parse_expr_lit st =
  (* Parse literal expressions (mostly handled in parse_expr_atom)
   * Syntax: 42, 3.14, "string", 'c', true
   * Example: return 42 *)
  failwith "TODO: parse_expr_lit"

and parse_expr_ident st =
  (* Parse identifier expressions (mostly handled in parse_expr_atom)
   * Syntax: x, name, function_name
   * Example: x + 1 *)
  failwith "TODO: parse_expr_ident"

and parse_expr_if st =
  (* Parse if expressions
   * Syntax: if cond { then_expr } else { else_expr }
   * Example: if x > 0 { x } else { -x } *)
  failwith "TODO: parse_expr_if"

and parse_expr_match st =
  (* Parse match expressions
   * Syntax: match expr { case pat => expr, case pat2 => expr2 }
   * Example: match opt { case Some(x) => x, case None => 0 } *)
  failwith "TODO: parse_expr_match"

and parse_expr_for st =
  (* Parse for loop expressions
   * Syntax: for x in range { body_expr }
   * Example: for i in 0..10 { process(i) } *)
  failwith "TODO: parse_expr_for"

and parse_expr_while st =
  (* Parse while loop expressions
   * Syntax: while cond { body_expr }
   * Example: while running { tick() } *)
  failwith "TODO: parse_expr_while"

and parse_expr_block st =
  (* Parse block expressions
   * Syntax: { stmt1; stmt2; expr }
   * Example: { val x := 1; x * 2 } *)
  failwith "TODO: parse_expr_block"

and parse_expr_fn st =
  (* Parse function literal expressions
   * Syntax: fn(x: T, y: U) -> R { body }, fn name(params) -> R { body }
   * Example: fn(x: Int32) -> Int32 { return x * 2; } *)
  failwith "TODO: parse_expr_fn"

and parse_expr_record st =
  (* Parse record definition expressions (name is optional)
   * Syntax: record Name { x: T, y: U }, record Name<T> { f: T }
   * Example: record { x: Int32, y: Bin64 } *)
  failwith "TODO: parse_expr_record"

and parse_expr_record_lit st =
  (* Parse record literal expressions (name is optional)
   * Syntax: Point { .x := 1, .y := 2 }, { .name := "Bob" }
   * Example: Point { .x := 3.0, .y := 4.0 } *)
  failwith "TODO: parse_expr_record_lit"

and parse_expr_choice st =
  (* Parse choice/sum type definition expressions (name is optional)
   * Syntax: choice<T> { case A(T), case B(U), case C }
   * Example: choice Option<T> { case Some(T), case None }
   * NOTE: choice cases have parameters--named and unnamed. using `var t: T` makes the case parameter mutable (same as in record fields/fn params) <-- they use the same system, including defaulting
   * NOTE: this means, choice case parameters, record fields, and function parameters use the exact same system:
   * named parameters (implicit 'val'): `case A(x: T), case B(var y: U)`
   * unnamed parameters (implicit 'val'): `case A(T), case B(var U)`
   *)
  failwith "TODO: parse_expr_choice"

and parse_expr_unsafe st =
  (* Parse unsafe block expressions
   * Syntax: unsafe { expr }
   * Example: unsafe { [ptr] <- 42 } *)
  failwith "TODO: parse_expr_unsafe"

and parse_expr_break st =
  (* Parse break expressions
   * Syntax: break, break expr
   * Example: break, break result_value *)
  failwith "TODO: parse_expr_break"

and parse_expr_cycle st =
  (* Parse cycle expressions (continue to next iteration)
   * Syntax: cycle
   * Example: if done { break } else { cycle } *)
  failwith "TODO: parse_expr_cycle"

and parse_expr_return st =
  (* Parse return expressions
   * Syntax: return, return expr
   * Example: return, return x + 1 *)
  failwith "TODO: parse_expr_return"

and parse_expr_defer st =
  (* Parse defer expressions
   * Syntax: defer expr
   * Example: defer close_file(handle) *)
  failwith "TODO: parse_expr_defer"

let rec parse_stmt st =
  (* Main statement dispatcher
   * Syntax: expr_stmt, bind_stmt, import_stmt, export_stmt, extern_stmt
   * Dispatches to specific statement parsers based on first token
   * expr_stmt requires mandatory semicolon at the end, this means 'expr' doesn't have semi, and expr_stmt (internally StmtExpr sum type variant)
   * adds a semicolon to the end of the expression
   *)
  failwith "TODO: parse_stmt"

and parse_stmt_bind st =
  (* Parse binding statements
   * Syntax: val name: T := expr, var name: T := expr
   * Example: val x: Int32 := 42, var y: String := "hello" *)
  failwith "TODO: parse_stmt_bind"

and parse_stmt_import st =
  (* Parse import statements
   * Syntax: import { names } from "module", import * as alias from "module"
   * Example: import { writeln } from "@std/io.ms" *)
  failwith "TODO: parse_stmt_import"

and parse_stmt_export st =
  (* Parse export statements
   * Syntax: export { names }, export * as alias, export name from "module"
   * Example: export { MyType, my_func } *)
  failwith "TODO: parse_stmt_export"

and parse_stmt_extern st =
  (* Parse extern block statements
   * Syntax: extern "ABI" { fn_sig1; fn_sig2; }
   * Example: extern "C" { fn malloc(size: Nat64) -> ^Unit; } *)
  failwith "TODO: parse_stmt_extern"

let parse_prog st =
  (* Parse complete program (list of statements)
   * Syntax: stmt1; stmt2; stmt3; ...
   * Example: val x := 1; val y := 2; x + y *)
  failwith "TODO: parse_prog"
