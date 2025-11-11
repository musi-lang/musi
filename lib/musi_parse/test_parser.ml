open Musi_basic
open Musi_lex
open Musi_parse
open Alcotest

let parse src =
  let interner = Interner.create () in
  let file_id, source = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let tokens, _ = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let stmts, diags = Parser.parse parser in
  Diagnostic.emit_all Format.std_formatter diags source;
  (stmts, diags, interner)

let get_expr stmts =
  match stmts with
  | [ { Node.skind = Node.StmtExpr (expr, _); _ } ] -> Some expr
  | _ -> None

(* === LITERAL TESTS === *)

let test_lit_int () =
  let stmts, diags, _ = parse "42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitInt "42"); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitInt"

let test_lit_bin () =
  let stmts, diags, _ = parse "3.14" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitBin _); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitBin"

let test_lit_str () =
  let stmts, diags, _ = parse "\"hello\"" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitStr _); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitStr"

let test_lit_bool_true () =
  let stmts, diags, _ = parse "true" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitBool true); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitBool true"

let test_lit_bool_false () =
  let stmts, diags, _ = parse "false" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitBool false); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitBool false"

let test_lit_record () =
  let stmts, diags, _ = parse "{ .x := 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprLiteral (Node.LitRecord _); _ } -> ()
  | _ -> fail "expected ExprLiteral.LitRecord"

(* === EXPRESSION TESTS === *)

let test_expr_ident () =
  let stmts, diags, _ = parse "foo" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprIdent _; _ } -> ()
  | _ -> fail "expected ExprIdent"

let test_expr_binary () =
  let stmts, diags, _ = parse "1 + 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprBinary (Token.Plus, _, _); _ } -> ()
  | _ -> fail "expected ExprBinary.Plus"

let test_expr_unary () =
  let stmts, diags, _ = parse "-5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprUnary (Token.Minus, _); _ } -> ()
  | _ -> fail "expected ExprUnary.Minus"

let test_expr_call () =
  let stmts, diags, _ = parse "foo(1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprCall (_, _, false); _ } -> ()
  | _ -> fail "expected ExprCall"

let test_expr_field () =
  let stmts, diags, _ = parse "obj.field" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprField (_, _, false); _ } -> ()
  | _ -> fail "expected ExprField"

let test_expr_index () =
  let stmts, diags, _ = parse "arr[0]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprIndex (_, _, false); _ } -> ()
  | _ -> fail "expected ExprIndex"

let test_expr_tuple () =
  let stmts, diags, _ = parse "(1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprTuple _; _ } -> ()
  | _ -> fail "expected ExprTuple"

let test_expr_array () =
  let stmts, diags, _ = parse "[1, 2, 3]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprArray _; _ } -> ()
  | _ -> fail "expected ExprArray"

let test_expr_record () =
  let stmts, diags, _ = parse "record { x: Nat }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprRecord _; _ } -> ()
  | _ -> fail "expected ExprRecord"

let test_expr_block () =
  let stmts, diags, _ = parse "{ 1; 2 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprBlock _; _ } -> ()
  | _ -> fail "expected ExprBlock"

let test_expr_if () =
  let stmts, diags, _ = parse "if true then 1 else 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprIf _; _ } -> ()
  | _ -> fail "expected ExprIf"

let test_expr_match () =
  let stmts, diags, _ = parse "match x with { case 0 -> 1, case _ -> 2 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprMatch _; _ } -> ()
  | _ -> fail "expected ExprMatch"

let test_expr_while () =
  let stmts, diags, _ = parse "while true do 1" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprWhile _; _ } -> ()
  | _ -> fail "expected ExprWhile"

let test_expr_do () =
  let stmts, diags, _ = parse "do { 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprDo _; _ } -> ()
  | _ -> fail "expected ExprDo"

let test_expr_for () =
  let stmts, diags, _ = parse "for x in arr do x" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprFor _; _ } -> ()
  | _ -> fail "expected ExprFor"

let test_expr_range () =
  let stmts, diags, _ = parse "1..10" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprRange (_, _, true); _ } -> ()
  | _ -> fail "expected ExprRange"

let test_expr_assign () =
  let stmts, diags, _ = parse "x <- 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprAssign _; _ } -> ()
  | _ -> fail "expected ExprAssign"

let test_expr_return () =
  let stmts, diags, _ = parse "return 42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprReturn (Some _); _ } -> ()
  | _ -> fail "expected ExprReturn"

let test_expr_break () =
  let stmts, diags, _ = parse "break" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprBreak None; _ } -> ()
  | _ -> fail "expected ExprBreak"

let test_expr_continue () =
  let stmts, diags, _ = parse "continue" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprContinue; _ } -> ()
  | _ -> fail "expected ExprContinue"

let test_expr_yield () =
  let stmts, diags, _ = parse "yield 1" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprYield (Some _); _ } -> ()
  | _ -> fail "expected ExprYield"

let test_expr_await () =
  let stmts, diags, _ = parse "await foo()" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprAwait _; _ } -> ()
  | _ -> fail "expected ExprAwait"

let test_expr_try () =
  let stmts, diags, _ = parse "try foo()" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprTry _; _ } -> ()
  | _ -> fail "expected ExprTry"

let test_expr_defer () =
  let stmts, diags, _ = parse "defer cleanup()" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprDefer _; _ } -> ()
  | _ -> fail "expected ExprDefer"

let test_expr_unwrap () =
  let stmts, diags, _ = parse "x!" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprUnwrap _; _ } -> ()
  | _ -> fail "expected ExprUnwrap"

let test_expr_cast () =
  let stmts, diags, _ = parse "x as Int" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprCast _; _ } -> ()
  | _ -> fail "expected ExprCast"

let test_expr_test () =
  let stmts, diags, _ = parse "x is Int" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprTest _; _ } -> ()
  | _ -> fail "expected ExprTest"

let test_expr_async () =
  let stmts, diags, _ = parse "async { 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprAsync _; _ } -> ()
  | _ -> fail "expected ExprAsync"

let test_expr_unsafe () =
  let stmts, diags, _ = parse "unsafe { 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprUnsafe _; _ } -> ()
  | _ -> fail "expected ExprUnsafe"

let test_expr_choice () =
  let stmts, diags, _ = parse "choice { case A, case B }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprChoice _; _ } -> ()
  | _ -> fail "expected ExprChoice"

let test_expr_binding_const () =
  let stmts, diags, _ = parse "const x := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprBinding (true, _, _, _, _, _); _ } -> ()
  | _ -> fail "expected ExprBinding const"

let test_expr_binding_var () =
  let stmts, diags, _ = parse "var x := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprBinding (false, _, _, _, _, _); _ } -> ()
  | _ -> fail "expected ExprBinding var"

let test_expr_proc () =
  let stmts, diags, _ = parse "proc () { 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some { Node.ekind = Node.ExprProc _; _ } -> ()
  | _ -> fail "expected ExprProc"

(* === PATTERN TESTS === *)

let test_pat_ident () =
  let stmts, diags, _ = parse "const x := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding (_, _, { Node.pkind = Node.PatIdent _; _ }, _, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.PatIdent"

let test_pat_wild () =
  let stmts, diags, _ = parse "match x with { case _ -> 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprMatch
            (_, [ { Node.cpat = { Node.pkind = Node.PatWild; _ }; _ } ])
      ; _
      } ->
    ()
  | _ -> fail "expected ExprMatch.PatWild"

let test_pat_tuple () =
  let stmts, diags, _ = parse "const (x, y) := (1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding (_, _, { Node.pkind = Node.PatTuple _; _ }, _, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.PatTuple"

let test_pat_array () =
  let stmts, diags, _ = parse "const [x, y] := [1, 2]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding (_, _, { Node.pkind = Node.PatArray _; _ }, _, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.PatArray"

let test_pat_record () =
  let stmts, diags, _ = parse "const { .x := a } := obj" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding (_, _, { Node.pkind = Node.PatRecord _; _ }, _, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.PatRecord"

let test_pat_literal () =
  let stmts, diags, _ = parse "match x with { case 42 -> 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprMatch
            (_, [ { Node.cpat = { Node.pkind = Node.PatLiteral _; _ }; _ } ])
      ; _
      } ->
    ()
  | _ -> fail "expected ExprMatch.PatLiteral"

let test_pat_binding () =
  let stmts, diags, _ = parse "match x with { case const n -> n }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprMatch
            (_, [ { Node.cpat = { Node.pkind = Node.PatBinding _; _ }; _ } ])
      ; _
      } ->
    ()
  | _ -> fail "expected ExprMatch.PatBinding"

(* === TYPE TESTS === *)

let test_ty_named () =
  let stmts, diags, _ = parse "const x: Int := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding
            (_, _, _, Some { Node.tkind = Node.TyNamed _; _ }, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.TyNamed"

let test_ty_tuple () =
  let stmts, diags, _ = parse "const x: (Int, Nat) := (1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding
            (_, _, _, Some { Node.tkind = Node.TyTuple _; _ }, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.TyTuple"

let test_ty_array () =
  let stmts, diags, _ = parse "const x: [Int] := [1]" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding
            (_, _, _, Some { Node.tkind = Node.TyArray _; _ }, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.TyArray"

let test_ty_optional () =
  let stmts, diags, _ = parse "const x: Int? := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match get_expr stmts with
  | Some
      {
        Node.ekind =
          Node.ExprBinding
            (_, _, _, Some { Node.tkind = Node.TyOptional _; _ }, _, _)
      ; _
      } ->
    ()
  | _ -> fail "expected ExprBinding.TyOptional"

(* === STATEMENT TESTS === *)

let test_stmt_import () =
  let stmts, diags, _ = parse "import { foo } from \"bar\"" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match stmts with
  | [ { Node.skind = Node.StmtImport _; _ } ] -> ()
  | _ -> fail "expected StmtImport"

let test_stmt_export () =
  let stmts, diags, _ = parse "export { foo }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match stmts with
  | [ { Node.skind = Node.StmtExport _; _ } ] -> ()
  | _ -> fail "expected StmtExport"

let () =
  run
    "Parser"
    [
      ( "literals"
      , [
          test_case "int" `Quick test_lit_int
        ; test_case "bin" `Quick test_lit_bin
        ; test_case "str" `Quick test_lit_str
        ; test_case "bool true" `Quick test_lit_bool_true
        ; test_case "bool false" `Quick test_lit_bool_false
        ; test_case "record" `Quick test_lit_record
        ] )
    ; ( "expressions"
      , [
          test_case "ident" `Quick test_expr_ident
        ; test_case "binary" `Quick test_expr_binary
        ; test_case "unary" `Quick test_expr_unary
        ; test_case "call" `Quick test_expr_call
        ; test_case "field" `Quick test_expr_field
        ; test_case "index" `Quick test_expr_index
        ; test_case "tuple" `Quick test_expr_tuple
        ; test_case "array" `Quick test_expr_array
        ; test_case "record" `Quick test_expr_record
        ; test_case "block" `Quick test_expr_block
        ; test_case "if" `Quick test_expr_if
        ; test_case "match" `Quick test_expr_match
        ; test_case "while" `Quick test_expr_while
        ; test_case "do" `Quick test_expr_do
        ; test_case "for" `Quick test_expr_for
        ; test_case "range" `Quick test_expr_range
        ; test_case "assign" `Quick test_expr_assign
        ; test_case "return" `Quick test_expr_return
        ; test_case "break" `Quick test_expr_break
        ; test_case "continue" `Quick test_expr_continue
        ; test_case "yield" `Quick test_expr_yield
        ; test_case "await" `Quick test_expr_await
        ; test_case "try" `Quick test_expr_try
        ; test_case "defer" `Quick test_expr_defer
        ; test_case "unwrap" `Quick test_expr_unwrap
        ; test_case "cast" `Quick test_expr_cast
        ; test_case "test" `Quick test_expr_test
        ; test_case "async" `Quick test_expr_async
        ; test_case "unsafe" `Quick test_expr_unsafe
        ; test_case "choice" `Quick test_expr_choice
        ; test_case "binding const" `Quick test_expr_binding_const
        ; test_case "binding var" `Quick test_expr_binding_var
        ; test_case "proc" `Quick test_expr_proc
        ] )
    ; ( "patterns"
      , [
          test_case "ident" `Quick test_pat_ident
        ; test_case "wild" `Quick test_pat_wild
        ; test_case "tuple" `Quick test_pat_tuple
        ; test_case "array" `Quick test_pat_array
        ; test_case "record" `Quick test_pat_record
        ; test_case "literal" `Quick test_pat_literal
        ; test_case "binding" `Quick test_pat_binding
        ] )
    ; ( "types"
      , [
          test_case "named" `Quick test_ty_named
        ; test_case "tuple" `Quick test_ty_tuple
        ; test_case "array" `Quick test_ty_array
        ; test_case "optional" `Quick test_ty_optional
        ] )
    ; ( "statements"
      , [
          test_case "import" `Quick test_stmt_import
        ; test_case "export" `Quick test_stmt_export
        ] )
    ]
