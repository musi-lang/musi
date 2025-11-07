open Alcotest

let make_parser source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _ = Lexer.lex lexer in
  (Parser.make tokens interner, interner)

let check_node_kind expected_kind source =
  let p, _ = make_parser source in
  let ast, diags = Parser.parse p in
  check bool "no errors" false (Diagnostic.has_errors diags);
  match ast with
  | [ { Node.kind; _ } ] when Node.show_kind kind = expected_kind -> ()
  | [ { Node.kind; _ } ] ->
    failwith
      (Printf.sprintf
         "expected '%s', got '%s'"
         expected_kind
         (Node.show_kind kind))
  | _ -> failwith "unexpected AST structure"

let check_expr_kind expected_kind source =
  let p, _ = make_parser source in
  let ast, diags = Parser.parse p in
  check bool "no errors" false (Diagnostic.has_errors diags);
  match ast with
  | [ { Node.kind = StmtExpr { kind; _ }; _ } ]
    when Node.show_kind kind = expected_kind ->
    ()
  | [ { Node.kind = StmtExpr { kind; _ }; _ } ] ->
    failwith
      (Printf.sprintf
         "expected '%s', got '%s'"
         expected_kind
         (Node.show_kind kind))
  | [ stmt ] ->
    failwith
      (Printf.sprintf
         "expected 'StmtExpr(%s)', got '%s'"
         expected_kind
         (Node.show_kind stmt.Node.kind))
  | _ -> failwith "unexpected AST structure"

let test_parse_ident () = check_expr_kind "ExprIdent" "foo;"
let test_parse_literal () = check_expr_kind "ExprLitNumeric" "42;"
let test_parse_text () = check_expr_kind "ExprLitText" "\"hello\";"
let test_parse_bool () = check_expr_kind "ExprLitBool" "true;"
let test_parse_record () = check_expr_kind "ExprLitRecord" "Foo{ .x := 1 };"
let test_parse_array () = check_expr_kind "ExprArray" "[1, 2, 3];"
let test_parse_tuple () = check_expr_kind "ExprTuple" "(1, 2);"
let test_parse_binary () = check_expr_kind "ExprBinary" "1 + 2;"
let test_parse_unary () = check_expr_kind "ExprUnary" "-1;"
let test_parse_assign () = check_expr_kind "ExprAssign" "x <- 1;"
let test_parse_call () = check_expr_kind "ExprCall" "foo();"
let test_parse_index () = check_expr_kind "ExprIndex" "arr[0];"
let test_parse_field () = check_expr_kind "ExprField" "obj.field;"
let test_parse_if () = check_expr_kind "ExprIf" "if x then { 1 };"
let test_parse_while () = check_expr_kind "ExprWhile" "while x do { 1 };"
let test_parse_for () = check_expr_kind "ExprFor" "for x in xs do { 1 };"
let test_parse_block () = check_expr_kind "ExprBlock" "{ 1; 2 };"

let test_parse_unsafe_block () =
  check_expr_kind "ExprBlockUnsafe" "unsafe { 1 };"

let test_parse_binding () = check_expr_kind "ExprBinding" "const x := 1;"

let test_parse_proc () =
  check_expr_kind "ExprBinding" "const f := proc () { 1 };"

let test_parse_return () = check_expr_kind "ExprReturn" "return 1;"

let test_parse_match () =
  check_expr_kind "ExprMatch" "match x with { case 1 -> 2 };"

let test_parse_break () = check_expr_kind "ExprBreak" "break;"
let test_parse_continue () = check_expr_kind "ExprContinue" "continue;"

let test_parse_import () =
  check_node_kind "StmtImport" "import { foo } from \"mod\";"

let test_parse_export () = check_node_kind "StmtExport" "export { foo };"
let test_parse_alias () = check_node_kind "StmtAlias" "alias Foo := Bar;"

let () =
  run
    "Parser"
    [
      ( "expressions"
      , [
          test_case "ExprIdent" `Quick test_parse_ident
        ; test_case "ExprLitNumeric" `Quick test_parse_literal
        ; test_case "ExprLitText" `Quick test_parse_text
        ; test_case "ExprLitBool" `Quick test_parse_bool
        ; test_case "ExprLitRecord" `Quick test_parse_record
        ; test_case "ExprArray" `Quick test_parse_array
        ; test_case "ExprTuple" `Quick test_parse_tuple
        ; test_case "ExprBinary" `Quick test_parse_binary
        ; test_case "ExprUnary" `Quick test_parse_unary
        ; test_case "ExprAssign" `Quick test_parse_assign
        ; test_case "ExprCall" `Quick test_parse_call
        ; test_case "ExprIndex" `Quick test_parse_index
        ; test_case "ExprField" `Quick test_parse_field
        ; test_case "ExprIf" `Quick test_parse_if
        ; test_case "ExprWhile" `Quick test_parse_while
        ; test_case "ExprFor" `Quick test_parse_for
        ; test_case "ExprBlock" `Quick test_parse_block
        ; test_case "ExprBlockUnsafe" `Quick test_parse_unsafe_block
        ; test_case "ExprBinding" `Quick test_parse_binding
        ; test_case "ExprProc" `Quick test_parse_proc
        ; test_case "ExprReturn" `Quick test_parse_return
        ; test_case "ExprMatch" `Quick test_parse_match
        ; test_case "ExprBreak" `Quick test_parse_break
        ; test_case "ExprContinue" `Quick test_parse_continue
        ] )
    ; ( "statements"
      , [
          test_case "StmtImport" `Quick test_parse_import
        ; test_case "StmtExport" `Quick test_parse_export
        ; test_case "StmtAlias" `Quick test_parse_alias
        ] )
    ]
