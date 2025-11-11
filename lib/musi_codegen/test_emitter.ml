open Musi_basic
open Musi_lex
open Musi_parse
open Musi_codegen
open Alcotest

let parse_and_emit src =
  let interner = Interner.create () in
  let file_id, source = Source.add_file Source.empty "<test>" src in
  let lexer = Lexer.make file_id src interner in
  let tokens, _ = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let stmts, diags = Parser.parse parser in
  Diagnostic.emit_all Format.std_formatter diags source;
  let emitter = Emitter.make () in
  let instrs = Emitter.emit emitter stmts in
  (instrs, diags, emitter)

(* === LITERAL TESTS === *)

let test_emit_lit_int () =
  let instrs, diags, _ = parse_and_emit "42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _ ] -> ()
  | _ -> fail "expected 'LdC' instruction"

let test_emit_lit_bool () =
  let instrs, diags, _ = parse_and_emit "true" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _ ] -> ()
  | _ -> fail "expected 'LdC' instruction"

let test_emit_lit_str () =
  let instrs, diags, _ = parse_and_emit "\"hello\"" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _ ] -> ()
  | _ -> fail "expected 'LdC' instruction"

(* === EXPRESSION TESTS === *)

let test_emit_expr_binary () =
  let instrs, diags, _ = parse_and_emit "1 + 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _; Instr.LdC _; Instr.Add ] -> ()
  | _ -> fail "expected 'LdC', 'LdC', 'Add'"

let test_emit_expr_unary () =
  let instrs, diags, _ = parse_and_emit "-5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _; Instr.Neg ] -> ()
  | _ -> fail "expected 'LdC', 'Neg'"

let test_emit_expr_ident () =
  let instrs, diags, _ = parse_and_emit "x" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdLoc _ ] -> ()
  | _ -> fail "expected 'LdLoc' instruction"

let test_emit_expr_assign () =
  let instrs, diags, _ = parse_and_emit "x <- 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _; Instr.StLoc _ ] -> ()
  | _ -> fail "expected 'LdC', 'StLoc'"

let test_emit_expr_call () =
  let instrs, diags, _ = parse_and_emit "foo(1, 2)" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _; Instr.LdC _; Instr.LdLoc _; Instr.Call _ ] -> ()
  | _ -> fail "expected 'LdC' (arg1), 'LdC' (arg2), 'LdLoc' (callee), 'Call'"

let test_emit_expr_field () =
  let instrs, diags, _ = parse_and_emit "obj.field" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdLoc _; Instr.LdFld _ ] -> ()
  | _ -> fail "expected 'LdLoc', 'LdFld'"

let test_emit_expr_record () =
  let instrs, diags, _ = parse_and_emit "record { x: Nat }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.NewObj _ ] -> ()
  | _ -> fail "expected 'NewObj' instruction"

(* === CONTROL FLOW TESTS === *)

let test_emit_expr_if () =
  let instrs, diags, _ = parse_and_emit "if true then 1 else 2" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check bool)
    "has 'BrFalse' instruction"
    true
    (List.exists (function Instr.BrFalse _ -> true | _ -> false) instrs)

let test_emit_expr_while () =
  let instrs, diags, _ = parse_and_emit "while true do 1" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check bool)
    "has 'Br', 'BrTrue' instructions"
    true
    (List.exists
       (function Instr.Br _ | Instr.BrTrue _ -> true | _ -> false)
       instrs)

let test_emit_expr_return () =
  let instrs, diags, _ = parse_and_emit "return 42" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match List.rev instrs with
  | Instr.Ret :: _ -> ()
  | _ -> fail "expected 'Ret' instruction"

let test_emit_expr_break () =
  let instrs, diags, _ = parse_and_emit "while true do break" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check bool)
    "has 'Br' instructions"
    true
    (List.exists (function Instr.Br _ -> true | _ -> false) instrs)

(* === BINDING TESTS === *)

let test_emit_expr_binding () =
  let instrs, diags, _ = parse_and_emit "const x := 5" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match instrs with
  | [ Instr.LdC _; Instr.StLoc _ ] -> ()
  | _ -> fail "expected 'LdC', 'StLoc'"

(* === CONSTANT POOL TESTS === *)

let test_const_dedupl () =
  let _, diags, emitter = parse_and_emit "1 + 1" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "const pool size" 1 (List.length (Emitter.const_pool emitter))

(* === PROCEDURE TESTS === *)

let test_emit_expr_proc () =
  let instrs, diags, emitter = parse_and_emit "proc () { 1 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check bool) "has instructions" true (List.length instrs > 0);
  (check int) "proc table has 1 entry" 1 (List.length (Emitter.procs emitter))

let test_proc_with_params () =
  let _, diags, emitter = parse_and_emit "proc (x: Nat, y: Nat) { x + y }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  match Emitter.procs emitter with
  | [ proc ] -> (check int) "param count is 2" 2 proc.param_count
  | _ -> fail "expected 1 proc in table"

let test_proc_with_body () =
  let instrs, diags, _ = parse_and_emit "proc () { return 42 }" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check bool)
    "has 'Ret' instruction"
    true
    (List.exists (function Instr.Ret -> true | _ -> false) instrs)

let test_proc_extern () =
  let instrs, diags, emitter = parse_and_emit "extern \"C\" proc test()" in
  (check bool) "no errors" false (Diagnostic.has_errors diags);
  (check int) "no instructions emitted" 0 (List.length instrs);
  match Emitter.procs emitter with
  | [ proc ] ->
    (check bool) "'is_extern' is true" true proc.is_extern;
    (check bool) "'abi' is Some" true (Option.is_some proc.abi)
  | _ -> fail "expected 1 proc in table"

let () =
  run
    "Emitter"
    [
      ( "literals"
      , [
          test_case "int" `Quick test_emit_lit_int
        ; test_case "bool" `Quick test_emit_lit_bool
        ; test_case "str" `Quick test_emit_lit_str
        ] )
    ; ( "expressions"
      , [
          test_case "binary" `Quick test_emit_expr_binary
        ; test_case "unary" `Quick test_emit_expr_unary
        ; test_case "ident" `Quick test_emit_expr_ident
        ; test_case "assign" `Quick test_emit_expr_assign
        ; test_case "call" `Quick test_emit_expr_call
        ; test_case "field" `Quick test_emit_expr_field
        ; test_case "record" `Quick test_emit_expr_record
        ] )
    ; ( "control flow"
      , [
          test_case "if" `Quick test_emit_expr_if
        ; test_case "while" `Quick test_emit_expr_while
        ; test_case "return" `Quick test_emit_expr_return
        ; test_case "break" `Quick test_emit_expr_break
        ] )
    ; ("bindings", [ test_case "const" `Quick test_emit_expr_binding ])
    ; ("optimisations", [ test_case "const dedupl" `Quick test_const_dedupl ])
    ; ( "procedures"
      , [
          test_case "proc definition" `Quick test_emit_expr_proc
        ; test_case "proc with params" `Quick test_proc_with_params
        ; test_case "proc with body" `Quick test_proc_with_body
        ; test_case "extern proc" `Quick test_proc_extern
        ] )
    ]
