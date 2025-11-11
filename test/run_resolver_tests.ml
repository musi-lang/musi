open Musi_basic
open Musi_sema

let test_pass path () =
  let stmts, interner, src, parse_diags = Test_helpers.run_pipeline path in
  let resolve_diags = ref Diagnostic.empty_bag in
  let base_path = Filename.dirname path in
  let _ = Resolver.resolve ~base_path stmts interner resolve_diags in
  let diags = Diagnostic.merge [ parse_diags; !resolve_diags ] in
  Diagnostic.emit_all Format.std_formatter diags src;
  Alcotest.(check bool) "no errors" false (Diagnostic.has_errors diags)

let test_fail path () =
  let stmts, interner, src, parse_diags = Test_helpers.run_pipeline path in
  let resolve_diags = ref Diagnostic.empty_bag in
  let base_path = Filename.dirname path in
  let _ = Resolver.resolve ~base_path stmts interner resolve_diags in
  let diags = Diagnostic.merge [ parse_diags; !resolve_diags ] in
  Diagnostic.emit_all Format.std_formatter diags src;
  Alcotest.(check bool) "has errors" true (Diagnostic.has_errors diags)

let () =
  let pass_files = Test_helpers.collect_files "resolver/pass" in
  let fail_files = Test_helpers.collect_files "resolver/fail" in
  let tests =
    Test_helpers.make_test_cases pass_files fail_files test_pass test_fail
  in
  Alcotest.run "Resolver Integration" tests
