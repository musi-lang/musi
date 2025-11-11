let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let collect_files dir =
  let rec loop acc handle =
    match Unix.readdir handle with
    | exception End_of_file -> acc
    | name ->
      if String.ends_with ~suffix:".ms" name then
        loop (Filename.concat dir name :: acc) handle
      else loop acc handle
  in
  let handle = Unix.opendir dir in
  let files = loop [] handle in
  Unix.closedir handle;
  List.sort String.compare files

let run_pipeline path =
  let open Musi_basic in
  let open Musi_lex in
  let open Musi_parse in
  let source = read_file path in
  let interner = Interner.create () in
  let file_id, src = Source.add_file Source.empty path source in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex_all lexer in
  let parser = Parser.make tokens interner in
  let stmts, parse_diags = Parser.parse parser in
  (stmts, interner, src, Diagnostic.merge [ lex_diags; parse_diags ])

let make_test_cases pass_files fail_files test_pass test_fail =
  let pass_tests =
    List.map
      (fun path ->
        Alcotest.test_case (Filename.basename path) `Quick (test_pass path))
      pass_files
  in
  let fail_tests =
    List.map
      (fun path ->
        Alcotest.test_case (Filename.basename path) `Quick (test_fail path))
      fail_files
  in
  [ ("pass", pass_tests); ("fail", fail_tests) ]
