open Alcotest
open Test_validate

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

let format_bytecode interner instr_arrays proc_table =
  let buf = Buffer.create 256 in
  Array.iteri
    (fun id instrs ->
      let name =
        if id = 0 then "<main>"
        else
          let found = ref None in
          Hashtbl.iter
            (fun n info ->
              if info.Emitter.id = id then
                found := Some (Interner.lookup interner n))
            proc_table;
          match !found with Some n -> n | None -> "<unknown>"
      in
      Buffer.add_string buf (Printf.sprintf "proc %d %s:\n" id name);
      List.iter
        (fun instr -> Buffer.add_string buf ("  " ^ Instr.show instr ^ "\n"))
        instrs;
      if id < Array.length instr_arrays - 1 then Buffer.add_string buf "\n")
    instr_arrays;
  Buffer.contents buf

type stage = Lex | Parse | Full

let empty_result interner diags =
  let empty_module_desc =
    { Metadata.module_name = None; exports = []; link_keys = [] }
  in
  (interner, diags, ([||], empty_module_desc), Hashtbl.create 0)

let run_pipeline source stage =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, lex_diags = Lexer.lex lexer in
  match stage with
  | Lex -> empty_result interner lex_diags
  | Parse ->
    let parser = Parser.make tokens interner in
    let _ast, parse_diags = Parser.parse parser in
    let diags = Diagnostic.merge [ lex_diags; parse_diags ] in
    empty_result interner diags
  | Full ->
    let parser = Parser.make tokens interner in
    let ast, parse_diags = Parser.parse parser in
    let resolver = Resolver.create interner in
    let resolve_diags = Resolver.resolve resolver ast in
    let checker = Checker.create interner (Resolver.symbols resolver) in
    let check_diags = Checker.check checker ast in
    let proc_table = Emitter.collect_procs interner ast in
    let codegen_diags = ref Diagnostic.empty_bag in
    let procs = Emitter.emit_program interner proc_table codegen_diags ast in
    let diags =
      Diagnostic.merge
        [ lex_diags; parse_diags; resolve_diags; check_diags; !codegen_diags ]
    in
    (interner, diags, procs, proc_table)

let test_pass path () =
  let source = read_file path in
  let basename = Filename.basename path in
  let stage =
    if String.starts_with ~prefix:"lex_" basename then Lex
    else if String.starts_with ~prefix:"parse_" basename then Parse
    else Full
  in
  let interner, diags, procs, proc_table = run_pipeline source stage in
  Validate.check_no_errors ~diags;
  let expect_path = String.sub path 0 (String.length path - 3) ^ ".expect" in
  if Sys.file_exists expect_path then
    let expected = read_file expect_path in
    let procs, _module_desc = procs in
    let actual = format_bytecode interner procs proc_table in
    Validate.check_bytecode ~expected ~actual

let test_fail path () =
  let source = read_file path in
  let basename = Filename.basename path in
  let stage =
    if String.starts_with ~prefix:"lex_" basename then Lex
    else if String.starts_with ~prefix:"parse_" basename then Parse
    else Full
  in
  let _interner, diags, _procs, _proc_table = run_pipeline source stage in
  Validate.check_has_errors ~diags

let () =
  let pass_files = collect_files "pass" in
  let fail_files = collect_files "fail" in
  let pass_tests =
    List.map
      (fun path -> test_case (Filename.basename path) `Quick (test_pass path))
      pass_files
  in
  let fail_tests =
    List.map
      (fun path -> test_case (Filename.basename path) `Quick (test_fail path))
      fail_files
  in
  run "Integration Tests" [ ("pass", pass_tests); ("fail", fail_tests) ]
