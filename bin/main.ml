open Basic
open Lex
open Parse
open Cmdliner

type config = { verbose : bool }

let make_config verbose = { verbose }

let check_file config path =
  if config.verbose then Printf.printf "Checking %s...\n" path;
  let interner = Interner.create () in
  try
    let content = In_channel.with_open_bin path In_channel.input_all in
    let source = Source.create path content in
    let file_id = 0 in
    let lexer = Lexer.create ~interner:(Some interner) source file_id in
    match Lexer.try_tokenize lexer with
    | Error bag ->
      Reporter.emit_all Format.err_formatter bag [ (file_id, source) ];
      if not config.verbose then exit 1
    | Ok tokens -> (
      match Parser.try_parse (List.to_seq tokens) source file_id interner with
      | Error bag ->
        Reporter.emit_all Format.err_formatter bag [ (file_id, source) ];
        if not config.verbose then exit 1
      | Ok _ -> if config.verbose then Printf.printf "Syntax OK\n")
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

let run_cmd_impl _config script_path args =
  Printf.printf "Running %s" script_path;
  if args <> [] then Printf.printf " with arg(s): %s" (String.concat " " args);
  print_newline ()

let fmt_cmd_impl _config files =
  Printf.printf "Formatting %d file(s)...\n" (List.length files)

let lint_cmd_impl _config files =
  Printf.printf "Linting %d file(s)...\n" (List.length files)

let test_cmd_impl _config files =
  Printf.printf "Running test(s) on %d file(s)...\n" (List.length files)

let verbose =
  let doc = "Print verbose output." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let config_t = Term.(const make_config $ verbose)

let check_cmd =
  let doc = "Check Musi source file for syntax errors" in
  let info = Cmd.info "check" ~doc in
  let path = Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE") in
  Cmd.v info Term.(const check_file $ config_t $ path)

let run_cmd =
  let doc = "Run Musi script" in
  let info = Cmd.info "run" ~doc in
  let script =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SCRIPT")
  in
  let args = Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS") in
  Cmd.v info Term.(const run_cmd_impl $ config_t $ script $ args)

let fmt_cmd =
  let doc = "Format Musi source files" in
  let info = Cmd.info "fmt" ~doc in
  let files = Arg.(value & pos_all string [] & info [] ~docv:"FILES") in
  Cmd.v info Term.(const fmt_cmd_impl $ config_t $ files)

let lint_cmd =
  let doc = "Lint Musi source files" in
  let info = Cmd.info "lint" ~doc in
  let files = Arg.(value & pos_all string [] & info [] ~docv:"FILES") in
  Cmd.v info Term.(const lint_cmd_impl $ config_t $ files)

let test_cmd =
  let doc = "Run tests" in
  let info = Cmd.info "test" ~doc in
  let files = Arg.(value & pos_all string [] & info [] ~docv:"FILES") in
  Cmd.v info Term.(const test_cmd_impl $ config_t $ files)

let main_cmd =
  let doc = "Musi Compiler & Toolchain" in
  let info = Cmd.info "music" ~doc ~version:"0.1.0" in
  Cmd.group info [ check_cmd; run_cmd; fmt_cmd; lint_cmd; test_cmd ]

let () = exit (Cmd.eval main_cmd)
