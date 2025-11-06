(** CLI command implementations. *)

let format_error_summary diags =
  let error_count = Diagnostic.error_count diags in
  let warning_count = Diagnostic.warning_count diags in
  let error_msg =
    if error_count = 1 then "aborting due to previous error"
    else Printf.sprintf "aborting due to %d previous errors" error_count
  in
  if warning_count > 0 then
    Printf.sprintf
      "%s; %d warning%s emitted"
      error_msg
      warning_count
      (if warning_count = 1 then "" else "s")
  else error_msg

let format_success_summary diags =
  let warning_count = Diagnostic.warning_count diags in
  if warning_count = 0 then "0 errors, 0 warnings"
  else
    Printf.sprintf
      "0 errors, %d warning%s"
      warning_count
      (if warning_count = 1 then "" else "s")

let compile input output_opt _search_paths =
  let output =
    match output_opt with
    | Some path -> path
    | None -> Filename.remove_extension input ^ ".msc"
  in
  Output.compiling input output;
  let start_time = Unix.gettimeofday () in
  Pipeline.compile_file input output;
  let elapsed = Unix.gettimeofday () -. start_time in
  Output.finished
    "dev [fast-compile + debuginfo]"
    1
    (Printf.sprintf "%.2fs" elapsed);
  0

let check input _search_paths =
  Output.checking input;
  let ic = open_in input in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let diags = Pipeline.check_source source in
  if Diagnostic.has_errors diags then (
    Pipeline.print_diagnostics diags source;
    Output.error (format_error_summary diags);
    1)
  else (
    Output.finished
      "dev [fast-compile + debuginfo]"
      1
      (format_success_summary diags);
    0)

let run _input _search_paths =
  Output.error "run command not yet implemented";
  Output.error "use: msc <input.ms> && musi <output.msc>";
  1

let help () =
  print_endline "Musi";
  print_endline "";
  print_endline "USAGE:";
  print_endline "    musi <COMMAND> [OPTIONS]";
  print_endline "";
  print_endline "COMMANDS:";
  print_endline "    compile <file>    Compile source to bytecode";
  print_endline "    check <file>      Type-check without code generation";
  print_endline "    run <file>        Compile and execute";
  print_endline "    disasm <file>     Disassemble bytecode file";
  print_endline "    help              Print this message";
  print_endline "    version           Print version information";
  print_endline "";
  print_endline "OPTIONS:";
  print_endline "    -o, --output <file>    Specify output file";
  print_endline "    -I <path>              Add module search path";
  print_endline "    -h, --help             Print help information";
  print_endline "    -V, --version          Print version information";
  0

let disasm input =
  let ic = open_in_bin input in
  let buf = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bytes_buf = Bytes.of_string buf in
  match Decoder.decode_program bytes_buf with
  | Error e ->
      Output.error (Printf.sprintf "failed to decode: %s" e);
      1
  | Ok decoded ->
      print_string (Disasm.format_disassembly decoded);
      0

let version () =
  print_endline "musi 0.1.0";
  0
