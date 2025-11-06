(** Compilation pipeline orchestrating all compiler stages. *)

let setup_pipeline source =
  let interner = Interner.create () in
  let file_id = 0 in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex lexer in
  let parser = Parser.make tokens interner in
  let ast, parse_diags = Parser.parse parser in
  let resolver = Resolver.create interner in
  let resolve_diags = Resolver.resolve resolver ast in
  let checker = Checker.create interner (Resolver.symbols resolver) in
  let check_diags = Checker.check checker ast in
  (interner, ast, lex_diags, parse_diags, resolve_diags, check_diags)

let check_source source =
  let _, _, lex_diags, parse_diags, resolve_diags, check_diags =
    setup_pipeline source
  in
  Diagnostic.merge [ lex_diags; parse_diags; resolve_diags; check_diags ]

let check_file input_path =
  let ic = open_in input_path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  check_source source

let compile_source source =
  let interner, ast, _, _, _, _ = setup_pipeline source in
  let proc_table = Emitter.collect_procs interner ast in
  let diags = ref Diagnostic.empty_bag in
  let procs, module_desc = Emitter.emit_program interner proc_table diags ast in
  let bytecode = Encoder.encode_program procs module_desc in
  bytecode

let compile_file input_path output_path =
  let ic = open_in input_path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bytecode = compile_source source in
  let oc = open_out_bin output_path in
  output_bytes oc bytecode;
  close_out oc

let print_diagnostics diags source =
  let files = Source.empty in
  let _, files = Source.add_file files "<input>" source in
  Diagnostic.emit_all Format.err_formatter diags files
