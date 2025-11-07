(** Compilation pipeline orchestrating all compiler stages. *)

let setup_pipeline filename source =
  let interner = Interner.create () in
  let file_id = 0 in
  let lexer = Lexer.make file_id filename source interner in
  let tokens, lex_diags = Lexer.lex lexer in
  let parser = Parser.make tokens interner in
  let ast, parse_diags = Parser.parse parser in
  let resolver = Resolver.create interner in
  let resolve_diags = Resolver.resolve resolver ast in
  let checker = Checker.create interner (Resolver.symbols resolver) in
  let check_diags = Checker.check checker ast in
  (interner, ast, lex_diags, parse_diags, resolve_diags, check_diags)

let check_source filename source =
  let _, _, lex_diags, parse_diags, resolve_diags, check_diags =
    setup_pipeline filename source
  in
  Diagnostic.merge [ lex_diags; parse_diags; resolve_diags; check_diags ]

let check_file input_path =
  let ic = open_in input_path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  check_source input_path source

let compile_source filename source =
  let interner, ast, _, _, _, _ = setup_pipeline filename source in
  let proc_table = Emitter.collect_procs interner ast in
  let diags = ref Diagnostic.empty_bag in
  let procs, module_desc = Emitter.emit_program interner proc_table diags ast in
  let bytecode = Encoder.encode_program procs module_desc in
  bytecode

let compile_file input_path output_path =
  let ic = open_in input_path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bytecode = compile_source input_path source in
  let oc = open_out_bin output_path in
  output_bytes oc bytecode;
  close_out oc

let print_diagnostics diags filename source =
  let files = Source.empty in
  let _, files = Source.add_file files filename source in
  Diagnostic.emit_all Format.err_formatter diags files

let compile_module input_path output_path =
  let interner = Interner.create () in
  let linker = Linker.create interner in
  let modules, link_diags = Linker.build_import_graph linker input_path in
  if Diagnostic.has_errors link_diags then (
    let ic = open_in input_path in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    print_diagnostics link_diags input_path source;
    failwith "linking failed");
  let sorted_modules, sort_diags = Linker.topological_sort linker modules in
  if Diagnostic.has_errors sort_diags then (
    let ic = open_in input_path in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    print_diagnostics sort_diags input_path source;
    failwith "dependency cycle detected");
  let resolver = Resolver.create interner in
  List.iter
    (fun (m : Linker.module_info) ->
      let resolve_diags = Resolver.resolve resolver m.ast in
      if Diagnostic.has_errors resolve_diags then (
        let ic = open_in m.path in
        let source = really_input_string ic (in_channel_length ic) in
        close_in ic;
        print_diagnostics resolve_diags m.path source;
        failwith "resolution failed"))
    sorted_modules;
  let proc_table = Hashtbl.create 64 in
  List.iter
    (fun (m : Linker.module_info) ->
      let m_procs = Emitter.collect_procs interner m.ast in
      Hashtbl.iter (fun k v -> Hashtbl.replace proc_table k v) m_procs)
    sorted_modules;
  List.iter
    (fun (m : Linker.module_info) ->
      let diags = ref Diagnostic.empty_bag in
      let procs, module_desc =
        Emitter.emit_program interner proc_table diags m.ast
      in
      if Diagnostic.has_errors !diags then (
        let ic = open_in m.path in
        let source = really_input_string ic (in_channel_length ic) in
        close_in ic;
        print_diagnostics !diags m.path source;
        failwith "codegen failed");
      let bytecode = Encoder.encode_program procs module_desc in
      let out_path =
        if m.path = input_path then output_path
        else String.sub m.path 0 (String.length m.path - 3) ^ ".msc"
      in
      let oc = open_out_bin out_path in
      output_bytes oc bytecode;
      close_out oc)
    sorted_modules
