open Musi_basic
open Musi_lex
open Musi_parse
open Musi_codegen

let compile ~input_path ~output_path =
  let content = In_channel.with_open_bin input_path In_channel.input_all in
  let interner = Interner.create () in
  let source = Source.empty in
  let file_id, _source = Source.add_file source input_path content in

  let lexer = Lexer.make file_id content interner in
  let tokens, lex_diags = Lexer.lex_all lexer in

  let parser = Parser.make tokens interner in
  let ast, parse_diags = Parser.parse parser in

  let diag_bag = ref (Diagnostic.merge [ lex_diags; parse_diags ]) in
  let _symbols = Musi_sema.Resolver.resolve ast interner diag_bag in

  let emitter = Emitter.make () in
  let instrs = Emitter.emit emitter ast in

  let encoder = Encoder.make interner in
  let bytecode = Encoder.encode encoder (Emitter.procs emitter) instrs in

  let oc = open_out_bin output_path in
  output_bytes oc bytecode;
  close_out oc
