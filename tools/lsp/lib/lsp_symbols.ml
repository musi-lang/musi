open Lsp
open Basic
open Ast.Nodes

let collect_symbols source ast =
  let symbols = ref [] in

  let add_symbol name kind span =
    let range = Lsp_utils.range_of_span source span in
    let sym =
      Types.DocumentSymbol.create ~name ~kind ~range ~selectionRange:range ()
    in
    symbols := sym :: !symbols
  in

  let visit_expr (v : 'ctx Ast.Visitor.visitor) ctx (expr : expr) =
    match expr.kind with
    | ExprFn (_, _, sig_, _) ->
      Option.iter
        (fun name -> add_symbol name Types.SymbolKind.Function expr.span)
        sig_.fn_name;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprRecord (_, _, Some name, _, _) ->
      add_symbol name Types.SymbolKind.Struct expr.span;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprSum (_, _, name_opt, _, _) ->
      Option.iter
        (fun name -> add_symbol name Types.SymbolKind.Enum expr.span)
        name_opt;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprBind (_, _, pat, _, init, _) ->
      let kind =
        match init.kind with
        | ExprFn _ -> Types.SymbolKind.Function
        | _ -> Types.SymbolKind.Variable
      in
      (match pat.kind with
      | PatIdent name -> add_symbol name kind expr.span
      | _ -> ());
      v.visit_expr v ctx init
    | _ -> Ast.Visitor.traverse_expr v ctx expr
  in

  let visitor = { Ast.Visitor.default_visitor with visit_expr } in
  List.iter (visitor.visit_stmt visitor false) ast;
  List.rev !symbols

let handle uri =
  match Lsp_store.get_document uri with
  | None -> `Null
  | Some doc -> (
    let interner = Interner.create () in
    let lexer =
      Lex.Lexer.create ~interner:(Some interner) doc.source doc.file_id
    in
    match Lex.Lexer.try_tokenize lexer with
    | Error _ -> `Null
    | Ok tokens -> (
      match
        Parse.Parser.try_parse
          (List.to_seq tokens)
          doc.source
          doc.file_id
          interner
      with
      | Ok ast ->
        let symbols = collect_symbols doc.source ast in
        `List (List.map Types.DocumentSymbol.yojson_of_t symbols)
      | Error _ -> `Null))
