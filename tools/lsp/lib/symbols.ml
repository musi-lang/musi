open Lsp
open Ast.Nodes
open Basic

let collect_symbols source ast =
  let symbols = ref [] in

  let add_symbol name kind span =
    let range = Utils.range_of_span source span in
    let sym =
      Types.DocumentSymbol.create ~name ~kind ~range ~selectionRange:range ()
    in
    symbols := sym :: !symbols
  in

  let visit_expr (v : bool Ast.Visitor.visitor) _ (expr : expr) =
    match expr.kind with
    | ExprFn (_, _, sig_, _) ->
      Option.iter
        (fun name -> add_symbol name Types.SymbolKind.Function expr.span)
        sig_.fn_name;
      Ast.Visitor.traverse_expr v false expr
    | ExprRecord (_, _, Some name, _, _) ->
      add_symbol name Types.SymbolKind.Struct expr.span;
      Ast.Visitor.traverse_expr v false expr
    | ExprSum (_, _, Some name, _, _) ->
      add_symbol name Types.SymbolKind.Enum expr.span;
      Ast.Visitor.traverse_expr v false expr
    | ExprBind (_, _, name, _, _, _) ->
      add_symbol name Types.SymbolKind.Variable expr.span;
      Ast.Visitor.traverse_expr v false expr
    | _ -> Ast.Visitor.traverse_expr v false expr
  in

  let visitor = { Ast.Visitor.default_visitor with visit_expr } in
  List.iter (visitor.visit_stmt visitor false) ast;
  List.rev !symbols

let handle params =
  let uri = params.Types.DocumentSymbolParams.textDocument.uri in
  match State.get_text uri with
  | None -> `Null
  | Some text -> (
    let path = Utils.path_of_uri uri in
    let source = Source.create path text in
    let interner = Interner.create () in
    let lexer = Lex.Lexer.create ~interner:(Some interner) source 0 in
    match Lex.Lexer.token_stream_opt lexer with
    | None -> `Null
    | Some seq -> (
      match Parse.Parser.try_parse seq source 0 interner with
      | Ok ast ->
        let symbols = collect_symbols source ast in
        `List (List.map Types.DocumentSymbol.yojson_of_t symbols)
      | Error _ -> `Null))
