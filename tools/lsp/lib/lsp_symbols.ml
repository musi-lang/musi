open Lsp
open Basic
open Ast.Nodes

let collect_symbols source ast =
  let symbols = ref [] in

  let add_symbol ?(detail = None) name kind span =
    let range = Lsp_utils.range_of_span source span in
    let sym =
      Types.DocumentSymbol.create
        ~name
        ~kind
        ~range
        ~selectionRange:range
        ?detail
        ()
    in
    symbols := sym :: !symbols
  in

  let is_constant_literal (expr : expr) =
    match expr.kind with ExprLit _ -> true | _ -> false
  in

  let visit_expr (v : 'ctx Ast.Visitor.visitor) ctx (expr : expr) =
    match expr.kind with
    | ExprFn (_, _, sig_, _) ->
      Option.iter
        (fun name ->
          let detail =
            Some
              (Printf.sprintf
                 "fn %s%s"
                 (match sig_.fn_name with Some _ -> name | None -> "")
                 (if sig_.fn_params = [] then "()"
                  else
                    sig_.fn_params
                    |> List.map (fun p -> p.param_name)
                    |> String.concat ", " |> Printf.sprintf "(%s)"))
          in
          add_symbol ~detail name Types.SymbolKind.Function expr.span)
        sig_.fn_name;

      List.iter
        (fun param ->
          let detail = Some "param" in
          add_symbol
            ~detail
            param.param_name
            Types.SymbolKind.Variable
            expr.span)
        sig_.fn_params;

      Ast.Visitor.traverse_expr v ctx expr
    | ExprRecord (_, _, name_opt, _, fields) ->
      Option.iter
        (fun name -> add_symbol name Types.SymbolKind.Struct expr.span)
        name_opt;

      List.iter
        (fun field ->
          let detail = Some "field" in
          add_symbol ~detail field.field_name Types.SymbolKind.Field expr.span)
        fields;

      Ast.Visitor.traverse_expr v ctx expr
    | ExprSum (_, _, name_opt, _, cases) ->
      Option.iter
        (fun name -> add_symbol name Types.SymbolKind.Enum expr.span)
        name_opt;

      List.iter
        (fun sum_case ->
          let detail = Some "case" in
          add_symbol
            ~detail
            sum_case.case_name
            Types.SymbolKind.EnumMember
            expr.span)
        cases;

      Ast.Visitor.traverse_expr v ctx expr
    | ExprAlias (_, _, name, ty_params, _) ->
      let detail =
        if ty_params = [] then Some "alias"
        else
          let params = String.concat ", " ty_params in
          Some (Printf.sprintf "alias<%s>" params)
      in
      add_symbol ~detail name Types.SymbolKind.Class expr.span
    | ExprBind (_, is_mutable, pat, ty_annot, init, _) ->
      let kind, detail_prefix =
        match init.kind with
        | ExprFn _ -> (Types.SymbolKind.Function, "fn")
        | ExprRecord _ -> (Types.SymbolKind.Struct, "record")
        | ExprSum _ -> (Types.SymbolKind.Enum, "sum")
        | _ ->
          if is_constant_literal init then (Types.SymbolKind.Constant, "const")
          else if is_mutable then (Types.SymbolKind.Variable, "var")
          else (Types.SymbolKind.Variable, "val")
      in

      let detail =
        match ty_annot with
        | Some _ -> Some detail_prefix
        | None -> Some detail_prefix
      in

      (match pat.kind with
      | PatIdent name -> add_symbol ~detail name kind expr.span
      | _ -> ());
      v.visit_expr v ctx init
    | ExprImport path -> add_symbol path Types.SymbolKind.Module expr.span
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
