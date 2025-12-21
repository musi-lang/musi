open Lsp
open Basic
open Ast.Nodes

let collect_symbols source ast interner =
  let symbols = ref [] in
  let resolve id =
    match Interner.lookup_opt interner id with Some s -> s | None -> "?"
  in

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

  let rec add_symbols_from_pat ?(detail = None) (pat : pat) kind =
    match pat.kind with
    | PatIdent name -> add_symbol ~detail (resolve name.kind) kind pat.span
    | PatLitTuple pats | PatLitArray pats ->
      List.iter (fun p -> add_symbols_from_pat ~detail p kind) pats.value.elems
    | PatLitRecord { fields; _ } ->
      List.iter
        (fun (f : pat_field) ->
          add_symbol ~detail (resolve f.name.kind) kind f.name.span)
        fields.value.elems
    | PatVariant { args; _ } ->
      Option.iter
        (fun (d : _ delimited) ->
          List.iter (fun p -> add_symbols_from_pat ~detail p kind) d.value.elems)
        args
    | PatCons (l, _, r) ->
      add_symbols_from_pat ~detail l kind;
      add_symbols_from_pat ~detail r kind
    | PatOr { kind = { elems; _ }; _ } ->
      List.iter (fun p -> add_symbols_from_pat ~detail p kind) elems
    | PatWild _ | PatLit _ | PatError -> ()
  in

  let is_constant_literal (expr : expr) =
    match expr.kind with ExprLit _ -> true | _ -> false
  in

  let visit_expr (v : 'ctx Ast.Visitor.visitor) ctx (expr : expr) =
    match expr.kind with
    | ExprFn { sig_; _ } ->
      Option.iter
        (fun name ->
          let name_str = resolve name.kind in
          let detail =
            Some
              (Printf.sprintf
                 "fn %s"
                 (if sig_.params.value.elems = [] then "()"
                  else
                    sig_.params.value.elems
                    |> List.map (fun (p : param) -> resolve p.name.kind)
                    |> String.concat ", " |> Printf.sprintf "(%s)"))
          in
          add_symbol ~detail name_str Types.SymbolKind.Function expr.span)
        sig_.name;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprRecord { name; fields; _ } ->
      Option.iter
        (fun name ->
          add_symbol (resolve name.kind) Types.SymbolKind.Struct expr.span)
        name;
      List.iter
        (fun (field : record_field_def) ->
          add_symbol
            ~detail:(Some "field")
            (resolve field.name.kind)
            Types.SymbolKind.Field
            field.name.span)
        fields.value.elems;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprSum { name; cases; _ } ->
      Option.iter
        (fun name ->
          add_symbol (resolve name.kind) Types.SymbolKind.Enum expr.span)
        name;
      List.iter
        (fun (sum_case : sum_case) ->
          add_symbol
            ~detail:(Some "case")
            (resolve sum_case.name.kind)
            Types.SymbolKind.EnumMember
            sum_case.name.span)
        cases.value.elems;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprAlias { name; ty_params; _ } ->
      let detail =
        match ty_params with
        | None -> Some "alias"
        | Some tp ->
          let params =
            String.concat
              ", "
              (List.map (fun i -> resolve i.kind) tp.value.elems)
          in
          Some (Printf.sprintf "alias<%s>" params)
      in
      add_symbol ~detail (resolve name.kind) Types.SymbolKind.Class expr.span;
      Ast.Visitor.traverse_expr v ctx expr
    | ExprBind { kind_kw; pat; ty_annot; init; _ } ->
      let is_var = Lex.Token.(kind_kw.kind = KwVar) in
      let kind =
        match init.value.kind with
        | ExprFn _ -> Types.SymbolKind.Function
        | ExprRecord _ -> Types.SymbolKind.Struct
        | ExprSum _ -> Types.SymbolKind.Enum
        | _ ->
          if (not is_var) && is_constant_literal init.value then
            Types.SymbolKind.Constant
          else if is_var then Types.SymbolKind.Variable
          else Types.SymbolKind.Constant
      in
      let detail =
        match ty_annot with Some _ -> Some "annotated" | None -> None
      in
      add_symbols_from_pat ~detail pat kind;
      v.visit_expr v ctx init.value
    | ExprImport (_, ident) ->
      add_symbol (resolve ident.kind) Types.SymbolKind.Module expr.span
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
        let symbols = collect_symbols doc.source ast interner in
        `List (List.map Types.DocumentSymbol.yojson_of_t symbols)
      | Error _ -> `Null))
