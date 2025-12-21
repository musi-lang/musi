open Lsp
open Basic
open Ast.Nodes

module TokenBuilder = struct
  type token = {
      line : int
    ; start_char : int
    ; length : int
    ; token_type : int
    ; token_modifiers : int
  }

  let token_types =
    [
      "namespace"
    ; "type"
    ; "class"
    ; "enum"
    ; "interface"
    ; "struct"
    ; "typeParameter"
    ; "parameter"
    ; "variable"
    ; "property"
    ; "enumMember"
    ; "event"
    ; "function"
    ; "method"
    ; "macro"
    ; "keyword"
    ; "modifier"
    ; "comment"
    ; "string"
    ; "number"
    ; "regexp"
    ; "operator"
    ]

  let token_modifiers =
    [
      "declaration"
    ; "definition"
    ; "readonly"
    ; "static"
    ; "deprecated"
    ; "abstract"
    ; "async"
    ; "modification"
    ; "documentation"
    ; "defaultLibrary"
    ]

  let legend =
    Types.SemanticTokensLegend.create
      ~tokenTypes:token_types
      ~tokenModifiers:token_modifiers

  let token_type_to_int t =
    let rec find idx = function
      | [] -> 0
      | x :: _ when x = t -> idx
      | _ :: xs -> find (idx + 1) xs
    in
    find 0 token_types

  let create () = ref []

  let add_token builder ~line ~start_char ~length ~kind ?(modifiers = 0) () =
    let token_type = token_type_to_int kind in
    builder :=
      { line; start_char; length; token_type; token_modifiers = modifiers }
      :: !builder

  let build builder =
    let tokens =
      List.sort
        (fun a b ->
          if a.line = b.line then compare a.start_char b.start_char
          else compare a.line b.line)
        !builder
    in
    let rec delta last_line last_char acc = function
      | [] -> List.rev acc
      | t :: rest ->
        let delta_line = t.line - last_line in
        let delta_start =
          if delta_line = 0 then t.start_char - last_char else t.start_char
        in
        let data =
          [ delta_line; delta_start; t.length; t.token_type; t.token_modifiers ]
        in
        delta t.line t.start_char (List.rev_append data acc) rest
    in
    delta 0 0 [] tokens
end

let collect_tokens source ast interner =
  let builder = TokenBuilder.create () in
  let resolve id =
    match Interner.lookup_opt interner id with Some s -> s | None -> "?"
  in

  let add_token_at_span span kind ?(modifiers = 0) () =
    let start_line, start_col = Source.line_col source span.Span.start in
    let line = start_line - 1 in
    let start_char = start_col - 1 in
    let length = span.Span.end_ - span.Span.start in
    if length > 0 then
      TokenBuilder.add_token
        builder
        ~line
        ~start_char
        ~length
        ~kind
        ~modifiers
        ()
  in

  let visit_expr (v : 'ctx Ast.Visitor.visitor) is_callee (expr : expr) =
    match expr.kind with
    | ExprBind { pat; ty_annot; kind_kw; init; _ } ->
      add_token_at_span kind_kw.span "keyword" ();
      let kind =
        match init.value.kind with ExprFn _ -> "function" | _ -> "variable"
      in
      let modifiers =
        match ty_annot with
        | Some _ -> TokenBuilder.token_type_to_int "declaration"
        | None -> 0
      in
      add_token_at_span pat.span kind ~modifiers ();
      v.visit_expr v false init.value
    | ExprCall { callee; args } ->
      v.visit_expr v true callee;
      List.iter (fun e -> v.visit_expr v false e) args.value.elems
    | ExprIf { if_kw; _ } ->
      add_token_at_span if_kw.span "keyword" ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprWhile { while_kw; _ } ->
      add_token_at_span while_kw.span "keyword" ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprFor { for_kw; in_kw; _ } ->
      add_token_at_span for_kw.span "keyword" ();
      add_token_at_span in_kw.span "keyword" ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprMatch { match_kw; _ } ->
      add_token_at_span match_kw.span "keyword" ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprBinary { op; _ } ->
      add_token_at_span op.span "operator" ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprLit lit ->
      let kind =
        match lit.kind with
        | LitInt _ | LitFloat _ -> "number"
        | LitString _ | LitRune _ -> "string"
        | LitBool _ -> "keyword"
      in
      add_token_at_span lit.span kind ();
      Ast.Visitor.traverse_expr v is_callee expr
    | ExprIdent name ->
      let name_str = resolve name.kind in
      let kind =
        if
          String.length name_str > 0
          && Char.uppercase_ascii name_str.[0] = name_str.[0]
        then "class"
        else if is_callee then "function"
        else "variable"
      in
      add_token_at_span expr.span kind ()
    | _ -> Ast.Visitor.traverse_expr v is_callee expr
  in

  let visitor = { Ast.Visitor.default_visitor with visit_expr } in
  List.iter (visitor.visit_stmt visitor false) ast;
  TokenBuilder.build builder

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
        let data = collect_tokens doc.source ast interner in
        let tokens =
          Types.SemanticTokens.create ~data:(Array.of_list data) ()
        in
        Types.SemanticTokens.yojson_of_t tokens
      | Error _ -> `Null))
