open Lsp
open Ast.Nodes
open Basic

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

let collect_tokens source ast =
  let builder = TokenBuilder.create () in

  let text_of_span span =
    let full_text = Source.text source in
    let start_pos = span.Span.start in
    let end_pos = span.Span.end_ in
    if end_pos <= String.length full_text && start_pos >= 0 then
      String.sub full_text start_pos (end_pos - start_pos)
    else ""
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

  let find_word_in_span span word =
    let text = text_of_span span in
    try
      let raw_index = Str.search_forward (Str.regexp_string word) text 0 in
      let start_pos = span.Span.start + raw_index in
      let end_pos = start_pos + String.length word in
      Some (Span.make span.file start_pos end_pos)
    with Not_found -> None
  in

  let visit_ident _ _ _ = () in

  let visit_expr (v : bool Ast.Visitor.visitor) is_callee (expr : expr) =
    match expr.kind with
    | ExprBind (_, _, name, _, init, _) ->
      let kind =
        match init.kind with ExprFn _ -> "function" | _ -> "variable"
      in
      (match find_word_in_span expr.span name with
      | Some span -> add_token_at_span span kind ()
      | None -> ());
      v.visit_expr v false init
    | ExprCall (callee, args) ->
      v.visit_expr v true callee;
      List.iter (v.visit_expr v false) args
    | ExprIdent name ->
      let kind =
        if
          String.get name 0 |> Char.uppercase_ascii
          |> Char.equal (String.get name 0)
        then "enumMember"
        else if is_callee then "function"
        else "variable"
      in
      add_token_at_span expr.span kind ()
    | ExprSum (_, _, Some name, _, cases) ->
      (match find_word_in_span expr.span name with
      | Some span -> add_token_at_span span "enum" ()
      | None -> ());
      List.iter
        (fun _ ->
          (*ignore enumMember highlighting until AST gets fixed *)
          ())
        cases
    | ExprRecord (_, _, Some name, _, _) ->
      (match find_word_in_span expr.span name with
      | Some span -> add_token_at_span span "struct" ()
      | None -> ());
      Ast.Visitor.traverse_expr v false expr
    | _ -> Ast.Visitor.traverse_expr v false expr
  in

  let visit_pat (v : bool Ast.Visitor.visitor) _ (pat : pat) =
    match pat.kind with
    | PatVariant (name, _, _) ->
      let span =
        Span.make
          pat.span.file
          pat.span.start
          (pat.span.start + String.length name)
      in
      add_token_at_span span "enumMember" ();
      Ast.Visitor.traverse_pat v false pat
    | _ -> Ast.Visitor.traverse_pat v false pat
  in

  let visitor =
    { Ast.Visitor.default_visitor with visit_expr; visit_pat; visit_ident }
  in

  List.iter (visitor.visit_stmt visitor false) ast;
  TokenBuilder.build builder

let handle params =
  let uri = params.Types.SemanticTokensParams.textDocument.uri in
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
        let data = collect_tokens source ast in
        let tokens =
          Types.SemanticTokens.create ~data:(Array.of_list data) ()
        in
        Types.SemanticTokens.yojson_of_t tokens
      | Error _ -> `Null))
