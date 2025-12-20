open Lsp
open Jsonrpc
open Ast.Nodes
open Basic

let log msg = Printf.eprintf "[Server] %s\n%!" msg
let documents = Hashtbl.create 16

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

module TokenBuilder = struct
  type token = {
      line : int
    ; start_char : int
    ; length : int
    ; token_type : int
    ; token_modifiers : int
  }

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

  let add_span span kind =
    let start_line, start_col = Source.line_col source span.Span.start in
    let line = start_line - 1 in

    let start_char = start_col - 1 in
    let length = span.Span.end_ - span.Span.start in
    if length > 0 then
      TokenBuilder.add_token builder ~line ~start_char ~length ~kind ()
  in

  let rec traverse_expr expr =
    match expr.kind with
    | ExprBind (_, _, _, _, _, _) -> add_span expr.span "variable"
    | ExprFn (_, _, sig_, body) ->
      Option.iter (fun _ -> add_span expr.span "function") sig_.fn_name;
      traverse_expr body
    | ExprSum (_, _, name, _, cases) ->
      Option.iter (fun _ -> add_span expr.span "enum") name;
      List.iter
        (fun _ ->
          (* case *)
          ())
        cases
    | ExprRecord (_, _, name, _, fields) ->
      Option.iter (fun _ -> add_span expr.span "struct") name;
      List.iter
        (fun _ ->
          (* field *)
          ())
        fields
    | ExprBlock (stmts, expr_opt) ->
      List.iter traverse_stmt stmts;
      Option.iter traverse_expr expr_opt
    | ExprCall (func, args) ->
      traverse_expr func;
      List.iter traverse_expr args
    | ExprIdent _ -> add_span expr.span "variable"
    | _ -> () (* catch all *)
  and traverse_stmt stmt =
    match stmt.kind with StmtExpr e -> traverse_expr e
  in

  List.iter traverse_stmt ast;

  TokenBuilder.build builder

let on_initialize _params =
  let caps =
    Types.ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
           (Types.TextDocumentSyncOptions.create
              ~openClose:true
              ~change:Full
              ()))
      ~semanticTokensProvider:
        (`SemanticTokensOptions
           (Types.SemanticTokensOptions.create
              ~legend
              ~range:false
              ~full:(`Bool true)
              ()))
      ()
  in
  Types.InitializeResult.create ~capabilities:caps ()

let on_did_open (params : Types.DidOpenTextDocumentParams.t) =
  let uri = params.textDocument.uri in
  let text = params.textDocument.text in
  Hashtbl.replace documents uri text;
  log ("Opened " ^ Uri.to_string uri)

let on_did_change (params : Types.DidChangeTextDocumentParams.t) =
  let uri = params.textDocument.uri in
  match params.contentChanges with
  | [ change ] ->
    Hashtbl.replace documents uri change.text;
    log ("Changed " ^ Uri.to_string uri)
  | _ -> log "Unexpected change event structure"

let on_semantic_tokens_full (params : Types.SemanticTokensParams.t) =
  let uri = params.textDocument.uri in
  match Hashtbl.find_opt documents uri with
  | None -> `Null
  | Some text -> (
    let path = "dummy.ms" in
    (* Dummy ID *)
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

let rec read_headers ic len =
  match In_channel.input_line ic with
  | None -> None
  | Some "" | Some "\r" -> Some len
  | Some line ->
    let line = String.trim line in
    if String.starts_with ~prefix:"Content-Length:" line then
      let len_str =
        String.trim (String.sub line 15 (String.length line - 15))
      in
      read_headers ic (int_of_string len_str)
    else read_headers ic len

let read_message ic =
  match read_headers ic 0 with
  | None -> None
  | Some len -> (
    let buf = Bytes.create len in
    match In_channel.really_input ic buf 0 len with
    | None -> None
    | Some () -> Some (Bytes.to_string buf))

let write_message oc json =
  let content = Yojson.Safe.to_string json in
  let len = String.length content in
  Printf.fprintf oc "Content-Length: %d\r\n\r\n%s%!" len content

let start () =
  log "Starting...";
  let ic = In_channel.stdin in
  let oc = Out_channel.stdout in
  set_binary_mode_out oc true;
  set_binary_mode_in ic true;

  let rec loop () =
    match read_message ic with
    | None -> log "EOF"
    | Some raw ->
      (try
         let json = Yojson.Safe.from_string raw in
         let packet = Packet.t_of_yojson json in
         match packet with
         | Request r ->
           let res_json =
             if r.method_ = "initialize" then
               let params =
                 Types.InitializeParams.t_of_yojson
                   (Option.get r.params :> Yojson.Safe.t)
               in
               Types.InitializeResult.yojson_of_t (on_initialize params)
             else if r.method_ = "textDocument/semanticTokens/full" then
               let params =
                 Types.SemanticTokensParams.t_of_yojson
                   (Option.get r.params :> Yojson.Safe.t)
               in
               on_semantic_tokens_full params
             else if r.method_ = "shutdown" then `Null
             else (
               log ("Unknown request: " ^ r.method_);
               `Null)
           in
           let response = Response.ok r.id res_json in
           write_message oc (Packet.yojson_of_t (Response response))
         | Notification n ->
           if n.method_ = "initialized" then log "Client initialized"
           else if n.method_ = "exit" then exit 0
           else if n.method_ = "textDocument/didOpen" then
             on_did_open
               (Types.DidOpenTextDocumentParams.t_of_yojson
                  (Option.get n.params :> Yojson.Safe.t))
           else if n.method_ = "textDocument/didChange" then
             on_did_change
               (Types.DidChangeTextDocumentParams.t_of_yojson
                  (Option.get n.params :> Yojson.Safe.t))
           else log ("Unknown notification: " ^ n.method_)
         | _ -> log "Unhandled packet type"
       with e ->
         log ("Error handling message: " ^ Printexc.to_string e);
         ());
      loop ()
  in
  loop ()
