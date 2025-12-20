open Lsp
open Jsonrpc
open Basic

let log msg = Printf.eprintf "[Server] %s\n%!" msg

let parse_and_diagnose uri text =
  let path = Utils.path_of_uri uri in
  let source = Source.create path text in
  let interner = Interner.create () in
  let lexer = Lex.Lexer.create ~interner:(Some interner) source 0 in
  match Lex.Lexer.token_stream_opt lexer with
  | None -> None
  | Some seq ->
    let bag =
      match Parse.Parser.try_parse seq source 0 interner with
      | Ok _ -> Reporter.empty_bag
      | Error diag -> diag
    in
    Some (Diagnostics.publish uri source bag)

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
              ~legend:Tokens.TokenBuilder.legend
              ~range:false
              ~full:(`Bool true)
              ()))
      ~documentSymbolProvider:(`Bool true)
      ()
  in
  Types.InitializeResult.create ~capabilities:caps ()

let on_did_open (params : Types.DidOpenTextDocumentParams.t) =
  let uri = params.textDocument.uri in
  let text = params.textDocument.text in
  State.set_text uri text;
  log ("Opened " ^ Uri.to_string uri);
  parse_and_diagnose uri text

let on_did_change (params : Types.DidChangeTextDocumentParams.t) =
  let uri = params.textDocument.uri in
  match params.contentChanges with
  | [ change ] ->
    State.set_text uri change.text;
    log ("Changed " ^ Uri.to_string uri);
    parse_and_diagnose uri change.text
  | _ ->
    log "Unexpected change event structure";
    None

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
               Tokens.handle params
             else if r.method_ = "textDocument/documentSymbol" then
               let params =
                 Types.DocumentSymbolParams.t_of_yojson
                   (Option.get r.params :> Yojson.Safe.t)
               in
               Symbols.handle params
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
             let params =
               Types.DidOpenTextDocumentParams.t_of_yojson
                 (Option.get n.params :> Yojson.Safe.t)
             in
             match on_did_open params with
             | Some p -> write_message oc (Packet.yojson_of_t p)
             | None -> ()
           else if n.method_ = "textDocument/didChange" then
             let params =
               Types.DidChangeTextDocumentParams.t_of_yojson
                 (Option.get n.params :> Yojson.Safe.t)
             in
             match on_did_change params with
             | Some p -> write_message oc (Packet.yojson_of_t p)
             | None -> ()
           else log ("Unknown notification: " ^ n.method_)
         | _ -> log "Unhandled packet type"
       with e ->
         log ("Error handling message: " ^ Printexc.to_string e);
         ());
      loop ()
  in
  loop ()
