open Lsp
open Basic

let diagnostic_severity = function
  | Reporter.Error -> Types.DiagnosticSeverity.Error
  | Reporter.Warning -> Types.DiagnosticSeverity.Warning
  | Reporter.Note -> Types.DiagnosticSeverity.Information

let publish uri source bag =
  let diagnostics =
    List.map
      (fun (d : Reporter.t) ->
        Types.Diagnostic.create
          ~range:(Utils.range_of_span source d.span)
          ~severity:(diagnostic_severity d.level)
          ~message:(`String (Printf.sprintf "Syntax Error: %s" d.message))
          ~source:"musi"
          ())
      (Reporter.to_list bag)
  in
  let params = Types.PublishDiagnosticsParams.create ~uri ~diagnostics () in
  let method_ = "textDocument/publishDiagnostics" in
  let json = Types.PublishDiagnosticsParams.yojson_of_t params in
  let params_structured =
    match json with (`Assoc _ | `List _) as j -> j | _ -> `Assoc []
  in
  Jsonrpc.Packet.Notification { method_; params = Some params_structured }
