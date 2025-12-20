open Lsp

type document = {
    uri : Types.DocumentUri.t
  ; text : string
  ; source : Basic.Source.t
  ; file_id : int
}

let documents : (Types.DocumentUri.t, document) Hashtbl.t = Hashtbl.create 16
let get_document uri = Hashtbl.find_opt documents uri

let set_document uri text =
  let path = Lsp_utils.path_of_uri uri in
  let source = Basic.Source.create path text in
  let file_id = 0 in
  (* We only have one file at a time in this simple LSP for now *)
  let doc = { uri; text; source; file_id } in
  Hashtbl.replace documents uri doc;
  doc
