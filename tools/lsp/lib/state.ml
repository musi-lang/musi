open Lsp

let documents : (Types.DocumentUri.t, string) Hashtbl.t = Hashtbl.create 16
let get_text uri = Hashtbl.find_opt documents uri
let set_text uri text = Hashtbl.replace documents uri text
