open Lsp
open Basic

let range_of_span source span =
  let start_line, start_col = Source.line_col source span.Span.start in
  let end_line, end_col = Source.line_col source span.Span.end_ in
  Types.Range.create
    ~start:
      (Types.Position.create ~line:(start_line - 1) ~character:(start_col - 1))
    ~end_:(Types.Position.create ~line:(end_line - 1) ~character:(end_col - 1))

let path_of_uri uri = Uri.to_path uri
