type t = string * string array

let create filename content =
  let lines = String.split_on_char '\n' content in
  (filename, Array.of_list lines)

let path (filename, _) = filename

let line_col (_, lines) pos =
  let rec find_line line_idx start_pos =
    if line_idx >= Array.length lines then (line_idx, pos - start_pos + 1)
    else
      let line_len = String.length (Array.get lines line_idx) + 1 in
      if start_pos + line_len > pos then (line_idx + 1, pos - start_pos + 1)
      else find_line (line_idx + 1) (start_pos + line_len)
  in
  find_line 0 0

let line_text_opt (_, lines) line_idx =
  if line_idx > 0 && line_idx <= Array.length lines then
    Some (Array.get lines (line_idx - 1))
  else None

let get_file_opt files file_id =
  try Some (List.assoc file_id files) with Not_found -> None

let text (_, lines) = String.concat "\n" (Array.to_list lines)
