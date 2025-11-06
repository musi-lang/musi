type file = {
    id : Span.file_id
  ; path : string
  ; source : string
  ; lines : int array
}

type t = file list

let compute_lines source =
  let rec loop i acc =
    if i >= String.length source then Array.of_list (List.rev acc)
    else if source.[i] = '\n' then loop (i + 1) ((i + 1) :: acc)
    else if
      source.[i] = '\r' && i + 1 < String.length source && source.[i + 1] = '\n'
    then loop (i + 2) ((i + 2) :: acc)
    else loop (i + 1) acc
  in
  loop 0 [ 0 ]

let empty = []

let add_file files path source =
  let id = List.length files in
  let file = { id; path; source; lines = compute_lines source } in
  (id, file :: files)

let get_file files id = List.find_opt (fun f -> f.id = id) files

let line_col file offset =
  let rec search lo hi =
    if lo > hi then lo - 1
    else
      let mid = (lo + hi) / 2 in
      if file.lines.(mid) <= offset then search (mid + 1) hi
      else search lo (mid - 1)
  in
  let line = max 0 (search 0 (Array.length file.lines - 1)) in
  let col = offset - file.lines.(line) in
  (line + 1, col + 1)

let line_text file line =
  if line < 1 || line > Array.length file.lines then None
  else
    let start = file.lines.(line - 1) in
    let end_ =
      if line < Array.length file.lines then file.lines.(line)
      else String.length file.source
    in
    if start < 0 || start > String.length file.source then None
    else if end_ < start || end_ > String.length file.source then None
    else
      let text = String.sub file.source start (end_ - start) in
      let trimmed =
        let len = String.length text in
        if len > 0 && text.[len - 1] = '\n' then String.sub text 0 (len - 1)
        else text
      in
      let trimmed =
        let len = String.length trimmed in
        if len > 0 && trimmed.[len - 1] = '\r' then
          String.sub trimmed 0 (len - 1)
        else trimmed
      in
      Some trimmed

let path file = file.path
