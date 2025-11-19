type file = { path : string; source : string; lines : int array }
type t = file array

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

let empty = [||]

let add_file files path source =
  let id = Array.length files in
  let file = { path; source; lines = compute_lines source } in
  (id, Array.append files [| file |])

let get_file files id =
  if id >= 0 && id < Array.length files then Some files.(id) else None

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

let trim_NL s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then String.sub s 0 (len - 1) else s

let trim_CR s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

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
      Some (trim_CR (trim_NL text))

let path file = file.path
