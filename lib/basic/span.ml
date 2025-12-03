type file_id = int
type t = { file : file_id; start : int; end_ : int }

let make file start end_ = { file; start; end_ }
let dummy = { file = 0; start = 0; end_ = 0 }
let file t = t.file
let start t = t.start
let end_ t = t.end_
let len t = t.end_ - t.start

let merge a b =
  { file = a.file; start = min a.start b.start; end_ = max a.end_ b.end_ }
