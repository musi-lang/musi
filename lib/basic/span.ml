module type S = sig
  type file_id = int
  type t = { file : file_id; start : int; end_ : int }

  val make : file_id -> int -> int -> t
  val dummy : t
  val file : t -> file_id
  val start : t -> int
  val end_ : t -> int
  val len : t -> int
  val merge : t -> t -> t
end

module Make () : S = struct
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
end

include Make ()
