type file_id = int
type t = { file : file_id; start : int; end_ : int }

val make : file_id -> int -> int -> t
val dummy : t
val file : t -> file_id
val start : t -> int
val end_ : t -> int
val len : t -> int
val merge : t -> t -> t
