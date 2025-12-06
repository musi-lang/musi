type t = { file : int; start : int; end_ : int }

val make : int -> int -> int -> t
val dummy : t
val file : t -> int
val start : t -> int
val end_ : t -> int
val len : t -> int
val merge : t -> t -> t
