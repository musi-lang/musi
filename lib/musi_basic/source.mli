type file
type t

val empty : t
val add_file : t -> string -> string -> Span.file_id * t
val get_file : t -> Span.file_id -> file option
val line_col : file -> int -> int * int
val line_text : file -> int -> string option
val path : file -> string
