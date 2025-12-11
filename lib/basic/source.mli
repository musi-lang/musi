type t

val path : t -> string
val line_col : t -> int -> int * int
val line_text_opt : t -> int -> string option
val get_file_opt : (int * t) list -> int -> t option
