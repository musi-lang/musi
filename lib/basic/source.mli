type file = { path : string; source : string; lines : int array }
type t = file array

val compute_lines : string -> int array
val empty : 'a array
val add_file : file array -> string -> string -> int * file array
val get_file : 'a array -> int -> 'a option
val line_col : file -> int -> int * int
val trim_NL : string -> string
val trim_CR : string -> string
val line_text : file -> int -> string option
val path : file -> string
