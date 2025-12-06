module Source : sig
  type file = { path : string; source : string; lines : int array }
  type t = file array

  val compute_lines : string -> int array
  val empty : t
  val add_file : t -> string -> string -> int * t
  val get_file : t -> int -> file option
  val line_col : file -> int -> int * int
  val trim_NL : string -> string
  val trim_CR : string -> string
  val line_text : file -> int -> string option
  val path : file -> string
end
