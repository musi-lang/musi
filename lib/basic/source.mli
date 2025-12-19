(** Source file content *)
type t

(** Create source from path and content *)
val create : string -> string -> t

(** Get file path *)
val path : t -> string

(** Convert offset to line and column *)
val line_col : t -> int -> int * int

(** Get line text by line number *)
val line_text_opt : t -> int -> string option

(** Find source file by ID in list *)
val get_file_opt : (int * t) list -> int -> t option

(** Get full source text *)
val text : t -> string
