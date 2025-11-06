(** Provides utilities for managing source files and resolving line and column information. *)

(** Represents loaded source file with cached line offsets. *)
type file

(** Stores all loaded source files for lookup by identifier. *)
type t

(** Creates empty source file collection. *)
val empty : t

(** Registers file, returning identifier alongside updated collection. *)
val add_file : t -> string -> string -> Span.file_id * t

(** Retrieves loaded file by identifier. *)
val get_file : t -> Span.file_id -> file option

(** Translates byte offset within file into 1-based line and column values. *)
val line_col : file -> int -> int * int

(** Fetches text for specified 1-based line when present. *)
val line_text : file -> int -> string option

(** Returns filesystem path recorded for file. *)
val path : file -> string
