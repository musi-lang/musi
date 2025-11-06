(** Provides helpers for tracking source locations used by diagnostics. *)

(** Represents numeric identifier assigned to source file. *)
type file_id = int

(** Describes byte range within source file. *)
type t = { file : file_id; start : int; end_ : int }

(** Constructs span from file identifier and byte offsets. *)
val make : file_id -> int -> int -> t

(** Provides sentinel span value for synthesised nodes. *)
val dummy : t

(** Retrieves file identifier associated with span. *)
val file : t -> file_id

(** Retrieves starting byte offset of span. *)
val start : t -> int

(** Retrieves ending byte offset of span. *)
val end_ : t -> int

(** Calculates number of bytes covered by span. *)
val len : t -> int

(** Combines two spans into single span covering both ranges. *)
val merge : t -> t -> t
