(** Source code range *)
type t = { file : int; start : int; end_ : int }

(** Create span from file ID and range *)
val make : int -> int -> int -> t

(** Dummy span for internal use *)
val dummy : t

(** Get file ID *)
val file : t -> int

(** Get start offset *)
val start : t -> int

(** Get end offset *)
val end_ : t -> int

(** Get span length *)
val len : t -> int

(** Merge two spans covering both ranges *)
val merge : t -> t -> t
