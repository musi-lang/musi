(** String interner state *)
type t

(** Interned string identifier *)
type ident = int

(** Create new interner *)
val create : unit -> t

(** Intern string returning identifier *)
val intern : t -> string -> ident

(** Lookup string by identifier *)
val lookup_opt : t -> ident -> string option

(** Clear all interned strings *)
val clear : t -> unit

(** Get number of interned strings *)
val size : t -> int
