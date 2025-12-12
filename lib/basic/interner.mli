type t
type ident = int

val create : unit -> t
val intern : t -> string -> ident
val lookup_opt : t -> ident -> string option
val clear : t -> unit
val size : t -> int
