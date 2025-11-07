type t
type name = private int

val create : unit -> t
val intern : t -> string -> name
val lookup : t -> name -> string
val compare : name -> name -> int
val equal : name -> name -> bool
