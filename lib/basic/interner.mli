type name = int
type file_id = int

type t = {
    mutable strings : string array
  ; mutable count : int
  ; table : (string, name) Hashtbl.t
}

val create : unit -> t
val intern : t -> string -> file_id
val lookup : t -> file_id -> string
val compare : file_id -> file_id -> file_id
val equal : file_id -> file_id -> bool
val empty_name : t -> file_id
