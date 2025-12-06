module Interner : sig
  type name = int
  type file_id = int

  type t = {
      mutable strings : string array
    ; mutable count : int
    ; table : (string, name) Hashtbl.t
  }

  val create : unit -> t
  val intern : t -> string -> name
  val lookup : t -> name -> string
  val compare : name -> name -> int
  val equal : name -> name -> bool
  val empty_name : t -> name
end
