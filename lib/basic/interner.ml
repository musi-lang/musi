module type INTERNER = sig
  type name = int
  type t

  val create : unit -> t
  val intern : t -> string -> name
  val lookup : t -> name -> string
  val compare : name -> name -> int
  val equal : name -> name -> bool
  val empty_name : t -> name
end

module Interner : INTERNER = struct
  type name = int

  type t = {
      mutable strings : string array
    ; mutable count : int
    ; table : (string, name) Hashtbl.t
  }

  let base_alloc_size = 256

  let create () =
    {
      strings = Array.make base_alloc_size ""
    ; count = 0
    ; table = Hashtbl.create base_alloc_size
    }

  let intern t str =
    match Hashtbl.find_opt t.table str with
    | Some n -> n
    | None ->
      let n = t.count in
      if n >= Array.length t.strings then (
        let new_cap = Array.length t.strings * 2 in
        let new_arr = Array.make new_cap "" in
        Array.blit t.strings 0 new_arr 0 (Array.length t.strings);
        t.strings <- new_arr);
      t.strings.(n) <- str;
      t.count <- t.count + 1;
      Hashtbl.add t.table str n;
      n

  let lookup t n = t.strings.(n)
  let compare (a : name) (b : name) = Int.compare a b
  let equal (a : name) (b : name) = a = b
  let empty_name t = intern t ""
end
