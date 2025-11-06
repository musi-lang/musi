type name = int

type t = {
    mutable strings : string array
  ; mutable count : int
  ; table : (string, name) Hashtbl.t
}

let create () =
  { strings = Array.make 16 ""; count = 0; table = Hashtbl.create 256 }

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
