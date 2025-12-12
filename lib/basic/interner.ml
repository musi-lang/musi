type t = {
    table : (string, int) Hashtbl.t
  ; mutable strings : string array
  ; next_id : int ref
}

type ident = int

let base_buf_size = 1024

let create () =
  { table = Hashtbl.create base_buf_size; strings = [||]; next_id = ref 0 }

let intern t s =
  try Hashtbl.find t.table s
  with Not_found ->
    let id = !(t.next_id) in
    Hashtbl.add t.table s id;
    t.strings <- Array.append t.strings [| s |];
    incr t.next_id;
    id

let lookup_opt t id =
  if id >= 0 && id < !(t.next_id) then Some t.strings.(id) else None

let clear t =
  Hashtbl.clear t.table;
  t.strings <- [||];
  t.next_id := 0

let size t = !(t.next_id)
