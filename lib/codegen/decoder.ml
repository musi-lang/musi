type decoded_program = {
    header : Metadata.header
  ; bytecode : bytes
  ; exports : (string * int) list
  ; links : (int * string) list
}

let decode_header buf =
  if Bytes.length buf < 32 then Error "file too small for header"
  else
    let magic = Bytes.sub_string buf 0 4 in
    if magic <> "\x4D\x53\x43\x00" then Error "invalid magic number"
    else
      let version = Binary.read_u32_le buf 4 in
      let bc_offset = Binary.read_u32_le buf 8 in
      let bc_size = Binary.read_u32_le buf 12 in
      let export_offset = Binary.read_u32_le buf 16 in
      let export_count = Binary.read_u32_le buf 20 in
      let link_offset = Binary.read_u32_le buf 24 in
      let link_count = Binary.read_u32_le buf 28 in
      Ok
        {
          Metadata.magic
        ; version
        ; bc_offset
        ; bc_size
        ; export_offset
        ; export_count
        ; link_offset
        ; link_count
        }

let decode_string buf offset =
  let len = Binary.read_u16_le buf offset in
  let str = Bytes.sub_string buf (offset + 2) len in
  (str, offset + 2 + len)

let decode_exports buf offset count =
  let rec loop acc pos remaining =
    if remaining = 0 then Ok (List.rev acc)
    else
      let name, next_pos = decode_string buf pos in
      let proc_id = Binary.read_u16_le buf next_pos in
      loop ((name, proc_id) :: acc) (next_pos + 2) (remaining - 1)
  in
  loop [] (Int32.to_int offset) (Int32.to_int count)

let decode_links buf offset count =
  let rec loop acc pos remaining =
    if remaining = 0 then Ok (List.rev acc)
    else
      let proc_id = Binary.read_u16_le buf pos in
      let name, next_pos = decode_string buf (pos + 2) in
      loop ((proc_id, name) :: acc) next_pos (remaining - 1)
  in
  loop [] (Int32.to_int offset) (Int32.to_int count)

let decode_program buf =
  match decode_header buf with
  | Error e -> Error e
  | Ok header -> (
    let bc_start = Int32.to_int header.bc_offset in
    let bc_len = Int32.to_int header.bc_size in
    if Bytes.length buf < bc_start + bc_len then
      Error "bytecode section out of bounds"
    else
      let bytecode = Bytes.sub buf bc_start bc_len in
      let exports =
        if header.export_count = 0l then Ok []
        else decode_exports buf header.export_offset header.export_count
      in
      let links =
        if header.link_count = 0l then Ok []
        else decode_links buf header.link_offset header.link_count
      in
      match (exports, links) with
      | Ok exports, Ok links -> Ok { header; bytecode; exports; links }
      | Error e, _ -> Error e
      | _, Error e -> Error e)
