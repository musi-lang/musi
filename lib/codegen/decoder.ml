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
    if magic <> "MUSI" then Error "invalid magic number"
    else
      let version = Binary.read_u32_le buf 4 in
      let bc_offset = Binary.read_u32_le buf 8 in
      let bc_size = Binary.read_u32_le buf 12 in
      let metadata_offset = Binary.read_u32_le buf 16 in
      let metadata_size = Binary.read_u32_le buf 20 in
      let reserved = Binary.read_i64_le buf 24 in
      Ok
        {
          Metadata.magic
        ; version
        ; bc_offset
        ; bc_size
        ; metadata_offset
        ; metadata_size
        ; reserved
        }

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
      let exports = Ok [] in
      let links = Ok [] in
      match (exports, links) with
      | Ok exports, Ok links -> Ok { header; bytecode; exports; links }
      | Error e, _ -> Error e
      | _, Error e -> Error e)
