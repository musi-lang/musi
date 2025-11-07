let check_buffer_bounds ctx buf offset size =
  if offset < 0 || offset + size - 1 >= Bytes.length buf then
    invalid_arg (Printf.sprintf "Binary.%s: offset out of bounds" ctx)

let read_byte buf offset =
  check_buffer_bounds "read_byte" buf offset 1;
  Char.code (Bytes.get buf offset)

let write_byte buf value = Buffer.add_char buf (Char.chr value)

let read_u16_le buf offset =
  check_buffer_bounds "read_u16_le" buf offset 2;
  let b0 = read_byte buf offset in
  let b1 = read_byte buf (offset + 1) in
  b0 lor (b1 lsl 8)

let read_u32_le buf offset =
  check_buffer_bounds "read_u32_le" buf offset 4;
  let b0 = read_byte buf offset in
  let b1 = read_byte buf (offset + 1) in
  let b2 = read_byte buf (offset + 2) in
  let b3 = read_byte buf (offset + 3) in
  Int32.logor
    (Int32.logor (Int32.of_int b0) (Int32.shift_left (Int32.of_int b1) 8))
    (Int32.logor
       (Int32.shift_left (Int32.of_int b2) 16)
       (Int32.shift_left (Int32.of_int b3) 24))

let read_i32_le = read_u32_le

let read_i64_le buf offset =
  check_buffer_bounds "read_i64_le" buf offset 8;
  let b0 = Int64.of_int (read_byte buf offset) in
  let b1 = Int64.of_int (read_byte buf (offset + 1)) in
  let b2 = Int64.of_int (read_byte buf (offset + 2)) in
  let b3 = Int64.of_int (read_byte buf (offset + 3)) in
  let b4 = Int64.of_int (read_byte buf (offset + 4)) in
  let b5 = Int64.of_int (read_byte buf (offset + 5)) in
  let b6 = Int64.of_int (read_byte buf (offset + 6)) in
  let b7 = Int64.of_int (read_byte buf (offset + 7)) in
  Int64.logor
    (Int64.logor
       (Int64.logor b0 (Int64.shift_left b1 8))
       (Int64.logor (Int64.shift_left b2 16) (Int64.shift_left b3 24)))
    (Int64.logor
       (Int64.logor (Int64.shift_left b4 32) (Int64.shift_left b5 40))
       (Int64.logor (Int64.shift_left b6 48) (Int64.shift_left b7 56)))

let write_u16_le buf value =
  write_byte buf (value land 0xFF);
  write_byte buf ((value lsr 8) land 0xFF)

let write_u32_le buf value =
  write_byte buf (Int32.to_int (Int32.logand value 0xFFl));
  write_byte
    buf
    (Int32.to_int (Int32.logand (Int32.shift_right_logical value 8) 0xFFl));
  write_byte
    buf
    (Int32.to_int (Int32.logand (Int32.shift_right_logical value 16) 0xFFl));
  write_byte
    buf
    (Int32.to_int (Int32.logand (Int32.shift_right_logical value 24) 0xFFl))

let write_i32_le = write_u32_le

let write_i64_le buf value =
  write_byte buf (Int64.to_int (Int64.logand value 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 8) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 16) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 24) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 32) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 40) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 48) 0xFFL));
  write_byte
    buf
    (Int64.to_int (Int64.logand (Int64.shift_right_logical value 56) 0xFFL))
