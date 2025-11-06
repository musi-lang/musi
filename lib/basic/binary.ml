let read_u16_le buf offset =
  let b0 = Char.code (Bytes.get buf offset) in
  let b1 = Char.code (Bytes.get buf (offset + 1)) in
  b0 lor (b1 lsl 8)

let read_u32_le buf offset =
  let b0 = Char.code (Bytes.get buf offset) in
  let b1 = Char.code (Bytes.get buf (offset + 1)) in
  let b2 = Char.code (Bytes.get buf (offset + 2)) in
  let b3 = Char.code (Bytes.get buf (offset + 3)) in
  Int32.logor
    (Int32.logor (Int32.of_int b0) (Int32.shift_left (Int32.of_int b1) 8))
    (Int32.logor
       (Int32.shift_left (Int32.of_int b2) 16)
       (Int32.shift_left (Int32.of_int b3) 24))

let read_i32_le = read_u32_le

let write_u16_le buf value =
  Buffer.add_char buf (Char.chr (value land 0xFF));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF))

let write_u32_le buf value =
  Buffer.add_char buf (Char.chr (Int32.to_int (Int32.logand value 0xFFl)));
  Buffer.add_char
    buf
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical value 8) 0xFFl)));
  Buffer.add_char
    buf
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical value 16) 0xFFl)));
  Buffer.add_char
    buf
    (Char.chr
       (Int32.to_int (Int32.logand (Int32.shift_right_logical value 24) 0xFFl)))

let write_i32_le = write_u32_le
