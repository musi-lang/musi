(* ========================================
   BYTE WRITING
   ======================================== *)

let write_u8 buf byte = Buffer.add_char buf (Char.chr byte)

(* ========================================
   INSTRUCTION ENCODING
   ======================================== *)

let encode_instr buf instr =
  write_u8 buf (Instr.to_opcode instr);
  match instr with
  | Instr.LdcI4 n -> Binary.write_i32_le buf n
  | Instr.LdcStr idx -> Binary.write_u16_le buf idx
  | Instr.LdLoc idx | Instr.StLoc idx | Instr.LdArg idx ->
    Binary.write_u16_le buf idx
  | Instr.CmpEq -> write_u8 buf 0x01
  | Instr.CmpNe -> write_u8 buf 0x02
  | Instr.CmpLt -> write_u8 buf 0x03
  | Instr.CmpGt -> write_u8 buf 0x04
  | Instr.CmpLe -> write_u8 buf 0x05
  | Instr.CmpGe -> write_u8 buf 0x06
  | Instr.Br offset | Instr.BrTrue offset | Instr.BrFalse offset ->
    Binary.write_i32_le buf (Int32.of_int offset)
  | Instr.Call proc_id -> Binary.write_i32_le buf (Int32.of_int proc_id)
  | _ -> ()

(* ========================================
   SECTION ENCODING
   ======================================== *)

let encode_const_pool buf const_pool =
  Binary.write_u32_le buf (Int32.of_int (List.length const_pool));
  List.iter
    (fun const ->
      match const with
      | Metadata.ConstI32 n ->
        write_u8 buf 0x01;
        Binary.write_i32_le buf n
      | Metadata.ConstI64 n ->
        write_u8 buf 0x02;
        Binary.write_i64_le buf n
      | Metadata.ConstText s ->
        write_u8 buf 0x05;
        Binary.write_u32_le buf (Int32.of_int (String.length s));
        Buffer.add_string buf s)
    const_pool

let encode_header buf hdr =
  Buffer.add_string buf hdr.Metadata.magic;
  Binary.write_u32_le buf hdr.version;
  Binary.write_u32_le buf hdr.bc_offset;
  Binary.write_u32_le buf hdr.bc_size;
  Binary.write_u32_le buf hdr.export_offset;
  Binary.write_u32_le buf hdr.export_count;
  Binary.write_u32_le buf hdr.link_offset;
  Binary.write_u32_le buf hdr.link_count

let encode_bytecode buf instrs = List.iter (encode_instr buf) (List.rev instrs)

(* ========================================
   PROGRAM ENCODING
   ======================================== *)

let encode_program procs module_desc =
  let buf = Buffer.create 1024 in
  let const_pool_buf = Buffer.create 256 in
  encode_const_pool const_pool_buf module_desc.Metadata.const_pool;
  let const_pool_bytes = Buffer.to_bytes const_pool_buf in
  let bc_buf = Buffer.create 1024 in
  Array.iter (encode_bytecode bc_buf) procs;
  let bc_bytes = Buffer.to_bytes bc_buf in
  let bc_offset =
    Int32.add 32l (Int32.of_int (Bytes.length const_pool_bytes))
  in
  let bc_size = Int32.of_int (Bytes.length bc_bytes) in
  let export_buf = Buffer.create 256 in
  List.iter
    (fun (name, proc_id) ->
      Binary.write_u16_le export_buf (String.length name);
      Buffer.add_string export_buf name;
      Binary.write_u16_le export_buf proc_id)
    module_desc.Metadata.exports;
  let export_bytes = Buffer.to_bytes export_buf in
  let export_offset = Int32.add bc_offset bc_size in
  let export_count = Int32.of_int (List.length module_desc.exports) in
  let link_buf = Buffer.create 256 in
  List.iter
    (fun (proc_id, link_key) ->
      Binary.write_u16_le link_buf proc_id;
      Binary.write_u16_le link_buf (String.length link_key);
      Buffer.add_string link_buf link_key)
    module_desc.link_keys;
  let link_bytes = Buffer.to_bytes link_buf in
  let link_offset =
    Int32.add export_offset (Int32.of_int (Bytes.length export_bytes))
  in
  let link_count = Int32.of_int (List.length module_desc.link_keys) in
  let hdr =
    {
      Metadata.magic = "MUSI"
    ; version = 1l
    ; bc_offset
    ; bc_size
    ; export_offset
    ; export_count
    ; link_offset
    ; link_count
    }
  in
  encode_header buf hdr;
  Buffer.add_bytes buf const_pool_bytes;
  Buffer.add_bytes buf bc_bytes;
  Buffer.add_bytes buf export_bytes;
  Buffer.add_bytes buf link_bytes;
  Buffer.to_bytes buf
