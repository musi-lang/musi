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
  Binary.write_u32_le buf hdr.metadata_offset;
  Binary.write_u32_le buf hdr.metadata_size;
  Binary.write_i64_le buf hdr.reserved

let encode_bytecode buf instrs = List.iter (encode_instr buf) instrs

(* ========================================
   PROGRAM ENCODING
   ======================================== *)

let encode_proc_table buf proc_descs =
  Binary.write_u32_le buf (Int32.of_int (List.length proc_descs));
  List.iter
    (fun (desc : Metadata.proc_desc) ->
      Binary.write_u32_le buf (Int32.of_int desc.bytecode_offset);
      Binary.write_u32_le buf (Int32.of_int desc.bytecode_length);
      Binary.write_u32_le buf (Int32.of_int desc.param_count);
      Buffer.add_char buf (if desc.is_extern then '\x01' else '\x00'))
    proc_descs

let encode_program procs module_desc =
  let buf = Buffer.create 1024 in
  let const_pool_buf = Buffer.create 256 in
  encode_const_pool const_pool_buf module_desc.Metadata.const_pool;
  let const_pool_bytes = Buffer.to_bytes const_pool_buf in
  let bc_buf = Buffer.create 1024 in
  let proc_descs =
    Array.to_list
      (Array.mapi
         (fun i instrs ->
           let offset = Buffer.length bc_buf in
           encode_bytecode bc_buf instrs;
           let length = Buffer.length bc_buf - offset in
           let is_extern =
             List.exists (fun (proc_id, _) -> proc_id = i) module_desc.link_keys
           in
           let param_count =
             match List.nth_opt module_desc.proc_infos i with
             | Some info -> info.param_count
             | None -> 0
           in
           {
             Metadata.name = None
           ; param_count
           ; local_count = 0
           ; max_stack = 0
           ; bytecode_offset = offset
           ; bytecode_length = length
           ; is_extern
           })
         procs)
  in
  let proc_table_buf = Buffer.create 256 in
  encode_proc_table proc_table_buf proc_descs;
  let proc_table_bytes = Buffer.to_bytes proc_table_buf in
  let bc_bytes = Buffer.to_bytes bc_buf in
  let metadata_buf = Buffer.create 512 in
  Buffer.add_bytes metadata_buf const_pool_bytes;
  Buffer.add_bytes metadata_buf proc_table_bytes;
  let export_buf = Buffer.create 256 in
  Binary.write_u32_le
    export_buf
    (Int32.of_int (List.length module_desc.exports));
  List.iter
    (fun (name, proc_id) ->
      Binary.write_u16_le export_buf (String.length name);
      Buffer.add_string export_buf name;
      Binary.write_u16_le export_buf proc_id)
    module_desc.Metadata.exports;
  Buffer.add_buffer metadata_buf export_buf;
  let link_buf = Buffer.create 256 in
  Binary.write_u32_le
    link_buf
    (Int32.of_int (List.length module_desc.link_keys));
  List.iter
    (fun (proc_id, link_key) ->
      Binary.write_u16_le link_buf proc_id;
      Binary.write_u16_le link_buf (String.length link_key);
      Buffer.add_string link_buf link_key)
    module_desc.link_keys;
  Buffer.add_buffer metadata_buf link_buf;
  let metadata_bytes = Buffer.to_bytes metadata_buf in
  let metadata_offset = 32l in
  let metadata_size = Int32.of_int (Bytes.length metadata_bytes) in
  let bc_offset = Int32.add metadata_offset metadata_size in
  let bc_size = Int32.of_int (Bytes.length bc_bytes) in
  let hdr =
    {
      Metadata.magic = "MUSI"
    ; version = 1l
    ; bc_offset
    ; bc_size
    ; metadata_offset
    ; metadata_size
    ; reserved = 0L
    }
  in
  encode_header buf hdr;
  Buffer.add_bytes buf metadata_bytes;
  Buffer.add_bytes buf bc_bytes;
  Buffer.to_bytes buf
