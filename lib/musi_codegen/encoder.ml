open Musi_basic

type t = {
    const_pool : Emitter.const_kind list
  ; procs : Emitter.proc_info list
  ; interner : Interner.t
}

let hdr_size = 32
let make interner = { const_pool = []; procs = []; interner }

let write_op_u32 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_u32_le buf (Int32.of_int operand)

let write_op_i32 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_i32_le buf (Int32.of_int operand)

let write_op_i64 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_i64_le buf operand

let encode_opcode interner buf = function
  (* Base Instructions *)
  | Instr.Nop -> Buffer.add_char buf '\x00'
  | Instr.Br offset -> write_op_i32 buf '\x38' offset
  | Instr.BrTrue offset -> write_op_i32 buf '\x39' offset
  | Instr.BrFalse offset -> write_op_i32 buf '\x3A' offset
  | Instr.Beq offset -> write_op_i32 buf '\x3B' offset
  | Instr.Bge offset -> write_op_i32 buf '\x3C' offset
  | Instr.Bgt offset -> write_op_i32 buf '\x3D' offset
  | Instr.Ble offset -> write_op_i32 buf '\x3E' offset
  | Instr.Blt offset -> write_op_i32 buf '\x3F' offset
  | Instr.Bne offset -> write_op_i32 buf '\x40' offset
  | Instr.Leave offset -> write_op_i32 buf '\xC7' offset
  | Instr.Ret -> Buffer.add_char buf '\x2A'
  | Instr.Throw -> Buffer.add_char buf '\x7A'
  | Instr.Rethrow -> Buffer.add_char buf '\x7B'
  | Instr.Try -> Buffer.add_char buf '\xC8'
  | Instr.Defer -> Buffer.add_char buf '\xC9'
  (* Stack Operations *)
  | Instr.Dup -> Buffer.add_char buf '\x25'
  | Instr.Pop -> Buffer.add_char buf '\x26'
  (* Load Constants *)
  | Instr.LdNull -> Buffer.add_char buf '\x14'
  | Instr.LdCI4 n ->
    Buffer.add_char buf '\x20';
    Binary.write_i32_le buf n
  | Instr.LdCI8 n ->
    Buffer.add_char buf '\x21';
    Binary.write_i64_le buf n
  | Instr.LdCN8 n -> write_op_u32 buf '\x22' n
  | Instr.LdCN16 n -> write_op_u32 buf '\x23' n
  | Instr.LdCN32 n -> write_op_u32 buf '\x24' n
  | Instr.LdCN64 n -> write_op_u32 buf '\x2F' n
  | Instr.LdCB32 f ->
    Buffer.add_char buf '\x30';
    Binary.write_f32_le buf f
  | Instr.LdCB64 f ->
    Buffer.add_char buf '\x31';
    Binary.write_f64_le buf f
  | Instr.LdCD32 f ->
    Buffer.add_char buf '\x32';
    Binary.write_f32_le buf f
  | Instr.LdCD64 f ->
    Buffer.add_char buf '\x33';
    Binary.write_f64_le buf f
  | Instr.LdCStr idx -> write_op_u32 buf '\x72' idx
  (* Load/Store Variables *)
  | Instr.LdLoc idx -> write_op_u32 buf '\x0E' idx
  | Instr.StLoc idx -> write_op_u32 buf '\x0F' idx
  | Instr.LdLocA idx -> write_op_u32 buf '\x10' idx
  | Instr.LdArg idx -> write_op_u32 buf '\x02' idx
  | Instr.StArg idx -> write_op_u32 buf '\x03' idx
  | Instr.LdArgA idx -> write_op_u32 buf '\x04' idx
  (* Object Operations *)
  | Instr.NewObj ctor -> write_op_u32 buf '\x73' ctor
  | Instr.Call name ->
    Buffer.add_char buf '\x28';
    Buffer.add_string buf (Interner.lookup interner name);
    Buffer.add_char buf '\x00'
  | Instr.CallVirt name ->
    Buffer.add_char buf '\x6F';
    Buffer.add_string buf (Interner.lookup interner name);
    Buffer.add_char buf '\x00'
  | Instr.CallI -> Buffer.add_char buf '\x29'
  | Instr.LdFld idx -> write_op_u32 buf '\x7B' idx
  | Instr.StFld idx -> write_op_u32 buf '\x7D' idx
  | Instr.LdFldA idx -> write_op_u32 buf '\x7C' idx
  | Instr.LdSFld idx -> write_op_u32 buf '\x80' idx
  | Instr.StSFld idx -> write_op_u32 buf '\x81' idx
  | Instr.LdSFldA idx -> write_op_u32 buf '\x82' idx
  | Instr.NewArr type_id -> write_op_u32 buf '\x8D' type_id
  | Instr.LdElem -> Buffer.add_char buf '\xA3'
  | Instr.StElem -> Buffer.add_char buf '\xA4'
  | Instr.LdElemA -> Buffer.add_char buf '\x8F'
  | Instr.LdLen -> Buffer.add_char buf '\x8E'
  (* Type Operations *)
  | Instr.LdType type_id -> write_op_u32 buf '\xD1' type_id
  | Instr.IsInst type_id -> write_op_u32 buf '\x75' type_id
  | Instr.CastClass type_id -> write_op_u32 buf '\x74' type_id
  | Instr.Box type_id -> write_op_u32 buf '\x8C' type_id
  | Instr.UnboxAny type_id -> write_op_u32 buf '\xA5' type_id
  | Instr.ConvI8 -> Buffer.add_char buf '\x67'
  | Instr.ConvI16 -> Buffer.add_char buf '\x68'
  | Instr.ConvI32 -> Buffer.add_char buf '\x69'
  | Instr.ConvI64 -> Buffer.add_char buf '\x6A'
  | Instr.ConvI128 -> Buffer.add_char buf '\xD0'
  | Instr.ConvN8 -> Buffer.add_char buf '\xD2'
  | Instr.ConvN16 -> Buffer.add_char buf '\xD3'
  | Instr.ConvN32 -> Buffer.add_char buf '\xD4'
  | Instr.ConvN64 -> Buffer.add_char buf '\xD5'
  | Instr.ConvN128 -> Buffer.add_char buf '\xD6'
  | Instr.ConvB32 -> Buffer.add_char buf '\xB6'
  | Instr.ConvB64 -> Buffer.add_char buf '\xB7'
  | Instr.ConvB128 -> Buffer.add_char buf '\xD7'
  | Instr.ConvD32 -> Buffer.add_char buf '\xD8'
  | Instr.ConvD64 -> Buffer.add_char buf '\xD9'
  | Instr.ConvD128 -> Buffer.add_char buf '\xDA'
  (* Arithmetic Operations *)
  | Instr.Add -> Buffer.add_char buf '\x58'
  | Instr.AddOvf -> Buffer.add_char buf '\xD6'
  | Instr.AddOvfUn -> Buffer.add_char buf '\xD7'
  | Instr.Sub -> Buffer.add_char buf '\x59'
  | Instr.SubOvf -> Buffer.add_char buf '\xD8'
  | Instr.SubOvfUn -> Buffer.add_char buf '\xD9'
  | Instr.Mul -> Buffer.add_char buf '\x5A'
  | Instr.MulOvf -> Buffer.add_char buf '\xDA'
  | Instr.MulOvfUn -> Buffer.add_char buf '\xDB'
  | Instr.Div -> Buffer.add_char buf '\x5B'
  | Instr.DivUn -> Buffer.add_char buf '\x5C'
  | Instr.Rem -> Buffer.add_char buf '\x5D'
  | Instr.RemUn -> Buffer.add_char buf '\x5E'
  | Instr.Neg -> Buffer.add_char buf '\x65'
  (* Logical/Bitwise Operations *)
  | Instr.And -> Buffer.add_char buf '\x5F'
  | Instr.Or -> Buffer.add_char buf '\x60'
  | Instr.Xor -> Buffer.add_char buf '\x61'
  | Instr.Not -> Buffer.add_char buf '\x66'
  | Instr.Shl -> Buffer.add_char buf '\x62'
  | Instr.Shr -> Buffer.add_char buf '\x63'
  | Instr.ShrUn -> Buffer.add_char buf '\x64'
  (* Comparison Operations *)
  | Instr.Ceq -> Buffer.add_char buf '\xFE'
  | Instr.Cgt -> Buffer.add_char buf '\xC2'
  | Instr.CgtUn -> Buffer.add_char buf '\xC3'
  | Instr.Clt -> Buffer.add_char buf '\xC4'
  | Instr.CltUn -> Buffer.add_char buf '\xC5'
  (* Memory Management *)
  | Instr.Pin -> Buffer.add_char buf '\xDF'
  | Instr.Unpin -> Buffer.add_char buf '\xE0'
  (* Dynamic Operations *)
  | Instr.LdFldDyn name ->
    Buffer.add_char buf '\xE1';
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00'
  | Instr.StFldDyn name ->
    Buffer.add_char buf '\xE2';
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00'
  | Instr.LdFldADyn name ->
    Buffer.add_char buf '\xE3';
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00'
  | Instr.CallDyn name ->
    Buffer.add_char buf '\xE4';
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00'
  | Instr.CallVirtDyn name ->
    Buffer.add_char buf '\xE5';
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00'

let encode_const t buf = function
  | Emitter.ConstInt n -> write_op_i64 buf '\x01' n
  | Emitter.ConstBin f -> write_op_i64 buf '\x02' (Int64.bits_of_float f)
  | Emitter.ConstStr name ->
    Buffer.add_char buf '\x03';
    Buffer.add_string buf (Interner.lookup t.interner name);
    Buffer.add_char buf '\x00'
  | Emitter.ConstBool true -> Buffer.add_char buf '\x04'
  | Emitter.ConstBool false -> Buffer.add_char buf '\x05'
  | Emitter.ConstUnit -> Buffer.add_char buf '\x00'

let encode_proc t buf proc =
  let name_str = Interner.lookup t.interner proc.Emitter.proc_name in
  Buffer.add_string buf name_str;
  Buffer.add_char buf '\x00';
  Binary.write_u32_le buf (Int32.of_int proc.Emitter.param_count);
  Binary.write_u32_le buf (Int32.of_int proc.Emitter.local_count);
  Binary.write_u32_le buf (Int32.of_int proc.Emitter.code_offset)

let encode t imports const_pool procs instrs =
  let t = { t with const_pool; procs } in
  let import_buf = Buffer.create 128 in
  let const_buf = Buffer.create 256 in
  let proc_buf = Buffer.create 256 in
  let code_buf = Buffer.create 1024 in

  Binary.write_u32_le import_buf (Int32.of_int (List.length imports));
  List.iter
    (fun imp ->
      Buffer.add_string import_buf imp;
      Buffer.add_char import_buf '\x00')
    imports;

  Binary.write_u32_le const_buf (Int32.of_int (List.length t.const_pool));
  List.iter (encode_const t const_buf) t.const_pool;

  Binary.write_u32_le proc_buf (Int32.of_int (List.length t.procs));
  List.iter (encode_proc t proc_buf) t.procs;

  List.iter (encode_opcode t.interner code_buf) instrs;

  let import_size = Buffer.length import_buf in
  let const_size = Buffer.length const_buf in
  let proc_size = Buffer.length proc_buf in
  let code_size = Buffer.length code_buf in

  let hdr = Buffer.create hdr_size in
  Buffer.add_string hdr "MUSI";
  Binary.write_u32_le hdr 1l;
  Binary.write_u32_le hdr (Int32.of_int hdr_size);
  Binary.write_u32_le hdr (Int32.of_int import_size);
  Binary.write_u32_le hdr (Int32.of_int (hdr_size + import_size));
  Binary.write_u32_le hdr (Int32.of_int const_size);
  Binary.write_u32_le hdr (Int32.of_int (hdr_size + import_size + const_size));
  Binary.write_u32_le hdr (Int32.of_int proc_size);

  let res =
    Buffer.create (hdr_size + import_size + const_size + proc_size + code_size)
  in
  Buffer.add_buffer res hdr;
  Buffer.add_buffer res import_buf;
  Buffer.add_buffer res const_buf;
  Buffer.add_buffer res proc_buf;
  Buffer.add_buffer res code_buf;
  Buffer.to_bytes res
