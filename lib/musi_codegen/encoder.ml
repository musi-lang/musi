open Musi_basic

type t = { const_pool : Emitter.const_kind list; interner : Interner.t }

let hdr_size = 32
let make interner = { const_pool = []; interner }

let write_op_u32 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_u32_le buf (Int32.of_int operand)

let write_op_i32 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_i32_le buf (Int32.of_int operand)

let write_op_i64 buf opcode operand =
  Buffer.add_char buf opcode;
  Binary.write_i64_le buf operand

let encode_opcode buf = function
  | Instr.Nop -> Buffer.add_char buf '\x00'
  | Instr.Pop -> Buffer.add_char buf '\x01'
  | Instr.Dup -> Buffer.add_char buf '\x02'
  | Instr.LdC idx -> write_op_u32 buf '\x10' idx
  | Instr.LdCI4 n ->
    Buffer.add_char buf '\x11';
    Binary.write_i32_le buf n
  | Instr.LdCStr idx -> write_op_u32 buf '\x12' idx
  | Instr.LdLoc slot -> write_op_u32 buf '\x20' slot
  | Instr.StLoc slot -> write_op_u32 buf '\x21' slot
  | Instr.LdArg idx -> write_op_u32 buf '\x22' idx
  | Instr.Add -> Buffer.add_char buf '\x30'
  | Instr.Sub -> Buffer.add_char buf '\x31'
  | Instr.Mul -> Buffer.add_char buf '\x32'
  | Instr.Div -> Buffer.add_char buf '\x33'
  | Instr.Mod -> Buffer.add_char buf '\x34'
  | Instr.Neg -> Buffer.add_char buf '\x35'
  | Instr.And -> Buffer.add_char buf '\x40'
  | Instr.Or -> Buffer.add_char buf '\x41'
  | Instr.Xor -> Buffer.add_char buf '\x42'
  | Instr.Not -> Buffer.add_char buf '\x43'
  | Instr.Shl -> Buffer.add_char buf '\x44'
  | Instr.Shr -> Buffer.add_char buf '\x45'
  | Instr.CmpEq -> Buffer.add_char buf '\x50'
  | Instr.CmpNe -> Buffer.add_char buf '\x51'
  | Instr.CmpLt -> Buffer.add_char buf '\x52'
  | Instr.CmpGt -> Buffer.add_char buf '\x53'
  | Instr.CmpLe -> Buffer.add_char buf '\x54'
  | Instr.CmpGe -> Buffer.add_char buf '\x55'
  | Instr.Br offset -> write_op_i32 buf '\x60' offset
  | Instr.BrTrue offset -> write_op_i32 buf '\x61' offset
  | Instr.BrFalse offset -> write_op_i32 buf '\x62' offset
  | Instr.Call idx -> write_op_u32 buf '\x70' idx
  | Instr.CallTail idx -> write_op_u32 buf '\x71' idx
  | Instr.Ret -> Buffer.add_char buf '\x72'
  | Instr.NewObj size -> write_op_u32 buf '\x80' size
  | Instr.LdFld idx -> write_op_u32 buf '\x81' idx
  | Instr.StFld idx -> write_op_u32 buf '\x82' idx
  | Instr.LdElem -> Buffer.add_char buf '\x83'
  | Instr.StElem -> Buffer.add_char buf '\x84'
  | Instr.LdLen -> Buffer.add_char buf '\x86'
  | Instr.LdCI4M1 -> Buffer.add_char buf '\x15'
  | Instr.LdCUnit -> Buffer.add_char buf '\x21'
  | Instr.IsInst idx -> write_op_u32 buf '\x90' idx
  | Instr.CastClass idx -> write_op_u32 buf '\x91' idx
  | Instr.RefInc -> Buffer.add_char buf '\xA0'
  | Instr.RefDec -> Buffer.add_char buf '\xA1'

let encode_const t buf = function
  | Emitter.ConstInt n -> write_op_i64 buf '\x01' n
  | Emitter.ConstBin f -> write_op_i64 buf '\x02' (Int64.bits_of_float f)
  | Emitter.ConstStr name ->
    Buffer.add_char buf '\x03';
    Buffer.add_string buf (Interner.lookup t.interner name)
  | Emitter.ConstBool true -> Buffer.add_char buf '\x04'
  | Emitter.ConstBool false -> Buffer.add_char buf '\x05'
  | Emitter.ConstUnit -> Buffer.add_char buf '\x00'

let encode t instrs =
  let const_buf = Buffer.create 256 in
  let code_buf = Buffer.create 1024 in

  List.iter (encode_const t const_buf) t.const_pool;
  List.iter (encode_opcode code_buf) instrs;

  let const_size = Buffer.length const_buf in
  let code_size = Buffer.length code_buf in

  let hdr = Buffer.create hdr_size in
  Buffer.add_string hdr "MUSI";
  Binary.write_u16_le hdr 1;
  Binary.write_u16_le hdr 0;
  Binary.write_u32_le hdr (Int32.of_int hdr_size);
  Binary.write_u32_le hdr (Int32.of_int const_size);
  Binary.write_u32_le hdr (Int32.of_int (hdr_size + const_size));
  Binary.write_u32_le hdr (Int32.of_int code_size);
  Buffer.add_bytes hdr (Bytes.make 8 '\x00');

  let res = Buffer.create (hdr_size + const_size + code_size) in
  Buffer.add_buffer res hdr;
  Buffer.add_buffer res const_buf;
  Buffer.add_buffer res code_buf;
  Buffer.to_bytes res
