open Musi_basic
open Musi_codegen
open Alcotest

let encode_instrs instrs =
  let interner = Interner.create () in
  let encoder = Encoder.make interner in
  Encoder.encode encoder [] [] [] instrs

let test_header_magic () =
  let bytecode = encode_instrs [] in
  let magic = Bytes.sub_string bytecode 0 4 in
  (check string) "magic is MUSI" "MUSI" magic

let hdr_size = 32

let test_header_size () =
  let bytecode = encode_instrs [] in
  (check bool)
    "header is at least 32 bytes"
    true
    (Bytes.length bytecode >= hdr_size)

let test_encode_nop () =
  let bytecode = encode_instrs [ Instr.Nop ] in
  let code_start = hdr_size in
  (check int) "'Nop' opcode is 0x00" 0x00 (Bytes.get_uint8 bytecode code_start)

let test_encode_add () =
  let bytecode = encode_instrs [ Instr.Add ] in
  let code_start = hdr_size in
  (check int) "'Add' opcode is 0x30" 0x30 (Bytes.get_uint8 bytecode code_start)

let test_encode_ret () =
  let bytecode = encode_instrs [ Instr.Ret ] in
  let code_start = hdr_size in
  (check int) "'Ret' opcode is 0x72" 0x72 (Bytes.get_uint8 bytecode code_start)

let test_encode_ldloc () =
  let bytecode = encode_instrs [ Instr.LdLoc 5 ] in
  let code_start = hdr_size in
  (check int)
    "'LdLoc' opcode is 0x20"
    0x20
    (Bytes.get_uint8 bytecode code_start)

let test_encode_stloc () =
  let bytecode = encode_instrs [ Instr.StLoc 3 ] in
  let code_start = hdr_size in
  (check int)
    "'StLoc' opcode is 0x21"
    0x21
    (Bytes.get_uint8 bytecode code_start)

let test_encode_multiple_instrs () =
  let bytecode = encode_instrs [ Instr.Nop; Instr.Add; Instr.Ret ] in
  let code_start = hdr_size in
  (check int) "first instr is 'Nop'" 0x00 (Bytes.get_uint8 bytecode code_start);
  (check int)
    "second instr is 'Add'"
    0x30
    (Bytes.get_uint8 bytecode (code_start + 1));
  (check int)
    "third instr is 'Ret'"
    0x72
    (Bytes.get_uint8 bytecode (code_start + 2))

let test_empty_bytecode () =
  let bytecode = encode_instrs [] in
  (check int) "bytecode is exactly header size" hdr_size (Bytes.length bytecode)

let test_proc_table_empty () =
  let bytecode = encode_instrs [] in
  let proc_size_offset = 20 in
  let proc_size = Bytes.get_int32_le bytecode proc_size_offset in
  (check int32) "proc table size is 0" 0l proc_size

let test_proc_table_with_proc () =
  let interner = Interner.create () in
  let proc_name = Interner.intern interner "test" in
  let proc =
    {
      Emitter.proc_name
    ; param_count = 2
    ; local_count = 3
    ; code_offset = 0
    ; is_extern = false
    ; abi = None
    }
  in
  let encoder = Encoder.make interner in
  let bytecode = Encoder.encode encoder [] [] [ proc ] [] in
  let proc_size_offset = 20 in
  let proc_size = Bytes.get_int32_le bytecode proc_size_offset in
  (check bool) "proc table has data" true (Int32.to_int proc_size > 0)

let () =
  run
    "Encoder"
    [
      ( "header"
      , [
          test_case "magic" `Quick test_header_magic
        ; test_case "size" `Quick test_header_size
        ; test_case "empty bytecode" `Quick test_empty_bytecode
        ] )
    ; ( "procedure table"
      , [
          test_case "empty proc table" `Quick test_proc_table_empty
        ; test_case "proc table with proc" `Quick test_proc_table_with_proc
        ] )
    ; ( "opcodes"
      , [
          test_case "nop" `Quick test_encode_nop
        ; test_case "add" `Quick test_encode_add
        ; test_case "ret" `Quick test_encode_ret
        ; test_case "ld.loc" `Quick test_encode_ldloc
        ; test_case "st.loc" `Quick test_encode_stloc
        ; test_case "multiple instrs" `Quick test_encode_multiple_instrs
        ] )
    ]
