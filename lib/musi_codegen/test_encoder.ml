open Musi_basic
open Musi_codegen
open Alcotest

let encode_instrs instrs =
  let interner = Interner.create () in
  let encoder = Encoder.make interner in
  Encoder.encode encoder instrs

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
  (check int) "nop opcode is 0x00" 0x00 (Bytes.get_uint8 bytecode code_start)

let test_encode_add () =
  let bytecode = encode_instrs [ Instr.Add ] in
  let code_start = hdr_size in
  (check int) "add opcode is 0x30" 0x30 (Bytes.get_uint8 bytecode code_start)

let test_encode_ret () =
  let bytecode = encode_instrs [ Instr.Ret ] in
  let code_start = hdr_size in
  (check int) "ret opcode is 0x72" 0x72 (Bytes.get_uint8 bytecode code_start)

let test_encode_ldloc () =
  let bytecode = encode_instrs [ Instr.LdLoc 5 ] in
  let code_start = hdr_size in
  (check int) "ldloc opcode is 0x20" 0x20 (Bytes.get_uint8 bytecode code_start)

let test_encode_stloc () =
  let bytecode = encode_instrs [ Instr.StLoc 3 ] in
  let code_start = hdr_size in
  (check int) "stloc opcode is 0x21" 0x21 (Bytes.get_uint8 bytecode code_start)

let test_encode_multiple_instrs () =
  let bytecode = encode_instrs [ Instr.Nop; Instr.Add; Instr.Ret ] in
  let code_start = hdr_size in
  (check int) "first instr is nop" 0x00 (Bytes.get_uint8 bytecode code_start);
  (check int)
    "second instr is add"
    0x30
    (Bytes.get_uint8 bytecode (code_start + 1));
  (check int)
    "third instr is ret"
    0x72
    (Bytes.get_uint8 bytecode (code_start + 2))

let test_empty_bytecode () =
  let bytecode = encode_instrs [] in
  (check int) "bytecode is exactly header size" hdr_size (Bytes.length bytecode)

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
    ; ( "opcodes"
      , [
          test_case "nop" `Quick test_encode_nop
        ; test_case "add" `Quick test_encode_add
        ; test_case "ret" `Quick test_encode_ret
        ; test_case "ldloc" `Quick test_encode_ldloc
        ; test_case "stloc" `Quick test_encode_stloc
        ; test_case "multiple instructions" `Quick test_encode_multiple_instrs
        ] )
    ]
