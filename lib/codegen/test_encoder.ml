open Alcotest

let header_size = 32

let empty_module_desc =
  { Metadata.module_name = None; exports = []; link_keys = [] }

let test_encode_header () =
  let bytes = Encoder.encode_program [||] empty_module_desc in
  check bool "has header" true (Bytes.length bytes >= header_size);
  check char "magic[0]" 'M' (Bytes.get bytes 0);
  check char "magic[1]" 'U' (Bytes.get bytes 1);
  check char "magic[2]" 'S' (Bytes.get bytes 2);
  check char "magic[3]" 'I' (Bytes.get bytes 3)

let test_encode_empty_program () =
  let bytes = Encoder.encode_program [||] empty_module_desc in
  check bool "non-empty" true (Bytes.length bytes > 0);
  check bool "has header" true (Bytes.length bytes >= header_size)

let test_encode_single_instruction () =
  let instrs = [ Instr.Nop ] in
  let bytes = Encoder.encode_program [| instrs |] empty_module_desc in
  check bool "has bytecode" true (Bytes.length bytes > header_size)

let test_encode_multiple_instructions () =
  let instrs = [ Instr.LdcI4_1; Instr.LdcI4_1; Instr.Add; Instr.Ret ] in
  let bytes = Encoder.encode_program [| instrs |] empty_module_desc in
  check bool "has bytecode" true (Bytes.length bytes > header_size)

let () =
  run
    "Encoder"
    [
      ( "encoding"
      , [
          test_case "header" `Quick test_encode_header
        ; test_case "empty program" `Quick test_encode_empty_program
        ; test_case "single instruction" `Quick test_encode_single_instruction
        ; test_case
            "multiple instructions"
            `Quick
            test_encode_multiple_instructions
        ] )
    ]
