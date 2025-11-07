let disass_bytecode buf =
  let len = Bytes.length buf in
  let rec loop offset =
    if offset >= len then []
    else
      let opcode = Char.code (Bytes.get buf offset) in
      let instr, next_offset =
        match opcode with
        | 0x00 -> ("nop", offset + 1)
        | 0x09 ->
          let idx = Binary.read_u16_le buf (offset + 1) in
          (Printf.sprintf "ld.arg %d" idx, offset + 3)
        | 0x0E ->
          let idx = Binary.read_u16_le buf (offset + 1) in
          (Printf.sprintf "ld.loc %d" idx, offset + 3)
        | 0x13 ->
          let idx = Binary.read_u16_le buf (offset + 1) in
          (Printf.sprintf "st.loc %d" idx, offset + 3)
        | 0x15 -> ("ld.c.i4.m1", offset + 1)
        | 0x20 ->
          let val32 = Binary.read_i32_le buf (offset + 1) in
          (Printf.sprintf "ld.c.i4 %ld" val32, offset + 5)
        | 0x21 -> ("ld.c.unit", offset + 1)
        | 0x22 ->
          let idx = Binary.read_u16_le buf (offset + 1) in
          (Printf.sprintf "ld.c.str %d" idx, offset + 3)
        | 0x25 -> ("dup", offset + 1)
        | 0x26 -> ("pop", offset + 1)
        | 0x28 ->
          let proc_id = Binary.read_i32_le buf (offset + 1) in
          (Printf.sprintf "call %ld" proc_id, offset + 5)
        | 0x2A -> ("ret", offset + 1)
        | 0x38 ->
          let off = Binary.read_i32_le buf (offset + 1) in
          (Printf.sprintf "br %ld" off, offset + 5)
        | 0x39 ->
          let off = Binary.read_i32_le buf (offset + 1) in
          (Printf.sprintf "br.true %ld" off, offset + 5)
        | 0x3A ->
          let off = Binary.read_i32_le buf (offset + 1) in
          (Printf.sprintf "br.false %ld" off, offset + 5)
        | 0x58 -> ("add", offset + 1)
        | 0x59 -> ("sub", offset + 1)
        | 0x5A -> ("mul", offset + 1)
        | 0x5B -> ("div", offset + 1)
        | 0x65 -> ("neg", offset + 1)
        | 0xFE ->
          let sub = Char.code (Bytes.get buf (offset + 1)) in
          let cmp_name =
            match sub with
            | 0x01 -> "cmp.eq"
            | 0x02 -> "cmp.ne"
            | 0x03 -> "cmp.lt"
            | 0x04 -> "cmp.gt"
            | 0x05 -> "cmp.le"
            | 0x06 -> "cmp.ge"
            | _ -> Printf.sprintf "cmp.unknown(0x%02X)" sub
          in
          (cmp_name, offset + 2)
        | _ -> (Printf.sprintf "unknown(0x%02X)" opcode, offset + 1)
      in
      (offset, instr) :: loop next_offset
  in
  loop 0

let format_disassembly decoded =
  let lines = Buffer.create 1024 in
  Buffer.add_string lines "// Metadata\n";
  Buffer.add_string
    lines
    (Printf.sprintf "// Version: %ld\n" decoded.Decoder.header.version);
  Buffer.add_string
    lines
    (Printf.sprintf
       "// Bytecode size: %ld bytes\n"
       decoded.Decoder.header.bc_size);
  if List.length decoded.exports > 0 then (
    Buffer.add_string lines "\n// .exports\n";
    List.iter
      (fun (name, proc_id) ->
        Buffer.add_string
          lines
          (Printf.sprintf ".export \"%s\" = proc_%d\n" name proc_id))
      decoded.exports);
  if List.length decoded.links > 0 then (
    Buffer.add_string lines "\n// .links\n";
    List.iter
      (fun (proc_id, name) ->
        Buffer.add_string
          lines
          (Printf.sprintf ".link proc_%d = \"%s\"\n" proc_id name))
      decoded.links);
  Buffer.add_string lines "\n// .bytecode\n";
  let instrs = disass_bytecode decoded.bytecode in
  List.iter
    (fun (offset, instr) ->
      Buffer.add_string lines (Printf.sprintf "IL_%04X: %s\n" offset instr))
    instrs;
  Buffer.contents lines
