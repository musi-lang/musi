use seam_ir::{Instruction, Opcode};

use super::{assemble_method, disassemble_method, format_instruction, parse_instruction};

#[test]
fn formatter_prints_simple_and_operand_instructions() {
    assert_eq!(format_instruction(&Instruction::simple(Opcode::Ret)), "ret");
    assert_eq!(
        format_instruction(&Instruction::with_type_len(Opcode::DataNew, 7, 2)),
        "data.new 7 2"
    );
}

#[test]
fn disassembler_reads_instruction_stream() {
    let lines = disassemble_method(&[
        Instruction::with_u16(Opcode::LdSmi, 1),
        Instruction::simple(Opcode::Ret),
    ]);
    assert_eq!(lines, vec!["ld.smi 1", "ret"]);
}

#[test]
fn parser_round_trips_instruction_text() {
    let instruction = parse_instruction("hdl.push 1 2 -4").expect("instruction should parse");
    assert_eq!(format_instruction(&instruction), "hdl.push 1 2 -4");
}

#[test]
fn assembler_parses_multiline_text() {
    let instructions = assemble_method("ld.smi 1\nret").expect("method should parse");
    assert_eq!(instructions.len(), 2);
    assert_eq!(instructions[0], Instruction::with_u16(Opcode::LdSmi, 1));
    assert_eq!(instructions[1], Instruction::simple(Opcode::Ret));
}
