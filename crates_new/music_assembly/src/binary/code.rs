#![allow(clippy::arithmetic_side_effects)]

use music_il::{Instruction, InstructionStream, Opcode, Operand};

use super::*;

pub(super) fn decode_instruction_stream(code: &[u8]) -> CodecResult<InstructionStream> {
    let mut position = 0_usize;
    let mut instructions = vec![];

    while position < code.len() {
        let opcode_byte = *code.get(position).ok_or(CodecError::TruncatedSection)?;
        let opcode = Opcode::from_byte(opcode_byte).ok_or(CodecError::InvalidOpcode {
            byte: opcode_byte,
            offset: position,
        })?;
        position += 1;

        let instruction = match opcode {
            Opcode::LdLoc | Opcode::StLoc | Opcode::Call | Opcode::CallTail | Opcode::EffCont => {
                let raw = *code.get(position).ok_or(CodecError::TruncatedSection)?;
                position += 1;
                Instruction::with_u8(opcode, raw)
            }
            Opcode::LdConst
            | Opcode::LdGlob
            | Opcode::LdUpv
            | Opcode::StGlob
            | Opcode::StUpv
            | Opcode::LdLocW
            | Opcode::StLocW
            | Opcode::LdSmi
            | Opcode::SeqGet
            | Opcode::SeqSet
            | Opcode::SeqLen
            | Opcode::SeqSlice
            | Opcode::DataGet
            | Opcode::DataSet
            | Opcode::DataTag
            | Opcode::TyChk
            | Opcode::TyCast
            | Opcode::TyId
            | Opcode::FfiCall => {
                let raw = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                Instruction::with_u16(opcode, raw)
            }
            Opcode::BrTrue | Opcode::BrFalse | Opcode::BrJmp => {
                let raw = i16::from_le_bytes(
                    read_array::<2>(code, position).ok_or(CodecError::TruncatedSection)?,
                );
                position += 2;
                Instruction::with_i16(opcode, raw)
            }
            Opcode::ClsNew => {
                let primary = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let secondary = *code.get(position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 3;
                Instruction::with_wide(opcode, primary, secondary)
            }
            Opcode::SeqNew | Opcode::DataNew => {
                let type_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let length = read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 4;
                Instruction::with_type_len(opcode, type_id, length)
            }
            Opcode::EffInvk => {
                let effect_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let operation_id =
                    read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 4;
                Instruction::with_effect(opcode, effect_id, operation_id)
            }
            Opcode::HdlPush => {
                let effect_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let operation_id =
                    read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                let jump = i16::from_le_bytes(
                    read_array::<2>(code, position + 4).ok_or(CodecError::TruncatedSection)?,
                );
                position += 6;
                Instruction::with_effect_jump(opcode, effect_id, operation_id, jump)
            }
            Opcode::BrTbl => {
                let entry_count = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                let mut table = Vec::with_capacity(usize::from(entry_count));
                for _ in 0..entry_count {
                    table.push(i16::from_le_bytes(
                        read_array::<2>(code, position).ok_or(CodecError::TruncatedSection)?,
                    ));
                    position += 2;
                }
                Instruction::with_table(opcode, table)
            }
            _ => Instruction::basic(opcode),
        };

        instructions.push(instruction);
    }

    Ok(instructions)
}

pub(super) fn encode_instruction(output: &mut SectionBytes, instruction: &Instruction) {
    output.push(instruction.opcode.to_byte());

    match &instruction.operand {
        Operand::None => {}
        Operand::U8(value) => output.push(*value),
        Operand::U16(value) => output.extend_from_slice(&value.to_le_bytes()),
        Operand::I16(value) => output.extend_from_slice(&value.to_le_bytes()),
        Operand::Wide(primary, secondary) => {
            output.extend_from_slice(&primary.to_le_bytes());
            output.push(*secondary);
        }
        Operand::TypeLen(type_id, length) => {
            output.extend_from_slice(&type_id.to_le_bytes());
            output.extend_from_slice(&length.to_le_bytes());
        }
        Operand::Effect(effect_id, operation_id) => {
            output.extend_from_slice(&effect_id.to_le_bytes());
            output.extend_from_slice(&operation_id.to_le_bytes());
        }
        Operand::EffectJump(effect_id, operation_id, jump) => {
            output.extend_from_slice(&effect_id.to_le_bytes());
            output.extend_from_slice(&operation_id.to_le_bytes());
            output.extend_from_slice(&jump.to_le_bytes());
        }
        Operand::BranchTable(offsets) => {
            let count = u16::try_from(offsets.len()).expect("branch table length fits in u16");
            output.extend_from_slice(&count.to_le_bytes());
            for offset in offsets {
                output.extend_from_slice(&offset.to_le_bytes());
            }
        }
    }
}

pub(super) fn scan_method_bytes(
    data: &[u8],
    position: &mut usize,
    instruction_count: u16,
) -> CodecResult<SectionBytes> {
    let start = *position;
    for _ in 0..instruction_count {
        let opcode_position = *position;
        let opcode_byte = *data.get(*position).ok_or(CodecError::TruncatedSection)?;
        *position += 1;
        let opcode = Opcode::from_byte(opcode_byte).ok_or(CodecError::InvalidOpcode {
            byte: opcode_byte,
            offset: opcode_position,
        })?;
        let extra = operand_size(opcode, data, *position)?;
        let _ = data
            .get(*position..*position + extra)
            .ok_or(CodecError::TruncatedSection)?;
        *position += extra;
    }

    data.get(start..*position)
        .map(<[u8]>::to_vec)
        .ok_or(CodecError::TruncatedSection)
}

fn operand_size(opcode: Opcode, data: &[u8], position: usize) -> CodecResult<usize> {
    match opcode {
        Opcode::LdLoc | Opcode::StLoc | Opcode::Call | Opcode::CallTail | Opcode::EffCont => Ok(1),
        Opcode::LdConst
        | Opcode::LdGlob
        | Opcode::LdUpv
        | Opcode::StGlob
        | Opcode::StUpv
        | Opcode::LdLocW
        | Opcode::StLocW
        | Opcode::LdSmi
        | Opcode::SeqGet
        | Opcode::SeqSet
        | Opcode::SeqLen
        | Opcode::SeqSlice
        | Opcode::DataGet
        | Opcode::DataSet
        | Opcode::DataTag
        | Opcode::TyChk
        | Opcode::TyCast
        | Opcode::TyId
        | Opcode::BrTrue
        | Opcode::BrFalse
        | Opcode::BrJmp
        | Opcode::FfiCall => Ok(2),
        Opcode::ClsNew => Ok(3),
        Opcode::SeqNew | Opcode::DataNew | Opcode::EffInvk => Ok(4),
        Opcode::HdlPush => Ok(6),
        Opcode::BrTbl => {
            let count = usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
            Ok(2 + count * 2)
        }
        _ => Ok(0),
    }
}
