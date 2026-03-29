use std::str::FromStr;

use seam_ir::{Instruction, InstructionStream, Opcode, Operand};

use crate::{CodecError, CodecResult};

pub fn format_instruction(instruction: &Instruction) -> String {
    match &instruction.operand {
        Operand::None => instruction.opcode.mnemonic().to_owned(),
        Operand::U8(raw) => format!("{} {raw}", instruction.opcode.mnemonic()),
        Operand::U16(raw) => format!("{} {raw}", instruction.opcode.mnemonic()),
        Operand::I16(raw) => format!("{} {raw}", instruction.opcode.mnemonic()),
        Operand::Wide(primary, secondary) => {
            format!("{} {primary} {secondary}", instruction.opcode.mnemonic())
        }
        Operand::TypeLen(type_id, length) => {
            format!("{} {type_id} {length}", instruction.opcode.mnemonic())
        }
        Operand::Effect(effect_id, operation_id) => format!(
            "{} {effect_id} {operation_id}",
            instruction.opcode.mnemonic()
        ),
        Operand::EffectJump(effect_id, operation_id, jump) => format!(
            "{} {effect_id} {operation_id} {jump}",
            instruction.opcode.mnemonic()
        ),
        Operand::Table(offsets) => format!(
            "{} {}",
            instruction.opcode.mnemonic(),
            offsets
                .iter()
                .map(i16::to_string)
                .collect::<Vec<_>>()
                .join(" ")
        ),
    }
}

pub fn disassemble_method(instructions: &[Instruction]) -> Vec<String> {
    instructions.iter().map(format_instruction).collect()
}

/// Parse one text-IL instruction line into a SEAM instruction.
///
/// # Errors
/// Returns an error when the mnemonic is unknown or its operands do not match
/// the opcode's expected shape.
pub fn parse_instruction(line: &str) -> CodecResult<Instruction> {
    let parts = line.split_whitespace().collect::<Vec<_>>();
    let Some((&mnemonic, operands)) = parts.split_first() else {
        return Err(CodecError::InvalidMnemonic {
            mnemonic: String::new(),
        });
    };

    let opcode = Opcode::from_mnemonic(mnemonic).ok_or_else(|| CodecError::InvalidMnemonic {
        mnemonic: mnemonic.to_owned(),
    })?;

    match operands {
        [] => Ok(Instruction::simple(opcode)),
        [single] => {
            if matches!(
                opcode,
                Opcode::LdLoc | Opcode::StLoc | Opcode::Call | Opcode::CallTail | Opcode::EffCont
            ) {
                return Ok(Instruction::with_u8(
                    opcode,
                    parse_number(single, mnemonic)?,
                ));
            }
            if matches!(opcode, Opcode::BrTrue | Opcode::BrFalse | Opcode::BrJmp) {
                return Ok(Instruction::with_i16(
                    opcode,
                    parse_number(single, mnemonic)?,
                ));
            }
            Ok(Instruction::with_u16(
                opcode,
                parse_number(single, mnemonic)?,
            ))
        }
        [first, second] => {
            if matches!(opcode, Opcode::ClsNew) {
                return Ok(Instruction::with_wide(
                    opcode,
                    parse_number(first, mnemonic)?,
                    parse_number(second, mnemonic)?,
                ));
            }
            if matches!(opcode, Opcode::SeqNew | Opcode::DataNew) {
                return Ok(Instruction::with_type_len(
                    opcode,
                    parse_number(first, mnemonic)?,
                    parse_number(second, mnemonic)?,
                ));
            }
            Ok(Instruction::with_effect(
                opcode,
                parse_number(first, mnemonic)?,
                parse_number(second, mnemonic)?,
            ))
        }
        [first, second, third] => {
            if opcode == Opcode::BrTbl {
                let offsets = vec![
                    parse_number(first, mnemonic)?,
                    parse_number(second, mnemonic)?,
                    parse_number(third, mnemonic)?,
                ];
                return Ok(Instruction::with_table(opcode, offsets));
            }
            Ok(Instruction::with_effect_jump(
                opcode,
                parse_number(first, mnemonic)?,
                parse_number(second, mnemonic)?,
                parse_number(third, mnemonic)?,
            ))
        }
        many if opcode == Opcode::BrTbl => Ok(Instruction::with_table(
            opcode,
            many.iter()
                .map(|raw| parse_number(raw, mnemonic))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        _ => Err(CodecError::InvalidOperand {
            mnemonic: mnemonic.to_owned(),
        }),
    }
}

/// Parse a text-IL method body into an instruction stream.
///
/// # Errors
/// Returns an error when any non-empty, non-label line is not a valid SEAM
/// instruction.
pub fn assemble_method(source: &str) -> CodecResult<InstructionStream> {
    source
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| !line.ends_with(':'))
        .map(parse_instruction)
        .collect()
}

fn parse_number<T>(raw: &str, mnemonic: &str) -> CodecResult<T>
where
    T: FromStr,
{
    raw.parse().map_err(|_| CodecError::InvalidOperand {
        mnemonic: mnemonic.to_owned(),
    })
}

#[cfg(test)]
mod tests;
