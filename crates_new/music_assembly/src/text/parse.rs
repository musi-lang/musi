use std::str::FromStr;

use music_il::{Instruction, InstructionStream, Opcode};

use crate::{CodecError, CodecResult};

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
        [] => Ok(Instruction::basic(opcode)),
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
                return Ok(Instruction::with_ty_len(
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
