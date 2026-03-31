use music_il::{Instruction, Operand};

#[must_use]
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
        Operand::BranchTable(offsets) => format!(
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

#[must_use]
pub fn disassemble_method(instructions: &[Instruction]) -> Vec<String> {
    instructions.iter().map(format_instruction).collect()
}
