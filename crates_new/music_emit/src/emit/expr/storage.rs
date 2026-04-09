use super::super::*;

use super::literals::compile_i64;
use super::names::resolve_global;
use super::support::{ensure_local_slot, ensure_temp_slot, push_expr_diag, scratch_slot};

pub(super) fn compile_let(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    _name: &str,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    super::compile_expr(emitter, value, true, diags);
    if let Some(binding) = binding {
        let slot = ensure_local_slot(emitter, binding);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    } else {
        let slot = scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
    emit_zero(emitter);
}

pub(super) fn compile_temp(emitter: &mut MethodEmitter<'_, '_>, temp: IrTempId) {
    let slot = ensure_temp_slot(emitter, temp);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(slot),
    )));
}

pub(super) fn compile_temp_let(
    emitter: &mut MethodEmitter<'_, '_>,
    temp: IrTempId,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    super::compile_expr(emitter, value, true, diags);
    let slot = ensure_temp_slot(emitter, temp);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(slot),
    )));
    emit_zero(emitter);
}

pub(super) fn compile_assign(
    emitter: &mut MethodEmitter<'_, '_>,
    target: &IrAssignTarget,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    match target {
        IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        } => {
            super::compile_expr(emitter, value, true, diags);
            if let Some(binding) = binding
                && let Some(slot) = emitter.locals.get(binding).copied()
            {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StLoc,
                    Operand::Local(slot),
                )));
                emit_zero(emitter);
                return;
            }
            if let Some(global) = resolve_global(emitter, *binding, name, module_target.as_ref()) {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StGlob,
                    Operand::Global(global),
                )));
                emit_zero(emitter);
                return;
            }
            push_expr_diag(
                diags,
                emitter.module_key,
                &value.origin,
                format!("unsupported emitted assignment target `{name}`"),
            );
            emit_zero(emitter);
        }
        IrAssignTarget::RecordField { base, index } => {
            super::compile_expr(emitter, base, true, diags);
            compile_i64(emitter, i64::from(*index));
            super::compile_expr(emitter, value, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::DataSet,
                Operand::None,
            )));
            emit_zero(emitter);
        }
        IrAssignTarget::Index { base, indices } => {
            super::compile_expr(emitter, base, true, diags);
            for index in indices {
                super::compile_expr(emitter, index, true, diags);
            }
            super::compile_expr(emitter, value, true, diags);
            let instruction = match indices.as_ref() {
                [_] => Instruction::new(Opcode::SeqSet, Operand::None),
                _ => Instruction::new(
                    Opcode::SeqSetN,
                    Operand::I16(i16::try_from(indices.len()).unwrap_or(i16::MAX)),
                ),
            };
            emitter.code.push(CodeEntry::Instruction(instruction));
            emit_zero(emitter);
        }
    }
}
