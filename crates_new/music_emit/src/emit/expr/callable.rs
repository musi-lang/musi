use super::super::*;
use crate::EmitDiagKind;

use super::literals::compile_i64;
use super::names::{resolve_foreign, resolve_method};
use super::sequence::compile_seq_parts_any;
use super::support::push_expr_diag;

pub(super) fn compile_variant_new(
    emitter: &mut MethodEmitter<'_, '_>,
    data_key: &DefinitionKey,
    tag_index: u16,
    field_count: u16,
    args: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        super::compile_expr(emitter, arg, true, diags);
    }
    compile_i64(emitter, i64::from(tag_index));

    let ty_name = qualified_name(&data_key.module, &data_key.name);
    let Some(ty) = emitter.layout.types.get(ty_name.as_ref()).copied() else {
        let origin = args.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |expr| expr.origin,
        );
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            &EmitDiagKind::UnknownDataType(ty_name),
        );
        emit_zero(emitter);
        return;
    };

    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataNew,
        Operand::TypeLen {
            ty,
            len: field_count,
        },
    )));
}

pub(super) fn compile_call(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrExpr,
    args: &[IrArg],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        if arg.spread {
            push_expr_diag(
                diags,
                emitter.module_key,
                &arg.expr.origin,
                &EmitDiagKind::SpreadCallArgsNotEmitted,
            );
        }
        super::compile_expr(emitter, &arg.expr, true, diags);
    }
    if let IrExprKind::Name {
        binding,
        name,
        module_target,
        ..
    } = &callee.kind
    {
        if let Some(binding) = binding
            && let Some(slot) = emitter.locals.get(binding).copied()
        {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CallCls,
                Operand::None,
            )));
            return;
        }
        if let Some(method) = resolve_method(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Call,
                Operand::Method(method),
            )));
            return;
        }
        if let Some(foreign) = resolve_foreign(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::FfiCall,
                Operand::Foreign(foreign),
            )));
            return;
        }
    }

    super::compile_expr(emitter, callee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CallCls,
        Operand::None,
    )));
}

pub(super) fn compile_call_seq(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrExpr,
    args: &[IrSeqPart],
    diags: &mut EmitDiagList,
) {
    compile_seq_parts_any(emitter, args, diags);
    if let IrExprKind::Name {
        binding,
        name,
        module_target,
        ..
    } = &callee.kind
    {
        if let Some(binding) = binding
            && let Some(slot) = emitter.locals.get(binding).copied()
        {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CallClsSeq,
                Operand::None,
            )));
            return;
        }
        if let Some(method) = resolve_method(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CallSeq,
                Operand::Method(method),
            )));
            return;
        }
        if let Some(foreign) = resolve_foreign(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::FfiCallSeq,
                Operand::Foreign(foreign),
            )));
            return;
        }
    }

    super::compile_expr(emitter, callee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CallClsSeq,
        Operand::None,
    )));
}

pub(super) fn compile_closure_new(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrNameRef,
    captures: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for cap in captures {
        super::compile_expr(emitter, cap, true, diags);
    }
    let Some(method) = resolve_method(
        emitter,
        callee.binding,
        callee.name.as_ref(),
        callee.module_target.as_ref(),
    ) else {
        let origin = captures.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |expr| expr.origin,
        );
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            &EmitDiagKind::UnknownClosureTarget(callee.name.clone()),
        );
        emit_zero(emitter);
        return;
    };
    let captures = u8::try_from(captures.len()).unwrap_or(u8::MAX);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::ClsNew,
        Operand::WideMethodCaptures { method, captures },
    )));
}
