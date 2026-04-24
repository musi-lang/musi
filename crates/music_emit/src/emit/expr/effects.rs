use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_perform(
        &mut self,
        effect_key: &DefinitionKey,
        op_index: u16,
        args: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            self.compile_expr(arg, true, diags);
        }
        let Some(effect) = self.layout.effects.get(effect_key).copied() else {
            let origin = args.first().map_or_else(
                || IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                |expr| expr.origin,
            );
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &origin,
                EmitDiagKind::UnknownEffect,
                DiagContext::new().with(
                    "effect",
                    format!("{}::{}", effect_key.module.as_str(), effect_key.name),
                ),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Raise,
            Operand::Effect {
                effect,
                op: op_index,
            },
        )));
    }

    pub(super) fn compile_handle(
        &mut self,
        effect_key: &DefinitionKey,
        answer: &IrExpr,
        body: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(answer, true, diags);

        let Some(effect) = self.layout.effects.get(effect_key).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &body.origin,
                EmitDiagKind::UnknownEffect,
                DiagContext::new().with(
                    "effect",
                    format!("{}::{}", effect_key.module.as_str(), effect_key.name),
                ),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::HdlPush,
            Operand::EffectId(effect),
        )));

        self.compile_expr(body, true, diags);

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::HdlPop,
            Operand::None,
        )));
    }

    pub(super) fn compile_answer_lit(
        &mut self,
        effect_key: &DefinitionKey,
        value: &IrExpr,
        ops: &[IrHandleOp],
        origin: &IrOrigin,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(value, true, diags);
        for op in ops {
            self.compile_expr(&op.closure, true, diags);
        }

        let answer_ty_name = answer_type_name(effect_key);
        let Some(answer_ty) = self.layout.types.get(answer_ty_name.as_ref()).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownHandlerType,
                DiagContext::new().with("type", answer_ty_name),
            );
            emit_zero(self);
            return;
        };
        let field_count = u16::try_from(ops.len().saturating_add(1)).unwrap_or(u16::MAX);
        self.compile_i64(0);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewObj,
            Operand::TypeLen {
                ty: answer_ty,
                len: field_count,
            },
        )));
    }

    pub(super) fn compile_resume(&mut self, expr: Option<&IrExpr>, diags: &mut EmitDiagList) {
        if let Some(expr) = expr {
            self.compile_expr(expr, true, diags);
        } else {
            emit_zero(self);
        }
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Resume,
            Operand::None,
        )));
    }

    pub(super) fn compile_perform_seq(
        &mut self,
        effect_key: &DefinitionKey,
        op_index: u16,
        args: &[IrSeqPart],
        diags: &mut EmitDiagList,
    ) {
        self.compile_seq_parts_any(args, diags);
        let Some(effect) = self.layout.effects.get(effect_key).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                EmitDiagKind::UnknownEffect,
                DiagContext::new().with(
                    "effect",
                    format!("{}::{}", effect_key.module.as_str(), effect_key.name),
                ),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Raise,
            Operand::Effect {
                effect,
                op: op_index | 0x8000,
            },
        )));
    }
}
