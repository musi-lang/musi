use super::variant::{pattern_variantish, variant_dispatch_span};
use crate::api::EmitDiagList;
use crate::emit::{ProcedureEmitter, emit_zero};
use music_ir::{IrExpr, IrMatchArm};
use music_seam::{CodeEntry, Instruction, Label, Opcode, Operand};

impl ProcedureEmitter<'_, '_> {
    pub(in crate::emit::expr) fn compile_case(
        &mut self,
        scrutinee: &IrExpr,
        arms: &[IrMatchArm],
        diags: &mut EmitDiagList,
    ) {
        let scrutinee_slot = Self::reserve_temp_slot(self);
        let end_label = self.alloc_label();
        self.compile_expr(scrutinee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(scrutinee_slot),
        )));

        let (variant_start, variant_end) = variant_dispatch_span(arms);
        if let Some((variant_start, variant_end)) = variant_start.zip(variant_end)
            && variant_start <= variant_end
            && arms[variant_start..=variant_end]
                .iter()
                .all(|arm| pattern_variantish(&arm.pattern).is_some())
        {
            self.emit_match_arms(
                scrutinee_slot,
                &arms[0..variant_start],
                Some(end_label),
                diags,
            );

            self.compile_case_variant_dispatch(
                scrutinee_slot,
                &arms[variant_start..=variant_end],
                &arms[(variant_end + 1)..],
                end_label,
                diags,
            );
            self.code.push(CodeEntry::Label(Label { id: end_label }));
            return;
        }

        self.emit_match_arms(scrutinee_slot, arms, Some(end_label), diags);

        emit_zero(self);
        self.code.push(CodeEntry::Label(Label { id: end_label }));
    }
}

impl ProcedureEmitter<'_, '_> {
    pub(super) fn emit_match_arms(
        &mut self,
        scrutinee_slot: u16,
        arms: &[IrMatchArm],
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
    ) {
        for arm in arms {
            let next_label = self.alloc_label();
            self.emit_case_arm(
                next_label,
                arm.guard.as_ref(),
                &arm.expr,
                end_label,
                diags,
                |emitter, next_label, diags| {
                    emitter.compile_case_pattern(&arm.pattern, scrutinee_slot, next_label, diags)
                },
            );
        }
    }
}

impl ProcedureEmitter<'_, '_> {
    pub(super) fn emit_case_arm<F>(
        &mut self,
        next_label: u16,
        guard: Option<&IrExpr>,
        expr: &IrExpr,
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
        mut compile_pattern: F,
    ) where
        F: FnMut(&mut ProcedureEmitter<'_, '_>, u16, &mut EmitDiagList) -> bool,
    {
        if !compile_pattern(self, next_label, diags) {
            return;
        }
        self.emit_case_arm_body(next_label, guard, expr, end_label, diags);
    }
}

impl ProcedureEmitter<'_, '_> {
    fn emit_case_arm_body(
        &mut self,
        next_label: u16,
        guard: Option<&IrExpr>,
        expr: &IrExpr,
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
    ) {
        if let Some(guard) = guard {
            self.compile_expr(guard, true, diags);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
        }
        self.compile_expr(expr, true, diags);
        if let Some(end_label) = end_label {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(end_label),
            )));
        }
        self.code.push(CodeEntry::Label(Label { id: next_label }));
    }
}
