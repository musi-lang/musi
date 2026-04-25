use crate::api::EmitDiagList;
use crate::emit::ProcedureEmitter;
use music_ir::IrExpr;
use music_seam::{CodeEntry, Instruction, Label, Opcode, Operand};

impl ProcedureEmitter<'_, '_> {
    pub(in crate::emit::expr) fn compile_bool_and(
        &mut self,
        left: &IrExpr,
        right: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_bool_short_circuit(left, right, false, diags);
    }

    pub(in crate::emit::expr) fn compile_bool_or(
        &mut self,
        left: &IrExpr,
        right: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_bool_short_circuit(left, right, true, diags);
    }

    fn compile_bool_short_circuit(
        &mut self,
        left: &IrExpr,
        right: &IrExpr,
        invert_branch_condition: bool,
        diags: &mut EmitDiagList,
    ) {
        let slot = Self::reserve_temp_slot(self);
        let end_label = self.alloc_label();
        self.compile_expr(left, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
        if invert_branch_condition {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Not,
                Operand::None,
            )));
        }
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::BrFalse,
            Operand::Label(end_label),
        )));
        self.compile_expr(right, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
        self.code.push(CodeEntry::Label(Label { id: end_label }));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
    }
}
