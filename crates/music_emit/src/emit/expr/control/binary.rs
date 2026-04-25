use crate::api::EmitDiagList;
use crate::diag::EmitDiagKind;
use crate::emit::ProcedureEmitter;
use music_base::diag::DiagContext;
use music_ir::{IrBinaryOp, IrExpr};
use music_seam::{CodeEntry, Instruction, Opcode, Operand};

impl ProcedureEmitter<'_, '_> {
    pub(in crate::emit::expr) fn compile_binary(
        &mut self,
        op: &IrBinaryOp,
        left: &IrExpr,
        right: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(left, true, diags);
        self.compile_expr(right, true, diags);
        let opcode = match op {
            IrBinaryOp::IAdd | IrBinaryOp::FAdd => Opcode::Add,
            IrBinaryOp::ISub | IrBinaryOp::FSub => Opcode::Sub,
            IrBinaryOp::IMul | IrBinaryOp::FMul => Opcode::Mul,
            IrBinaryOp::IDiv | IrBinaryOp::FDiv => Opcode::DivS,
            IrBinaryOp::IRem | IrBinaryOp::FRem => Opcode::RemS,
            IrBinaryOp::StrCat => Opcode::CallInd,
            IrBinaryOp::Eq => Opcode::Ceq,
            IrBinaryOp::Ne => Opcode::Cne,
            IrBinaryOp::Lt => Opcode::CltS,
            IrBinaryOp::Gt => Opcode::CgtS,
            IrBinaryOp::Le => Opcode::CleS,
            IrBinaryOp::Ge => Opcode::CgeS,
            IrBinaryOp::LogicalXor | IrBinaryOp::BitsXor => Opcode::Xor,
            IrBinaryOp::BitsAnd => Opcode::And,
            IrBinaryOp::BitsOr => Opcode::Or,
            IrBinaryOp::Other(name) => {
                super::super::support::push_expr_diag_with(
                    diags,
                    self.module_key,
                    &left.origin,
                    EmitDiagKind::UnsupportedBinaryOperator,
                    DiagContext::new().with("operator", name),
                );
                Opcode::Ceq
            }
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            opcode,
            Operand::None,
        )));
    }
}
