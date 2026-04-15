use super::super::*;
use crate::EmitDiagKind;
use music_ir::IrRangeKind;

use super::support::push_expr_diag;

impl MethodEmitter<'_, '_> {
    pub(super) fn compile_range(
        &mut self,
        ty_name: &str,
        kind: IrRangeKind,
        lower: &IrExpr,
        upper: &IrExpr,
        bounds_evidence: Option<&IrExpr>,
        diags: &mut EmitDiagList,
    ) {
        if let Some(bounds_evidence) = bounds_evidence {
            self.compile_expr(bounds_evidence, true, diags);
        }
        self.compile_expr(lower, true, diags);
        self.compile_expr(upper, true, diags);
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            push_expr_diag(
                diags,
                self.module_key,
                &lower.origin,
                EmitDiagKind::UnknownSequenceType,
                format!("unknown emitted sequence type `{ty_name}`"),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::RangeNew,
            Operand::TypeLen {
                ty,
                len: range_kind_flag(kind),
            },
        )));
    }

    pub(super) fn compile_range_contains(
        &mut self,
        value: &IrExpr,
        range: &IrExpr,
        evidence: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(evidence, true, diags);
        self.compile_expr(range, true, diags);
        self.compile_expr(value, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::RangeContains,
            Operand::None,
        )));
    }

    pub(super) fn compile_range_materialize(
        &mut self,
        range: &IrExpr,
        evidence: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(evidence, true, diags);
        self.compile_expr(range, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::RangeMaterialize,
            Operand::None,
        )));
    }

    pub(super) fn compile_sequence(&mut self, exprs: &[IrExpr], diags: &mut EmitDiagList) {
        for (index, expr) in exprs.iter().enumerate() {
            let keep_result = index + 1 == exprs.len();
            self.compile_expr(expr, keep_result, diags);
        }
    }

    pub(super) fn compile_sequence_literal(
        &mut self,
        ty_name: &str,
        items: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        for item in items {
            self.compile_expr(item, true, diags);
        }
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            let missing_origin = items.first().map_or_else(
                || IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                |expr| expr.origin,
            );
            push_expr_diag(
                diags,
                self.module_key,
                &missing_origin,
                EmitDiagKind::UnknownSequenceType,
                format!("unknown emitted sequence type `{ty_name}`"),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::SeqNew,
            Operand::TypeLen {
                ty,
                len: u16::try_from(items.len()).unwrap_or(u16::MAX),
            },
        )));
    }

    pub(super) fn compile_array_cat(
        &mut self,
        ty_name: &str,
        parts: &[IrSeqPart],
        diags: &mut EmitDiagList,
    ) {
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            push_expr_diag(
                diags,
                self.module_key,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                EmitDiagKind::UnknownSequenceType,
                format!("unknown emitted sequence type `{ty_name}`"),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::SeqNew,
            Operand::TypeLen { ty, len: 0 },
        )));
        for part in parts {
            match part {
                IrSeqPart::Expr(expr) => {
                    self.compile_expr(expr, true, diags);
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::SeqNew,
                        Operand::TypeLen { ty, len: 1 },
                    )));
                }
                IrSeqPart::Spread(expr) => {
                    self.compile_expr(expr, true, diags);
                }
            }
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::SeqCat,
                Operand::None,
            )));
        }
    }

    pub(super) fn compile_seq_parts_any(&mut self, parts: &[IrSeqPart], diags: &mut EmitDiagList) {
        // Runtime spread lowers via `IrSeqPart::Spread` and uses a sequence runtime contract.
        // The SEAM metadata type chosen here is an emission detail.
        let ty_name = "[]Any";
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            push_expr_diag(
                diags,
                self.module_key,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                EmitDiagKind::UnknownSequenceType,
                format!("unknown emitted sequence type `{ty_name}`"),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::SeqNew,
            Operand::TypeLen { ty, len: 0 },
        )));
        self.compile_seq_parts_any_append(ty, parts, diags);
    }

    fn compile_seq_parts_any_append(
        &mut self,
        ty: TypeId,
        parts: &[IrSeqPart],
        diags: &mut EmitDiagList,
    ) {
        for part in parts {
            match part {
                IrSeqPart::Expr(expr) => {
                    self.compile_expr(expr, true, diags);
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::SeqNew,
                        Operand::TypeLen { ty, len: 1 },
                    )));
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::SeqCat,
                        Operand::None,
                    )));
                }
                IrSeqPart::Spread(expr) => {
                    self.compile_expr(expr, true, diags);
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::SeqCat,
                        Operand::None,
                    )));
                }
            }
        }
    }

    pub(super) fn compile_index(
        &mut self,
        base: &IrExpr,
        indices: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(base, true, diags);
        for index in indices {
            self.compile_expr(index, true, diags);
        }
        let instruction = match indices {
            [_] => Instruction::new(Opcode::SeqGet, Operand::None),
            _ => Instruction::new(
                Opcode::SeqGetN,
                Operand::I16(i16::try_from(indices.len()).unwrap_or(i16::MAX)),
            ),
        };
        self.code.push(CodeEntry::Instruction(instruction));
    }
}

const fn range_kind_flag(kind: IrRangeKind) -> u16 {
    match kind {
        IrRangeKind::Open => 0,
        IrRangeKind::Closed => 1,
        IrRangeKind::From => 2,
        IrRangeKind::UpTo => 3,
        IrRangeKind::Thru => 4,
    }
}
