use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;
use music_ir::{IrRangeEndpoint, IrRangeKind};

use super::support::push_expr_diag_with;

const ANY_TY: &str = "Any";
const BOOL_TY: &str = "Bool";

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_range(
        &mut self,
        ty_name: &str,
        kind: IrRangeKind,
        lower: &IrExpr,
        upper: &IrExpr,
        bounds_evidence: Option<&IrExpr>,
        diags: &mut EmitDiagList,
    ) {
        let (symbol, args): (&str, Vec<&IrExpr>) = match (kind.lower, kind.upper) {
            (IrRangeEndpoint::Included, IrRangeEndpoint::Included) => {
                ("range.construct.closed", vec![lower, upper])
            }
            (IrRangeEndpoint::Included, IrRangeEndpoint::Excluded) => {
                ("range.construct.open", vec![lower, upper])
            }
            (IrRangeEndpoint::Excluded, IrRangeEndpoint::Included) => {
                ("range.construct.open_closed", vec![lower, upper])
            }
            (IrRangeEndpoint::Excluded, IrRangeEndpoint::Excluded) => {
                ("range.construct.open_open", vec![lower, upper])
            }
            (IrRangeEndpoint::Included | IrRangeEndpoint::Excluded, IrRangeEndpoint::Missing) => {
                let Some(bounds_evidence) = bounds_evidence else {
                    emit_zero(self);
                    return;
                };
                (
                    match kind.lower {
                        IrRangeEndpoint::Included => "range.construct.from",
                        IrRangeEndpoint::Excluded => "range.construct.from_exclusive",
                        IrRangeEndpoint::Missing => {
                            emit_zero(self);
                            return;
                        }
                    },
                    vec![bounds_evidence, lower],
                )
            }
            (IrRangeEndpoint::Missing, IrRangeEndpoint::Included | IrRangeEndpoint::Excluded) => {
                let Some(bounds_evidence) = bounds_evidence else {
                    emit_zero(self);
                    return;
                };
                (
                    match kind.upper {
                        IrRangeEndpoint::Included => "range.construct.thru",
                        IrRangeEndpoint::Excluded => "range.construct.up_to",
                        IrRangeEndpoint::Missing => {
                            emit_zero(self);
                            return;
                        }
                    },
                    vec![bounds_evidence, upper],
                )
            }
            (IrRangeEndpoint::Missing, IrRangeEndpoint::Missing) => {
                emit_zero(self);
                return;
            }
        };
        self.compile_range_intrinsic(symbol, &args, ty_name, &lower.origin, diags);
    }

    pub(super) fn compile_range_contains(
        &mut self,
        value: &IrExpr,
        range: &IrExpr,
        evidence: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_range_intrinsic(
            "range.contains",
            &[evidence, range, value],
            BOOL_TY,
            &value.origin,
            diags,
        );
    }

    pub(super) fn compile_range_materialize(
        &mut self,
        range: &IrExpr,
        evidence: &IrExpr,
        result_ty_name: &str,
        diags: &mut EmitDiagList,
    ) {
        self.compile_range_intrinsic(
            "range.materialize",
            &[evidence, range],
            result_ty_name,
            &range.origin,
            diags,
        );
    }

    fn compile_range_intrinsic(
        &mut self,
        symbol: &str,
        args: &[&IrExpr],
        result_ty: &str,
        origin: &IrOrigin,
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            self.compile_expr(arg, true, diags);
        }
        let param_tys = vec![Box::<str>::from(ANY_TY); args.len()];
        let Some(foreign) = self.intern_intrinsic_foreign(symbol, &param_tys, result_ty) else {
            push_expr_diag_with(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownSequenceType,
                DiagContext::new().with("type", symbol),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallFfi,
            Operand::Foreign(foreign),
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
            push_expr_diag_with(
                diags,
                self.module_key,
                &missing_origin,
                EmitDiagKind::UnknownSequenceType,
                DiagContext::new().with("type", ty_name),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewArr,
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
            push_expr_diag_with(
                diags,
                self.module_key,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                EmitDiagKind::UnknownSequenceType,
                DiagContext::new().with("type", ty_name),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewArr,
            Operand::TypeLen { ty, len: 0 },
        )));
        for part in parts {
            match part {
                IrSeqPart::Expr(expr) => {
                    self.compile_expr(expr, true, diags);
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::NewArr,
                        Operand::TypeLen { ty, len: 1 },
                    )));
                }
                IrSeqPart::Spread(expr) => {
                    self.compile_expr(expr, true, diags);
                }
            }
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Call,
                Operand::None,
            )));
        }
    }

    pub(super) fn compile_seq_parts_any(&mut self, parts: &[IrSeqPart], diags: &mut EmitDiagList) {
        // Runtime spread lowers via `IrSeqPart::Spread` and uses a sequence runtime contract.
        // The SEAM metadata type chosen here is an emission detail.
        let ty_name = "[]Any";
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            push_expr_diag_with(
                diags,
                self.module_key,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                EmitDiagKind::UnknownSequenceType,
                DiagContext::new().with("type", ty_name),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewArr,
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
                        Opcode::NewArr,
                        Operand::TypeLen { ty, len: 1 },
                    )));
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::Call,
                        Operand::None,
                    )));
                }
                IrSeqPart::Spread(expr) => {
                    self.compile_expr(expr, true, diags);
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::Call,
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
            [_] => Instruction::new(Opcode::LdElem, Operand::None),
            _ => Instruction::new(
                Opcode::LdElem,
                Operand::I16(i16::try_from(indices.len()).unwrap_or(i16::MAX)),
            ),
        };
        self.code.push(CodeEntry::Instruction(instruction));
    }
}
