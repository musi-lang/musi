use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_intrinsic_call(
        &mut self,
        symbol: &str,
        param_tys: &[Box<str>],
        result_ty: &str,
        args: &[IrArg],
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            self.compile_expr(&arg.expr, true, diags);
        }
        let Some(foreign) = self.intern_intrinsic_foreign(symbol, param_tys, result_ty) else {
            let fallback_origin = IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            };
            let origin = args
                .first()
                .map_or(&fallback_origin, |arg| &arg.expr.origin);
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownTypeNameForOp,
                DiagContext::new()
                    .with("type", symbol)
                    .with("operation", "intrinsic"),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallFfi,
            Operand::Foreign(foreign),
        )));
    }

    pub(super) fn intern_intrinsic_foreign(
        &mut self,
        symbol: &str,
        param_tys: &[Box<str>],
        result_ty: &str,
    ) -> Option<ForeignId> {
        let name = format!("{}::{symbol}", self.module_key.as_str());
        let name_id = self.artifact.intern_string(&name);
        let abi_id = self.artifact.intern_string("musi");
        let symbol_id = self.artifact.intern_string(symbol);
        let param_tys = param_tys
            .iter()
            .map(|ty| self.layout.types.get(ty.as_ref()).copied())
            .collect::<Option<Vec<_>>>()?
            .into_boxed_slice();
        let result_ty = self.layout.types.get(result_ty).copied()?;
        Some(self.artifact.foreigns.alloc(ForeignDescriptor::new(
            name_id, param_tys, result_ty, abi_id, symbol_id,
        )))
    }

    pub(super) fn compile_variant_new(
        &mut self,
        data_key: &DefinitionKey,
        tag_value: i64,
        field_count: u16,
        args: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            self.compile_expr(arg, true, diags);
        }
        self.compile_i64(tag_value);

        let ty_name = qualified_name(&data_key.module, &data_key.name);
        let Some(ty) = self.layout.types.get(ty_name.as_ref()).copied() else {
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
                EmitDiagKind::UnknownDataType,
                DiagContext::new().with("type", ty_name),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewObj,
            Operand::TypeLen {
                ty,
                len: field_count,
            },
        )));
    }

    pub(super) fn compile_call(
        &mut self,
        callee: &IrExpr,
        args: &[IrArg],
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            if arg.spread {
                super::support::push_expr_diag_with(
                    diags,
                    self.module_key,
                    &arg.expr.origin,
                    EmitDiagKind::SpreadCallArgsNotEmitted,
                    DiagContext::new(),
                );
            }
            self.compile_expr(&arg.expr, true, diags);
        }
        if let IrExprKind::Name {
            binding,
            name,
            import_record_target,
            ..
        } = &callee.kind
        {
            if let Some(binding) = binding
                && let Some(slot) = self.locals.get(binding).copied()
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdLoc,
                    Operand::Local(slot),
                )));
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallInd,
                    Operand::None,
                )));
                return;
            }
            if let Some(slot) = self.synthetic_locals.get(name).copied() {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdLoc,
                    Operand::Local(slot),
                )));
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallInd,
                    Operand::None,
                )));
                return;
            }
            if let Some(procedure) =
                self.resolve_procedure(*binding, name, import_record_target.as_ref())
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Call,
                    Operand::Procedure(procedure),
                )));
                return;
            }
            if let Some(foreign) =
                self.resolve_foreign(*binding, name, import_record_target.as_ref())
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallFfi,
                    Operand::Foreign(foreign),
                )));
                return;
            }
        }

        self.compile_expr(callee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallInd,
            Operand::None,
        )));
    }

    #[allow(dead_code)]
    pub(super) fn compile_call_parts(
        &mut self,
        callee: &IrExpr,
        args: &[IrSeqPart],
        diags: &mut EmitDiagList,
    ) {
        if args.iter().all(|arg| matches!(arg, IrSeqPart::Expr(_))) {
            let args = args
                .iter()
                .filter_map(|arg| match arg {
                    IrSeqPart::Expr(expr) => Some(IrArg::new(false, expr.clone())),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Vec<_>>();
            self.compile_call(callee, &args, diags);
            return;
        }
        if let IrExprKind::Name {
            binding,
            name,
            import_record_target,
            ..
        } = &callee.kind
            && let Some(procedure) =
                self.resolve_procedure(*binding, name, import_record_target.as_ref())
            && self.compile_seq_parts_as_stack(args, diags)
        {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Call,
                Operand::Procedure(procedure),
            )));
            return;
        }
        self.compile_seq_parts_any(args, diags);
        if let IrExprKind::Name {
            binding,
            name,
            import_record_target,
            ..
        } = &callee.kind
        {
            if let Some(binding) = binding
                && let Some(slot) = self.locals.get(binding).copied()
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdLoc,
                    Operand::Local(slot),
                )));
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallInd,
                    Operand::I16(0),
                )));
                return;
            }
            if let Some(slot) = self.synthetic_locals.get(name).copied() {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdLoc,
                    Operand::Local(slot),
                )));
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallInd,
                    Operand::I16(0),
                )));
                return;
            }
            if let Some(procedure) =
                self.resolve_procedure(*binding, name, import_record_target.as_ref())
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Call,
                    Operand::WideProcedureCaptures {
                        procedure,
                        captures: 0,
                    },
                )));
                return;
            }
            if let Some(foreign) =
                self.resolve_foreign(*binding, name, import_record_target.as_ref())
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdFfi,
                    Operand::Foreign(foreign),
                )));
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallInd,
                    Operand::I16(0),
                )));
                return;
            }
        }

        self.compile_expr(callee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallInd,
            Operand::I16(0),
        )));
    }

    fn compile_seq_parts_as_stack(&mut self, args: &[IrSeqPart], diags: &mut EmitDiagList) -> bool {
        if !args.iter().all(|arg| {
            matches!(arg, IrSeqPart::Expr(_))
                || matches!(
                    arg,
                    IrSeqPart::Spread(IrExpr {
                        kind: IrExprKind::Array { .. } | IrExprKind::Tuple { .. },
                        ..
                    })
                )
        }) {
            return false;
        }
        for arg in args {
            match arg {
                IrSeqPart::Expr(expr) => self.compile_expr(expr, true, diags),
                IrSeqPart::Spread(expr) => match &expr.kind {
                    IrExprKind::Array { items, .. } | IrExprKind::Tuple { items, .. } => {
                        for item in items {
                            self.compile_expr(item, true, diags);
                        }
                    }
                    _ => return false,
                },
            }
        }
        true
    }

    pub(super) fn compile_closure_new(
        &mut self,
        callee: &IrNameRef,
        captures: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        for cap in captures {
            self.compile_expr(cap, true, diags);
        }
        let Some(procedure) = self.resolve_procedure(
            callee.binding,
            callee.name.as_ref(),
            callee.import_record_target.as_ref(),
        ) else {
            let origin = captures.first().map_or_else(
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
                EmitDiagKind::UnknownClosureTarget,
                DiagContext::new().with("target", &callee.name),
            );
            emit_zero(self);
            return;
        };
        let captures = u8::try_from(captures.len()).unwrap_or(u8::MAX);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewFn,
            Operand::WideProcedureCaptures {
                procedure,
                captures,
            },
        )));
    }
}
