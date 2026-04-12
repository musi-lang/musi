use super::super::*;
use crate::EmitDiagKind;

impl MethodEmitter<'_, '_> {
    pub(super) fn compile_variant_new(
        &mut self,
        data_key: &DefinitionKey,
        tag_index: u16,
        field_count: u16,
        args: &[IrExpr],
        diags: &mut EmitDiagList,
    ) {
        for arg in args {
            self.compile_expr(arg, true, diags);
        }
        self.compile_i64(i64::from(tag_index));

        let ty_name = qualified_name(&data_key.module, &data_key.name);
        let Some(ty) = self.layout.types.get(ty_name.as_ref()).copied() else {
            let origin = args.first().map_or_else(
                || IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                |expr| expr.origin,
            );
            super::support::push_expr_diag(
                diags,
                self.module_key,
                &origin,
                EmitDiagKind::UnknownDataType,
                format!("unknown emitted data type `{ty_name}`"),
            );
            emit_zero(self);
            return;
        };

        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::DataNew,
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
                super::support::push_expr_diag(
                    diags,
                    self.module_key,
                    &arg.expr.origin,
                    EmitDiagKind::SpreadCallArgsNotEmitted,
                    "spread call args have no emitted form",
                );
            }
            self.compile_expr(&arg.expr, true, diags);
        }
        if let IrExprKind::Name {
            binding,
            name,
            module_target,
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
                    Opcode::CallCls,
                    Operand::None,
                )));
                return;
            }
            if let Some(method) = self.resolve_method(*binding, name, module_target.as_ref()) {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Call,
                    Operand::Method(method),
                )));
                return;
            }
            if let Some(foreign) = self.resolve_foreign(*binding, name, module_target.as_ref()) {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::FfiCall,
                    Operand::Foreign(foreign),
                )));
                return;
            }
        }

        self.compile_expr(callee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallCls,
            Operand::None,
        )));
    }

    pub(super) fn compile_call_seq(
        &mut self,
        callee: &IrExpr,
        args: &[IrSeqPart],
        diags: &mut EmitDiagList,
    ) {
        self.compile_seq_parts_any(args, diags);
        if let IrExprKind::Name {
            binding,
            name,
            module_target,
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
                    Opcode::CallClsSeq,
                    Operand::None,
                )));
                return;
            }
            if let Some(method) = self.resolve_method(*binding, name, module_target.as_ref()) {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::CallSeq,
                    Operand::Method(method),
                )));
                return;
            }
            if let Some(foreign) = self.resolve_foreign(*binding, name, module_target.as_ref()) {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::FfiCallSeq,
                    Operand::Foreign(foreign),
                )));
                return;
            }
        }

        self.compile_expr(callee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::CallClsSeq,
            Operand::None,
        )));
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
        let Some(method) = self.resolve_method(
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
            super::support::push_expr_diag(
                diags,
                self.module_key,
                &origin,
                EmitDiagKind::UnknownClosureTarget,
                format!("unknown emitted closure target `{}`", callee.name),
            );
            emit_zero(self);
            return;
        };
        let captures = u8::try_from(captures.len()).unwrap_or(u8::MAX);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::ClsNew,
            Operand::WideMethodCaptures { method, captures },
        )));
    }
}
