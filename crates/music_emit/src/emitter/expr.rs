use std::mem;

use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    #[expect(
        clippy::panic,
        reason = "Piecewise must be lowered to Branch before emission — reaching this arm is a compiler ICE"
    )]
    pub(super) fn emit_expr(&mut self, expr_id: ExprId) -> EmitResult {
        let kind = self.typed_module.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Lit(ref lit) => self.emit_literal(lit),
            ExprKind::Var(ref ident) => self.emit_var(ident),
            ExprKind::App(callee, ref args) => self.emit_app(callee, args)?,
            ExprKind::BinOp(op, lhs, rhs) => self.emit_binop(op, lhs, rhs, expr_id)?,
            ExprKind::UnaryOp(op, operand) => self.emit_unary(op, operand, expr_id)?,
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => self.emit_branch(cond, then_br, else_br)?,
            ExprKind::Let(ref binding) => self.emit_let(binding)?,
            ExprKind::Lambda {
                ref params, body, ..
            } => self.emit_lambda(expr_id, params, body)?,
            ExprKind::Seq(ref stmts) => self.emit_seq(stmts)?,
            ExprKind::Case(ref data) => self.emit_case(data.scrutinee, &data.arms)?,
            ExprKind::RecordLit(ref fields) => self.emit_record_lit(expr_id, fields)?,
            ExprKind::VariantLit(ref tag, ref args) => self.emit_variant_lit(expr_id, tag, args)?,
            ExprKind::Access { expr, field, .. } => self.emit_access(expr, &field)?,
            ExprKind::Return(opt) => self.emit_return(opt)?,
            ExprKind::TupleLit(ref elems) => self.emit_tuple_lit(expr_id, elems)?,
            ExprKind::ArrayLit(ref elems) => self.emit_array_lit(expr_id, elems)?,
            ExprKind::Assign(target, value) => self.emit_assign(target, value)?,
            ExprKind::Index {
                expr, ref indices, ..
            } => self.emit_index(expr, indices)?,
            ExprKind::FStrLit(ref parts) => self.emit_fstr(parts)?,
            ExprKind::Perform(operand) => self.emit_perform(expr_id, operand)?,
            ExprKind::Handle(ref data) => self.emit_handle(expr_id, &data.clauses, data.body)?,
            ExprKind::Resume(opt) => self.emit_resume(opt)?,
            ExprKind::MatrixLit(ref rows) => self.emit_matrix_lit(expr_id, rows)?,
            ExprKind::RecordUpdate { base, ref fields } => self.emit_record_update(base, fields)?,
            ExprKind::Postfix { expr, op } => self.emit_postfix(expr, op)?,
            ExprKind::TypeOp { expr, ty, kind } => self.emit_type_op(expr, ty, kind)?,
            ExprKind::InstanceDef(ref inst) => self.emit_instance_def(expr_id, inst)?,
            ExprKind::ForeignImport(sym) => self.emit_foreign_import(sym),
            ExprKind::Comprehension(ref data) => {
                self.emit_comprehension(data.expr, &data.clauses)?;
            }
            ExprKind::Quote(ref qk) => self.emit_quote(qk)?,
            ExprKind::Splice(ref sk) => self.emit_splice(sk)?,
            ExprKind::Import { path, ref kind } => self.emit_import(path, kind),
            ExprKind::DataDef(_) | ExprKind::EffectDef(_) | ExprKind::ClassDef(_) => {
                self.push(Instruction::simple(Opcode::LdUnit));
            }
            ExprKind::Piecewise(_) => {
                panic!("Piecewise should be lowered to Branch before emission")
            }
        }
        Ok(())
    }

    pub(super) fn emit_literal(&mut self, lit: &Literal) {
        match *lit {
            Literal::Int(0) => self.push(Instruction::simple(Opcode::LdNil)),
            Literal::Int(1) => self.push(Instruction::simple(Opcode::LdOne)),
            Literal::Int(n) => {
                if let Ok(small) = i16::try_from(n) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(n));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
            }
            Literal::Float(f) => {
                let idx = self.pool.add(ConstantEntry::Float(f.to_bits()));
                self.push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            Literal::Str(ref s) => {
                let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                self.push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            Literal::Rune(c) => {
                let scalar = u32::from(c);
                if let Ok(small) = i16::try_from(scalar) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(i64::from(scalar)));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
            }
        }
    }

    #[expect(
        clippy::panic,
        reason = "unresolved variable after sema is a compiler ICE — sema guarantees all variables resolve"
    )]
    pub(super) fn emit_var(&mut self, ident: &Ident) {
        if let Some(slot) = self.find_local(ident.name) {
            self.emit_ld_loc(slot);
            if self.cell_locals.contains(&ident.name) {
                self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
            }
        } else if let Some(slot) = self.find_upvalue(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
            if self.cell_locals.contains(&ident.name) {
                self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
            }
        } else if let Some(idx) = self.find_global(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdGlob, idx));
        } else if let Some(idx) = self
            .imported_globals
            .get(self.typed_module.db.interner.resolve(ident.name))
        {
            self.push_absolute_global_load(*idx);
        } else {
            panic!(
                "unresolved variable: '{}'",
                self.typed_module.db.interner.resolve(ident.name)
            )
        }
    }

    pub(super) fn emit_app(&mut self, callee: ExprId, args: &ExprList) -> EmitResult {
        if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(callee) {
            for &arg in args {
                self.emit_expr(arg)?;
            }
            self.push(Instruction::simple(*opcode));
            return Ok(());
        }

        if let Some(foreign_idx) = self.resolve_foreign_callee(callee) {
            for &arg in args {
                self.emit_expr(arg)?;
            }
            let pin_count = self.emit_ffi_pins(foreign_idx);
            self.push(Instruction::with_u16(Opcode::FfiCall, foreign_idx));
            for _ in 0..pin_count {
                self.push(Instruction::simple(Opcode::GcUnpin));
            }
            return Ok(());
        }

        self.emit_expr(callee)?;
        for &arg in args {
            self.emit_expr(arg)?;
        }
        let arity = u8::try_from(args.len()).expect("too many arguments (>255)");
        self.push(Instruction::with_u8(Opcode::Call, arity));
        Ok(())
    }

    pub(super) fn emit_binop(
        &mut self,
        op: BinOp,
        lhs: ExprId,
        rhs: ExprId,
        expr_id: ExprId,
    ) -> EmitResult {
        if matches!(op, BinOp::Range | BinOp::RangeExcl) {
            return self.emit_range(op, lhs, rhs);
        }
        if op == BinOp::NilCoalesce {
            return self.emit_nil_coalesce(lhs, rhs);
        }

        if let Some(dispatch) = self.typed_module.dispatch(expr_id) {
            match dispatch {
                DispatchInfo::Static { opcode } => {
                    self.emit_expr(lhs)?;
                    self.emit_expr(rhs)?;
                    self.push(Instruction::simple(*opcode));
                    return Ok(());
                }
                DispatchInfo::Dictionary { class, method_idx } => {
                    let method_u8 =
                        u8::try_from(*method_idx).expect("method index overflow (>255)");
                    let class_id = self.typed_module.class_id(*class).unwrap_or(0);
                    let lhs_type_id = self.expr_runtime_type_id(lhs, format::BUILTIN_TYPE_ANY);
                    self.emit_expr(lhs)?;
                    self.emit_runtime_type_id(lhs_type_id);
                    self.push(Instruction::with_u16(Opcode::TyclDict, class_id));
                    self.push(Instruction::with_u8(Opcode::TyclCall, method_u8));
                    self.push(Instruction::simple(Opcode::Swap));
                    self.emit_expr(rhs)?;
                    self.push(Instruction::with_u8(Opcode::Call, 2));
                    return Ok(());
                }
                DispatchInfo::Dynamic => {
                    self.emit_expr(lhs)?;
                    self.emit_expr(rhs)?;
                    return self.emit_dynamic_binop(op);
                }
            }
        }

        self.emit_expr(lhs)?;
        self.emit_expr(rhs)?;
        self.push(Instruction::simple(binop_to_opcode(op)));
        Ok(())
    }

    pub(super) fn emit_dynamic_binop(&mut self, op: BinOp) -> EmitResult {
        let Some((int_opcode, float_opcode)) = dynamic_binop_opcodes(op) else {
            return Err(EmitError::Unimplemented(
                "dynamic dispatch for this operator",
            ));
        };

        self.push(Instruction::simple(Opcode::Swap));
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::TyTag));
        self.push(Instruction::with_i16(
            Opcode::LdSmi,
            i16::from(format::NAN_BOX_SMI),
        ));
        self.push(Instruction::simple(Opcode::CmpEq));
        let int_path_jump = self.placeholder_jump(Opcode::BrTrue);

        self.push(Instruction::simple(Opcode::Swap));
        self.push(Instruction::simple(float_opcode));
        let end_jump = self.placeholder_jump(Opcode::BrJmp);

        self.patch_jump(int_path_jump);
        self.push(Instruction::simple(Opcode::Swap));
        self.push(Instruction::simple(int_opcode));

        self.patch_jump(end_jump);
        Ok(())
    }

    pub(super) fn emit_unary(
        &mut self,
        op: UnaryOp,
        operand: ExprId,
        expr_id: ExprId,
    ) -> EmitResult {
        self.emit_expr(operand)?;
        match op {
            UnaryOp::Neg => {
                if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(expr_id) {
                    self.push(Instruction::simple(*opcode));
                    return Ok(());
                }
                self.push(Instruction::simple(Opcode::INeg));
            }
            UnaryOp::Not => self.push(Instruction::simple(Opcode::Not)),
            UnaryOp::Mut | UnaryOp::Spread => {}
        }
        Ok(())
    }

    pub(super) fn emit_branch(
        &mut self,
        cond: ExprId,
        then_br: ExprId,
        else_br: ExprId,
    ) -> EmitResult {
        self.emit_expr(cond)?;
        let false_jump = self.placeholder_jump(Opcode::BrFalse);
        self.emit_expr(then_br)?;
        let end_jump = self.placeholder_jump(Opcode::BrJmp);
        self.patch_jump(false_jump);
        self.emit_expr(else_br)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    pub(super) fn emit_let(&mut self, binding: &LetBinding) -> EmitResult {
        let pat_node = self.typed_module.db.ast.pats.get(binding.pat);
        if let PatKind::Bind(ident) = &pat_node.kind {
            if let Some(value) = binding.value {
                let is_captured_mut = binding.modifiers.mutable
                    && self
                        .typed_module
                        .type_env
                        .captured_mutable_names
                        .contains(&ident.name);

                if is_captured_mut {
                    self.push_arr_new(format::INTERNAL_TYPE_CELL, 1);
                    self.emit_expr(value)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                    let _inserted = self.cell_locals.insert(ident.name);
                } else if let Some(ref sig) = binding.sig {
                    if sig.has_param_list {
                        let method_idx =
                            self.compile_function_method(Some(ident.name), &sig.params, value)?;
                        self.push(Instruction::with_wide(Opcode::ClsNew, method_idx, 0));
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    } else {
                        self.emit_expr(value)?;
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    }
                } else {
                    self.emit_expr(value)?;
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                }
            }
        }
        self.push(Instruction::simple(Opcode::LdUnit));
        Ok(())
    }

    pub(super) fn emit_lambda(
        &mut self,
        expr_id: ExprId,
        params: &[Param],
        body: ExprId,
    ) -> EmitResult {
        let captured = self
            .typed_module
            .resolution
            .captures
            .get(&expr_id)
            .cloned()
            .unwrap_or_default();

        for &cap in &captured {
            if let Some(slot) = self.find_local(cap) {
                self.emit_ld_loc(slot);
            } else if let Some(slot) = self.find_upvalue(cap) {
                self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
            } else if let Some(idx) = self.find_global(cap) {
                self.push(Instruction::with_u16(Opcode::LdGlob, idx));
            } else if let Some(idx) = self
                .imported_globals
                .get(self.typed_module.db.interner.resolve(cap))
            {
                self.push_absolute_global_load(*idx);
            }
        }

        let saved_instructions = mem::take(&mut self.current_instructions);
        let saved_absolute_global_loads = mem::take(&mut self.current_absolute_global_loads);
        let saved_locals = mem::take(&mut self.current_locals);
        let saved_upvalues = mem::take(&mut self.current_upvalues);
        let saved_cells = mem::take(&mut self.cell_locals);

        self.current_upvalues.clone_from(&captured);
        for &cap in &captured {
            if saved_cells.contains(&cap) {
                let _inserted = self.cell_locals.insert(cap);
            }
        }

        for param in params {
            let _slot = self.local_slot(param.name.name);
        }

        self.emit_expr(body)?;
        self.push(Instruction::simple(Opcode::Ret));

        let locals_count =
            u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        let method_idx = u16::try_from(self.methods.len()).expect("too many methods (>65535)");
        let upval_count = u8::try_from(captured.len()).expect("too many upvalues (>255)");
        let anon_name = self.alloc_anon_symbol();

        self.methods.push(MethodEntry {
            name: Some(anon_name),
            source_name: None,
            instructions: mem::replace(&mut self.current_instructions, saved_instructions),
            locals_count,
            absolute_global_loads: mem::replace(
                &mut self.current_absolute_global_loads,
                saved_absolute_global_loads,
            ),
        });
        self.current_locals = saved_locals;
        self.current_upvalues = saved_upvalues;
        self.cell_locals = saved_cells;

        self.push(Instruction::with_wide(
            Opcode::ClsNew,
            method_idx,
            upval_count,
        ));
        Ok(())
    }

    pub(super) fn emit_seq(&mut self, stmts: &ExprList) -> EmitResult {
        if stmts.is_empty() {
            self.push(Instruction::simple(Opcode::LdUnit));
            return Ok(());
        }
        for (i, &stmt) in stmts.iter().enumerate() {
            self.emit_expr(stmt)?;
            if i + 1 < stmts.len() {
                self.push(Instruction::simple(Opcode::Pop));
            }
        }
        Ok(())
    }

    pub(super) fn emit_record_lit(
        &mut self,
        expr_id: ExprId,
        fields: &[RecordField],
    ) -> EmitResult {
        let field_count = u16::try_from(fields.len()).expect("too many fields (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new(type_id, field_count);

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id)?;
                        self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));
                }
            }
        }
        Ok(())
    }

    pub(super) fn emit_variant_lit(
        &mut self,
        expr_id: ExprId,
        tag: &Ident,
        args: &ExprList,
    ) -> EmitResult {
        if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(expr_id) {
            self.push(Instruction::simple(*opcode));
            return Ok(());
        }

        let tag_idx = self.pool.add(ConstantEntry::Str(
            self.typed_module.db.interner.resolve(tag.name).to_owned(),
        ));
        let tag_byte = u8::try_from(tag_idx).expect("variant tag constant index overflow (>255)");

        let len = u16::try_from(args.len()).expect("too many variant args (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new_tagged(type_id, tag_byte, len);

        for (i, &arg) in args.iter().enumerate() {
            self.emit_expr(arg)?;
            let idx = u8::try_from(i).expect("too many variant args (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    pub(super) fn emit_access(&mut self, expr: ExprId, field: &FieldTarget) -> EmitResult {
        self.emit_expr(expr)?;
        match *field {
            FieldTarget::Index(idx) => {
                let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, field_u8));
            }
            FieldTarget::Name(ref ident) => {
                if let Some(idx) = self.resolve_field_index(expr, ident.name) {
                    self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                } else if self.is_len_access(expr, ident.name) {
                    self.push(Instruction::simple(Opcode::ArrLen));
                } else {
                    return Err(EmitError::Unimplemented("named field access on non-record"));
                }
            }
        }
        Ok(())
    }

    pub(super) fn emit_return(&mut self, value: Option<ExprId>) -> EmitResult {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id)?;
        } else {
            self.push(Instruction::simple(Opcode::LdUnit));
        }
        self.push(Instruction::simple(Opcode::Ret));
        Ok(())
    }

    pub(super) fn emit_tuple_lit(&mut self, expr_id: ExprId, elems: &ExprList) -> EmitResult {
        let len = u16::try_from(elems.len()).expect("too many tuple elements (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new(type_id, len);
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem)?;
            let idx = u8::try_from(i).expect("too many tuple elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    pub(super) fn emit_array_lit(&mut self, expr_id: ExprId, elems: &ExprList) -> EmitResult {
        let type_id = self.expr_runtime_type_id(expr_id, BuiltinType::Array.type_id());
        if elems.iter().any(|&elem| self.is_spread_expr(elem)) {
            self.push_arr_new(type_id, 0);
            for &elem in elems {
                if let Some(inner) = self.spread_expr_operand(elem) {
                    self.emit_expr(inner)?;
                } else {
                    self.push_arr_new(type_id, 1);
                    self.emit_expr(elem)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                }
                self.push(Instruction::simple(Opcode::ArrCaten));
            }
            return Ok(());
        }

        let len = u16::try_from(elems.len()).expect("too many array elements (>65535)");
        self.push_arr_new(type_id, len);
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem)?;
            let idx = u8::try_from(i).expect("too many array elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    pub(super) fn emit_index(&mut self, expr: ExprId, indices: &ExprList) -> EmitResult {
        self.emit_expr(expr)?;
        for &index in indices {
            self.emit_expr(index)?;
            self.push(Instruction::simple(Opcode::ArrGet));
        }
        Ok(())
    }

    pub(super) fn emit_fstr(&mut self, parts: &[FStrPart]) -> EmitResult {
        if parts.is_empty() {
            let idx = self.pool.add(ConstantEntry::Str(String::new()));
            self.push(Instruction::with_u16(Opcode::LdConst, idx));
            return Ok(());
        }

        if parts.len() == 1 {
            match &parts[0] {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id)?,
            }
            return Ok(());
        }

        for (i, part) in parts.iter().enumerate() {
            match part {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id)?,
            }
            if i > 0 {
                self.push(Instruction::simple(Opcode::ArrCaten));
            }
        }
        Ok(())
    }

    pub(super) fn emit_range(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId) -> EmitResult {
        let tag_str = if op == BinOp::RangeExcl {
            "RangeExcl"
        } else {
            "Range"
        };
        let tag_idx = self.pool.add(ConstantEntry::Str(tag_str.into()));
        let tag_byte = u8::try_from(tag_idx & 0xFF).expect("tag index overflow");
        self.push_arr_new_tagged(format::BUILTIN_TYPE_ANY, tag_byte, 2);

        self.emit_expr(lhs)?;
        self.push(Instruction::with_u8(Opcode::ArrSetI, 0));

        self.emit_expr(rhs)?;
        self.push(Instruction::with_u8(Opcode::ArrSetI, 1));
        Ok(())
    }

    pub(super) fn emit_matrix_lit(&mut self, expr_id: ExprId, rows: &[ExprList]) -> EmitResult {
        let row_count = u16::try_from(rows.len()).expect("too many matrix rows (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, BuiltinType::Array.type_id());
        self.push_arr_new(type_id, row_count);

        for (ri, row) in rows.iter().enumerate() {
            let col_count = u16::try_from(row.len()).expect("too many matrix columns (>65535)");
            self.push_arr_new(BuiltinType::Array.type_id(), col_count);

            for (ci, &elem) in row.iter().enumerate() {
                self.emit_expr(elem)?;
                let col_idx = u8::try_from(ci).expect("too many matrix columns (>255)");
                self.push(Instruction::with_u8(Opcode::ArrSetI, col_idx));
            }

            let row_idx = u8::try_from(ri).expect("too many matrix rows (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, row_idx));
        }
        Ok(())
    }

    pub(super) fn emit_record_update(
        &mut self,
        base: ExprId,
        fields: &[RecordField],
    ) -> EmitResult {
        self.emit_expr(base)?;
        self.push(Instruction::simple(Opcode::ArrCopy));

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id)?;
                        self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));
                }
            }
        }
        Ok(())
    }

    pub(super) fn emit_postfix(&mut self, expr: ExprId, op: PostfixOp) -> EmitResult {
        self.emit_expr(expr)?;
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdConst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let skip_jump = self.placeholder_jump(Opcode::BrFalse);

        match op {
            PostfixOp::Force => self.push(Instruction::simple(Opcode::Panic)),
            PostfixOp::Propagate => self.push(Instruction::simple(Opcode::Ret)),
        }

        self.patch_jump(skip_jump);
        self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
        Ok(())
    }

    pub(super) fn emit_type_op(
        &mut self,
        expr: ExprId,
        ty_id: TyId,
        kind: TypeOpKind,
    ) -> EmitResult {
        self.emit_expr(expr)?;
        let type_id = self.type_to_table_id(ty_id);
        match kind {
            TypeOpKind::Test(opt_ident) => {
                self.push(Instruction::with_u16(Opcode::TyChk, type_id));
                if let Some(ident) = opt_ident {
                    self.push(Instruction::simple(Opcode::Dup));
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                }
            }
            TypeOpKind::Cast => {
                self.push(Instruction::with_u16(Opcode::TyCast, type_id));
            }
        }
        Ok(())
    }

    pub(super) fn emit_nil_coalesce(&mut self, lhs: ExprId, rhs: ExprId) -> EmitResult {
        self.emit_expr(lhs)?;
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdConst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let end_jump = self.placeholder_jump(Opcode::BrFalse);

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_expr(rhs)?;

        self.patch_jump(end_jump);
        Ok(())
    }

    pub(super) fn emit_instance_def(&mut self, _expr_id: ExprId, inst: &InstanceDef) -> EmitResult {
        match &inst.body {
            InstanceBody::Methods(members) => {
                for member in members {
                    if let MemberDecl::Fn(decl) = member {
                        if let Some(body) = decl.body {
                            let name = match decl.name {
                                MemberName::Ident(ident) | MemberName::Op(ident, _) => ident.name,
                            };
                            let params = decl.params.as_deref().unwrap_or(&[]);
                            self.emit_function(name, params, body)?;
                        }
                    }
                }
            }
            InstanceBody::Via(_) => {}
        }
        Ok(())
    }

    pub(super) fn emit_comprehension(
        &mut self,
        body: ExprId,
        clauses: &[CompClause],
    ) -> EmitResult {
        self.push_arr_new(BuiltinType::Array.type_id(), 0);

        for (clause_idx, clause) in clauses.iter().enumerate() {
            match clause {
                CompClause::Generator { pat, iter } => {
                    let iter_expr = *iter;
                    let pat_id = *pat;

                    self.emit_expr(iter_expr)?;
                    let iter_slot = self.alloc_anon_slot();
                    self.emit_st_loc(iter_slot);

                    self.push(Instruction::simple(Opcode::LdNil));
                    let counter_slot = self.alloc_anon_slot();
                    self.emit_st_loc(counter_slot);

                    let loop_start = self.current_instructions.len();

                    self.emit_ld_loc(counter_slot);
                    self.emit_ld_loc(iter_slot);
                    self.push(Instruction::simple(Opcode::ArrLen));
                    self.push(Instruction::simple(Opcode::CmpGeq));
                    let end_jump = self.placeholder_jump(Opcode::BrTrue);

                    self.emit_ld_loc(iter_slot);
                    self.emit_ld_loc(counter_slot);
                    self.push(Instruction::simple(Opcode::ArrGet));

                    let pat_node = self.typed_module.db.ast.pats.get(pat_id);
                    if let PatKind::Bind(ident) = &pat_node.kind {
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    } else {
                        self.push(Instruction::simple(Opcode::Pop));
                    }

                    let mut filter_jumps = Vec::new();
                    for subsequent in &clauses[clause_idx + 1..] {
                        match subsequent {
                            CompClause::Filter(guard) => {
                                self.emit_expr(*guard)?;
                                filter_jumps.push(self.placeholder_jump(Opcode::BrFalse));
                            }
                            CompClause::Generator { .. } => break,
                        }
                    }

                    self.emit_expr(body)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));

                    for fj in filter_jumps {
                        self.patch_jump(fj);
                    }

                    self.emit_ld_loc(counter_slot);
                    self.push(Instruction::simple(Opcode::LdOne));
                    self.push(Instruction::simple(Opcode::IAdd));
                    self.emit_st_loc(counter_slot);

                    let brback_pos = self.current_instructions.len();
                    self.push(Instruction::with_i16(Opcode::BrBack, 0));
                    let back_bytes: usize = self.current_instructions[loop_start..=brback_pos]
                        .iter()
                        .map(instruction_byte_size)
                        .sum();
                    let back_i16 = -i16::try_from(back_bytes).expect("backward jump too far");
                    self.current_instructions[brback_pos] =
                        Instruction::with_i16(Opcode::BrBack, back_i16);

                    self.patch_jump(end_jump);
                }
                CompClause::Filter(_) => {}
            }
        }
        Ok(())
    }

    pub(super) fn emit_assign(&mut self, target: ExprId, value: ExprId) -> EmitResult {
        let target_kind = self.typed_module.db.ast.exprs.get(target).kind.clone();
        match target_kind {
            ExprKind::Var(ident) => {
                if self.cell_locals.contains(&ident.name) {
                    if let Some(slot) = self.find_local(ident.name) {
                        self.emit_ld_loc(slot);
                    } else if let Some(slot) = self.find_upvalue(ident.name) {
                        self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
                    }
                    self.emit_expr(value)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                } else {
                    self.emit_expr(value)?;
                    if let Some(slot) = self.find_local(ident.name) {
                        self.emit_st_loc(slot);
                    } else if let Some(slot) = self.find_upvalue(ident.name) {
                        self.push(Instruction::with_u16(Opcode::StUpv, u16::from(slot)));
                    } else if let Some(idx) = self.find_global(ident.name) {
                        self.push(Instruction::with_u16(Opcode::StGlob, idx));
                    }
                }
            }
            ExprKind::Index {
                expr, ref indices, ..
            } => {
                let indices = indices.clone();
                if let Some((last_idx, leading)) = indices.split_last() {
                    self.emit_expr(expr)?;
                    for &idx in leading {
                        self.emit_expr(idx)?;
                        self.push(Instruction::simple(Opcode::ArrGet));
                    }
                    self.emit_expr(*last_idx)?;
                    self.emit_expr(value)?;
                    self.push(Instruction::simple(Opcode::ArrSet));
                }
            }
            ExprKind::Access { expr, field, .. } => {
                self.emit_expr(expr)?;
                self.emit_expr(value)?;
                match field {
                    FieldTarget::Index(idx) => {
                        let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                        self.push(Instruction::with_u8(Opcode::ArrSetI, field_u8));
                    }
                    FieldTarget::Name(ref ident) => {
                        if let Some(idx) = self.resolve_field_index(expr, ident.name) {
                            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                        } else {
                            return Err(EmitError::Unimplemented(
                                "named field assignment on non-record",
                            ));
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn emit_quote(&mut self, qk: &QuoteKind) -> EmitResult {
        match qk {
            QuoteKind::Expr(e) => self.emit_expr(*e)?,
            QuoteKind::Block(stmts) => self.emit_seq(stmts)?,
        }
        Ok(())
    }

    pub(super) fn emit_splice(&mut self, sk: &SpliceKind) -> EmitResult {
        match sk {
            SpliceKind::Ident(_) => return Err(EmitError::Unimplemented("splice-by-name")),
            SpliceKind::Expr(e) => self.emit_expr(*e)?,
            SpliceKind::Array(es) => {
                let len = u16::try_from(es.len()).expect("too many splice elements (>65535)");
                self.push_arr_new(BuiltinType::Array.type_id(), len);
                for (i, &e) in es.iter().enumerate() {
                    self.emit_expr(e)?;
                    let idx = u8::try_from(i).expect("too many splice elements (>255)");
                    self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                }
            }
        }
        Ok(())
    }
}
