use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn emit_perform(&mut self, expr_id: ExprId, operand: ExprId) -> EmitResult {
        match self.typed_module.db.ast.exprs.get(operand).kind.clone() {
            ExprKind::App(_, args) => {
                if args.is_empty() {
                    self.push(Instruction::simple(Opcode::LdUnit));
                } else if args.len() == 1 {
                    self.emit_expr(args[0])?;
                } else {
                    self.push_arr_new(
                        BuiltinType::Array.type_id(),
                        u16::try_from(args.len()).expect("too many effect arguments (>65535)"),
                    );
                    for (i, arg) in args.iter().enumerate() {
                        self.emit_expr(*arg)?;
                        self.push(Instruction::with_u8(
                            Opcode::ArrSetI,
                            u8::try_from(i).expect("too many effect arguments (>255)"),
                        ));
                    }
                }
            }
            _ => self.emit_expr(operand)?,
        }
        let effect_use = self
            .typed_module
            .type_env
            .perform_effects
            .get(&expr_id)
            .copied()
            .expect("missing effect metadata for perform expression");
        self.push(Instruction::with_effect(
            Opcode::EffInvk,
            effect_use.effect_id,
            effect_use.op_id,
        ));
        Ok(())
    }

    pub(super) fn emit_handle(
        &mut self,
        expr_id: ExprId,
        clauses: &[HandlerClause],
        body: ExprId,
    ) -> EmitResult {
        let effect_id = self
            .typed_module
            .type_env
            .handle_effects
            .get(&expr_id)
            .copied()
            .expect("missing effect metadata for handle expression");

        for clause in clauses {
            let HandlerClause::Op {
                name,
                args,
                cont,
                body: handler_body,
            } = clause
            else {
                continue;
            };
            let op_name = name.name;
            let op_id = self
                .typed_module
                .type_env
                .effect_by_id(effect_id)
                .and_then(|effect| effect.operations.iter().find(|op| op.name == op_name))
                .map(|op| op.id)
                .expect("missing effect operation metadata for handler");
            let pos = self.current_instructions.len();
            self.push(Instruction::with_effect_jump(
                Opcode::HdlPush,
                effect_id,
                op_id,
                0,
            ));
            self.emit_handler_clause_prologue(effect_id, op_name, args, *cont);
            self.emit_expr(*handler_body)?;
            let current = self.current_instructions.len();
            let byte_offset: usize = self.current_instructions[pos + 1..current]
                .iter()
                .map(instruction_byte_size)
                .sum();
            let offset = i16::try_from(byte_offset).expect("jump too far");
            self.current_instructions[pos] =
                Instruction::with_effect_jump(Opcode::HdlPush, effect_id, op_id, offset);
        }

        let return_clause = clauses.iter().find_map(|clause| match clause {
            HandlerClause::Return { binder, body } => Some((*binder, *body)),
            HandlerClause::Op { .. } => None,
        });
        self.emit_expr(body)?;
        if let Some((binder, return_body)) = return_clause {
            let slot = self.local_slot(binder.name);
            self.emit_st_loc(slot);
            self.emit_expr(return_body)?;
        }
        for clause in clauses {
            if matches!(clause, HandlerClause::Op { .. }) {
                self.push(Instruction::simple(Opcode::HdlPop));
            }
        }
        Ok(())
    }

    pub(super) fn emit_handler_clause_prologue(
        &mut self,
        effect_id: u16,
        op_name: Symbol,
        args: &[Ident],
        cont: Ident,
    ) {
        let cont_slot = self.local_slot(cont.name);
        self.emit_st_loc(cont_slot);
        let Some(op_info) = self
            .typed_module
            .type_env
            .effect_by_id(effect_id)
            .and_then(|effect| effect.operations.iter().find(|op| op.name == op_name))
        else {
            return;
        };
        match self.effect_payload_arity(op_info.param_ty) {
            0 => self.push(Instruction::simple(Opcode::Pop)),
            1 => {
                if let Some(arg) = args.first() {
                    let arg_slot = self.local_slot(arg.name);
                    self.emit_st_loc(arg_slot);
                } else {
                    self.push(Instruction::simple(Opcode::Pop));
                }
            }
            _ => {
                let payload_sym = self.alloc_anon_symbol();
                let payload_slot = self.local_slot(payload_sym);
                self.emit_st_loc(payload_slot);
                for (idx, arg) in args.iter().enumerate() {
                    self.emit_ld_loc(payload_slot);
                    self.push(Instruction::with_u8(
                        Opcode::ArrGetI,
                        u8::try_from(idx).expect("too many handler payload arguments (>255)"),
                    ));
                    let arg_slot = self.local_slot(arg.name);
                    self.emit_st_loc(arg_slot);
                }
            }
        }
    }

    pub(super) fn effect_payload_arity(&self, param_ty: Option<SemaTypeId>) -> usize {
        let Some(param_ty) = param_ty else {
            return 0;
        };
        let resolved = self.typed_module.type_env.resolve_var(param_ty);
        match self.typed_module.type_env.types.get(resolved) {
            Ty::Unit => 0,
            Ty::Tuple(elems) => elems.len(),
            _ => 1,
        }
    }

    pub(super) fn emit_resume(&mut self, value: Option<ExprId>) -> EmitResult {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id)?;
            self.push(Instruction::with_u8(Opcode::EffCont, 1));
        } else {
            self.push(Instruction::with_u8(Opcode::EffCont, 0));
        }
        Ok(())
    }
}
