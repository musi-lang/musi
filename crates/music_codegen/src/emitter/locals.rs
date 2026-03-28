use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn placeholder_jump(&mut self, opcode: Opcode) -> usize {
        let pos = self.current_instructions.len();
        self.push(Instruction::with_i16(opcode, 0));
        pos
    }

    pub(super) fn patch_jump(&mut self, placeholder_pos: usize) {
        let current = self.current_instructions.len();
        let byte_offset: usize = self.current_instructions[placeholder_pos + 1..current]
            .iter()
            .map(instruction_byte_size)
            .sum();
        let offset = i16::try_from(byte_offset).expect("jump too far");
        let old_opcode = self.current_instructions[placeholder_pos].opcode;
        self.current_instructions[placeholder_pos] = Instruction::with_i16(old_opcode, offset);
    }

    pub(super) fn alloc_anon_slot(&mut self) -> u16 {
        let sym = self.alloc_anon_symbol();
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(sym);
        slot
    }

    pub(super) fn alloc_anon_symbol(&mut self) -> Symbol {
        let sym = Symbol::synthetic(self.next_anon);
        self.next_anon = self.next_anon.wrapping_sub(1);
        sym
    }

    pub(super) fn push(&mut self, instruction: Instruction) {
        self.current_instructions.push(instruction);
    }

    pub(super) fn push_absolute_global_load(&mut self, index: u16) {
        let position = self.current_instructions.len();
        self.current_absolute_global_loads.push(position);
        self.push(Instruction::with_u16(Opcode::LdGlob, index));
    }

    pub(super) fn local_slot(&mut self, name: Symbol) -> u16 {
        if let Some(slot) = self.find_local(name) {
            return slot;
        }
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(name);
        slot
    }

    pub(super) fn find_local(&self, name: Symbol) -> Option<u16> {
        self.current_locals
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u16::try_from(p).ok())
    }

    pub(super) fn emit_ld_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::LdLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::LdLocW, slot));
        }
    }

    pub(super) fn emit_st_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::StLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::StLocW, slot));
        }
    }

    pub(super) fn is_spread_expr(&self, expr_id: ExprId) -> bool {
        matches!(
            self.typed_module.db.ast.exprs.get(expr_id).kind,
            ExprKind::UnaryOp(UnaryOp::Spread, _)
        )
    }

    pub(super) fn spread_expr_operand(&self, expr_id: ExprId) -> Option<ExprId> {
        match self.typed_module.db.ast.exprs.get(expr_id).kind {
            ExprKind::UnaryOp(UnaryOp::Spread, inner) => Some(inner),
            _ => None,
        }
    }

    pub(super) fn is_len_access(&self, expr_id: ExprId, field: Symbol) -> bool {
        if self.typed_module.db.interner.resolve(field) != "len" {
            return false;
        }

        let Some(expr_ty) = self.typed_module.expr_type(expr_id) else {
            return true;
        };
        matches!(
            self.typed_module
                .type_env
                .types
                .get(self.typed_module.type_env.resolve_var(expr_ty)),
            Ty::Array(_) | Ty::Builtin(BuiltinType::String) | Ty::Any | Ty::Unknown | Ty::Var(_)
        )
    }

    pub(super) fn builtin_bool_variant_opcode(&self, tag: Symbol) -> Option<Opcode> {
        match self.typed_module.db.interner.resolve(tag) {
            "True" => Some(Opcode::LdTru),
            "False" => Some(Opcode::LdFls),
            _ => None,
        }
    }

    pub(super) fn push_arr_new(&mut self, type_id: u16, len: u16) {
        self.push(Instruction::with_type_len(Opcode::ArrNew, type_id, len));
    }

    pub(super) fn push_arr_new_tagged(&mut self, type_id: u16, tag: u8, len: u16) {
        self.push(Instruction::with_type_tagged(
            Opcode::ArrNewT,
            type_id,
            tag,
            len,
        ));
    }

    pub(super) fn resolve_field_index(&self, base_expr: ExprId, field_name: Symbol) -> Option<u8> {
        let ty_id = self.typed_module.expr_type(base_expr)?;
        let resolved = self.typed_module.type_env.resolve_var(ty_id);
        let ty = self.typed_module.type_env.types.get(resolved);
        if let Ty::Record { fields } = ty {
            for (i, &(name, _)) in fields.iter().enumerate() {
                if name == field_name {
                    return u8::try_from(i).ok();
                }
            }
        }
        None
    }

    pub(super) fn find_upvalue(&self, name: Symbol) -> Option<u8> {
        self.current_upvalues
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u8::try_from(p).ok())
    }

    pub(super) fn find_global(&self, name: Symbol) -> Option<u16> {
        self.globals
            .iter()
            .position(|g| g.name == name)
            .and_then(|p| u16::try_from(p).ok())
    }
}
