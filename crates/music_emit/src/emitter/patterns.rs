use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn emit_case(&mut self, scrutinee: ExprId, arms: &[CaseArm]) -> EmitResult {
        self.emit_expr(scrutinee)?;

        if arms.is_empty() {
            return Ok(());
        }

        if self.try_emit_brtbl_case(arms)?.is_some() {
            return Ok(());
        }

        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i + 1 == arms.len();
            let pat_node = self.typed_module.db.ast.pats.get(arm.pat);

            match &pat_node.kind {
                PatKind::Variant { tag, fields } => {
                    let tag = *tag;
                    let fields = fields.clone();
                    self.emit_match_variant(&tag, &fields, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Bind(ident) => {
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps)?;
                }
                PatKind::Lit(literal) => {
                    let literal = literal.clone();
                    self.emit_match_lit(&literal, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Tuple(pats) | PatKind::Array(pats) => {
                    let pats = pats.clone();
                    self.emit_match_tuple_or_array(&pats, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Record(fields) => {
                    let fields = fields.clone();
                    self.emit_match_record(&fields, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Or(pats) => {
                    let pats = pats.clone();
                    self.emit_match_or(&pats, arm, is_last, &mut end_jumps)?;
                }
                PatKind::As { name, pat } => {
                    let name = *name;
                    let inner_pat = *pat;
                    self.emit_match_as(name, inner_pat, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Wildcard => {
                    self.push(Instruction::simple(Opcode::Pop));
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps)?;
                }
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }
        Ok(())
    }

    pub(super) fn try_emit_brtbl_case(&mut self, arms: &[CaseArm]) -> EmitResult<Option<()>> {
        let _ = arms;
        Ok(None)
    }

    pub(super) fn emit_guard_and_body(
        &mut self,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        let guard_jump = if let Some(guard) = arm.guard {
            self.emit_expr(guard)?;
            if is_last {
                None
            } else {
                Some(self.placeholder_jump(Opcode::BrFalse))
            }
        } else {
            None
        };

        self.emit_expr(arm.body)?;

        if !is_last {
            end_jumps.push(self.placeholder_jump(Opcode::BrJmp));
        }
        if let Some(gj) = guard_jump {
            self.patch_jump(gj);
        }
        Ok(())
    }

    pub(super) fn emit_match_lit(
        &mut self,
        literal: &Literal,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        self.emit_literal(literal);
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
        Ok(())
    }

    pub(super) fn emit_match_tuple_or_array(
        &mut self,
        pats: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        for (i, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.typed_module.db.ast.pats.get(sub_pat);
            if let PatKind::Bind(ident) = &sub_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let idx = u8::try_from(i).expect("too many tuple/array elements (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;
        Ok(())
    }

    pub(super) fn emit_match_record(
        &mut self,
        fields: &[RecordPatField],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        for (i, field) in fields.iter().enumerate() {
            let bind_ident = if let Some(pat_id) = field.pat {
                let pat_node = self.typed_module.db.ast.pats.get(pat_id);
                if let PatKind::Bind(ident) = &pat_node.kind {
                    Some(*ident)
                } else {
                    None
                }
            } else {
                Some(field.name)
            };

            if let Some(ident) = bind_ident {
                self.push(Instruction::simple(Opcode::Dup));
                let idx = u8::try_from(i).expect("too many record fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;
        Ok(())
    }

    pub(super) fn emit_match_variant(
        &mut self,
        tag: &Ident,
        fields: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        if let Some(opcode) = self.builtin_bool_variant_opcode(tag.name) {
            self.push(Instruction::simple(opcode));
        } else {
            let const_idx = self.pool.add(ConstantEntry::Str(
                self.typed_module.db.interner.resolve(tag.name).into(),
            ));
            self.push(Instruction::simple(Opcode::ArrTag));
            self.push(Instruction::with_u16(Opcode::LdConst, const_idx));
        }
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        for (fi, &field_pat) in fields.iter().enumerate() {
            let field_pat_node = self.typed_module.db.ast.pats.get(field_pat);
            if let PatKind::Bind(field_ident) = &field_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let field_u8 = u8::try_from(fi).expect("too many fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, field_u8));
                let slot = self.local_slot(field_ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
        Ok(())
    }

    pub(super) fn emit_match_or(
        &mut self,
        pats: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        let mut body_jumps = Vec::new();

        for (si, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.typed_module.db.ast.pats.get(sub_pat);
            let is_last_sub = si + 1 == pats.len();

            match &sub_pat_node.kind {
                PatKind::Lit(literal) => {
                    let literal = literal.clone();
                    self.push(Instruction::simple(Opcode::Dup));
                    self.emit_literal(&literal);
                    self.push(Instruction::simple(Opcode::CmpEq));
                    if is_last_sub {
                        if is_last {
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                            self.patch_jump(next_arm_jump);
                        }
                    } else {
                        body_jumps.push(self.placeholder_jump(Opcode::BrTrue));
                    }
                }
                PatKind::Variant { tag, .. } => {
                    let tag = *tag;
                    self.push(Instruction::simple(Opcode::Dup));
                    self.push(Instruction::simple(Opcode::ArrTag));
                    let tag_idx = self.pool.add(ConstantEntry::Str(
                        self.typed_module.db.interner.resolve(tag.name).into(),
                    ));
                    self.push(Instruction::with_u16(Opcode::LdConst, tag_idx));
                    self.push(Instruction::simple(Opcode::CmpEq));
                    if is_last_sub {
                        if is_last {
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                            self.patch_jump(next_arm_jump);
                        }
                    } else {
                        body_jumps.push(self.placeholder_jump(Opcode::BrTrue));
                    }
                }
                _ => {
                    if is_last_sub {
                        self.push(Instruction::simple(Opcode::Pop));
                        for bj in &body_jumps {
                            self.patch_jump(*bj);
                        }
                        self.emit_guard_and_body(arm, is_last, end_jumps)?;
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn emit_match_as(
        &mut self,
        name: Ident,
        inner_pat: PatId,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        let name_slot = self.local_slot(name.name);
        self.emit_st_loc(name_slot);

        let inner_node = self.typed_module.db.ast.pats.get(inner_pat);
        match &inner_node.kind {
            PatKind::Variant { tag, fields } => {
                let tag = *tag;
                let fields = fields.clone();
                self.emit_match_variant(&tag, &fields, arm, is_last, end_jumps)?;
            }
            PatKind::Lit(literal) => {
                let literal = literal.clone();
                self.emit_match_lit(&literal, arm, is_last, end_jumps)?;
            }
            PatKind::Bind(ident) => {
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
                self.emit_guard_and_body(arm, is_last, end_jumps)?;
            }
            _ => {
                self.push(Instruction::simple(Opcode::Pop));
                self.emit_guard_and_body(arm, is_last, end_jumps)?;
            }
        }
        Ok(())
    }
}
