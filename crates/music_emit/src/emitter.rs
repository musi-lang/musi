use std::mem;

use music_ast::common::{FnDecl, MemberDecl, MemberName, Param};
use music_ast::expr::{
    BinOp, CompClause, ExprKind, FStrPart, FieldTarget, InstanceBody, InstanceDef, LetBinding,
    MatchArm, PostfixOp, QuoteKind, RecordField, SpliceKind, TypeOpKind, UnaryOp,
};
use music_ast::pat::{PatKind, RecordPatField};
use music_ast::{ExprId, ExprList};
use music_found::{Ident, Literal, Symbol};
use music_hir::HirBundle;
use music_il::instruction::{Instruction, Operand};
use music_il::opcode::Opcode;
use music_resolve::def::Visibility;
use music_sema::env::DispatchInfo;

use crate::pool::{ConstantEntry, ConstantPool};

/// A compiled method ready for serialisation.
///
/// `name` is `None` for the implicit module-level main method.
pub struct MethodEntry {
    pub name: Option<Symbol>,
    pub instructions: Vec<Instruction>,
    pub locals_count: u16,
}

/// A module-level global binding.
pub struct GlobalEntry {
    pub name: Symbol,
    pub exported: bool,
    pub opaque: bool,
}

/// The fully-emitted module, ready for `.seam` serialisation.
pub struct SeamModule {
    pub constants: ConstantPool,
    pub methods: Vec<MethodEntry>,
    pub globals: Vec<GlobalEntry>,
}

/// Lowers THIR expressions to SEAM bytecode instructions.
struct Emitter<'thir> {
    thir: &'thir HirBundle,
    pool: ConstantPool,
    methods: Vec<MethodEntry>,
    globals: Vec<GlobalEntry>,
    current_instructions: Vec<Instruction>,
    current_locals: Vec<Symbol>,
    current_upvalues: Vec<Symbol>,
    next_anon: u32,
}

/// Entry point: lower a typed IR bundle into a [`SeamModule`].
#[must_use]
pub fn emit(thir: &HirBundle) -> SeamModule {
    let mut emitter = Emitter {
        thir,
        pool: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        current_instructions: Vec::new(),
        current_locals: Vec::new(),
        current_upvalues: Vec::new(),
        next_anon: u32::MAX,
    };
    emitter.emit_module();
    SeamModule {
        constants: emitter.pool,
        methods: emitter.methods,
        globals: emitter.globals,
    }
}

const fn instruction_byte_size(instr: &Instruction) -> usize {
    1 + match &instr.operand {
        Operand::None => 0,
        Operand::U8(_) => 1,
        Operand::I16(_) | Operand::U16(_) => 2,
        Operand::Wide(_, _) | Operand::Tagged(_, _) => 3,
        Operand::Table(offsets) => 2 + offsets.len() * 2,
    }
}

impl Emitter<'_> {
    fn emit_module(&mut self) {
        let root = self.thir.db.ast.root.clone();
        for expr_id in root {
            self.emit_top_level(expr_id);
        }
        self.finish_main();
    }

    fn emit_top_level(&mut self, expr_id: ExprId) {
        let kind = self.thir.db.ast.exprs.get(expr_id).kind.clone();
        if let ExprKind::Let(binding) = kind {
            self.emit_top_let(&binding);
        } else {
            self.emit_expr(expr_id);
        }
    }

    fn emit_top_let(&mut self, binding: &LetBinding) {
        let pat_node = self.thir.db.ast.pats.get(binding.pat);
        let PatKind::Bind(ident) = &pat_node.kind else {
            return;
        };
        let name = ident.name;

        if let Some(ref sig) = binding.sig {
            if let Some(body) = binding.value {
                self.emit_function(name, &sig.params, body);
            }
        } else if let Some(value) = binding.value {
            let global_idx = u16::try_from(self.globals.len()).expect("too many globals (>65535)");
            let vis = resolve_visibility(binding);
            self.globals.push(GlobalEntry {
                name,
                exported: vis == Visibility::Exported || vis == Visibility::Opaque,
                opaque: vis == Visibility::Opaque,
            });
            self.emit_expr(value);
            self.push(Instruction::with_u16(Opcode::StGlb, global_idx));
        }
    }

    fn emit_function(&mut self, name: Symbol, params: &[Param], body: ExprId) {
        let saved_instructions = mem::take(&mut self.current_instructions);
        let saved_locals = mem::take(&mut self.current_locals);

        for param in params {
            let _slot = self.local_slot(param.name.name);
        }

        self.emit_expr(body);
        self.push(Instruction::simple(Opcode::Ret));

        let locals_count =
            u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.methods.push(MethodEntry {
            name: Some(name),
            instructions: mem::replace(&mut self.current_instructions, saved_instructions),
            locals_count,
        });
        self.current_locals = saved_locals;
    }

    fn finish_main(&mut self) {
        if !self.current_instructions.is_empty() {
            self.push(Instruction::simple(Opcode::Halt));
            let locals_count =
                u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
            self.methods.push(MethodEntry {
                name: None,
                instructions: mem::take(&mut self.current_instructions),
                locals_count,
            });
        }
    }

    fn emit_expr(&mut self, expr_id: ExprId) {
        let kind = self.thir.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Lit(ref lit) => self.emit_literal(lit),
            ExprKind::Var(ref ident) => self.emit_var(ident),
            ExprKind::App(callee, ref args) => self.emit_app(callee, args),
            ExprKind::BinOp(op, lhs, rhs) => self.emit_binop(op, lhs, rhs, expr_id),
            ExprKind::UnaryOp(op, operand) => self.emit_unary(op, operand, expr_id),
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => self.emit_branch(cond, then_br, else_br),
            ExprKind::Let(ref binding) => self.emit_let(binding),
            ExprKind::Lambda {
                ref params, body, ..
            } => self.emit_lambda(expr_id, params, body),
            ExprKind::Seq(ref stmts) => self.emit_seq(stmts),
            ExprKind::Match(scrutinee, ref arms) => self.emit_match(scrutinee, arms),
            ExprKind::RecordLit(ref fields) => self.emit_record_lit(fields),
            ExprKind::VariantLit(ref tag, ref args) => self.emit_variant_lit(expr_id, tag, args),
            ExprKind::Access { expr, field, .. } => self.emit_access(expr, &field),
            ExprKind::Return(opt) => self.emit_return(opt),
            ExprKind::TupleLit(ref elems) => self.emit_tuple_lit(elems),
            ExprKind::ArrayLit(ref elems) => self.emit_array_lit(elems),
            ExprKind::Assign(target, value) => self.emit_assign(target, value),
            ExprKind::Index {
                expr, ref indices, ..
            } => self.emit_index(expr, indices),
            ExprKind::FStrLit(ref parts) => self.emit_fstr(parts),
            ExprKind::Need(operand) => self.emit_need(operand),
            ExprKind::Handle {
                ref handlers, body, ..
            } => self.emit_handle(handlers, body),
            ExprKind::Resume(opt) => self.emit_resume(opt),
            ExprKind::MatrixLit(ref rows) => self.emit_matrix_lit(rows),
            ExprKind::RecordUpdate { base, ref fields } => self.emit_record_update(base, fields),
            ExprKind::Postfix { expr, op } => self.emit_postfix(expr, op),
            ExprKind::TypeOp { expr, kind, .. } => self.emit_type_op(expr, kind),
            ExprKind::InstanceDef(ref inst) => self.emit_instance_def(inst),
            ExprKind::ForeignImport(sym) => self.emit_foreign_import(sym),
            ExprKind::Comprehension {
                expr: body,
                ref clauses,
            } => self.emit_comprehension(body, clauses),
            ExprKind::Quote(ref qk) => self.emit_quote(qk),
            ExprKind::Splice(ref sk) => self.emit_splice(sk),
            _ => self.push(Instruction::simple(Opcode::Nop)),
        }
    }

    fn emit_literal(&mut self, lit: &Literal) {
        match *lit {
            Literal::Int(0) => self.push(Instruction::simple(Opcode::LdZero)),
            Literal::Int(1) => self.push(Instruction::simple(Opcode::LdOne)),
            Literal::Int(n) => {
                if let Ok(small) = i16::try_from(n) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(n));
                    self.push(Instruction::with_u16(Opcode::LdCst, idx));
                }
            }
            Literal::Float(f) => {
                let idx = self.pool.add(ConstantEntry::Float(f.to_bits()));
                self.push(Instruction::with_u16(Opcode::LdCst, idx));
            }
            Literal::Str(ref s) => {
                let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                self.push(Instruction::with_u16(Opcode::LdCst, idx));
            }
            Literal::Rune(c) => {
                let scalar = u32::from(c);
                if let Ok(small) = i16::try_from(scalar) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(i64::from(scalar)));
                    self.push(Instruction::with_u16(Opcode::LdCst, idx));
                }
            }
        }
    }

    fn emit_var(&mut self, ident: &Ident) {
        if let Some(slot) = self.find_local(ident.name) {
            self.emit_ld_loc(slot);
        } else if let Some(slot) = self.find_upvalue(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
        } else if let Some(idx) = self.find_global(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdGlb, idx));
        } else {
            self.push(Instruction::simple(Opcode::Nop));
        }
    }

    fn emit_app(&mut self, callee: ExprId, args: &ExprList) {
        if let Some(DispatchInfo::Static { intrinsic }) = self.thir.dispatch(callee) {
            if let Some(opcode) = Opcode::from_mnemonic(intrinsic) {
                for &arg in args {
                    self.emit_expr(arg);
                }
                self.push(Instruction::simple(opcode));
                return;
            }
        }
        self.emit_expr(callee);
        for &arg in args {
            self.emit_expr(arg);
        }
        let arity = u8::try_from(args.len()).expect("too many arguments (>255)");
        self.push(Instruction::with_u8(Opcode::Call, arity));
    }

    fn emit_binop(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, expr_id: ExprId) {
        if matches!(op, BinOp::Range | BinOp::RangeExcl) {
            return self.emit_range(op, lhs, rhs);
        }
        if op == BinOp::NilCoalesce {
            return self.emit_nil_coalesce(lhs, rhs);
        }

        self.emit_expr(lhs);
        self.emit_expr(rhs);

        if let Some(dispatch) = self.thir.dispatch(expr_id) {
            match dispatch {
                DispatchInfo::Static { intrinsic } => {
                    if let Some(opcode) = Opcode::from_mnemonic(intrinsic) {
                        self.push(Instruction::simple(opcode));
                        return;
                    }
                }
                DispatchInfo::Dictionary { method_idx, .. } => {
                    let method_u8 =
                        u8::try_from(*method_idx).expect("method index overflow (>255)");
                    // ClsDict takes a u16 dictionary index; 0 is a placeholder until Phase 4
                    self.push(Instruction::with_u16(Opcode::ClsDict, 0u16));
                    self.push(Instruction::with_u8(Opcode::ClsCall, method_u8));
                    return;
                }
                DispatchInfo::Dynamic => {
                    self.push(Instruction::simple(Opcode::Nop));
                    return;
                }
            }
        }

        self.push(Instruction::simple(binop_to_opcode(op)));
    }

    fn emit_unary(&mut self, op: UnaryOp, operand: ExprId, expr_id: ExprId) {
        self.emit_expr(operand);
        match op {
            UnaryOp::Neg => {
                if let Some(DispatchInfo::Static { intrinsic }) = self.thir.dispatch(expr_id) {
                    if let Some(opcode) = Opcode::from_mnemonic(intrinsic) {
                        self.push(Instruction::simple(opcode));
                        return;
                    }
                }
                self.push(Instruction::simple(Opcode::INeg));
            }
            UnaryOp::Not => self.push(Instruction::simple(Opcode::Not)),
            UnaryOp::Mut | UnaryOp::Spread => {}
        }
    }

    fn emit_branch(&mut self, cond: ExprId, then_br: ExprId, else_br: ExprId) {
        self.emit_expr(cond);
        let false_jump = self.placeholder_jump(Opcode::BrFalse);
        self.emit_expr(then_br);
        let end_jump = self.placeholder_jump(Opcode::BrJmp);
        self.patch_jump(false_jump);
        self.emit_expr(else_br);
        self.patch_jump(end_jump);
    }

    fn placeholder_jump(&mut self, opcode: Opcode) -> usize {
        let pos = self.current_instructions.len();
        self.push(Instruction::with_i16(opcode, 0));
        pos
    }

    fn patch_jump(&mut self, placeholder_pos: usize) {
        let current = self.current_instructions.len();
        let byte_offset: usize = self.current_instructions[placeholder_pos + 1..current]
            .iter()
            .map(instruction_byte_size)
            .sum();
        let offset = i16::try_from(byte_offset).expect("jump too far");
        let old_opcode = self.current_instructions[placeholder_pos].opcode;
        self.current_instructions[placeholder_pos] = Instruction::with_i16(old_opcode, offset);
    }

    fn emit_let(&mut self, binding: &LetBinding) {
        let pat_node = self.thir.db.ast.pats.get(binding.pat);
        if let PatKind::Bind(ident) = &pat_node.kind {
            if let Some(value) = binding.value {
                self.emit_expr(value);
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }
    }

    fn emit_lambda(&mut self, expr_id: ExprId, params: &[Param], body: ExprId) {
        let captured = self
            .thir
            .resolution
            .captures
            .get(&expr_id)
            .cloned()
            .unwrap_or_default();

        // Before entering the lambda body, push captured values onto the stack
        // in the *outer* frame so ClsNew can close over them.
        for &cap in &captured {
            if let Some(slot) = self.find_local(cap) {
                self.emit_ld_loc(slot);
            } else if let Some(slot) = self.find_upvalue(cap) {
                self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
            } else if let Some(idx) = self.find_global(cap) {
                self.push(Instruction::with_u16(Opcode::LdGlb, idx));
            }
        }

        let saved_instructions = mem::take(&mut self.current_instructions);
        let saved_locals = mem::take(&mut self.current_locals);
        let saved_upvalues = mem::take(&mut self.current_upvalues);

        // Register captures as upvalue slots inside the lambda body
        self.current_upvalues.clone_from(&captured);

        for param in params {
            let _slot = self.local_slot(param.name.name);
        }

        self.emit_expr(body);
        self.push(Instruction::simple(Opcode::Ret));

        let locals_count =
            u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        let method_idx = u16::try_from(self.methods.len()).expect("too many methods (>65535)");
        let upval_count = u8::try_from(captured.len()).expect("too many upvalues (>255)");

        self.methods.push(MethodEntry {
            name: params.first().map(|p| p.name.name),
            instructions: mem::replace(&mut self.current_instructions, saved_instructions),
            locals_count,
        });
        self.current_locals = saved_locals;
        self.current_upvalues = saved_upvalues;

        self.push(Instruction::with_wide(
            Opcode::ClsNew,
            method_idx,
            upval_count,
        ));
    }

    fn emit_seq(&mut self, stmts: &ExprList) {
        if stmts.is_empty() {
            self.push(Instruction::simple(Opcode::LdUnit));
            return;
        }
        for (i, &stmt) in stmts.iter().enumerate() {
            self.emit_expr(stmt);
            if i + 1 < stmts.len() {
                self.push(Instruction::simple(Opcode::Pop));
            }
        }
    }

    fn emit_match(&mut self, scrutinee: ExprId, arms: &[MatchArm]) {
        self.emit_expr(scrutinee);

        if arms.is_empty() {
            return;
        }

        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i + 1 == arms.len();
            let pat_node = self.thir.db.ast.pats.get(arm.pat);

            match &pat_node.kind {
                PatKind::Variant { tag, fields } => {
                    let tag = *tag;
                    let fields = fields.clone();
                    self.emit_match_variant(&tag, &fields, arm, is_last, &mut end_jumps);
                }
                PatKind::Bind(ident) => {
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps);
                }
                PatKind::Lit(literal) => {
                    let literal = literal.clone();
                    self.emit_match_lit(&literal, arm, is_last, &mut end_jumps);
                }
                PatKind::Tuple(pats) | PatKind::Array(pats) => {
                    let pats = pats.clone();
                    self.emit_match_tuple_or_array(&pats, arm, is_last, &mut end_jumps);
                }
                PatKind::Record(fields) => {
                    let fields = fields.clone();
                    self.emit_match_record(&fields, arm, is_last, &mut end_jumps);
                }
                PatKind::Or(pats) => {
                    let pats = pats.clone();
                    self.emit_match_or(&pats, arm, is_last, &mut end_jumps);
                }
                PatKind::As { name, pat } => {
                    let name = *name;
                    let inner_pat = *pat;
                    self.emit_match_as(name, inner_pat, arm, is_last, &mut end_jumps);
                }
                PatKind::Wildcard => {
                    self.push(Instruction::simple(Opcode::Pop));
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps);
                }
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }
    }

    fn emit_guard_and_body(&mut self, arm: &MatchArm, is_last: bool, end_jumps: &mut Vec<usize>) {
        let guard_jump = arm.guard.and_then(|guard| {
            self.emit_expr(guard);
            if is_last {
                None
            } else {
                Some(self.placeholder_jump(Opcode::BrFalse))
            }
        });

        self.emit_expr(arm.body);

        if !is_last {
            end_jumps.push(self.placeholder_jump(Opcode::BrJmp));
        }
        if let Some(gj) = guard_jump {
            self.patch_jump(gj);
        }
    }

    fn emit_match_lit(
        &mut self,
        literal: &Literal,
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        self.push(Instruction::simple(Opcode::Dup));
        self.emit_literal(literal);
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps);

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
    }

    fn emit_match_tuple_or_array(
        &mut self,
        pats: &[music_ast::PatId],
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        for (i, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.thir.db.ast.pats.get(sub_pat);
            if let PatKind::Bind(ident) = &sub_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let idx = u8::try_from(i).expect("too many tuple/array elements (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGeti, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps);
    }

    fn emit_match_record(
        &mut self,
        fields: &[RecordPatField],
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        for (i, field) in fields.iter().enumerate() {
            let bind_ident = if let Some(pat_id) = field.pat {
                let pat_node = self.thir.db.ast.pats.get(pat_id);
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
                self.push(Instruction::with_u8(Opcode::ArrGeti, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps);
    }

    fn emit_match_variant(
        &mut self,
        tag: &Ident,
        fields: &[music_ast::PatId],
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let tag_idx = self.pool.add(ConstantEntry::Str(
            self.thir.db.interner.resolve(tag.name).into(),
        ));
        self.push(Instruction::with_u16(Opcode::LdCst, tag_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        for (fi, &field_pat) in fields.iter().enumerate() {
            let field_pat_node = self.thir.db.ast.pats.get(field_pat);
            if let PatKind::Bind(field_ident) = &field_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let field_u8 = u8::try_from(fi).expect("too many fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGeti, field_u8));
                let slot = self.local_slot(field_ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps);

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
    }

    fn emit_record_lit(&mut self, fields: &[RecordField]) {
        let field_count = u16::try_from(fields.len()).expect("too many fields (>65535)");
        self.push(Instruction::with_u16(Opcode::ArrNew, field_count));

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id);
                        self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id);
                    self.push(Instruction::simple(Opcode::ArrConcat));
                }
            }
        }
    }

    fn emit_variant_lit(&mut self, expr_id: ExprId, tag: &Ident, args: &ExprList) {
        if let Some(DispatchInfo::Static { intrinsic }) = self.thir.dispatch(expr_id) {
            if let Some(opcode) = Opcode::from_mnemonic(intrinsic) {
                self.push(Instruction::simple(opcode));
                return;
            }
        }
        let tag_name = self.thir.db.interner.resolve(tag.name);
        let tag_idx = self.pool.add(ConstantEntry::Str(tag_name.into()));
        let tag_byte = u8::try_from(tag_idx & 0xFF).expect("tag index overflow");
        let len = u16::try_from(args.len()).expect("too many variant args (>65535)");
        self.push(Instruction::with_tagged(Opcode::ArrNewt, tag_byte, len));

        for (i, &arg) in args.iter().enumerate() {
            self.emit_expr(arg);
            let idx = u8::try_from(i).expect("too many variant args (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
        }
    }

    fn emit_access(&mut self, expr: ExprId, field: &FieldTarget) {
        self.emit_expr(expr);
        match *field {
            FieldTarget::Index(idx) => {
                let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGeti, field_u8));
            }
            FieldTarget::Name(_) => {
                self.push(Instruction::with_u8(Opcode::ArrGeti, 0));
            }
        }
    }

    fn emit_return(&mut self, value: Option<ExprId>) {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id);
        } else {
            self.push(Instruction::simple(Opcode::LdUnit));
        }
        self.push(Instruction::simple(Opcode::Ret));
    }

    fn emit_tuple_lit(&mut self, elems: &ExprList) {
        let len = u16::try_from(elems.len()).expect("too many tuple elements (>65535)");
        self.push(Instruction::with_u16(Opcode::ArrNew, len));
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem);
            let idx = u8::try_from(i).expect("too many tuple elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
        }
    }

    fn emit_array_lit(&mut self, elems: &ExprList) {
        let len = u16::try_from(elems.len()).expect("too many array elements (>65535)");
        self.push(Instruction::with_u16(Opcode::ArrNew, len));
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem);
            let idx = u8::try_from(i).expect("too many array elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
        }
    }

    fn emit_index(&mut self, expr: ExprId, indices: &ExprList) {
        self.emit_expr(expr);
        for &index in indices {
            self.emit_expr(index);
            self.push(Instruction::simple(Opcode::ArrGet));
        }
    }

    fn emit_fstr(&mut self, parts: &[FStrPart]) {
        if parts.is_empty() {
            let idx = self.pool.add(ConstantEntry::Str(String::new()));
            self.push(Instruction::with_u16(Opcode::LdCst, idx));
            return;
        }

        if parts.len() == 1 {
            match &parts[0] {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdCst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id),
            }
            return;
        }

        for (i, part) in parts.iter().enumerate() {
            match part {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdCst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id),
            }
            if i > 0 {
                self.push(Instruction::simple(Opcode::ArrConcat));
            }
        }
    }

    fn emit_range(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId) {
        let tag_str = if op == BinOp::RangeExcl {
            "RangeExcl"
        } else {
            "Range"
        };
        let tag_idx = self.pool.add(ConstantEntry::Str(tag_str.into()));
        let tag_byte = u8::try_from(tag_idx & 0xFF).expect("tag index overflow");
        self.push(Instruction::with_tagged(Opcode::ArrNewt, tag_byte, 2));

        self.emit_expr(lhs);
        self.push(Instruction::with_u8(Opcode::ArrSeti, 0));

        self.emit_expr(rhs);
        self.push(Instruction::with_u8(Opcode::ArrSeti, 1));
    }

    fn emit_need(&mut self, operand: ExprId) {
        self.emit_expr(operand);
        self.push(Instruction::with_u16(Opcode::EffNeed, 0));
    }

    fn emit_handle(&mut self, handlers: &[FnDecl], body: ExprId) {
        let skip_jump = self.placeholder_jump(Opcode::EffPush);

        for handler in handlers {
            if let Some(handler_body) = handler.body {
                self.emit_expr(handler_body);
            }
        }

        self.patch_jump(skip_jump);
        self.emit_expr(body);
        self.push(Instruction::simple(Opcode::EffPop));
    }

    fn emit_resume(&mut self, value: Option<ExprId>) {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id);
            self.push(Instruction::with_u8(Opcode::EffResume, 1));
        } else {
            self.push(Instruction::with_u8(Opcode::EffResume, 0));
        }
    }

    fn emit_matrix_lit(&mut self, rows: &[ExprList]) {
        let row_count = u16::try_from(rows.len()).expect("too many matrix rows (>65535)");
        self.push(Instruction::with_u16(Opcode::ArrNew, row_count));

        for (ri, row) in rows.iter().enumerate() {
            let col_count = u16::try_from(row.len()).expect("too many matrix columns (>65535)");
            self.push(Instruction::with_u16(Opcode::ArrNew, col_count));

            for (ci, &elem) in row.iter().enumerate() {
                self.emit_expr(elem);
                let col_idx = u8::try_from(ci).expect("too many matrix columns (>255)");
                self.push(Instruction::with_u8(Opcode::ArrSeti, col_idx));
            }

            let row_idx = u8::try_from(ri).expect("too many matrix rows (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSeti, row_idx));
        }
    }

    fn emit_record_update(&mut self, base: ExprId, fields: &[RecordField]) {
        self.emit_expr(base);
        self.push(Instruction::simple(Opcode::ArrCopy));

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id);
                        self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id);
                    self.push(Instruction::simple(Opcode::ArrConcat));
                }
            }
        }
    }

    fn emit_postfix(&mut self, expr: ExprId, op: PostfixOp) {
        self.emit_expr(expr);
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdCst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let skip_jump = self.placeholder_jump(Opcode::BrFalse);

        match op {
            PostfixOp::Force => self.push(Instruction::simple(Opcode::Panic)),
            PostfixOp::Propagate => self.push(Instruction::simple(Opcode::Ret)),
        }

        self.patch_jump(skip_jump);
        self.push(Instruction::with_u8(Opcode::ArrGeti, 0));
    }

    fn emit_type_op(&mut self, expr: ExprId, kind: TypeOpKind) {
        self.emit_expr(expr);
        match kind {
            TypeOpKind::Test(opt_ident) => {
                self.push(Instruction::simple(Opcode::TyChk));
                if let Some(ident) = opt_ident {
                    self.push(Instruction::simple(Opcode::Dup));
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                }
            }
            TypeOpKind::Cast => {
                self.push(Instruction::simple(Opcode::TyCast));
            }
        }
    }

    fn emit_nil_coalesce(&mut self, lhs: ExprId, rhs: ExprId) {
        self.emit_expr(lhs);
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdCst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let end_jump = self.placeholder_jump(Opcode::BrFalse);

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_expr(rhs);

        self.patch_jump(end_jump);
    }

    fn emit_instance_def(&mut self, inst: &InstanceDef) {
        match &inst.body {
            InstanceBody::Methods(members) => {
                for member in members {
                    if let MemberDecl::Fn(decl) = member {
                        if let Some(body) = decl.body {
                            let name = match decl.name {
                                MemberName::Ident(ident) | MemberName::Op(ident) => ident.name,
                            };
                            let params = decl.params.as_deref().unwrap_or(&[]);
                            self.emit_function(name, params, body);
                        }
                    }
                }
            }
            InstanceBody::Via(_) => {
                self.push(Instruction::simple(Opcode::Nop));
            }
        }
    }

    fn emit_foreign_import(&mut self, sym: Symbol) {
        let name = self.thir.db.interner.resolve(sym);
        let idx = self.pool.add(ConstantEntry::Str(name.into()));
        self.push(Instruction::with_u16(Opcode::LdCst, idx));
        // FfiCall takes a u16 FFI index; 0 is a placeholder until Phase 4
        self.push(Instruction::with_u16(Opcode::FfiCall, 0u16));
    }

    fn alloc_anon_slot(&mut self) -> u16 {
        let sym = Symbol::synthetic(self.next_anon);
        self.next_anon = self.next_anon.wrapping_sub(1);
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(sym);
        slot
    }

    fn emit_comprehension(&mut self, body: ExprId, clauses: &[CompClause]) {
        self.push(Instruction::with_u16(Opcode::ArrNew, 0));

        for (clause_idx, clause) in clauses.iter().enumerate() {
            match clause {
                CompClause::Generator { pat, iter } => {
                    let iter_expr = *iter;
                    let pat_id = *pat;

                    self.emit_expr(iter_expr);
                    let iter_slot = self.alloc_anon_slot();
                    self.emit_st_loc(iter_slot);

                    self.push(Instruction::simple(Opcode::LdZero));
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

                    let pat_node = self.thir.db.ast.pats.get(pat_id);
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
                                self.emit_expr(*guard);
                                filter_jumps.push(self.placeholder_jump(Opcode::BrFalse));
                            }
                            CompClause::Generator { .. } => break,
                        }
                    }

                    self.emit_expr(body);
                    self.push(Instruction::simple(Opcode::ArrConcat));

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
                CompClause::Filter(_) => {
                    // Handled inline within the Generator arm
                }
            }
        }
    }

    fn emit_match_or(
        &mut self,
        pats: &[music_ast::PatId],
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        let mut body_jumps = Vec::new();

        for (si, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.thir.db.ast.pats.get(sub_pat);
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
                            self.emit_guard_and_body(arm, is_last, end_jumps);
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps);
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
                        self.thir.db.interner.resolve(tag.name).into(),
                    ));
                    self.push(Instruction::with_u16(Opcode::LdCst, tag_idx));
                    self.push(Instruction::simple(Opcode::CmpEq));
                    if is_last_sub {
                        if is_last {
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps);
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps);
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
                        self.emit_guard_and_body(arm, is_last, end_jumps);
                    }
                }
            }
        }
    }

    fn emit_match_as(
        &mut self,
        name: Ident,
        inner_pat: music_ast::PatId,
        arm: &MatchArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) {
        self.push(Instruction::simple(Opcode::Dup));
        let name_slot = self.local_slot(name.name);
        self.emit_st_loc(name_slot);

        let inner_node = self.thir.db.ast.pats.get(inner_pat);
        match &inner_node.kind {
            PatKind::Variant { tag, fields } => {
                let tag = *tag;
                let fields = fields.clone();
                self.emit_match_variant(&tag, &fields, arm, is_last, end_jumps);
            }
            PatKind::Lit(literal) => {
                let literal = literal.clone();
                self.emit_match_lit(&literal, arm, is_last, end_jumps);
            }
            PatKind::Bind(ident) => {
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
                self.emit_guard_and_body(arm, is_last, end_jumps);
            }
            _ => {
                self.push(Instruction::simple(Opcode::Pop));
                self.emit_guard_and_body(arm, is_last, end_jumps);
            }
        }
    }

    fn emit_assign(&mut self, target: ExprId, value: ExprId) {
        let target_kind = self.thir.db.ast.exprs.get(target).kind.clone();
        match target_kind {
            ExprKind::Var(ident) => {
                self.emit_expr(value);
                if let Some(slot) = self.find_local(ident.name) {
                    self.emit_st_loc(slot);
                } else if let Some(slot) = self.find_upvalue(ident.name) {
                    self.push(Instruction::with_u16(Opcode::StUpv, u16::from(slot)));
                } else if let Some(idx) = self.find_global(ident.name) {
                    self.push(Instruction::with_u16(Opcode::StGlb, idx));
                }
            }
            ExprKind::Index {
                expr, ref indices, ..
            } => {
                // arr.[i] <- val  →  [arr, idx, val] ArrSet
                // For chained indices like arr.[i].[j] <- val, navigate with ArrGet
                // to the penultimate array, then ArrSet the last index.
                let indices = indices.clone();
                if let Some((last_idx, leading)) = indices.split_last() {
                    // Navigate to the containing array.
                    self.emit_expr(expr);
                    for &idx in leading {
                        self.emit_expr(idx);
                        self.push(Instruction::simple(Opcode::ArrGet));
                    }
                    self.emit_expr(*last_idx);
                    self.emit_expr(value);
                    self.push(Instruction::simple(Opcode::ArrSet));
                }
            }
            ExprKind::Access { expr, field, .. } => {
                // rec.field <- val  →  [rec, val] ArrSeti(field_idx)
                self.emit_expr(expr);
                self.emit_expr(value);
                match field {
                    FieldTarget::Index(idx) => {
                        let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                        self.push(Instruction::with_u8(Opcode::ArrSeti, field_u8));
                    }
                    FieldTarget::Name(_) => {
                        // Named fields are resolved to positional indices by sema.
                        // Until that lowering exists, emit Nop as a placeholder.
                        self.push(Instruction::simple(Opcode::Nop));
                    }
                }
            }
            _ => {}
        }
    }

    fn push(&mut self, instruction: Instruction) {
        self.current_instructions.push(instruction);
    }

    fn local_slot(&mut self, name: Symbol) -> u16 {
        if let Some(slot) = self.find_local(name) {
            return slot;
        }
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(name);
        slot
    }

    fn find_local(&self, name: Symbol) -> Option<u16> {
        self.current_locals
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u16::try_from(p).ok())
    }

    fn emit_ld_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::LdLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::LdLocW, slot));
        }
    }

    fn emit_st_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::StLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::StLocW, slot));
        }
    }

    fn find_upvalue(&self, name: Symbol) -> Option<u8> {
        self.current_upvalues
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u8::try_from(p).ok())
    }

    fn find_global(&self, name: Symbol) -> Option<u16> {
        self.globals
            .iter()
            .position(|g| g.name == name)
            .and_then(|p| u16::try_from(p).ok())
    }

    fn emit_quote(&mut self, qk: &QuoteKind) {
        match qk {
            QuoteKind::Expr(e) => self.emit_expr(*e),
            QuoteKind::Block(stmts) => self.emit_seq(stmts),
        }
    }

    fn emit_splice(&mut self, sk: &SpliceKind) {
        match sk {
            SpliceKind::Ident(_) => self.push(Instruction::simple(Opcode::Nop)),
            SpliceKind::Expr(e) => self.emit_expr(*e),
            SpliceKind::Array(es) => {
                let len = u16::try_from(es.len()).expect("too many splice elements (>65535)");
                self.push(Instruction::with_u16(Opcode::ArrNew, len));
                for (i, &e) in es.iter().enumerate() {
                    self.emit_expr(e);
                    let idx = u8::try_from(i).expect("too many splice elements (>255)");
                    self.push(Instruction::with_u8(Opcode::ArrSeti, idx));
                }
            }
        }
    }
}

const fn resolve_visibility(binding: &LetBinding) -> Visibility {
    if binding.modifiers.opaque {
        Visibility::Opaque
    } else if binding.modifiers.exported {
        Visibility::Exported
    } else {
        Visibility::Private
    }
}

const fn binop_to_opcode(op: BinOp) -> Opcode {
    match op {
        BinOp::Add => Opcode::IAdd,
        BinOp::Sub => Opcode::ISub,
        BinOp::Mul => Opcode::IMul,
        BinOp::Div => Opcode::IDiv,
        BinOp::Rem => Opcode::IMod,
        BinOp::Eq => Opcode::CmpEq,
        BinOp::NotEq => Opcode::CmpNeq,
        BinOp::Lt => Opcode::CmpLt,
        BinOp::Gt => Opcode::CmpGt,
        BinOp::LtEq => Opcode::CmpLeq,
        BinOp::GtEq => Opcode::CmpGeq,
        BinOp::And => Opcode::And,
        BinOp::Or => Opcode::Or,
        BinOp::Xor => Opcode::Xor,
        BinOp::Cons => Opcode::ArrConcat,
        BinOp::NilCoalesce | BinOp::PipeRight | BinOp::Range | BinOp::RangeExcl => {
            // NilCoalesce, Range, and RangeExcl are intercepted before reaching
            // this function; PipeRight is desugared. This arm is unreachable in
            // practice but needed for exhaustiveness.
            Opcode::Nop
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
