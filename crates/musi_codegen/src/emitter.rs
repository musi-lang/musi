//! AST to bytecode emitter.

#![allow(clippy::too_many_lines)]

use std::collections::HashMap;

use musi_ast::{
    AttrArg, BinOp, ChoiceVariant, Cond, ElifBranch, Expr, FieldInit, LitValue, MatchArm,
    Modifier, ParsedModule, Pat, PatSuffix, PostfixOp, PrefixOp, VariantPayload,
};
use musi_shared::{Arena, Idx, Interner};

use crate::error::CodegenError;
use crate::intrinsics::{self, Intrinsic};
use crate::{ConstEntry, FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

// -- FnEmitter ----------------------------------------------------------------

struct LoopCtx {
    start_pos: usize,
    break_fixups: Vec<usize>,
}

struct FnEmitter {
    code: Vec<u8>,
    scopes: Vec<HashMap<String, u16>>,
    next_slot: u16,
    loop_stack: Vec<LoopCtx>,
    /// Maps local slot -> type name (for field-access type tracking).
    local_types: HashMap<u16, String>,
}

impl FnEmitter {
    const BR: u8 = 0x60;
    const BR_TRUE: u8 = 0x61;
    const BR_FALSE: u8 = 0x62;

    fn new() -> Self {
        Self {
            code: Vec::new(),
            scopes: Vec::new(),
            next_slot: 0,
            loop_stack: Vec::new(),
            local_types: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _discarded = self.scopes.pop();
    }

    fn define_local(&mut self, name: &str) -> Result<u16, CodegenError> {
        let slot = self.next_slot;
        self.next_slot = self
            .next_slot
            .checked_add(1)
            .ok_or(CodegenError::TooManyLocals)?;
        let scope = self
            .scopes
            .last_mut()
            .expect("define_local called with no active scope");
        let _prev = scope.insert(name.to_owned(), slot);
        Ok(slot)
    }

    fn lookup_local(&self, name: &str) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            if let Some(&slot) = scope.get(name) {
                return Some(slot);
            }
        }
        None
    }

    fn push(&mut self, op: &Opcode) {
        op.encode_into(&mut self.code);
    }

    const fn len(&self) -> usize {
        self.code.len()
    }

    fn emit_jump_placeholder(&mut self, opcode_tag: u8) -> usize {
        let pos = self.code.len();
        self.code.push(opcode_tag);
        self.code.extend_from_slice(&i32::MAX.to_le_bytes());
        pos
    }

    fn patch_jump_to_here(&mut self, fixup_pos: usize) -> Result<(), CodegenError> {
        let after_instr = fixup_pos.checked_add(5).expect("fixup_pos + 5 fits usize");
        let target = self.code.len();
        let offset = i32::try_from(
            isize::try_from(target).map_err(|_| CodegenError::JumpOffsetOverflow)?
                .wrapping_sub(
                    isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?,
                ),
        )
        .map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.code[fixup_pos + 1..fixup_pos + 5].copy_from_slice(&offset.to_le_bytes());
        Ok(())
    }

    fn emit_br_back(&mut self, target_pos: usize) -> Result<(), CodegenError> {
        let after_instr = self
            .code
            .len()
            .checked_add(5)
            .expect("code len + 5 fits usize");
        let offset = i32::try_from(
            isize::try_from(target_pos)
                .map_err(|_| CodegenError::JumpOffsetOverflow)?
                .wrapping_sub(
                    isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?,
                ),
        )
        .map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.push(&Opcode::Br(offset));
        Ok(())
    }

    fn start_loop(&mut self) -> usize {
        let start_pos = self.len();
        self.loop_stack.push(LoopCtx {
            start_pos,
            break_fixups: Vec::new(),
        });
        start_pos
    }

    fn pop_loop(&mut self) -> LoopCtx {
        self.loop_stack
            .pop()
            .expect("pop_loop called with no active loop")
    }

    fn current_loop_start(&self) -> Option<usize> {
        self.loop_stack.last().map(|ctx| ctx.start_pos)
    }

    fn push_break_fixup(&mut self, fixup_pos: usize) {
        self.loop_stack
            .last_mut()
            .expect("push_break_fixup called outside loop")
            .break_fixups
            .push(fixup_pos);
    }

    fn close_loop(&mut self) -> Result<(), CodegenError> {
        let ctx = self.pop_loop();
        for pos in ctx.break_fixups {
            self.patch_jump_to_here(pos)?;
        }
        self.push(&Opcode::LdImmUnit);
        Ok(())
    }

    const fn local_count(&self) -> u16 {
        self.next_slot
    }

    fn flush_into(self, buf: &mut Vec<u8>) {
        buf.extend_from_slice(&self.code);
    }

    const fn byte_len(&self) -> usize {
        self.code.len()
    }
}

// -- Emit context (immutable borrow of arenas) --------------------------------

struct EmitArenas<'a> {
    exprs: &'a Arena<Expr>,
    interner: &'a Interner,
}

// -- Type / variant metadata (emitter-internal, not stored in Module) ---------

struct TypeInfo {
    field_names: Vec<String>,
}

struct VariantInfo {
    /// Name of the parent type (for local_types tracking).
    type_name: String,
    discriminant: i64,
    payload_count: u16,
    total_field_count: u16,
}

struct PendingLambda {
    fn_idx: u16,
    param_names: Vec<String>,
    body: Idx<Expr>,
}

// -- Emit state (mutable) -----------------------------------------------------

struct EmitState {
    fn_map: HashMap<String, u16>,
    /// type_name -> field names in declaration order
    type_map: HashMap<String, TypeInfo>,
    /// variant_name -> variant metadata
    variant_map: HashMap<String, VariantInfo>,
    lambda_counter: u16,
    pending_lambdas: Vec<PendingLambda>,
}

// -- Declaration pass ---------------------------------------------------------

fn register_fn_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::FnDef { attrs, modifiers, name, params, body, .. } = exprs.get(item_idx) else {
        return Ok(());
    };

    let fn_name = interner.resolve(*name).to_owned();
    let is_native = body.is_none() || modifiers.iter().any(|m| matches!(m, Modifier::Native(_)));
    let flags_raw: u8 = if is_native { SymbolFlags::NATIVE } else { 0 };

    let intrinsic_id: u16 = if is_native {
        attrs
            .iter()
            .find_map(|attr| {
                if interner.resolve(attr.name) != "intrinsic" {
                    return None;
                }
                attr.args.first().and_then(|arg| match arg {
                    AttrArg::Named { name: arg_name, .. } => {
                        Intrinsic::from_name(interner.resolve(*arg_name)).map(Intrinsic::id)
                    }
                    AttrArg::Value { value: LitValue::Str(sym), .. } => {
                        Intrinsic::from_name(interner.resolve(*sym)).map(Intrinsic::id)
                    }
                    AttrArg::Value { .. } => None,
                })
            })
            .or_else(|| Intrinsic::from_name(&fn_name).map(Intrinsic::id))
            .unwrap_or(intrinsics::NONE_ID)
    } else {
        intrinsics::NONE_ID
    };

    let sym_idx = module.push_symbol(SymbolEntry {
        name: fn_name.clone().into_boxed_str(),
        flags: SymbolFlags::new(flags_raw),
        intrinsic_id,
    })?;

    let param_count =
        u8::try_from(params.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;

    let fn_idx = module.push_function(FunctionEntry {
        symbol_idx: sym_idx,
        param_count,
        local_count: 0,
        code_offset: 0,
        code_length: 0,
    })?;

    let _prev = state.fn_map.insert(fn_name, fn_idx);
    Ok(())
}

fn register_record(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::Record { name: Some(name), fields, .. } = exprs.get(item_idx) else {
        return Ok(());
    };
    let type_name = interner.resolve(*name).to_owned();
    let field_names: Vec<String> = fields
        .iter()
        .map(|f| interner.resolve(f.name).to_owned())
        .collect();
    let _prev = state.type_map.insert(type_name, TypeInfo { field_names });
    Ok(())
}

fn register_choice(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::Choice { name: Some(name), variants, .. } = exprs.get(item_idx) else {
        return Ok(());
    };
    let type_name = interner.resolve(*name).to_owned();

    let max_payload = variants
        .iter()
        .map(|v| payload_count(v))
        .max()
        .unwrap_or(0);
    let total_field_count = max_payload + 1; // +1 for discriminant

    for (disc, variant) in variants.iter().enumerate() {
        let v_name = interner.resolve(variant.name).to_owned();
        let payload = payload_count(variant);

        // If variant has an explicit discriminant, use it; otherwise use position.
        let discriminant: i64 = if let Some(VariantPayload::Discriminant(LitValue::Int(v))) =
            &variant.payload
        {
            *v
        } else {
            i64::try_from(disc).map_err(|_| CodegenError::UnsupportedExpr)?
        };

        let _prev = state.variant_map.insert(
            v_name,
            VariantInfo {
                type_name: type_name.clone(),
                discriminant,
                payload_count: payload,
                total_field_count,
            },
        );
    }
    Ok(())
}

fn payload_count(v: &ChoiceVariant) -> u16 {
    match &v.payload {
        None | Some(VariantPayload::Discriminant(_)) => 0,
        Some(VariantPayload::Positional(tys)) => {
            u16::try_from(tys.len()).unwrap_or(u16::MAX)
        }
        Some(VariantPayload::Named(fields)) => {
            u16::try_from(fields.len()).unwrap_or(u16::MAX)
        }
    }
}

// -- Fn-body emission ---------------------------------------------------------

fn emit_fn_body(
    fn_idx: u16,
    param_names: &[String],
    body_idx: Idx<Expr>,
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let mut out = FnEmitter::new();
    out.push_scope();
    for name in param_names {
        let _slot = out.define_local(name)?;
    }
    let body = arenas.exprs.get(body_idx).clone();
    emit_expr(arenas, state, &body, module, &mut out)?;
    out.push(&Opcode::Ret);
    out.pop_scope();

    let local_count = out.local_count();
    let code_offset =
        u32::try_from(module.code.len()).map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = out.byte_len();
    out.flush_into(&mut module.code);
    let code_length =
        u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let entry = module
        .function_table
        .get_mut(usize::from(fn_idx))
        .ok_or(CodegenError::UnsupportedExpr)?;
    entry.local_count = local_count;
    entry.code_offset = code_offset;
    entry.code_length = code_length;
    Ok(())
}

// -- emit_expr (main recursive emitter) --------------------------------------

fn emit_expr(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    expr: &Expr,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match expr {
        Expr::Lit { value, .. } => emit_lit(value, arenas, module, out),

        Expr::Unit { .. } => {
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Paren { inner, .. } => {
            let inner_expr = arenas.exprs.get(*inner).clone();
            emit_expr(arenas, state, &inner_expr, module, out)
        }

        Expr::Ident { name, .. } => {
            let name_str = arenas.interner.resolve(*name);
            // Unit variant (choice with no payload)
            if let Some(vinfo) = state.variant_map.get(name_str) {
                let disc = vinfo.discriminant;
                let total = vinfo.total_field_count;
                out.push(&Opcode::LdImmI64(disc));
                for _ in 0..total.saturating_sub(1) {
                    out.push(&Opcode::LdImmUnit);
                }
                out.push(&Opcode::NewObj(total));
                return Ok(());
            }
            let slot = out
                .lookup_local(name_str)
                .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
            out.push(&Opcode::LdLoc(slot));
            Ok(())
        }

        Expr::Bind { pat, init, .. } => {
            let Pat::Ident { name, .. } = pat else {
                return Err(CodegenError::UnsupportedExpr);
            };
            let name_str = arenas.interner.resolve(*name);
            let slot = out.define_local(name_str)?;

            // Detect type for local_types tracking (before emitting init)
            if let Some(init_idx) = init {
                let init_expr = arenas.exprs.get(*init_idx);
                maybe_track_type(init_expr, slot, arenas, state, out);
                let init_cloned = init_expr.clone();
                emit_expr(arenas, state, &init_cloned, module, out)?;
            } else {
                out.push(&Opcode::LdImmUnit);
            }
            out.push(&Opcode::StLoc(slot));
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Assign { target, value, .. } => {
            let Expr::Ident { name, .. } = arenas.exprs.get(*target) else {
                return Err(CodegenError::UnsupportedExpr);
            };
            let name_str = arenas.interner.resolve(*name);
            let slot = out
                .lookup_local(name_str)
                .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
            let val = arenas.exprs.get(*value).clone();
            emit_expr(arenas, state, &val, module, out)?;
            out.push(&Opcode::StLoc(slot));
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Prefix { op, operand, .. } => {
            let opcode = match op {
                PrefixOp::Neg => Opcode::NegI64,
                PrefixOp::Not => Opcode::Not,
                PrefixOp::BitNot => Opcode::BitNot,
                PrefixOp::Deref | PrefixOp::AddrOf => {
                    return Err(CodegenError::UnsupportedExpr);
                }
            };
            let operand_expr = arenas.exprs.get(*operand).clone();
            emit_expr(arenas, state, &operand_expr, module, out)?;
            out.push(&opcode);
            Ok(())
        }

        Expr::Binary { op, lhs, rhs, .. } => emit_binary(arenas, state, *op, *lhs, *rhs, module, out),

        Expr::Block { stmts, tail, .. } => {
            out.push_scope();
            for stmt_idx in stmts {
                let stmt = arenas.exprs.get(*stmt_idx).clone();
                emit_expr(arenas, state, &stmt, module, out)?;
                out.push(&Opcode::Drop);
            }
            emit_opt(arenas, state, *tail, module, out)?;
            out.pop_scope();
            Ok(())
        }

        Expr::Postfix { base, op, .. } => {
            emit_postfix(arenas, state, *base, op, module, out)
        }

        Expr::Lambda { params, body, .. } => {
            // Register as an anonymous function, emit later
            let lambda_name = format!("__lambda_{}", state.lambda_counter);
            state.lambda_counter = state.lambda_counter.checked_add(1).ok_or(CodegenError::TooManyFunctions)?;

            let param_names: Vec<String> = params
                .iter()
                .map(|p| arenas.interner.resolve(p.name).to_owned())
                .collect();
            let param_count = u8::try_from(param_names.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;

            let sym_idx = module.push_symbol(SymbolEntry {
                name: lambda_name.clone().into_boxed_str(),
                flags: SymbolFlags::new(0),
                intrinsic_id: intrinsics::NONE_ID,
            })?;
            let fn_idx = module.push_function(FunctionEntry {
                symbol_idx: sym_idx,
                param_count,
                local_count: 0,
                code_offset: 0,
                code_length: 0,
            })?;
            let _prev = state.fn_map.insert(lambda_name, fn_idx);
            state.pending_lambdas.push(PendingLambda {
                fn_idx,
                param_names,
                body: *body,
            });
            out.push(&Opcode::LdFnIdx(fn_idx));
            Ok(())
        }

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            ..
        } => emit_if(arenas, state, cond, *then_body, elif_chains, *else_body, module, out),

        Expr::While { cond, guard, body, .. } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_while(arenas, state, cond, *body, module, out)
        }

        Expr::Loop { body, post_cond, .. } => {
            if post_cond.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_loop(arenas, state, *body, module, out)
        }

        Expr::For { pat, iter, guard, body, .. } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_for(arenas, state, pat, *iter, *body, module, out)
        }

        Expr::Break { label, value, .. } => {
            if label.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_opt(arenas, state, *value, module, out)?;
            let fixup = out.emit_jump_placeholder(FnEmitter::BR);
            out.push_break_fixup(fixup);
            Ok(())
        }

        Expr::Cycle { label, guard, .. } => {
            if label.is_some() || guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            let start_pos = out
                .current_loop_start()
                .ok_or(CodegenError::UnsupportedExpr)?;
            out.emit_br_back(start_pos)?;
            Ok(())
        }

        Expr::Return { value, .. } => {
            emit_opt(arenas, state, *value, module, out)?;
            out.push(&Opcode::Ret);
            Ok(())
        }

        Expr::Match { scrutinee, arms, .. } => {
            emit_match(arenas, state, *scrutinee, arms, module, out)
        }

        Expr::Tuple { .. }
        | Expr::Array { .. }
        | Expr::AnonRec { .. }
        | Expr::Label { .. }
        | Expr::Defer { .. }
        | Expr::Import { .. }
        | Expr::Record { .. }
        | Expr::Choice { .. }
        | Expr::FnDef { .. }
        | Expr::Error { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

// -- Postfix dispatch ---------------------------------------------------------

fn emit_postfix(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    op: &PostfixOp,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match op {
        PostfixOp::Call { args, .. } => emit_call(arenas, state, base, args, module, out),
        PostfixOp::Field { name, .. } => emit_field_access(arenas, state, base, *name, module, out),
        PostfixOp::RecDot { fields, .. } => emit_rec_dot(arenas, state, base, fields, module, out),
        PostfixOp::Index { .. } | PostfixOp::As { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

fn emit_call(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    args: &[Idx<Expr>],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let base_expr = arenas.exprs.get(base);

    if let Expr::Ident { name, .. } = base_expr {
        let callee_str = arenas.interner.resolve(*name);

        // Variant constructor
        if let Some(vinfo) = state.variant_map.get(callee_str) {
            let disc = vinfo.discriminant;
            let payload_count = vinfo.payload_count;
            let total = vinfo.total_field_count;
            out.push(&Opcode::LdImmI64(disc));
            for arg_idx in args {
                let arg = arenas.exprs.get(*arg_idx).clone();
                emit_expr(arenas, state, &arg, module, out)?;
            }
            // Pad missing payload slots with Unit
            let provided = u16::try_from(args.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
            for _ in provided..payload_count {
                out.push(&Opcode::LdImmUnit);
            }
            // Pad remaining fields (beyond payload) with Unit
            for _ in payload_count.saturating_add(1)..total {
                out.push(&Opcode::LdImmUnit);
            }
            out.push(&Opcode::NewObj(total));
            return Ok(());
        }

        // Named function (static dispatch)
        if let Some(&fn_idx) = state.fn_map.get(callee_str) {
            for arg_idx in args {
                let arg = arenas.exprs.get(*arg_idx).clone();
                emit_expr(arenas, state, &arg, module, out)?;
            }
            out.push(&Opcode::Call(fn_idx));
            return Ok(());
        }

        // Local variable holding a function (dynamic dispatch)
        if let Some(slot) = out.lookup_local(callee_str) {
            for arg_idx in args {
                let arg = arenas.exprs.get(*arg_idx).clone();
                emit_expr(arenas, state, &arg, module, out)?;
            }
            out.push(&Opcode::LdLoc(slot));
            out.push(&Opcode::CallDynamic);
            return Ok(());
        }

        return Err(CodegenError::UnknownFunction(callee_str.into()));
    }

    // General expression base -- emit base, then CallDynamic
    let base_cloned = base_expr.clone();
    for arg_idx in args {
        let arg = arenas.exprs.get(*arg_idx).clone();
        emit_expr(arenas, state, &arg, module, out)?;
    }
    emit_expr(arenas, state, &base_cloned, module, out)?;
    out.push(&Opcode::CallDynamic);
    Ok(())
}

fn emit_field_access(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    field_sym: musi_shared::Symbol,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let field_name = arenas.interner.resolve(field_sym);
    let base_expr = arenas.exprs.get(base);

    // For simple `local.field`, look up local type info
    if let Expr::Ident { name: base_name, .. } = base_expr {
        let base_name_str = arenas.interner.resolve(*base_name);
        let slot = out
            .lookup_local(base_name_str)
            .ok_or_else(|| CodegenError::UndefinedVariable(base_name_str.into()))?;
        let type_name = out
            .local_types
            .get(&slot)
            .ok_or(CodegenError::UnsupportedExpr)?
            .clone();
        let type_info = state
            .type_map
            .get(&type_name)
            .ok_or_else(|| CodegenError::UnknownType(type_name.clone().into_boxed_str()))?;
        let field_idx = type_info
            .field_names
            .iter()
            .position(|f| f == field_name)
            .ok_or_else(|| CodegenError::UnknownField(field_name.into()))?;
        let field_idx_u16 =
            u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
        let base_cloned = base_expr.clone();
        emit_expr(arenas, state, &base_cloned, module, out)?;
        out.push(&Opcode::LdFld(field_idx_u16));
        return Ok(());
    }

    Err(CodegenError::UnsupportedExpr)
}

fn emit_rec_dot(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    fields: &[FieldInit],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let base_expr = arenas.exprs.get(base);
    let Expr::Ident { name: type_name_sym, .. } = base_expr else {
        return Err(CodegenError::UnsupportedExpr);
    };
    let type_name = arenas.interner.resolve(*type_name_sym).to_owned();

    let type_info = state
        .type_map
        .get(&type_name)
        .ok_or_else(|| CodegenError::UnknownType(type_name.clone().into_boxed_str()))?;
    let declared_fields = type_info.field_names.clone();
    let field_count =
        u16::try_from(declared_fields.len()).map_err(|_| CodegenError::UnsupportedExpr)?;

    // Find spread, if any
    let spread_expr: Option<Idx<Expr>> = fields.iter().find_map(|fi| {
        if let FieldInit::Spread { expr, .. } = fi {
            Some(*expr)
        } else {
            None
        }
    });

    // Build explicit field map: field_name -> value_expr
    let explicit: HashMap<String, Idx<Expr>> = fields
        .iter()
        .filter_map(|fi| {
            if let FieldInit::Named { name, value, .. } = fi {
                Some((arenas.interner.resolve(*name).to_owned(), *value))
            } else {
                None
            }
        })
        .collect();

    // If there's a spread, emit it into a temp local
    let spread_slot: Option<u16> = if let Some(spread_idx) = spread_expr {
        let temp_name = format!("$spread_{}", out.next_slot);
        let slot = out.define_local(&temp_name)?;
        let spread = arenas.exprs.get(spread_idx).clone();
        emit_expr(arenas, state, &spread, module, out)?;
        out.push(&Opcode::StLoc(slot));
        // Track the spread slot's type too
        if let Expr::Ident { name: spread_name, .. } = arenas.exprs.get(spread_idx) {
            let spread_name_str = arenas.interner.resolve(*spread_name);
            if let Some(src_slot) = out.lookup_local(spread_name_str) {
                if let Some(t) = out.local_types.get(&src_slot).cloned() {
                    let _prev = out.local_types.insert(slot, t);
                }
            }
        }
        Some(slot)
    } else {
        None
    };

    // Emit each field in declaration order
    for (field_idx, field_name) in declared_fields.iter().enumerate() {
        if let Some(&val_idx) = explicit.get(field_name) {
            let val = arenas.exprs.get(val_idx).clone();
            emit_expr(arenas, state, &val, module, out)?;
        } else if let Some(spread_slot_id) = spread_slot {
            let fidx =
                u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
            out.push(&Opcode::LdLoc(spread_slot_id));
            out.push(&Opcode::LdFld(fidx));
        } else {
            return Err(CodegenError::UnknownField(field_name.clone().into_boxed_str()));
        }
    }
    out.push(&Opcode::NewObj(field_count));
    Ok(())
}

// -- Match expression ---------------------------------------------------------

fn emit_match(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    scrutinee: Idx<Expr>,
    arms: &[MatchArm],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    // Store scrutinee in a temp local to allow multiple accesses
    let scrutinee_name = format!("$scrutinee_{}", out.next_slot);
    let scrutinee_slot = out.define_local(&scrutinee_name)?;
    let scr = arenas.exprs.get(scrutinee).clone();
    emit_expr(arenas, state, &scr, module, out)?;
    out.push(&Opcode::StLoc(scrutinee_slot));

    let mut end_fixups: Vec<usize> = Vec::new();

    for arm in arms {
        if arm.guard.is_some() {
            return Err(CodegenError::UnsupportedExpr);
        }
        // Test the pattern; BrFalse to next_arm_fixup if no match
        let next_arm_fixup = emit_pattern_test(
            arenas,
            state,
            &arm.pat,
            scrutinee_slot,
            module,
            out,
        )?;

        // Bind pattern variables
        out.push_scope();
        emit_pattern_bindings(arenas, state, &arm.pat, scrutinee_slot, out)?;

        let body = arenas.exprs.get(arm.body).clone();
        emit_expr(arenas, state, &body, module, out)?;
        out.pop_scope();

        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));

        if let Some(fixup) = next_arm_fixup {
            out.patch_jump_to_here(fixup)?;
        }
    }

    // No arm matched
    out.push(&Opcode::HaltError);

    for fixup in end_fixups {
        out.patch_jump_to_here(fixup)?;
    }
    Ok(())
}

/// Emits the test for `pat` against `scrutinee_slot`.
/// Returns Some(fixup_pos) of a BrFalse placeholder if the pattern can fail,
/// or None if it always matches.
fn emit_pattern_test(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    _module: &mut Module,
    out: &mut FnEmitter,
) -> Result<Option<usize>, CodegenError> {
    match pat {
        Pat::Wild { .. } => Ok(None),

        Pat::Ident { suffix: None, .. } => Ok(None),

        Pat::Lit { value, .. } => {
            out.push(&Opcode::LdLoc(scrutinee_slot));
            match value {
                LitValue::Int(v) => out.push(&Opcode::LdImmI64(*v)),
                LitValue::Bool(v) => out.push(&Opcode::LdImmBool(*v)),
                _ => return Err(CodegenError::UnsupportedExpr),
            }
            // Compare by type: Int vs Int, Bool vs Bool
            match value {
                LitValue::Int(_) => out.push(&Opcode::EqI64),
                LitValue::Bool(_) => out.push(&Opcode::EqBool),
                _ => return Err(CodegenError::UnsupportedExpr),
            }
            let fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
            Ok(Some(fixup))
        }

        Pat::Ident { name, suffix: Some(PatSuffix::Positional { .. }), .. } => {
            let variant_name = arenas.interner.resolve(*name);
            let vinfo = state
                .variant_map
                .get(variant_name)
                .ok_or_else(|| CodegenError::UnknownVariant(variant_name.into()))?;
            let disc = vinfo.discriminant;
            // Check discriminant: load scrutinee, LdTag, compare
            out.push(&Opcode::LdLoc(scrutinee_slot));
            out.push(&Opcode::LdTag);
            out.push(&Opcode::LdImmI64(disc));
            out.push(&Opcode::EqI64);
            let fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
            Ok(Some(fixup))
        }

        _ => Err(CodegenError::UnsupportedExpr),
    }
}

/// Emits StLoc instructions to bind pattern variables from the scrutinee.
fn emit_pattern_bindings(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match pat {
        Pat::Wild { .. } => Ok(()),

        Pat::Ident { name, suffix: None, .. } => {
            let name_str = arenas.interner.resolve(*name);
            let slot = out.define_local(name_str)?;
            out.push(&Opcode::LdLoc(scrutinee_slot));
            out.push(&Opcode::StLoc(slot));
            Ok(())
        }

        Pat::Ident { suffix: Some(PatSuffix::Positional { args: sub_pats, .. }), .. } => {
            for (field_i, sub_pat) in sub_pats.iter().enumerate() {
                let field_idx =
                    u16::try_from(field_i + 1).map_err(|_| CodegenError::UnsupportedExpr)?;
                match sub_pat {
                    Pat::Wild { .. } => {}
                    Pat::Ident { name, suffix: None, .. } => {
                        let name_str = arenas.interner.resolve(*name);
                        let sub_slot = out.define_local(name_str)?;
                        out.push(&Opcode::LdLoc(scrutinee_slot));
                        out.push(&Opcode::LdFld(field_idx));
                        out.push(&Opcode::StLoc(sub_slot));
                    }
                    _ => {
                        // Use a temp scrutinee slot for nested match
                        let temp_name = format!("$sub_{}_{}_{}", scrutinee_slot, field_i, out.next_slot);
                        let temp_slot = out.define_local(&temp_name)?;
                        out.push(&Opcode::LdLoc(scrutinee_slot));
                        out.push(&Opcode::LdFld(field_idx));
                        out.push(&Opcode::StLoc(temp_slot));
                        emit_pattern_bindings(arenas, state, sub_pat, temp_slot, out)?;
                    }
                }
            }
            Ok(())
        }

        Pat::Lit { .. } => Ok(()),

        _ => Err(CodegenError::UnsupportedExpr),
    }
}

// -- Helper: detect type of a binding init for local_types tracking -----------

fn maybe_track_type(
    init_expr: &Expr,
    slot: u16,
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    out: &mut FnEmitter,
) {
    match init_expr {
        Expr::Postfix { base, op: PostfixOp::RecDot { .. }, .. } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let type_name = arenas.interner.resolve(*name);
                if state.type_map.contains_key(type_name) {
                    let _prev = out.local_types.insert(slot, type_name.to_owned());
                }
            }
        }
        Expr::Postfix { base, op: PostfixOp::Call { .. }, .. } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let variant_name = arenas.interner.resolve(*name);
                if let Some(vinfo) = state.variant_map.get(variant_name) {
                    let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
                }
            }
        }
        // Unit variant (Ident that's in variant_map)
        Expr::Ident { name, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if let Some(vinfo) = state.variant_map.get(name_str) {
                let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
            }
        }
        _ => {}
    }
}

// -- Remaining emit helpers ---------------------------------------------------

fn emit_opt(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    opt: Option<Idx<Expr>>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match opt {
        Some(idx) => {
            let expr = arenas.exprs.get(idx).clone();
            emit_expr(arenas, state, &expr, module, out)
        }
        None => {
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }
    }
}

fn emit_short_circuit(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    lhs: Idx<Expr>,
    rhs: Idx<Expr>,
    branch_tag: u8,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let lhs_expr = arenas.exprs.get(lhs).clone();
    emit_expr(arenas, state, &lhs_expr, module, out)?;
    let fixup = out.emit_jump_placeholder(branch_tag);
    let rhs_expr = arenas.exprs.get(rhs).clone();
    emit_expr(arenas, state, &rhs_expr, module, out)?;
    out.patch_jump_to_here(fixup)
}

fn emit_binary(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    op: BinOp,
    lhs: Idx<Expr>,
    rhs: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    if op == BinOp::And {
        return emit_short_circuit(arenas, state, lhs, rhs, FnEmitter::BR_FALSE, module, out);
    }
    if op == BinOp::Or {
        return emit_short_circuit(arenas, state, lhs, rhs, FnEmitter::BR_TRUE, module, out);
    }

    let lhs_expr = arenas.exprs.get(lhs).clone();
    emit_expr(arenas, state, &lhs_expr, module, out)?;
    let rhs_expr = arenas.exprs.get(rhs).clone();
    emit_expr(arenas, state, &rhs_expr, module, out)?;

    let opcode = match op {
        BinOp::Add => Opcode::AddI64,
        BinOp::Sub => Opcode::SubI64,
        BinOp::Mul => Opcode::MulI64,
        BinOp::Div => Opcode::DivI64,
        BinOp::Rem => Opcode::RemI64,
        BinOp::BitOr => Opcode::BitOr,
        BinOp::BitXor => Opcode::BitXor,
        BinOp::Xor    => Opcode::NeqBool,
        BinOp::BitAnd => Opcode::BitAnd,
        BinOp::Shl => Opcode::Shl,
        BinOp::Shr => Opcode::Shr,
        BinOp::Eq => Opcode::EqI64,
        BinOp::NotEq => Opcode::NeqI64,
        BinOp::Lt => Opcode::LtI64,
        BinOp::Gt => Opcode::GtI64,
        BinOp::LtEq => Opcode::LeqI64,
        BinOp::GtEq => Opcode::GeqI64,
        BinOp::In
        | BinOp::Range
        | BinOp::RangeExcl
        | BinOp::Cons
        | BinOp::And
        | BinOp::Or => return Err(CodegenError::UnsupportedExpr),
    };
    out.push(&opcode);
    Ok(())
}

fn emit_if(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    then_body: Idx<Expr>,
    elif_chains: &[ElifBranch],
    else_body: Option<Idx<Expr>>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_cond(arenas, state, cond, module, out)?;
    let else_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
    let then = arenas.exprs.get(then_body).clone();
    emit_expr(arenas, state, &then, module, out)?;
    let first_end_fixup = out.emit_jump_placeholder(FnEmitter::BR);
    out.patch_jump_to_here(else_fixup)?;

    let mut end_fixups = vec![first_end_fixup];
    for branch in elif_chains {
        if branch.guard.is_some() {
            return Err(CodegenError::UnsupportedExpr);
        }
        emit_cond(arenas, state, &branch.cond, module, out)?;
        let next_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
        let branch_body = arenas.exprs.get(branch.body).clone();
        emit_expr(arenas, state, &branch_body, module, out)?;
        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));
        out.patch_jump_to_here(next_fixup)?;
    }

    emit_opt(arenas, state, else_body, module, out)?;
    for fixup in end_fixups {
        out.patch_jump_to_here(fixup)?;
    }
    Ok(())
}

fn emit_cond(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match cond {
        Cond::Expr(idx) => {
            let expr = arenas.exprs.get(*idx).clone();
            emit_expr(arenas, state, &expr, module, out)
        }
        Cond::Case { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

fn emit_while(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    body: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();
    emit_cond(arenas, state, cond, module, out)?;
    let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;
    out.push(&Opcode::Drop);
    out.emit_br_back(start_pos)?;
    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()
}

fn emit_loop(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    body: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();
    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;
    out.push(&Opcode::Drop);
    out.emit_br_back(start_pos)?;
    out.close_loop()
}

fn emit_for(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    iter: Idx<Expr>,
    body: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let Pat::Ident { name: pat_name, suffix: None, .. } = pat else {
        return Err(CodegenError::UnsupportedExpr);
    };

    let iter_expr = arenas.exprs.get(iter).clone();
    let Expr::Binary { op, lhs, rhs, .. } = iter_expr else {
        return Err(CodegenError::UnsupportedExpr);
    };
    if !matches!(op, BinOp::Range | BinOp::RangeExcl) {
        return Err(CodegenError::UnsupportedExpr);
    }

    let cmp_op = if matches!(op, BinOp::RangeExcl) {
        Opcode::LtI64
    } else {
        Opcode::LeqI64
    };

    out.push_scope();
    let counter_slot = out.define_local(arenas.interner.resolve(*pat_name))?;
    let limit_slot = out.define_local("$limit")?;

    let lhs_expr = arenas.exprs.get(lhs).clone();
    emit_expr(arenas, state, &lhs_expr, module, out)?;
    out.push(&Opcode::StLoc(counter_slot));
    let rhs_expr = arenas.exprs.get(rhs).clone();
    emit_expr(arenas, state, &rhs_expr, module, out)?;
    out.push(&Opcode::StLoc(limit_slot));

    let start_pos = out.start_loop();
    out.push(&Opcode::LdLoc(counter_slot));
    out.push(&Opcode::LdLoc(limit_slot));
    out.push(&cmp_op);
    let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;
    out.push(&Opcode::Drop);

    out.push(&Opcode::LdLoc(counter_slot));
    out.push(&Opcode::LdImmI64(1));
    out.push(&Opcode::AddI64);
    out.push(&Opcode::StLoc(counter_slot));

    out.emit_br_back(start_pos)?;
    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()?;

    out.pop_scope();
    Ok(())
}

fn emit_lit(
    value: &LitValue,
    arenas: &EmitArenas<'_>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match value {
        LitValue::Str(sym) => {
            let s: Box<str> = arenas.interner.resolve(*sym).into();
            let const_idx = module.push_const(ConstEntry::String(s))?;
            out.push(&Opcode::LdConst(const_idx));
            Ok(())
        }
        LitValue::Int(v) => {
            out.push(&Opcode::LdImmI64(*v));
            Ok(())
        }
        LitValue::Float(v) => {
            out.push(&Opcode::LdImmF64(*v));
            Ok(())
        }
        LitValue::Bool(v) => {
            out.push(&Opcode::LdImmBool(*v));
            Ok(())
        }
        LitValue::Char(c) => {
            out.push(&Opcode::LdImmI64(i64::from(u32::from(*c))));
            Ok(())
        }
    }
}

// -- Public entry point -------------------------------------------------------

/// Compiles a prelude module and a user module into a [`Module`] ready for the VM.
pub fn emit(
    prelude: &ParsedModule,
    user: &ParsedModule,
    interner: &Interner,
) -> Result<Module, CodegenError> {
    let mut module = Module::new();
    let mut state = EmitState {
        fn_map: HashMap::new(),
        type_map: HashMap::new(),
        variant_map: HashMap::new(),
        lambda_counter: 0,
        pending_lambdas: Vec::new(),
    };

    // Pass 1: register all declarations from prelude + user
    for item_idx in &prelude.items {
        register_fn_def(*item_idx, &prelude.ctx.exprs, interner, &mut module, &mut state)?;
    }
    for item_idx in &user.items {
        let exprs = &user.ctx.exprs;
        register_fn_def(*item_idx, exprs, interner, &mut module, &mut state)?;
        register_record(*item_idx, exprs, interner, &mut state)?;
        register_choice(*item_idx, exprs, interner, &mut state)?;
    }

    // Pass 1.5: emit bodies for user-defined (non-native) functions
    {
        let arenas = EmitArenas { exprs: &user.ctx.exprs, interner };
        let mut pending_fns: Vec<(u16, Vec<String>, Idx<Expr>)> = Vec::new();
        for item_idx in &user.items {
            if let Expr::FnDef { name, params, body: Some(body_idx), modifiers, .. } =
                user.ctx.exprs.get(*item_idx)
            {
                let is_native = modifiers.iter().any(|m| matches!(m, Modifier::Native(_)));
                if is_native {
                    continue;
                }
                let fn_name = interner.resolve(*name).to_owned();
                let fn_idx = *state
                    .fn_map
                    .get(&fn_name)
                    .ok_or_else(|| CodegenError::UnknownFunction(fn_name.clone().into_boxed_str()))?;
                let param_names: Vec<String> = params
                    .iter()
                    .map(|p| interner.resolve(p.name).to_owned())
                    .collect();
                pending_fns.push((fn_idx, param_names, *body_idx));
            }
        }
        for (fn_idx, param_names, body_idx) in pending_fns {
            emit_fn_body(fn_idx, &param_names, body_idx, &arenas, &mut state, &mut module)?;
        }
        // Flush any lambdas encountered during fn body emission
        flush_pending_lambdas(&arenas, &mut state, &mut module)?;
    }

    // Pass 2: emit main body (top-level statements)
    let arenas = EmitArenas { exprs: &user.ctx.exprs, interner };
    let mut main_emitter = FnEmitter::new();
    main_emitter.push_scope();

    for item_idx in &user.items {
        let expr = user.ctx.exprs.get(*item_idx);
        match expr {
            Expr::FnDef { .. }
            | Expr::Record { .. }
            | Expr::Choice { .. } => {}
            _ => {
                let expr_cloned = expr.clone();
                emit_expr(&arenas, &mut state, &expr_cloned, &mut module, &mut main_emitter)?;
                main_emitter.push(&Opcode::Drop);
            }
        }
    }

    // Flush lambdas from main body
    flush_pending_lambdas(&arenas, &mut state, &mut module)?;

    main_emitter.pop_scope();
    main_emitter.push(&Opcode::Halt);

    let local_count = main_emitter.local_count();
    let code_offset =
        u32::try_from(module.code.len()).map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = main_emitter.byte_len();
    main_emitter.flush_into(&mut module.code);
    let code_length =
        u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let main_sym_idx = module.push_symbol(SymbolEntry {
        name: "main".into(),
        flags: SymbolFlags::new(0),
        intrinsic_id: intrinsics::NONE_ID,
    })?;

    let _main_fn_idx = module.push_function(FunctionEntry {
        symbol_idx: main_sym_idx,
        param_count: 0,
        local_count,
        code_offset,
        code_length,
    })?;

    Ok(module)
}

fn flush_pending_lambdas(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    loop {
        let pending = std::mem::take(&mut state.pending_lambdas);
        if pending.is_empty() {
            break;
        }
        for lam in pending {
            emit_fn_body(lam.fn_idx, &lam.param_names, lam.body, arenas, state, module)?;
        }
    }
    Ok(())
}
