#![allow(clippy::too_many_lines)]

use std::collections::HashMap;
use std::mem;

use musi_ast::{
    AttrArg, BinOp, ChoiceVariant, ClassMember, Cond, ElifBranch, Expr, FieldInit, LitValue,
    MatchArm, Modifier, ParsedModule, Pat, PatSuffix, PostfixOp, PrefixOp, Ty, VariantPayload,
};
use musi_shared::{Arena, Idx, Interner};

use crate::error::CodegenError;
use crate::intrinsics::{self, Intrinsic};
use crate::module::MethodEntry;
use crate::{ConstEntry, FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum TypeTag {
    Int = 0,
    Float = 1,
    Str = 2,
    Unit = 3,
}

impl TypeTag {
    fn from_type_name(name: &str) -> Option<Self> {
        match name {
            "Int" | "Int8" | "Int16" | "Int32" | "Int64"
            | "Nat" | "Nat8" | "Nat16" | "Nat32" | "Nat64" => Some(Self::Int),
            "Float" | "Float32" | "Float64" => Some(Self::Float),
            "String" => Some(Self::Str),
            "Unit" => Some(Self::Unit),
            _ => None,
        }
    }
}

struct LoopCtx {
    start_pos: usize,
    break_fixups: Vec<usize>,
}

struct FnEmitter {
    code: Vec<u8>,
    scopes: Vec<HashMap<String, u16>>,
    next_slot: u16,
    loop_stack: Vec<LoopCtx>,
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
            isize::try_from(target)
                .map_err(|_| CodegenError::JumpOffsetOverflow)?
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

struct EmitArenas<'a> {
    exprs: &'a Arena<Expr>,
    interner: &'a Interner,
}

struct TypeInfo {
    field_names: Vec<String>,
}

struct VariantInfo {
    type_name: String,
    discriminant: i64,
    payload_count: u16,
    total_field_count: u16,
}

impl Clone for VariantInfo {
    fn clone(&self) -> Self {
        Self {
            type_name: self.type_name.clone(),
            discriminant: self.discriminant,
            payload_count: self.payload_count,
            total_field_count: self.total_field_count,
        }
    }
}

struct PendingLambda {
    fn_idx: u16,
    /// (param_name, optional_type_name)
    params: Vec<(String, Option<String>)>,
    body: Idx<Expr>,
}

struct EmitState {
    fn_map: HashMap<String, u16>,
    /// Maps function name → return type name (only for fns with a named return type).
    fn_return_types: HashMap<String, String>,
    type_map: HashMap<String, TypeInfo>,
    variant_map: HashMap<String, VariantInfo>,
    lambda_counter: u16,
    pending_lambdas: Vec<PendingLambda>,
    /// Names of methods declared in any `class` body -- used to route UFCS calls to CallMethod.
    class_method_names: std::collections::HashSet<String>,
    /// User-defined type tags assigned to named record/choice types (starts at 10).
    type_tag_map: HashMap<String, u16>,
    next_type_tag: u16,
}

fn ty_name_str(ty: &Ty, interner: &Interner) -> Option<String> {
    if let Ty::Named { name, .. } = ty {
        return Some(interner.resolve(*name).to_owned());
    }
    None
}

fn param_list_with_types(params: &[musi_ast::Param], interner: &Interner) -> Vec<(String, Option<String>)> {
    params.iter().map(|p| {
        let name = interner.resolve(p.name).to_owned();
        let type_name = p.ty.as_ref().and_then(|t| ty_name_str(t, interner));
        (name, type_name)
    }).collect()
}

fn register_fn_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::FnDef {
        attrs,
        modifiers,
        name,
        params,
        ret_ty,
        body,
        ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };

    let fn_name = interner.resolve(*name).to_owned();
    let native_abi: Box<str> = modifiers
        .iter()
        .find_map(|m| {
            if let Modifier::Native(Some(sym)) = m {
                Some(Box::from(interner.resolve(*sym)))
            } else {
                None
            }
        })
        .unwrap_or_else(|| Box::from(""));
    let is_native = body.is_none() || modifiers.iter().any(|m| matches!(m, Modifier::Native(_)));
    let is_export = modifiers.iter().any(|m| matches!(m, Modifier::Export));
    let flags_raw: u8 = if is_native { SymbolFlags::NATIVE } else { 0 }
        | if is_export { SymbolFlags::EXPORT } else { 0 };

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
                    AttrArg::Value {
                        value: LitValue::Str(sym),
                        ..
                    } => Intrinsic::from_name(interner.resolve(*sym)).map(Intrinsic::id),
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
        abi: native_abi,
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

    let _prev = state.fn_map.insert(fn_name.clone(), fn_idx);
    if let Some(ret_name) = ret_ty.as_ref().and_then(|t| ty_name_str(t, interner)) {
        let _prev = state.fn_return_types.insert(fn_name, ret_name);
    }
    Ok(())
}

fn register_record(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) {
    let Expr::Record {
        name: Some(name),
        fields,
        ..
    } = exprs.get(item_idx)
    else {
        return;
    };
    let type_name = interner.resolve(*name).to_owned();
    let field_names: Vec<String> = fields
        .iter()
        .map(|f| interner.resolve(f.name).to_owned())
        .collect();
    if !state.type_tag_map.contains_key(&type_name) {
        let tag = state.next_type_tag;
        state.next_type_tag = state.next_type_tag.saturating_add(1);
        let _prev = state.type_tag_map.insert(type_name.clone(), tag);
    }
    let _prev = state.type_map.insert(type_name, TypeInfo { field_names });
}

fn register_choice(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::Choice {
        name: Some(name),
        variants,
        ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };
    let type_name = interner.resolve(*name).to_owned();
    if !state.type_tag_map.contains_key(&type_name) {
        let tag = state.next_type_tag;
        state.next_type_tag = state.next_type_tag.saturating_add(1);
        let _prev = state.type_tag_map.insert(type_name.clone(), tag);
    }

    let max_payload = variants.iter().map(payload_count).max().unwrap_or(0);
    let total_field_count = max_payload + 1; // +1 for discriminant

    for (disc, variant) in variants.iter().enumerate() {
        let v_name = interner.resolve(variant.name).to_owned();
        let payload = payload_count(variant);

        let discriminant: i64 =
            if let Some(VariantPayload::Discriminant(LitValue::Int(v))) = &variant.payload {
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

fn register_class_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) {
    let Expr::ClassDef { members, .. } = exprs.get(item_idx) else {
        return;
    };
    for member in members {
        if let ClassMember::Method(method_idx) = member {
            if let Expr::FnDef { name, .. } = exprs.get(*method_idx) {
                let name_str = interner.resolve(*name).to_owned();
                let _inserted = state.class_method_names.insert(name_str);
            }
        }
    }
}

fn register_given_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::GivenDef { class_app, members, .. } = exprs.get(item_idx) else {
        return Ok(());
    };

    let type_tag: u16 = match class_app {
        Ty::Named { args, .. } if !args.is_empty() => {
            if let Ty::Named { name: arg_name, .. } = &args[0] {
                let arg_str = interner.resolve(*arg_name);
                if let Some(prim) = TypeTag::from_type_name(arg_str) {
                    prim as u16
                } else if let Some(&user_tag) = state.type_tag_map.get(arg_str) {
                    user_tag
                } else {
                    return Ok(());
                }
            } else {
                return Ok(());
            }
        }
        _ => return Ok(()),
    };

    let members = members.clone();
    for member in &members {
        let ClassMember::Method(method_idx) = member else {
            continue;
        };
        let fn_expr = exprs.get(*method_idx);
        let Expr::FnDef { name, params, body: Some(_), modifiers, .. } = fn_expr else {
            continue;
        };
        if modifiers.iter().any(|m| matches!(m, Modifier::Native(_))) {
            continue;
        }
        let fn_name = interner.resolve(*name).to_owned();
        let param_count =
            u8::try_from(params.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;

        let sym_idx = module.push_symbol(SymbolEntry {
            name: fn_name.clone().into_boxed_str(),
            flags: SymbolFlags::new(0),
            intrinsic_id: intrinsics::NONE_ID,
            abi: Box::from(""),
        })?;
        let fn_idx = module.push_function(FunctionEntry {
            symbol_idx: sym_idx,
            param_count,
            local_count: 0,
            code_offset: 0,
            code_length: 0,
        })?;

        module.method_table.push(MethodEntry {
            name: fn_name.clone().into_boxed_str(),
            type_tag,
            fn_idx,
        });
        // Do NOT insert into fn_map: class methods are dispatched via CallMethod,
        // not via static Call. Inserting would overwrite other implementations' entries.
    }
    Ok(())
}

fn register_module_decls(
    parsed: &ParsedModule,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let exprs = &parsed.ctx.exprs;
    for &item_idx in &parsed.items {
        register_fn_def(item_idx, exprs, interner, module, state)?;
        register_record(item_idx, exprs, interner, state);
        register_choice(item_idx, exprs, interner, state)?;
        register_class_def(item_idx, exprs, interner, state);
        register_given_def(item_idx, exprs, interner, module, state)?;
    }
    Ok(())
}

fn payload_count(v: &ChoiceVariant) -> u16 {
    match &v.payload {
        None | Some(VariantPayload::Discriminant(_)) => 0,
        Some(VariantPayload::Positional(tys)) => u16::try_from(tys.len()).unwrap_or(u16::MAX),
        Some(VariantPayload::Named(fields)) => u16::try_from(fields.len()).unwrap_or(u16::MAX),
    }
}

fn emit_fn_body(
    fn_idx: u16,
    params: &[(String, Option<String>)],
    body_idx: Idx<Expr>,
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let mut out = FnEmitter::new();
    out.push_scope();
    for (name, type_name_opt) in params {
        let slot = out.define_local(name)?;
        if let Some(type_name) = type_name_opt {
            if state.type_map.contains_key(type_name) {
                let _prev = out.local_types.insert(slot, type_name.clone());
            }
        }
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
    let code_length = u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let entry = module
        .function_table
        .get_mut(usize::from(fn_idx))
        .ok_or(CodegenError::UnsupportedExpr)?;
    entry.local_count = local_count;
    entry.code_offset = code_offset;
    entry.code_length = code_length;
    Ok(())
}

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
            if let Some(vinfo) = state.variant_map.get(name_str).cloned() {
                let disc = vinfo.discriminant;
                let total = vinfo.total_field_count;
                let type_tag = state.type_tag_map.get(&vinfo.type_name).copied().unwrap_or(0);
                out.push(&Opcode::LdImmI64(disc));
                for _ in 0..total.saturating_sub(1) {
                    out.push(&Opcode::LdImmUnit);
                }
                out.push(&Opcode::NewObj { type_tag, field_count: total });
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

            if let Some(init_idx) = init {
                let init_expr = arenas.exprs.get(*init_idx);
                track_type_of_binding(init_expr, slot, arenas, state, out);
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

        Expr::Binary { op, lhs, rhs, .. } => {
            emit_binary(arenas, state, *op, *lhs, *rhs, module, out)
        }

        Expr::Block { stmts, tail, .. } => {
            out.push_scope();
            for stmt_idx in stmts {
                let stmt = arenas.exprs.get(*stmt_idx).clone();
                emit_expr(arenas, state, &stmt, module, out)?;
                out.push(&Opcode::Drop);
            }
            emit_expr_or_unit(arenas, state, *tail, module, out)?;
            out.pop_scope();
            Ok(())
        }

        Expr::Postfix { base, op, .. } => emit_postfix(arenas, state, *base, op, module, out),

        Expr::Lambda { params, body, .. } => {
            let lambda_name = format!("__lambda_{}", state.lambda_counter);
            state.lambda_counter = state
                .lambda_counter
                .checked_add(1)
                .ok_or(CodegenError::TooManyFunctions)?;

            let params_with_types = param_list_with_types(params, arenas.interner);
            let param_count = u8::try_from(params_with_types.len())
                .map_err(|_| CodegenError::ParameterCountOverflow)?;

            let sym_idx = module.push_symbol(SymbolEntry {
                name: lambda_name.clone().into_boxed_str(),
                flags: SymbolFlags::new(0),
                intrinsic_id: intrinsics::NONE_ID,
                abi: Box::from(""),
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
                params: params_with_types,
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
        } => emit_if(
            arenas,
            state,
            IfData {
                cond,
                then_body: *then_body,
                elif_chains,
                else_body: *else_body,
            },
            module,
            out,
        ),

        Expr::While {
            cond, guard, body, ..
        } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_while(arenas, state, cond, *body, module, out)
        }

        Expr::Loop {
            body, post_cond, ..
        } => {
            if post_cond.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_loop(arenas, state, *body, module, out)
        }

        Expr::For {
            pat,
            iter,
            guard,
            body,
            ..
        } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_for(arenas, state, pat, *iter, *body, module, out)
        }

        Expr::Break { label, value, .. } => {
            if label.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_expr_or_unit(arenas, state, *value, module, out)?;
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
            emit_expr_or_unit(arenas, state, *value, module, out)?;
            out.push(&Opcode::Ret);
            Ok(())
        }

        Expr::Match {
            scrutinee, arms, ..
        } => emit_match(arenas, state, *scrutinee, arms, module, out),

        Expr::DotPrefix { name, args, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if let Some(vinfo) = state.variant_map.get(name_str).cloned() {
                return emit_variant_construct(&vinfo, args, arenas, state, module, out);
            }
            if let Some(&fn_idx) = state.fn_map.get(name_str) {
                return emit_static_call(fn_idx, args, arenas, state, module, out);
            }
            Err(CodegenError::UnknownFunction(name_str.into()))
        }

        Expr::Tuple { .. }
        | Expr::Array { .. }
        | Expr::AnonRec { .. }
        | Expr::Label { .. }
        | Expr::Defer { .. }
        | Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Using { .. }
        | Expr::Record { .. }
        | Expr::Choice { .. }
        | Expr::FnDef { .. }
        | Expr::ClassDef { .. }
        | Expr::GivenDef { .. }
        | Expr::Error { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

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
        PostfixOp::OptField { .. } | PostfixOp::Index { .. } | PostfixOp::As { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

fn emit_args(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    args: &[Idx<Expr>],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    for &arg_idx in args {
        let arg = arenas.exprs.get(arg_idx).clone();
        emit_expr(arenas, state, &arg, module, out)?;
    }
    Ok(())
}

fn emit_variant_construct(
    vinfo: &VariantInfo,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let type_tag = state.type_tag_map.get(&vinfo.type_name).copied().unwrap_or(0);
    out.push(&Opcode::LdImmI64(vinfo.discriminant));
    emit_args(arenas, state, args, module, out)?;
    let provided = u16::try_from(args.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
    for _ in provided..vinfo.payload_count {
        out.push(&Opcode::LdImmUnit);
    }
    for _ in vinfo.payload_count.saturating_add(1)..vinfo.total_field_count {
        out.push(&Opcode::LdImmUnit);
    }
    out.push(&Opcode::NewObj { type_tag, field_count: vinfo.total_field_count });
    Ok(())
}

fn emit_static_call(
    fn_idx: u16,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    out.push(&Opcode::Call(fn_idx));
    Ok(())
}

fn emit_local_fn_call(
    slot: u16,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    out.push(&Opcode::LdLoc(slot));
    out.push(&Opcode::CallDynamic);
    Ok(())
}

fn receiver_has_field(
    recv_idx: Idx<Expr>,
    field_name: &str,
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    out: &FnEmitter,
) -> bool {
    (|| -> Option<bool> {
        let Expr::Ident {
            name: recv_name, ..
        } = arenas.exprs.get(recv_idx)
        else {
            return None;
        };
        let recv_str = arenas.interner.resolve(*recv_name);
        let slot = out.lookup_local(recv_str)?;
        let type_name = out.local_types.get(&slot)?;
        let tinfo = state.type_map.get(type_name)?;
        Some(tinfo.field_names.iter().any(|f| f == field_name))
    })()
    .unwrap_or(false)
}

fn emit_ufcs_call(
    recv_idx: Idx<Expr>,
    method_sym: musi_shared::Symbol,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let method_name = arenas.interner.resolve(method_sym);

    if receiver_has_field(recv_idx, method_name, arenas, state, out) {
        emit_args(arenas, state, args, module, out)?;
        emit_field_access(arenas, state, recv_idx, method_sym, module, out)?;
        out.push(&Opcode::CallDynamic);
        return Ok(());
    }

    // Free function takes priority over class dispatch (user-defined fn shadows class method).
    if let Some(&fn_idx) = state.fn_map.get(method_name) {
        let recv_expr = arenas.exprs.get(recv_idx).clone();
        emit_expr(arenas, state, &recv_expr, module, out)?;
        emit_args(arenas, state, args, module, out)?;
        out.push(&Opcode::Call(fn_idx));
        return Ok(());
    }

    if state.class_method_names.contains(method_name) {
        let arg_count =
            u16::try_from(1 + args.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
        let name_const_idx = module.add_string_const(method_name)?;
        let recv_expr = arenas.exprs.get(recv_idx).clone();
        emit_expr(arenas, state, &recv_expr, module, out)?;
        emit_args(arenas, state, args, module, out)?;
        out.push(&Opcode::CallMethod { method_idx: name_const_idx, arg_count });
        return Ok(());
    }

    Err(CodegenError::UnknownFunction(method_name.into()))
}

fn emit_expr_call(
    callee: &Expr,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    emit_expr(arenas, state, callee, module, out)?;
    out.push(&Opcode::CallDynamic);
    Ok(())
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
        if let Some(vinfo) = state.variant_map.get(callee_str).cloned() {
            return emit_variant_construct(&vinfo, args, arenas, state, module, out);
        }
        if let Some(&fn_idx) = state.fn_map.get(callee_str) {
            return emit_static_call(fn_idx, args, arenas, state, module, out);
        }
        if let Some(slot) = out.lookup_local(callee_str) {
            return emit_local_fn_call(slot, args, arenas, state, module, out);
        }
        return Err(CodegenError::UnknownFunction(callee_str.into()));
    }

    if let Expr::Postfix {
        base: recv_idx,
        op: PostfixOp::Field {
            name: method_sym, ..
        },
        ..
    } = base_expr.clone()
    {
        return emit_ufcs_call(recv_idx, method_sym, args, arenas, state, module, out);
    }

    let callee = base_expr.clone();
    emit_expr_call(&callee, args, arenas, state, module, out)
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

    if let Expr::Ident {
        name: base_name, ..
    } = base_expr
    {
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
        let field_idx_u16 = u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
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
    let Expr::Ident {
        name: type_name_sym,
        ..
    } = base_expr
    else {
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

    let spread_expr: Option<Idx<Expr>> = fields.iter().find_map(|fi| {
        if let FieldInit::Spread { expr, .. } = fi {
            Some(*expr)
        } else {
            None
        }
    });

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

    let spread_slot: Option<u16> = if let Some(spread_idx) = spread_expr {
        let temp_name = format!("$spread_{}", out.next_slot);
        let slot = out.define_local(&temp_name)?;
        let spread = arenas.exprs.get(spread_idx).clone();
        emit_expr(arenas, state, &spread, module, out)?;
        out.push(&Opcode::StLoc(slot));
        if let Expr::Ident {
            name: spread_name, ..
        } = arenas.exprs.get(spread_idx)
        {
            let spread_name_str = arenas.interner.resolve(*spread_name);
            if let Some(src_slot) = out.lookup_local(spread_name_str)
                && let Some(t) = out.local_types.get(&src_slot).cloned()
            {
                let _prev = out.local_types.insert(slot, t);
            }
        }
        Some(slot)
    } else {
        None
    };

    for (field_idx, field_name) in declared_fields.iter().enumerate() {
        if let Some(&val_idx) = explicit.get(field_name) {
            let val = arenas.exprs.get(val_idx).clone();
            emit_expr(arenas, state, &val, module, out)?;
        } else if let Some(spread_slot_id) = spread_slot {
            let fidx = u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
            out.push(&Opcode::LdLoc(spread_slot_id));
            out.push(&Opcode::LdFld(fidx));
        } else {
            return Err(CodegenError::UnknownField(
                field_name.clone().into_boxed_str(),
            ));
        }
    }
    let type_tag = state.type_tag_map.get(&type_name).copied().unwrap_or(0);
    out.push(&Opcode::NewObj { type_tag, field_count });
    Ok(())
}

fn emit_match(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    scrutinee: Idx<Expr>,
    arms: &[MatchArm],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let scrutinee_name = format!("$scrutinee_{}", out.next_slot);
    let scrutinee_slot = out.define_local(&scrutinee_name)?;
    let scr = arenas.exprs.get(scrutinee).clone();
    emit_expr(arenas, state, &scr, module, out)?;
    out.push(&Opcode::StLoc(scrutinee_slot));

    let mut end_fixups: Vec<usize> = Vec::new();

    for arm in arms {
        let next_arm_fixup =
            emit_pattern_test(arenas, state, &arm.pat, scrutinee_slot, module, out)?;

        let guard_fixup: Option<usize> = if let Some(guard_idx) = arm.guard {
            out.push_scope();
            emit_pattern_bindings(arenas, state, &arm.pat, scrutinee_slot, out)?;
            let guard_expr = arenas.exprs.get(guard_idx).clone();
            emit_expr(arenas, state, &guard_expr, module, out)?;
            let fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
            Some(fixup)
        } else {
            out.push_scope();
            emit_pattern_bindings(arenas, state, &arm.pat, scrutinee_slot, out)?;
            None
        };

        let body = arenas.exprs.get(arm.body).clone();
        emit_expr(arenas, state, &body, module, out)?;
        out.pop_scope();

        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));

        if let Some(fixup) = guard_fixup {
            out.patch_jump_to_here(fixup)?;
        }
        if let Some(fixup) = next_arm_fixup {
            out.patch_jump_to_here(fixup)?;
        }
    }

    out.push(&Opcode::HaltError);

    for fixup in end_fixups {
        out.patch_jump_to_here(fixup)?;
    }
    Ok(())
}

fn emit_discriminant_test(disc: i64, scrutinee_slot: u16, out: &mut FnEmitter) -> usize {
    out.push(&Opcode::LdLoc(scrutinee_slot));
    out.push(&Opcode::LdTag);
    out.push(&Opcode::LdImmI64(disc));
    out.push(&Opcode::EqI64);
    out.emit_jump_placeholder(FnEmitter::BR_FALSE)
}

/// Returns `(variant_name, sub_patterns)` for patterns that match a choice variant.
/// Covers both `Name(args)` (Pat::Ident + Positional) and `.Name(args)` (Pat::DotPrefix).
fn variant_name_and_args<'p>(
    pat: &'p Pat,
    interner: &'p Interner,
) -> Option<(&'p str, &'p [Pat])> {
    match pat {
        Pat::Ident {
            name,
            suffix: Some(PatSuffix::Positional { args, .. }),
            ..
        } => Some((interner.resolve(*name), args.as_slice())),
        Pat::DotPrefix { name, args, .. } => Some((interner.resolve(*name), args.as_slice())),
        _ => None,
    }
}

fn emit_pattern_test(
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    _module: &mut Module,
    out: &mut FnEmitter,
) -> Result<Option<usize>, CodegenError> {
    match pat {
        Pat::Wild { .. } => Ok(None),

        Pat::Ident {
            name, suffix: None, ..
        } => {
            let name_str = arenas.interner.resolve(*name);
            state.variant_map.get(name_str).map_or(Ok(None), |vinfo| {
                let fixup = emit_discriminant_test(vinfo.discriminant, scrutinee_slot, out);
                Ok(Some(fixup))
            })
        }

        Pat::Lit { value, .. } => {
            out.push(&Opcode::LdLoc(scrutinee_slot));
            match value {
                LitValue::Int(v) => out.push(&Opcode::LdImmI64(*v)),
                _ => return Err(CodegenError::UnsupportedExpr),
            }
            out.push(&Opcode::EqI64);
            let fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
            Ok(Some(fixup))
        }

        pat => {
            if let Some((variant_name, _)) = variant_name_and_args(pat, arenas.interner) {
                let vinfo = state
                    .variant_map
                    .get(variant_name)
                    .ok_or_else(|| CodegenError::UnknownVariant(variant_name.into()))?;
                let fixup = emit_discriminant_test(vinfo.discriminant, scrutinee_slot, out);
                Ok(Some(fixup))
            } else {
                Err(CodegenError::UnsupportedExpr)
            }
        }
    }
}

fn emit_pattern_bindings(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match pat {
        Pat::Ident {
            name, suffix: None, ..
        } => {
            let name_str = arenas.interner.resolve(*name);
            if !state.variant_map.contains_key(name_str) {
                let slot = out.define_local(name_str)?;
                out.push(&Opcode::LdLoc(scrutinee_slot));
                out.push(&Opcode::StLoc(slot));
            }
            Ok(())
        }

        Pat::Wild { .. } | Pat::Lit { .. } => Ok(()),

        pat => {
            if let Some((_, sub_pats)) = variant_name_and_args(pat, arenas.interner) {
                for (field_i, sub_pat) in sub_pats.iter().enumerate() {
                    let field_idx =
                        u16::try_from(field_i + 1).map_err(|_| CodegenError::UnsupportedExpr)?;
                    match sub_pat {
                        Pat::Wild { .. } => {}
                        Pat::Ident {
                            name, suffix: None, ..
                        } => {
                            let name_str = arenas.interner.resolve(*name);
                            let sub_slot = out.define_local(name_str)?;
                            out.push(&Opcode::LdLoc(scrutinee_slot));
                            out.push(&Opcode::LdFld(field_idx));
                            out.push(&Opcode::StLoc(sub_slot));
                        }
                        _ => {
                            let temp_name =
                                format!("$sub_{}_{}_{}", scrutinee_slot, field_i, out.next_slot);
                            let temp_slot = out.define_local(&temp_name)?;
                            out.push(&Opcode::LdLoc(scrutinee_slot));
                            out.push(&Opcode::LdFld(field_idx));
                            out.push(&Opcode::StLoc(temp_slot));
                            emit_pattern_bindings(arenas, state, sub_pat, temp_slot, out)?;
                        }
                    }
                }
                Ok(())
            } else {
                Err(CodegenError::UnsupportedExpr)
            }
        }
    }
}

fn track_type_of_binding(
    init_expr: &Expr,
    slot: u16,
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    out: &mut FnEmitter,
) {
    match init_expr {
        Expr::Postfix {
            base,
            op: PostfixOp::RecDot { .. },
            ..
        } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let type_name = arenas.interner.resolve(*name);
                if state.type_map.contains_key(type_name) {
                    let _prev = out.local_types.insert(slot, type_name.to_owned());
                }
            }
        }
        Expr::Postfix {
            base,
            op: PostfixOp::Call { .. },
            ..
        } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let callee_name = arenas.interner.resolve(*name);
                // Variant constructor: SomeVariant(args)
                if let Some(vinfo) = state.variant_map.get(callee_name) {
                    let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
                } else if let Some(ret_type) = state.fn_return_types.get(callee_name).cloned() {
                    // Function call with known return type
                    if state.type_map.contains_key(&ret_type) {
                        let _prev = out.local_types.insert(slot, ret_type);
                    }
                }
            } else if let Expr::Postfix {
                base: recv_idx,
                op: PostfixOp::Field { .. },
                ..
            } = arenas.exprs.get(*base)
            {
                // UFCS call: receiver.method(args) — propagate receiver's type
                if let Expr::Ident { name: recv_name, .. } = arenas.exprs.get(*recv_idx) {
                    let recv_str = arenas.interner.resolve(*recv_name);
                    if let Some(recv_slot) = out.lookup_local(recv_str) {
                        if let Some(type_name) = out.local_types.get(&recv_slot).cloned() {
                            let _prev = out.local_types.insert(slot, type_name);
                        }
                    }
                }
            }
        }
        Expr::Ident { name, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if let Some(vinfo) = state.variant_map.get(name_str) {
                let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
            }
        }
        _ => {}
    }
}

fn emit_expr_or_unit(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    opt: Option<Idx<Expr>>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    if let Some(idx) = opt {
        let expr = arenas.exprs.get(idx).clone();
        emit_expr(arenas, state, &expr, module, out)
    } else {
        out.push(&Opcode::LdImmUnit);
        Ok(())
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
        BinOp::Xor => Opcode::NeqBool,
        BinOp::BitAnd => Opcode::BitAnd,
        BinOp::Shl => Opcode::Shl,
        BinOp::Shr => Opcode::Shr,
        BinOp::Eq => Opcode::EqI64,
        BinOp::NotEq => Opcode::NeqI64,
        BinOp::Lt => Opcode::LtI64,
        BinOp::Gt => Opcode::GtI64,
        BinOp::LtEq => Opcode::LeqI64,
        BinOp::GtEq => Opcode::GeqI64,
        BinOp::NilCoalesce => {
            out.push(&Opcode::NilCoalesce);
            return Ok(());
        }
        BinOp::In | BinOp::Range | BinOp::RangeExcl | BinOp::Cons | BinOp::And | BinOp::Or => {
            return Err(CodegenError::UnsupportedExpr);
        }
    };
    out.push(&opcode);
    Ok(())
}

#[derive(Clone, Copy)]
struct IfData<'a> {
    cond: &'a Cond,
    then_body: Idx<Expr>,
    elif_chains: &'a [ElifBranch],
    else_body: Option<Idx<Expr>>,
}

fn emit_if(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    data: IfData<'_>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let else_fixup = match data.cond {
        Cond::Expr(_) => {
            emit_cond(arenas, state, data.cond, module, out)?;
            out.emit_jump_placeholder(FnEmitter::BR_FALSE)
        }
        Cond::Case { pat, init, .. } => {
            let (tmp_slot, test_fixup) =
                emit_case_cond(arenas, state, pat, *init, module, out)?;
            let fixup = test_fixup
                .map(Ok)
                .unwrap_or_else(|| Ok(out.emit_jump_placeholder(FnEmitter::BR_FALSE)))?;
            out.push_scope();
            emit_pattern_bindings(arenas, state, pat, tmp_slot, out)?;
            // bindings scope is popped after the then-body below
            fixup
        }
    };

    let then = arenas.exprs.get(data.then_body).clone();
    emit_expr(arenas, state, &then, module, out)?;
    if matches!(data.cond, Cond::Case { .. }) {
        out.pop_scope();
    }
    let first_end_fixup = out.emit_jump_placeholder(FnEmitter::BR);
    out.patch_jump_to_here(else_fixup)?;

    let mut end_fixups = vec![first_end_fixup];
    for branch in data.elif_chains {
        if branch.guard.is_some() {
            return Err(CodegenError::UnsupportedExpr);
        }
        let next_fixup = match branch.cond.as_ref() {
            Cond::Expr(_) => {
                emit_cond(arenas, state, &branch.cond, module, out)?;
                out.emit_jump_placeholder(FnEmitter::BR_FALSE)
            }
            Cond::Case { pat, init, .. } => {
                let (tmp_slot, test_fixup) =
                    emit_case_cond(arenas, state, pat, *init, module, out)?;
                let fixup = test_fixup
                    .map(Ok)
                    .unwrap_or_else(|| Ok(out.emit_jump_placeholder(FnEmitter::BR_FALSE)))?;
                out.push_scope();
                emit_pattern_bindings(arenas, state, pat, tmp_slot, out)?;
                fixup
            }
        };
        let branch_body = arenas.exprs.get(branch.body).clone();
        emit_expr(arenas, state, &branch_body, module, out)?;
        if matches!(branch.cond.as_ref(), Cond::Case { .. }) {
            out.pop_scope();
        }
        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));
        out.patch_jump_to_here(next_fixup)?;
    }

    emit_expr_or_unit(arenas, state, data.else_body, module, out)?;
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

/// Emits a `Cond::Case` test: evaluates init into a fresh temp slot, runs the
/// pattern test (leaving a bool on the stack), and returns `(tmp_slot, fixup)`.
/// The caller is responsible for emitting pattern bindings from `tmp_slot` after
/// the branch decision, and patching `fixup` to point to the else/exit target.
fn emit_case_cond(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    init: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(u16, Option<usize>), CodegenError> {
    let tmp_name = format!("$case_tmp_{}", out.next_slot);
    let tmp_slot = out.define_local(&tmp_name)?;
    let init_expr = arenas.exprs.get(init).clone();
    emit_expr(arenas, state, &init_expr, module, out)?;
    out.push(&Opcode::StLoc(tmp_slot));
    let fixup = emit_pattern_test(arenas, state, pat, tmp_slot, module, out)?;
    Ok((tmp_slot, fixup))
}

fn emit_while(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    body: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match cond {
        Cond::Expr(_) => {
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
        Cond::Case { pat, init, .. } => {
            let pat = pat.clone();
            let init = *init;
            let start_pos = out.start_loop();
            let (tmp_slot, test_fixup) =
                emit_case_cond(arenas, state, &pat, init, module, out)?;
            let end_fixup = test_fixup
                .map(Ok)
                .unwrap_or_else(|| Ok(out.emit_jump_placeholder(FnEmitter::BR_FALSE)))?;
            out.push_scope();
            emit_pattern_bindings(arenas, state, &pat, tmp_slot, out)?;
            let body_expr = arenas.exprs.get(body).clone();
            emit_expr(arenas, state, &body_expr, module, out)?;
            out.pop_scope();
            out.push(&Opcode::Drop);
            out.emit_br_back(start_pos)?;
            out.patch_jump_to_here(end_fixup)?;
            out.close_loop()
        }
    }
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
    let Pat::Ident {
        name: pat_name,
        suffix: None,
        ..
    } = pat
    else {
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
            let raw = arenas.interner.resolve(*sym);
            let unquoted = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')).unwrap_or(raw);
            let s: Box<str> = unquoted.into();
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
        LitValue::Char(c) => {
            out.push(&Opcode::LdImmI64(i64::from(u32::from(*c))));
            Ok(())
        }
    }
}

fn emit_module_fn_bodies(
    parsed: &ParsedModule,
    interner: &Interner,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let arenas = EmitArenas {
        exprs: &parsed.ctx.exprs,
        interner,
    };
    let mut pending: Vec<(u16, Vec<(String, Option<String>)>, Idx<Expr>)> = Vec::new();
    for &item_idx in &parsed.items {
        match parsed.ctx.exprs.get(item_idx) {
            Expr::FnDef {
                name,
                params,
                body: Some(body_idx),
                modifiers,
                ..
            } => {
                if modifiers.iter().any(|m| matches!(m, Modifier::Native(_))) {
                    continue;
                }
                let fn_name = interner.resolve(*name).to_owned();
                let fn_idx = *state
                    .fn_map
                    .get(&fn_name)
                    .ok_or_else(|| CodegenError::UnknownFunction(fn_name.clone().into_boxed_str()))?;
                let param_info = param_list_with_types(params, interner);
                pending.push((fn_idx, param_info, *body_idx));
            }
            Expr::GivenDef { class_app, members, .. } => {
                // Determine the type_tag so we can find the exact method_table entry
                // for this given, even when multiple givens implement the same method name.
                let type_tag_opt: Option<u16> = match class_app {
                    Ty::Named { args, .. } if !args.is_empty() => {
                        if let Ty::Named { name: arg_name, .. } = &args[0] {
                            let arg_str = interner.resolve(*arg_name);
                            if let Some(prim) = TypeTag::from_type_name(arg_str) {
                                Some(prim as u16)
                            } else {
                                state.type_tag_map.get(arg_str).copied()
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                let Some(type_tag) = type_tag_opt else {
                    continue;
                };
                let members = members.clone();
                for member in &members {
                    let ClassMember::Method(method_idx) = member else {
                        continue;
                    };
                    let fn_expr = parsed.ctx.exprs.get(*method_idx);
                    let Expr::FnDef {
                        name,
                        params,
                        body: Some(body_idx),
                        modifiers,
                        ..
                    } = fn_expr
                    else {
                        continue;
                    };
                    if modifiers.iter().any(|m| matches!(m, Modifier::Native(_))) {
                        continue;
                    }
                    let fn_name = interner.resolve(*name).to_owned();
                    // Look up the exact fn_idx from method_table using (name, type_tag).
                    let fn_idx = match module.method_table.iter().find(|e| {
                        e.name.as_ref() == fn_name.as_str() && e.type_tag == type_tag
                    }) {
                        Some(entry) => entry.fn_idx,
                        None => continue,
                    };
                    let param_info = param_list_with_types(params, interner);
                    pending.push((fn_idx, param_info, *body_idx));
                }
            }
            _ => {}
        }
    }
    for (fn_idx, param_names, body_idx) in pending {
        emit_fn_body(fn_idx, &param_names, body_idx, &arenas, state, module)?;
    }
    emit_pending_lambdas(&arenas, state, module)
}

fn emit_main_body(
    user: &ParsedModule,
    interner: &Interner,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let arenas = EmitArenas {
        exprs: &user.ctx.exprs,
        interner,
    };
    let mut out = FnEmitter::new();
    out.push_scope();
    for &item_idx in &user.items {
        match user.ctx.exprs.get(item_idx) {
            Expr::FnDef { .. }
            | Expr::Record { .. }
            | Expr::Choice { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::ClassDef { .. }
            | Expr::GivenDef { .. } => {}
            expr => {
                let e = expr.clone();
                emit_expr(&arenas, state, &e, module, &mut out)?;
                out.push(&Opcode::Drop);
            }
        }
    }
    emit_pending_lambdas(&arenas, state, module)?;
    out.pop_scope();
    out.push(&Opcode::Halt);

    let local_count = out.local_count();
    let code_offset =
        u32::try_from(module.code.len()).map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = out.byte_len();
    out.flush_into(&mut module.code);
    let code_length = u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let sym_idx = module.push_symbol(SymbolEntry {
        name: "main".into(),
        flags: SymbolFlags::new(0),
        intrinsic_id: intrinsics::NONE_ID,
        abi: Box::from(""),
    })?;
    let _main = module.push_function(FunctionEntry {
        symbol_idx: sym_idx,
        param_count: 0,
        local_count,
        code_offset,
        code_length,
    })?;
    Ok(())
}

/// Compile a set of parsed modules into a bytecode `Module`.
///
/// # Errors
///
/// Returns `CodegenError` if any expression cannot be compiled.
pub fn emit(
    prelude: &ParsedModule,
    deps: &[&ParsedModule],
    user: &ParsedModule,
    interner: &Interner,
) -> Result<Module, CodegenError> {
    let mut module = Module::new();
    let mut state = EmitState {
        fn_map: HashMap::new(),
        fn_return_types: HashMap::new(),
        type_map: HashMap::new(),
        variant_map: HashMap::new(),
        lambda_counter: 0,
        pending_lambdas: Vec::new(),
        class_method_names: std::collections::HashSet::new(),
        type_tag_map: HashMap::new(),
        next_type_tag: 10,
    };

    register_module_decls(prelude, interner, &mut module, &mut state)?;
    for &dep in deps {
        register_module_decls(dep, interner, &mut module, &mut state)?;
    }
    register_module_decls(user, interner, &mut module, &mut state)?;

    emit_module_fn_bodies(prelude, interner, &mut state, &mut module)?;
    for &dep in deps {
        emit_module_fn_bodies(dep, interner, &mut state, &mut module)?;
    }
    emit_module_fn_bodies(user, interner, &mut state, &mut module)?;

    emit_main_body(user, interner, &mut state, &mut module)?;

    Ok(module)
}

fn emit_pending_lambdas(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    loop {
        let pending = mem::take(&mut state.pending_lambdas);
        if pending.is_empty() {
            break;
        }
        for lam in pending {
            emit_fn_body(
                lam.fn_idx,
                &lam.params,
                lam.body,
                arenas,
                state,
                module,
            )?;
        }
    }
    Ok(())
}
