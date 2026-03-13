//! Expression emission: tree-walks AST expressions, leaving results on the stack.

use musi_bc::Opcode;
use music_ast::Pat;
use music_ast::TyIdx;
use music_ast::ty::Ty;
use music_ast::expr::{
    Arg, ArrayElem, BinOp, BindKind, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param,
    PwArm, PwGuard, RecField, TypeCheckKind, UnaryOp,
};
use music_ast::lit::Lit;
use music_sema::DefId;
use music_sema::def::DefKind;
use music_sema::types::Type;
use music_shared::Symbol;

use std::collections::{HashMap, HashSet};

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use music_ast::ExprIdx;

use super::super::emitter::{Emitter, FnBytecode, FnEntry};
use super::FnCtx;
use super::desugar;

/// Where a captured variable lives in the parent scope.
#[derive(Clone, Copy)]
enum CaptureSource {
    /// Parent has it as a local at the given slot.
    Local(u32),
    /// Parent captured it as an upvalue at the given index.
    Upvalue(u16),
}

/// Walk the AST body and collect free variables — names that reference
/// definitions in an enclosing scope rather than local params/bindings.
fn collect_free_vars(
    em: &Emitter<'_>,
    body: ExprIdx,
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
) -> Vec<(DefId, CaptureSource)> {
    let mut found = Vec::new();
    let mut seen = HashSet::new();
    collect_free_vars_walk(em, body, local_defs, parent_locals, parent_upvalues, &mut found, &mut seen);
    found
}

fn cfv_walk_rec_fields(
    em: &Emitter<'_>,
    fields: &[RecField],
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
    found: &mut Vec<(DefId, CaptureSource)>,
    seen: &mut HashSet<DefId>,
) {
    for f in fields {
        match f {
            RecField::Named { value: Some(v), .. } => {
                collect_free_vars_walk(em, *v, local_defs, parent_locals, parent_upvalues, found, seen);
            }
            RecField::Spread { expr, .. } => {
                collect_free_vars_walk(em, *expr, local_defs, parent_locals, parent_upvalues, found, seen);
            }
            RecField::Named { value: None, .. } => {}
        }
    }
}

fn cfv_walk_piecewise(
    em: &Emitter<'_>,
    arms: &[PwArm],
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
    found: &mut Vec<(DefId, CaptureSource)>,
    seen: &mut HashSet<DefId>,
) {
    for arm in arms {
        if let PwGuard::When { expr, .. } = arm.guard {
            collect_free_vars_walk(em, expr, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        collect_free_vars_walk(em, arm.result, local_defs, parent_locals, parent_upvalues, found, seen);
    }
}

#[allow(clippy::too_many_arguments)]
fn cfv_walk_match(
    em: &Emitter<'_>,
    scrutinee: ExprIdx,
    arms: &[MatchArm],
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
    found: &mut Vec<(DefId, CaptureSource)>,
    seen: &mut HashSet<DefId>,
) {
    collect_free_vars_walk(em, scrutinee, local_defs, parent_locals, parent_upvalues, found, seen);
    for arm in arms {
        if let Some(g) = arm.guard {
            collect_free_vars_walk(em, g, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        collect_free_vars_walk(em, arm.result, local_defs, parent_locals, parent_upvalues, found, seen);
    }
}

fn collect_free_vars_walk(
    em: &Emitter<'_>,
    expr_idx: ExprIdx,
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
    found: &mut Vec<(DefId, CaptureSource)>,
    seen: &mut HashSet<DefId>,
) {
    macro_rules! recurse {
        ($e:expr) => {
            collect_free_vars_walk(em, $e, local_defs, parent_locals, parent_upvalues, found, seen)
        };
    }
    match &em.ast.exprs[expr_idx] {
        Expr::Name { .. } => {
            let Some(&def_id) = em.sema.resolution.expr_defs.get(&expr_idx) else { return };
            if local_defs.contains(&def_id)
                || em.fn_map.contains_key(&def_id)
                || em.foreign_map.contains_key(&def_id)
                || seen.contains(&def_id)
            {
                return;
            }
            if let Some(&slot) = parent_locals.get(&def_id) {
                let _ = seen.insert(def_id);
                found.push((def_id, CaptureSource::Local(slot)));
            } else if let Some(&idx) = parent_upvalues.get(&def_id) {
                let _ = seen.insert(def_id);
                found.push((def_id, CaptureSource::Upvalue(idx)));
            }
        }
        Expr::Lit { .. }
        | Expr::Error { .. }
        | Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Choice { .. }
        | Expr::RecordDef { .. }
        | Expr::Class { .. }
        | Expr::Effect { .. }
        | Expr::Foreign { .. }
        | Expr::Instance { .. }
        | Expr::Return { value: None, .. } => {}
        Expr::Paren { inner, .. }
        | Expr::Annotated { inner, .. }
        | Expr::Return { value: Some(inner), .. }
        | Expr::Field { object: inner, .. }
        | Expr::UnaryOp { operand: inner, .. }
        | Expr::Fn { body: inner, .. }
        | Expr::TypeCheck { operand: inner, .. } => recurse!(*inner),
        Expr::BinOp { left, right, .. } | Expr::Index { object: left, index: right, .. } => {
            recurse!(*left);
            recurse!(*right);
        }
        Expr::Block { stmts, tail, .. } => {
            for &s in stmts { recurse!(s); }
            if let Some(t) = *tail { recurse!(t); }
        }
        Expr::Let { fields, body, .. } => {
            if let Some(v) = fields.value { recurse!(v); }
            if let Some(b) = *body { recurse!(b); }
        }
        Expr::Binding { fields, .. } => {
            if let Some(v) = fields.value { recurse!(v); }
        }
        Expr::Call { callee, args, .. } => {
            recurse!(*callee);
            for arg in args {
                let e = match arg { Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => *expr };
                recurse!(e);
            }
        }
        Expr::Tuple { elems, .. } => { for &e in elems { recurse!(e); } }
        Expr::Variant { args, .. } => { for &a in args { recurse!(a); } }
        Expr::Array { elems, .. } => {
            for e in elems {
                let idx = match e { ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => *expr };
                recurse!(idx);
            }
        }
        Expr::Record { fields, .. } => {
            cfv_walk_rec_fields(em, fields, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        Expr::Update { base, fields, .. } => {
            recurse!(*base);
            cfv_walk_rec_fields(em, fields, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        Expr::Piecewise { arms, .. } => {
            cfv_walk_piecewise(em, arms, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        Expr::Match { scrutinee, arms, .. } => {
            cfv_walk_match(em, *scrutinee, arms, local_defs, parent_locals, parent_upvalues, found, seen);
        }
        Expr::Handle { body, ops, .. } => {
            recurse!(*body);
            for op in ops { recurse!(op.body); }
        }
    }
}

/// Type family used to select the right arithmetic/comparison opcodes.
#[derive(Clone, Copy)]
pub enum TypeFamily {
    Signed,
    Unsigned,
    Float,
    Bool,
}

/// Emit bytecode for `expr_idx`. Returns `true` if a value was pushed onto the stack.
///
/// `is_tail` indicates whether this expression is in tail position of a function body.
/// When true, direct calls can be emitted as tail calls.
pub fn emit_expr(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
) -> Result<bool, EmitError> {
    emit_expr_tail(em, fc, expr_idx, false)
}

fn emit_type_check(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    kind: TypeCheckKind,
    operand: ExprIdx,
    ty: TyIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    match kind {
        TypeCheckKind::Test => emit_type_test(em, fc, operand, ty),
        TypeCheckKind::Cast => emit_type_cast(em, fc, operand, ty, is_tail),
    }
}

/// Emit bytecode for `expr_idx` in explicit tail position.
pub fn emit_expr_tail(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    match &em.ast.exprs[expr_idx] {
        Expr::Lit { lit, .. } => emit_lit(em, fc, lit),
        Expr::Name { name, span } => {
            let (name, span) = (*name, *span);
            emit_name(em, fc, expr_idx, name, span)
        }
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => {
            let inner = *inner;
            emit_expr_tail(em, fc, inner, is_tail)
        }
        Expr::Block { stmts, tail, .. } => {
            let (stmts, tail) = (stmts.clone(), *tail);
            emit_block(em, fc, &stmts, tail, is_tail)
        }
        Expr::Let { fields, body, .. } => {
            let (fields, body) = (fields.clone(), *body);
            emit_let(em, fc, &fields, body, is_tail)
        }
        Expr::Binding { fields, .. } => emit_binding(em, fc, &fields.clone()),
        Expr::BinOp { op, left, right, .. } => {
            let (op, left, right) = (*op, *left, *right);
            emit_binop_expr(em, fc, expr_idx, op, left, right)
        }
        Expr::UnaryOp { op, operand, span } => {
            let (op, operand, span) = (*op, *operand, *span);
            if op == UnaryOp::Do { emit_do(em, fc, operand, is_tail) } else { emit_unary(em, fc, op, operand, span) }
        }
        Expr::Call { callee, args, .. } => {
            let (callee, args) = (*callee, args.clone());
            emit_call(em, fc, callee, &args, is_tail)
        }
        Expr::Tuple { elems, .. } => emit_tuple(em, fc, &elems.clone()),
        Expr::Record { fields, .. } => emit_record_lit(em, fc, &fields.clone()),
        Expr::Array { elems, .. } => { emit_array(em, fc, &elems.clone())?; Ok(true) }
        Expr::Variant { name, args, .. } => emit_variant(em, fc, *name, &args.clone()),
        Expr::Field { object, field, .. } => emit_field(em, fc, *object, *field),
        Expr::Index { object, index, .. } => emit_index(em, fc, *object, *index),
        Expr::Return { value, .. } => emit_return(em, fc, *value),
        Expr::Piecewise { arms, .. } => {
            super::control::emit_piecewise(em, fc, &arms.clone(), is_tail)?;
            Ok(true)
        }
        Expr::Match { scrutinee, arms, .. } => {
            let (scrutinee, arms) = (*scrutinee, arms.clone());
            super::control::emit_match(em, fc, scrutinee, &arms, is_tail)?;
            Ok(true)
        }
        Expr::Fn { params, body, .. } => emit_fn(em, fc, &params.clone(), *body),
        Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Choice { .. }
        | Expr::RecordDef { .. }
        | Expr::Class { .. }
        | Expr::Instance { .. }
        | Expr::Effect { .. }
        | Expr::Foreign { .. }
        | Expr::Error { .. } => Ok(false),
        Expr::Update { base, fields, .. } => emit_update(em, fc, *base, &fields.clone()),
        Expr::TypeCheck { kind, operand, ty, .. } => {
            emit_type_check(em, fc, *kind, *operand, *ty, is_tail)
        }
        Expr::Handle { effect_ty, ops, body, .. } => {
            let (effect_ty, ops, body) = (*effect_ty, ops.clone(), *body);
            emit_handle(em, fc, effect_ty, &ops, body)
        }
    }
}

fn emit_name(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    name: music_shared::Symbol,
    _span: music_shared::Span,
) -> Result<bool, EmitError> {
    let name_str = em.interner.resolve(name);
    if name_str == "true" {
        let cv = ConstValue::Bool(true);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if name_str == "false" {
        let cv = ConstValue::Bool(false);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    let Some(&def_id) = em.sema.resolution.expr_defs.get(&expr_idx) else {
        return Err(EmitError::UnsupportedFeature {
            desc: format!("unresolved name `{}`", em.interner.resolve(name)).into(),
        });
    };
    if let Some(&fn_id) = em.fn_map.get(&def_id) {
        let cv = ConstValue::FnRef(fn_id);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
        let cv = ConstValue::FnRef(ffi_idx);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if let Some(&slot) = fc.local_map.get(&def_id) {
        fc.fe.emit_ld_loc(slot);
        if fc.ref_locals.contains(&def_id) {
            fc.fe.emit_ld_fld(0)?;
        }
        return Ok(true);
    }
    if let Some(&upv_idx) = fc.upvalue_map.get(&def_id) {
        let idx = u8::try_from(upv_idx).map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
        fc.fe.emit_ld_upv(idx);
        return Ok(true);
    }
    Err(EmitError::UnsupportedFeature {
        desc: format!("unresolved local `{}`", em.interner.resolve(name)).into(),
    })
}

fn emit_block(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    stmts: &[ExprIdx],
    tail: Option<ExprIdx>,
    is_tail: bool,
) -> Result<bool, EmitError> {
    for &stmt_idx in stmts {
        let produced = emit_expr(em, fc, stmt_idx)?;
        if produced {
            fc.fe.emit_pop();
        }
    }
    tail.map_or(Ok(false), |tail_idx| {
        emit_expr_tail(em, fc, tail_idx, is_tail)
    })
}

fn emit_let(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &LetFields,
    body: Option<ExprIdx>,
    is_tail: bool,
) -> Result<bool, EmitError> {
    if let Some(val_idx) = fields.value {
        let produced = emit_expr(em, fc, val_idx)?;
        if produced {
            if fields.kind == BindKind::Mut {
                bind_pat_ref(em, fc, fields.pat)?;
            } else {
                bind_pat(em, fc, fields.pat)?;
            }
        }
    }
    body.map_or(Ok(false), |body_idx| {
        emit_expr_tail(em, fc, body_idx, is_tail)
    })
}

fn emit_binding(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &LetFields,
) -> Result<bool, EmitError> {
    let Some(val_idx) = fields.value else {
        return Ok(false);
    };
    let is_fn = matches!(em.ast.exprs[val_idx], Expr::Fn { .. });
    if is_fn {
        return Ok(false);
    }
    let produced = emit_expr(em, fc, val_idx)?;
    if produced {
        if fields.kind == BindKind::Mut {
            bind_pat_ref(em, fc, fields.pat)?;
        } else {
            bind_pat(em, fc, fields.pat)?;
        }
    }
    Ok(false)
}

fn emit_unary(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    op: UnaryOp,
    operand: ExprIdx,
    _span: music_shared::Span,
) -> Result<bool, EmitError> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "unary operand produced no value".into(),
        });
    }
    match op {
        UnaryOp::Neg => {
            let family = classify_type_family(em, operand);
            let opcode = match family {
                Some(TypeFamily::Float) => Opcode::F_NEG,
                _ => Opcode::I_NEG,
            };
            fc.fe.emit_unop(opcode);
            Ok(true)
        }
        UnaryOp::Not => {
            fc.fe.emit_unop(Opcode::B_NOT);
            Ok(true)
        }
        UnaryOp::ForceUnwrap => emit_force_unwrap(em, fc, operand),
        UnaryOp::Propagate => {
            desugar::emit_propagate(em, fc, operand)?;
            Ok(true)
        }
        UnaryOp::Try => {
            desugar::emit_try(em, fc, operand)?;
            Ok(true)
        }
        UnaryOp::Defer => emit_defer(em, fc, operand),
        UnaryOp::Do => {
            // emit_expr_tail routes Do before calling emit_unary; reaching here is a bug.
            #[allow(clippy::unreachable)]
            { unreachable!("UnaryOp::Do is handled before emit_unary is called") }
        }
    }
}

fn emit_tuple(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ExprIdx]) -> Result<bool, EmitError> {
    let n = elems.len();
    for &e in elems {
        let produced = emit_expr(em, fc, e)?;
        if !produced {
            return Err(EmitError::UnsupportedFeature {
                desc: "tuple element produced no value".into(),
            });
        }
    }
    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("tuple element count"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("tuple element count"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
}

fn emit_field(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    object: ExprIdx,
    field: FieldKey,
) -> Result<bool, EmitError> {
    let produced = emit_expr(em, fc, object)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "field object produced no value".into(),
        });
    }
    let index = match field {
        FieldKey::Pos { index, .. } => index,
        FieldKey::Name { name, .. } => resolve_field_name(em, object, name)?,
    };
    fc.fe.emit_ld_fld(index)?;
    Ok(true)
}

fn emit_index(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    object: ExprIdx,
    index: ExprIdx,
) -> Result<bool, EmitError> {
    let produced_obj = emit_expr(em, fc, object)?;
    if !produced_obj {
        return Err(EmitError::UnsupportedFeature {
            desc: "index object produced no value".into(),
        });
    }
    let produced_idx = emit_expr(em, fc, index)?;
    if !produced_idx {
        return Err(EmitError::UnsupportedFeature {
            desc: "index value produced no value".into(),
        });
    }
    fc.fe.emit_ld_idx();
    Ok(true)
}

fn emit_return(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    value: Option<ExprIdx>,
) -> Result<bool, EmitError> {
    if let Some(val_idx) = value {
        let produced = emit_expr(em, fc, val_idx)?;
        if produced {
            if !fc.deferred.is_empty() {
                let tmp = fc.alloc_local();
                fc.fe.emit_st_loc(tmp);
                emit_deferred_cleanup(em, fc)?;
                fc.fe.emit_ld_loc(tmp);
            }
            fc.fe.emit_ret();
        } else {
            emit_deferred_cleanup(em, fc)?;
            fc.fe.emit_ret_u();
        }
    } else {
        emit_deferred_cleanup(em, fc)?;
        fc.fe.emit_ret_u();
    }
    Ok(false)
}

/// Resolve an AST type annotation (`TyIdx`) to a bytecode `type_id`.
///
/// First checks well-known types by name. If not found, walks sema defs to
/// find a matching named type and lowers its sema type. Returns `None` if
/// the type cannot be resolved (e.g. type variable, unresolved name).
fn lower_ast_ty_to_type_id(em: &mut Emitter<'_>, ty: TyIdx) -> Option<u32> {
    let name = match &em.ast.tys[ty] {
        Ty::Named { name, .. } | Ty::Var { name, .. } => *name,
        _ => return None,
    };
    let wk = &em.sema.well_known;
    let name_str = em.interner.resolve(name);
    // Well-known primitive types — match by name so they work even without sema ty_info.
    match name_str {
        "Bool" => return em.tp.lower_well_known_def(wk.bool, wk),
        "Int" | "Int64" => return em.tp.lower_well_known_def(wk.ints.int, wk),
        "Int8" => return em.tp.lower_well_known_def(wk.ints.int8, wk),
        "Int16" => return em.tp.lower_well_known_def(wk.ints.int16, wk),
        "Int32" => return em.tp.lower_well_known_def(wk.ints.int32, wk),
        "UInt8" => return em.tp.lower_well_known_def(wk.uints.uint8, wk),
        "UInt16" => return em.tp.lower_well_known_def(wk.uints.uint16, wk),
        "UInt32" => return em.tp.lower_well_known_def(wk.uints.uint32, wk),
        "UInt64" => return em.tp.lower_well_known_def(wk.uints.uint64, wk),
        "Float32" => return em.tp.lower_well_known_def(wk.floats.float32, wk),
        "Float" | "Float64" => return em.tp.lower_well_known_def(wk.floats.float64, wk),
        "Rune" => return em.tp.lower_well_known_def(wk.rune, wk),
        "String" => return em.tp.lower_well_known_def(wk.string, wk),
        "Unit" => return em.tp.lower_well_known_def(wk.unit, wk),
        _ => {}
    }
    // User-defined named type: find by name in sema defs.
    let def = em
        .sema
        .defs
        .iter()
        .find(|d| d.name == name && matches!(d.kind, DefKind::Type | DefKind::Effect))?;
    let ty_idx = def.ty_info.ty?;
    em.tp
        .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
        .ok()
}

fn emit_type_test(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
    ty: TyIdx,
) -> Result<bool, EmitError> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        let cv = ConstValue::Bool(true);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if let Some(type_id) = lower_ast_ty_to_type_id(em, ty) {
        fc.fe.emit_type_chk(type_id);
    } else {
        // Type couldn't be resolved — fall back to always-true.
        fc.fe.emit_pop();
        let cv = ConstValue::Bool(true);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
    }
    Ok(true)
}

/// Emit `:?>` (type cast): evaluate operand, check type, UNR on mismatch.
fn emit_type_cast(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
    ty: TyIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    let produced = emit_expr_tail(em, fc, operand, is_tail)?;
    if !produced {
        return Ok(false);
    }
    let Some(type_id) = lower_ast_ty_to_type_id(em, ty) else {
        // Can't resolve type — leave value on stack, assume caller knows what they're doing.
        return Ok(true);
    };
    // Duplicate so we still have the value after the check.
    fc.fe.emit_dup();
    fc.fe.emit_type_chk(type_id);
    // Jump past UNR if check passed.
    let ok_label = fc.fresh_label();
    fc.fe.emit_jmp_t(ok_label);
    fc.fe.emit_unop(musi_bc::Opcode::UNR);
    fc.fe.emit_label(ok_label);
    Ok(true)
}

fn emit_binop_expr(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    op: BinOp,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<bool, EmitError> {
    match op {
        BinOp::And => {
            desugar::emit_and(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::Or => {
            desugar::emit_or(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::Assign => {
            desugar::emit_assign(em, fc, left, right)?;
            Ok(false)
        }
        BinOp::Pipe => {
            desugar::emit_pipe(em, fc, expr_idx, left, right)?;
            Ok(true)
        }
        BinOp::ForceCoal => {
            desugar::emit_err_coal(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::NilCoal => {
            desugar::emit_nil_coal(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::In => Err(EmitError::UnsupportedFeature {
            desc: "binary operator `In`".into(),
        }),
        BinOp::RangeInc | BinOp::RangeExc => {
            desugar::emit_range(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::Cons => {
            desugar::emit_cons(em, fc, left, right)?;
            Ok(true)
        }
        _ => {
            let produced_left = emit_expr(em, fc, left)?;
            if !produced_left {
                return Err(EmitError::UnsupportedFeature {
                    desc: "binop left operand produced no value".into(),
                });
            }
            let produced_right = emit_expr(em, fc, right)?;
            if !produced_right {
                return Err(EmitError::UnsupportedFeature {
                    desc: "binop right operand produced no value".into(),
                });
            }
            let family = classify_type_family(em, left);
            let opcode = map_binop(op, family)?;
            fc.fe.emit_binop(opcode);
            Ok(true)
        }
    }
}

fn emit_call(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    callee: ExprIdx,
    args: &[Arg],
    is_tail: bool,
) -> Result<bool, EmitError> {
    let arg_count = emit_call_args(em, fc, args)?;
    if let Expr::Name { .. } = &em.ast.exprs[callee] {
        if let Some(&def_id) = em.sema.resolution.expr_defs.get(&callee) {
            if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
                let ac = i32::try_from(arg_count).map_err(|_| EmitError::overflow("arg count"))?;
                fc.fe.emit_inv_ffi(ffi_idx, ac);
                return Ok(true);
            }
            if let Some(&fn_id) = em.fn_map.get(&def_id) {
                if is_tail {
                    fc.fe.emit_inv_tail(fn_id, false);
                    return Ok(true);
                }
                let ac = i32::try_from(arg_count).map_err(|_| EmitError::overflow("arg count"))?;
                fc.fe.emit_inv(fn_id, false, ac);
                return Ok(true);
            }
            if let Some(&slot) = fc.local_map.get(&def_id) {
                fc.fe.emit_ld_loc(slot);
                let ac_i = i32::try_from(arg_count).map_err(|_| EmitError::overflow("arg count"))?;
                fc.fe.emit_inv_dyn(ac_i)?;
                return Ok(true);
            }
        }
        Err(EmitError::UnsupportedFeature {
            desc: "unresolved callee".into(),
        })
    } else {
        let produced = emit_expr(em, fc, callee)?;
        if !produced {
            return Err(EmitError::UnsupportedFeature {
                desc: "callee produced no value".into(),
            });
        }
        let ac_i = i32::try_from(arg_count).map_err(|_| EmitError::overflow("arg count"))?;
        fc.fe.emit_inv_dyn(ac_i)?;
        Ok(true)
    }
}

fn emit_record_lit(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
) -> Result<bool, EmitError> {
    let named_fields: Vec<_> = fields
        .iter()
        .filter_map(|f| match f {
            RecField::Named { value, .. } => Some(*value),
            RecField::Spread { .. } => None,
        })
        .collect();
    let n = named_fields.len();
    for val_opt in named_fields {
        if let Some(val_idx) = val_opt {
            let produced = emit_expr(em, fc, val_idx)?;
            if !produced {
                return Err(EmitError::UnsupportedFeature {
                    desc: "record field produced no value".into(),
                });
            }
        } else {
            return Err(EmitError::UnsupportedFeature {
                desc: "record field with no value".into(),
            });
        }
    }
    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
}

fn emit_variant(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    name: Symbol,
    args: &[ExprIdx],
) -> Result<bool, EmitError> {
    for &arg_idx in args {
        let produced = emit_expr(em, fc, arg_idx)?;
        if !produced {
            return Err(EmitError::UnsupportedFeature {
                desc: "variant arg produced no value".into(),
            });
        }
    }
    let tag = resolve_variant_tag(em, name)?;
    fc.fe.emit_mk_var(tag)?;
    Ok(true)
}

fn emit_fn(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    params: &[Param],
    body: ExprIdx,
) -> Result<bool, EmitError> {
    let nested_fn_id = em.alloc_fn_id();
    let nested_param_count =
        u16::try_from(params.len()).map_err(|_| EmitError::overflow("nested fn param count"))?;
    let nested_entry = FnEntry {
        fn_id: nested_fn_id,
        def_id: None,
        name: format!("<closure@{nested_fn_id}>"),
        params: params.to_vec(),
        body,
        effect_mask: 0,
    };

    let mut nested_fc = FnCtx::new(nested_param_count);

    // Build the set of locally-defined DefIds (params).
    let mut local_defs = HashSet::new();
    for (i, param) in params.iter().enumerate() {
        let slot = u32::try_from(i).map_err(|_| EmitError::overflow("nested fn param index"))?;
        if let Some(&did) = em.sema.resolution.pat_defs.get(&param.span) {
            let prev = nested_fc.local_map.insert(did, slot);
            debug_assert!(prev.is_none(), "duplicate local slot for def");
            let _ = local_defs.insert(did);
        } else {
            for def in &em.sema.defs {
                if def.kind == DefKind::Param && def.name == param.name && def.span == param.span {
                    let prev = nested_fc.local_map.insert(def.id, slot);
                    debug_assert!(prev.is_none(), "duplicate local slot for def");
                    let _ = local_defs.insert(def.id);
                    break;
                }
            }
        }
    }

    // Collect free variables from body that reference the parent scope.
    let captures = collect_free_vars(em, body, &local_defs, &fc.local_map, &fc.upvalue_map);
    let upvalue_count = u16::try_from(captures.len()).map_err(|_| EmitError::overflow("too many captured variables"))?;

    // Populate the nested function's upvalue_map.
    for (upv_idx, &(def_id, _)) in captures.iter().enumerate() {
        let idx = u16::try_from(upv_idx)
            .map_err(|_| EmitError::overflow("upvalue index in populate loop"))?;
        let _ = nested_fc.upvalue_map.insert(def_id, idx);
    }

    let had_value = emit_expr_tail(em, &mut nested_fc, body, true)?;
    if had_value {
        if !nested_fc.deferred.is_empty() {
            let tmp = nested_fc.alloc_local();
            nested_fc.fe.emit_st_loc(tmp);
            emit_deferred_cleanup(em, &mut nested_fc)?;
            nested_fc.fe.emit_ld_loc(tmp);
        }
        nested_fc.fe.emit_ret();
    } else {
        emit_deferred_cleanup(em, &mut nested_fc)?;
        nested_fc.fe.emit_ret_u();
    }
    nested_fc.fe.resolve_fixups(&nested_entry.name)?;
    let _code_len = nested_fc.fe.validate_code_len()?;
    let type_id = if let Some(&ty_idx) = em.sema.expr_types.get(&body) {
        em.tp
            .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
            .unwrap_or(0)
    } else {
        em.tp
            .lower_well_known_def(em.sema.well_known.unit, &em.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))?
    };
    let fn_bytecode = FnBytecode {
        fn_id: nested_fn_id,
        type_id,
        local_count: nested_fc.fe.local_count,
        param_count: nested_fc.fe.param_count,
        max_stack: nested_fc.fe.max_stack,
        effect_mask: 0,
        upvalue_count,
        code: nested_fc.fe.code,
        handlers: nested_fc.fe.handlers,
    };
    em.nested_fns.push(fn_bytecode);

    if upvalue_count == 0 {
        // Non-capturing lambda: use existing LD_CST FnRef path.
        let cv = ConstValue::FnRef(nested_fn_id);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
    } else {
        // Capturing closure: push each captured value, then MK_CLO.
        for &(_, source) in &captures {
            match source {
                CaptureSource::Local(slot) => fc.fe.emit_ld_loc(slot),
                CaptureSource::Upvalue(idx) => {
                    let u8_idx = u8::try_from(idx).map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
                    fc.fe.emit_ld_upv(u8_idx);
                }
            }
        }
        fc.fe.emit_mk_clo(nested_fn_id, upvalue_count);
    }
    Ok(true)
}

fn emit_lit(em: &mut Emitter<'_>, fc: &mut FnCtx, lit: &Lit) -> Result<bool, EmitError> {
    match lit {
        Lit::Unit { .. } => Ok(false),
        Lit::Int { value, .. } => {
            let cv = ConstValue::Int(*value);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::Float { value, .. } => {
            let cv = ConstValue::Float(*value);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::Rune { codepoint, .. } => {
            let cv = ConstValue::Rune(*codepoint);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::Str { value, .. } => {
            let cv = ConstValue::Str(*value);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::FStr { parts, .. } => {
            desugar::emit_fstr(em, fc, parts)?;
            Ok(true)
        }
    }
}

/// Consume the top-of-stack value and bind it to `pat_idx`.
pub fn bind_pat(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    pat_idx: music_ast::PatIdx,
) -> Result<(), EmitError> {
    match &em.ast.pats[pat_idx] {
        Pat::Wild { .. } => {
            fc.fe.emit_pop();
            Ok(())
        }
        Pat::Bind { span, .. } => {
            let span = *span;
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.sema.resolution.pat_defs.get(&span) {
                let prev = fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
            }
            Ok(())
        }
        Pat::Tuple { elems, .. } => {
            let elems = elems.clone();
            // Store product into a temp slot, then extract each field.
            let tmp = fc.alloc_local();
            fc.fe.emit_st_loc(tmp);
            for (i, elem) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| EmitError::overflow("tuple destructure index"))?;
                fc.fe.emit_ld_loc(tmp);
                fc.fe.emit_ld_fld(idx)?;
                bind_pat(em, fc, *elem)?;
            }
            Ok(())
        }
        _ => {
            // For other patterns (Lit, Variant, etc.) in binding position,
            // just store to a scratch slot and discard.
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            Ok(())
        }
    }
}

/// Bind a pattern as a ref cell: wraps the top-of-stack value in `ALC_REF`.
fn bind_pat_ref(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    pat_idx: music_ast::PatIdx,
) -> Result<(), EmitError> {
    match &em.ast.pats[pat_idx] {
        Pat::Bind { span, .. } => {
            let span = *span;
            let wk = &em.sema.well_known;
            let type_id = em.tp.lower_well_known_def(wk.any, wk).unwrap_or(0);
            fc.fe.emit_alc_ref(type_id);
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.sema.resolution.pat_defs.get(&span) {
                let prev = fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
                let is_new = fc.ref_locals.insert(did);
                debug_assert!(is_new, "duplicate ref local for def");
            }
            Ok(())
        }
        _ => bind_pat(em, fc, pat_idx),
    }
}

fn emit_call_args(em: &mut Emitter<'_>, fc: &mut FnCtx, args: &[Arg]) -> Result<usize, EmitError> {
    let mut count = 0usize;
    for &arg in args {
        match arg {
            Arg::Pos { expr, .. } => {
                let produced = emit_expr(em, fc, expr)?;
                if !produced {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "call arg produced no value".into(),
                    });
                }
                count += 1;
            }
            Arg::Spread { .. } => {
                return Err(EmitError::UnsupportedFeature {
                    desc: "spread argument".into(),
                });
            }
        }
    }
    Ok(count)
}

fn emit_array(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> Result<(), EmitError> {
    let count = u32::try_from(elems.len()).map_err(|_| EmitError::overflow("array element count"))?;
    fc.fe.emit_mk_arr(count);

    for (i, &elem) in elems.iter().enumerate() {
        let expr_idx = match elem {
            ArrayElem::Elem { expr, .. } => expr,
            ArrayElem::Spread { .. } => {
                return Err(EmitError::UnsupportedFeature {
                    desc: "spread array element".into(),
                });
            }
        };
        let idx_u32 = u32::try_from(i).map_err(|_| EmitError::overflow("array index"))?;
        // dup array ref, push index, push value, ST_IDX
        fc.fe.emit_dup();
        let idx_cv = ConstValue::Int(i64::from(idx_u32));
        let ci = em.cp.intern(&idx_cv, em.interner)?;
        fc.fe.emit_ld_cst(ci);
        let produced = emit_expr(em, fc, expr_idx)?;
        if !produced {
            return Err(EmitError::UnsupportedFeature {
                desc: "array element produced no value".into(),
            });
        }
        fc.fe.emit_st_idx();
    }
    Ok(())
}

/// Resolve a variant's tag by scanning the def table for sibling variants.
pub fn resolve_variant_tag_by_name(em: &Emitter<'_>, name: Symbol) -> Result<u32, EmitError> {
    resolve_variant_tag(em, name)
}

fn resolve_variant_tag(em: &Emitter<'_>, name: Symbol) -> Result<u32, EmitError> {
    let name_str = em.interner.resolve(name);
    for def in &em.sema.defs {
        if def.kind == DefKind::Variant && em.interner.resolve(def.name) == name_str {
            let Some(parent_id) = def.parent else {
                return Err(EmitError::FieldNotFound {
                    desc: format!("variant `{name_str}` has no parent choice type").into(),
                });
            };
            let mut siblings: Vec<u32> = em
                .sema
                .defs
                .iter()
                .filter(|d| d.kind == DefKind::Variant && d.parent == Some(parent_id))
                .map(|d| d.id.0)
                .collect();
            siblings.sort_unstable();
            let pos = siblings
                .iter()
                .position(|&x| x == def.id.0)
                .ok_or_else(|| EmitError::FieldNotFound {
                    desc: format!("variant `{name_str}` not found among its siblings").into(),
                })?;
            return u32::try_from(pos).map_err(|_| EmitError::overflow(format!("variant tag index for `{name_str}`")));
        }
    }
    Err(EmitError::FieldNotFound {
        desc: format!("variant `{name_str}` not found in any choice type").into(),
    })
}

/// Resolve a named field to its positional index by inspecting the object's type.
fn resolve_field_name(
    em: &Emitter<'_>,
    object_expr: ExprIdx,
    name: Symbol,
) -> Result<u32, EmitError> {
    let Some(&ty_idx) = em.sema.expr_types.get(&object_expr) else {
        return Err(EmitError::NoTypeInfo {
            desc: "field access object".into(),
        });
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { fields, .. } => {
            for (i, f) in fields.iter().enumerate() {
                if f.name == name {
                    return u32::try_from(i).map_err(|_| EmitError::overflow(format!("record field index for `{}`", em.interner.resolve(name))));
                }
            }
            Err(EmitError::FieldNotFound {
                desc: format!("record field `{}`", em.interner.resolve(name)).into(),
            })
        }
        Type::Tuple { elems } => {
            let name_str = em.interner.resolve(name);
            let n = name_str
                .parse::<usize>()
                .map_err(|_| EmitError::FieldNotFound {
                    desc: format!("tuple index `{name_str}`").into(),
                })?;
            let _ = elems;
            u32::try_from(n).map_err(|_| EmitError::overflow(format!("tuple field index `{name_str}`")))
        }
        _ => Err(EmitError::FieldNotFound {
            desc: format!("field `{}` on non-record type", em.interner.resolve(name)).into(),
        }),
    }
}

/// Classify the type family of an expression for opcode selection.
pub fn classify_type_family(em: &Emitter<'_>, expr_idx: ExprIdx) -> Option<TypeFamily> {
    let ty_idx = em.sema.expr_types.get(&expr_idx).copied()?;
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    let ty = &em.sema.types[resolved];
    let wk = &em.sema.well_known;
    match ty {
        Type::Named { def, .. } => {
            let d = *def;
            if d == wk.ints.int
                || d == wk.ints.int8
                || d == wk.ints.int16
                || d == wk.ints.int32
                || d == wk.ints.int64
            {
                return Some(TypeFamily::Signed);
            }
            if d == wk.uints.uint8
                || d == wk.uints.uint16
                || d == wk.uints.uint32
                || d == wk.uints.uint64
            {
                return Some(TypeFamily::Unsigned);
            }
            if d == wk.floats.float32 || d == wk.floats.float64 {
                return Some(TypeFamily::Float);
            }
            if d == wk.bool {
                return Some(TypeFamily::Bool);
            }
            None
        }
        _ => None,
    }
}

/// Map an AST `BinOp` + `TypeFamily` to the corresponding bytecode `Opcode`.
pub fn map_binop(op: BinOp, family: Option<TypeFamily>) -> Result<Opcode, EmitError> {
    let opcode = match (op, family) {
        (BinOp::Add, Some(TypeFamily::Float)) => Opcode::F_ADD,
        (BinOp::Add, _) => Opcode::I_ADD,
        (BinOp::Sub, Some(TypeFamily::Float)) => Opcode::F_SUB,
        (BinOp::Sub, _) => Opcode::I_SUB,
        (BinOp::Mul, Some(TypeFamily::Float)) => Opcode::F_MUL,
        (BinOp::Mul, _) => Opcode::I_MUL,
        (BinOp::Div, Some(TypeFamily::Float)) => Opcode::F_DIV,
        (BinOp::Div, Some(TypeFamily::Unsigned)) => Opcode::I_DIV_UN,
        (BinOp::Div, _) => Opcode::I_DIV,
        (BinOp::Rem, Some(TypeFamily::Float)) => Opcode::F_REM,
        (BinOp::Rem, Some(TypeFamily::Unsigned)) => Opcode::I_REM_UN,
        (BinOp::Rem, _) => Opcode::I_REM,
        (BinOp::Eq, Some(TypeFamily::Float)) => Opcode::CMP_F_EQ,
        (BinOp::Eq, _) => Opcode::CMP_EQ,
        (BinOp::Ne, Some(TypeFamily::Float)) => Opcode::CMP_F_NE,
        (BinOp::Ne, _) => Opcode::CMP_NE,
        (BinOp::Lt, Some(TypeFamily::Float)) => Opcode::CMP_F_LT,
        (BinOp::Lt, Some(TypeFamily::Unsigned)) => Opcode::CMP_LT_UN,
        (BinOp::Lt, _) => Opcode::CMP_LT,
        (BinOp::Le, Some(TypeFamily::Float)) => Opcode::CMP_F_LE,
        (BinOp::Le, Some(TypeFamily::Unsigned)) => Opcode::CMP_LE_UN,
        (BinOp::Le, _) => Opcode::CMP_LE,
        (BinOp::Gt, Some(TypeFamily::Float)) => Opcode::CMP_F_GT,
        (BinOp::Gt, Some(TypeFamily::Unsigned)) => Opcode::CMP_GT_UN,
        (BinOp::Gt, _) => Opcode::CMP_GT,
        (BinOp::Ge, Some(TypeFamily::Float)) => Opcode::CMP_F_GE,
        (BinOp::Ge, Some(TypeFamily::Unsigned)) => Opcode::CMP_GE_UN,
        (BinOp::Ge, _) => Opcode::CMP_GE,
        (BinOp::Xor, _) => Opcode::B_XOR,
        (BinOp::Shl, _) => Opcode::B_SHL,
        (BinOp::Shr, _) => Opcode::B_SHR,
        (op, _) => {
            return Err(EmitError::UnsupportedFeature {
                desc: format!("binary operator `{op:?}`").into(),
            });
        }
    };
    Ok(opcode)
}

/// Emit a force-unwrap: evaluate `operand`, check it is the `Some` variant
/// (tag 0), emit `UNR` if None, then extract the payload with `ld.pay(0)`.
fn emit_force_unwrap(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
) -> Result<bool, EmitError> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "force-unwrap operand produced no value".into(),
        });
    }
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    let ok_label = fc.fresh_label();
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_cmp_tag(em.some_tag)?;
    fc.fe.emit_jmp_t(ok_label);
    fc.fe.emit_unop(musi_bc::Opcode::UNR);
    fc.fe.emit_label(ok_label);

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ld_pay(0);
    Ok(true)
}

/// Emit a `handle` expression: compile each handler op as a nested function,
/// push the effect handler, emit the body, then pop the handler.
/// Resolve the numeric effect ID for a `handle` expression's effect type.
///
/// Walks the AST `TyIdx` to find the effect name, then looks up `effect_id_map`.
/// Falls back to 0 for well-known effects not yet in the pool.
fn resolve_handle_effect_id(em: &Emitter<'_>, effect_ty: TyIdx) -> u8 {
    let effect_name = match &em.ast.tys[effect_ty] {
        Ty::Named { name, .. } | Ty::Var { name, .. } => *name,
        _ => return 0,
    };
    em.sema
        .defs
        .iter()
        .find(|d| d.kind == DefKind::Effect && d.name == effect_name)
        .and_then(|d| em.effect_id_map.get(&d.id).copied())
        .unwrap_or(0)
}

/// Emit a `handle` expression: compile each handler op as a nested function,
/// push the effect handler, emit the body, then pop the handler.
fn emit_handle(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    effect_ty: TyIdx,
    ops: &[HandlerOp],
    body: ExprIdx,
) -> Result<bool, EmitError> {
    let effect_id = resolve_handle_effect_id(em, effect_ty);

    // For each handler op, compile as a nested fn that takes op params and evaluates body.
    for op in ops {
        let handler_fn_id = em.alloc_fn_id();
        let param_count =
            u16::try_from(op.params.len()).map_err(|_| EmitError::overflow("handler op param count"))?;
        let mut handler_fc = FnCtx::new(param_count);
        for (i, param) in op.params.iter().enumerate() {
            let slot = u32::try_from(i).map_err(|_| EmitError::overflow("handler param index"))?;
            if let Some(&did) = em.sema.resolution.pat_defs.get(&param.span) {
                let prev = handler_fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
            }
        }
        let had_value = emit_expr(em, &mut handler_fc, op.body)?;
        if had_value {
            handler_fc.fe.emit_eff_res();
        } else {
            handler_fc.fe.emit_ret_u();
        }
        handler_fc
            .fe
            .resolve_fixups(&format!("<handler@{handler_fn_id}>"))?;
        let _code_len = handler_fc.fe.validate_code_len()?;
        let type_id = if let Some(&ty_idx) = em.sema.expr_types.get(&op.body) {
            em.tp
                .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
                .unwrap_or(0)
        } else {
            em.tp
                .lower_well_known_def(em.sema.well_known.unit, &em.sema.well_known)
                .ok_or_else(|| EmitError::unresolvable("Unit type"))?
        };
        let fn_bytecode = FnBytecode {
            fn_id: handler_fn_id,
            type_id,
            local_count: handler_fc.fe.local_count,
            param_count: handler_fc.fe.param_count,
            max_stack: handler_fc.fe.max_stack,
            effect_mask: 0,
            upvalue_count: 0,
            code: handler_fc.fe.code,
            handlers: handler_fc.fe.handlers,
        };
        em.nested_fns.push(fn_bytecode);
        fc.fe.emit_eff_psh(u32::from(effect_id), handler_fn_id)?;
    }

    let produced = emit_expr(em, fc, body)?;

    for _ in ops {
        fc.fe.emit_eff_pop(u32::from(effect_id))?;
    }

    Ok(produced)
}

/// Emit a `do expr` expression. When the body is a call to an effect operation,
/// emit `EFF_DO` instead of a regular call.
fn emit_do(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    body: ExprIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    if let Expr::Call { callee, args, .. } = &em.ast.exprs[body] {
        let callee = *callee;
        let args: Vec<_> = args.clone();
        if let Some(&def_id) = em.sema.resolution.expr_defs.get(&callee) {
            let is_effect_op = em
                .sema
                .defs
                .iter()
                .any(|d| d.id == def_id && d.kind == DefKind::EffectOp);
            if is_effect_op {
                let op_def = em.sema.defs.iter().find(|d| d.id == def_id);
                let is_async = op_def.and_then(|d| d.parent)
                    == Some(em.sema.well_known.effects.async_eff);

                if is_async {
                    let op_name = op_def
                        .map(|d| em.interner.resolve(d.name).to_owned())
                        .unwrap_or_default();
                    return emit_async_op(em, fc, &op_name, &args);
                }

                let op_index = resolve_effect_op_index(em, def_id);
                let arg_count = emit_call_args(em, fc, &args)?;
                let ac = i32::try_from(arg_count).map_err(|_| EmitError::overflow("effect op arg count"))?;
                fc.fe.emit_eff_do(op_index, ac);
                return Ok(true);
            }
        }
    }
    emit_expr_tail(em, fc, body, is_tail)
}

/// Resolve the ordinal index of an effect operation among its sibling ops.
fn resolve_effect_op_index(em: &Emitter<'_>, op_def_id: DefId) -> u32 {
    let def = em.sema.defs.iter().find(|d| d.id == op_def_id);
    let Some(def) = def else { return 0 };
    let Some(parent_id) = def.parent else {
        return 0;
    };
    let mut siblings: Vec<u32> = em
        .sema
        .defs
        .iter()
        .filter(|d| d.kind == DefKind::EffectOp && d.parent == Some(parent_id))
        .map(|d| d.id.0)
        .collect();
    siblings.sort_unstable();
    u32::try_from(siblings.iter().position(|&x| x == op_def_id.0).unwrap_or(0)).unwrap_or(0)
}

/// Emit the TSK_* opcode for an `Async` effect operation.
///
/// Called from `emit_do` when the resolved op's parent effect is the well-known `Async` effect.
/// Each arm handles argument layout for the corresponding VM instruction.
fn emit_async_op(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    op_name: &str,
    args: &[Arg],
) -> Result<bool, EmitError> {
    match op_name {
        "spawn" => {
            // `spawn(f)` or `spawn(f, arg1, arg2, ...)`:
            // The first argument is a static function reference whose fn_id becomes the TSK_SPN
            // operand. Any remaining arguments are the spawned function's call arguments, which
            // TSK_SPN pops from the caller's stack.
            let fn_expr = match args.first() {
                Some(Arg::Pos { expr, .. }) => *expr,
                _ => {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "spawn requires a function argument".into(),
                    });
                }
            };
            let fn_def_id =
                em.sema.resolution.expr_defs.get(&fn_expr).copied().ok_or_else(|| {
                    EmitError::UnsupportedFeature {
                        desc: "spawn argument is not a resolved name".into(),
                    }
                })?;
            let fn_id = em.fn_map.get(&fn_def_id).copied().ok_or_else(|| {
                EmitError::UnsupportedFeature {
                    desc: "spawn argument must be a statically known function".into(),
                }
            })?;
            let extra_args = args.get(1..).unwrap_or(&[]);
            let extra_count = emit_call_args(em, fc, extra_args)?;
            let ac = i32::try_from(extra_count).map_err(|_| EmitError::overflow("spawn extra arg count"))?;
            fc.fe.emit_tsk_spn(fn_id, ac);
            Ok(true)
        }
        "await" => {
            // `await(task)`: push the task handle then await it.
            let _arg_count = emit_call_args(em, fc, args)?;
            fc.fe.emit_tsk_awt();
            Ok(true)
        }
        "channel_make" | "make_channel" => {
            // No arguments; TSK_CMK creates the channel and pushes its handle.
            fc.fe.emit_tsk_cmk();
            Ok(true)
        }
        "send" => {
            // `send(chan, value)`: push chan then value, then TSK_CHS.
            let _arg_count = emit_call_args(em, fc, args)?;
            fc.fe.emit_tsk_chs();
            Ok(true)
        }
        "recv" => {
            // `recv(chan)`: push chan then TSK_CHR.
            let _arg_count = emit_call_args(em, fc, args)?;
            fc.fe.emit_tsk_chr();
            Ok(true)
        }
        _ => Err(EmitError::UnsupportedFeature {
            desc: format!("unknown Async effect op `{op_name}`").into(),
        }),
    }
}

/// Emit deferred expressions (in reverse order) for cleanup before function exit.
pub fn emit_deferred_cleanup(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
) -> Result<(), EmitError> {
    // Snapshot needed: emit_expr may push to fc.deferred (via nested defer), so we can't
    // iterate the vec while fc is mutably borrowed inside the loop.
    let deferred = fc.deferred.clone();
    for &def_expr in deferred.iter().rev() {
        let produced = emit_expr(em, fc, def_expr)?;
        if produced {
            fc.fe.emit_pop();
        }
    }
    Ok(())
}

/// Emit `defer operand`: record the operand expression for deferred execution
/// at function exit. Produces no stack value.
#[allow(clippy::unnecessary_wraps)]
fn emit_defer(
    _em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
) -> Result<bool, EmitError> {
    fc.deferred.push(operand);
    Ok(false)
}

/// Emit `{ base | field = val, ... }` (functional record update).
/// Copies unmodified fields from base, substitutes updated fields, then builds
/// a new product.
fn emit_update(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    base: ExprIdx,
    fields: &[RecField],
) -> Result<bool, EmitError> {
    let produced = emit_expr(em, fc, base)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "update base produced no value".into(),
        });
    }
    let base_slot = fc.alloc_local();
    fc.fe.emit_st_loc(base_slot);

    // Look up the record type to determine field layout.
    let Some(&ty_idx) = em.sema.expr_types.get(&base) else {
        return Err(EmitError::NoTypeInfo {
            desc: "update base".into(),
        });
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    // Clone needed: the borrow of em.sema.types must end before emit_expr borrows em mutably.
    let type_fields = match &em.sema.types[resolved] {
        Type::Record { fields, .. } => fields.clone(),
        _ => {
            return Err(EmitError::UnsupportedFeature {
                desc: "update on non-record type".into(),
            });
        }
    };

    let n = type_fields.len();

    for (i, type_field) in type_fields.iter().enumerate() {
        // Check if this field is being updated.
        let update_val = fields.iter().find_map(|f| {
            if let RecField::Named { name, value: Some(val_idx), .. } = f
                && *name == type_field.name
            {
                return Some(*val_idx);
            }
            None
        });

        if let Some(val_idx) = update_val {
            let produced = emit_expr(em, fc, val_idx)?;
            if !produced {
                return Err(EmitError::UnsupportedFeature {
                    desc: "update field value produced no value".into(),
                });
            }
        } else {
            let idx = u32::try_from(i).map_err(|_| EmitError::overflow("record field index"))?;
            fc.fe.emit_ld_loc(base_slot);
            fc.fe.emit_ld_fld(idx)?;
        }
    }

    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
}
