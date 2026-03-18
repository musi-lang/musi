//! Expression emission: tree-walks AST expressions, leaving results on the stack.

use musi_bc::Opcode;
use music_ast::Pat;
use music_ast::PatIdx;
use music_ast::TyIdx;
use music_ast::expr::{
    Arg, ArrayElem, BinOp, BindKind, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param, PwArm,
    PwGuard, RecField, TypeCheckKind, UnaryOp,
};
use music_ast::lit::Lit;
use music_ast::ty::Ty;
use music_sema::DefId;
use music_sema::TypeIdx;
use music_sema::def::DefKind;
use music_sema::types::{RecordField, Type};
use music_sema::unify::types_match;
use music_shared::Span;
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

struct CfvCtx<'a, 'b> {
    em: &'a Emitter<'b>,
    local_defs: &'a HashSet<DefId>,
    parent_locals: &'a HashMap<DefId, u32>,
    parent_upvalues: &'a HashMap<DefId, u16>,
    found: &'a mut Vec<(DefId, CaptureSource)>,
    seen: &'a mut HashSet<DefId>,
}

/// Walk the AST body and collect free variables - names that reference
/// definitions in an enclosing scope rather than local params/bindings.
fn collect_free_vars(
    em: &Emitter<'_>,
    body: ExprIdx,
    local_defs: &HashSet<DefId>,
    parent_locals: &HashMap<DefId, u32>,
    parent_upvalues: &HashMap<DefId, u16>,
) -> Vec<(DefId, CaptureSource)> {
    let mut found = vec![];
    let mut seen = HashSet::new();
    let mut cx = CfvCtx {
        em,
        local_defs,
        parent_locals,
        parent_upvalues,
        found: &mut found,
        seen: &mut seen,
    };
    cfv_walk(&mut cx, body);
    found
}

/// Check whether `body` contains a free reference to `target` (i.e. the
/// binding being defined references itself - a recursive closure).
fn body_references_def(em: &Emitter<'_>, body: ExprIdx, target: DefId) -> bool {
    let local_defs = HashSet::new();
    let mut parent_locals = HashMap::new();
    let _ = parent_locals.insert(target, 0);
    let parent_upvalues = HashMap::new();
    let captures = collect_free_vars(em, body, &local_defs, &parent_locals, &parent_upvalues);
    captures.iter().any(|(did, _)| *did == target)
}

fn cfv_walk_rec_fields(cx: &mut CfvCtx<'_, '_>, fields: &[RecField]) {
    for f in fields {
        match f {
            RecField::Named { value: Some(v), .. } => cfv_walk(cx, *v),
            RecField::Spread { expr, .. } => cfv_walk(cx, *expr),
            RecField::Named { value: None, .. } => {}
        }
    }
}

fn cfv_walk_piecewise(cx: &mut CfvCtx<'_, '_>, arms: &[PwArm]) {
    for arm in arms {
        if let PwGuard::When { expr, .. } = arm.guard {
            cfv_walk(cx, expr);
        }
        cfv_walk(cx, arm.result);
    }
}

fn cfv_walk_match(cx: &mut CfvCtx<'_, '_>, scrutinee: ExprIdx, arms: &[MatchArm]) {
    cfv_walk(cx, scrutinee);
    for arm in arms {
        if let Some(g) = arm.guard {
            cfv_walk(cx, g);
        }
        cfv_walk(cx, arm.result);
    }
}

fn cfv_check_name(cx: &mut CfvCtx<'_, '_>, expr_idx: ExprIdx) {
    let Some(&def_id) = cx.em.expr_defs().get(&expr_idx) else {
        return;
    };
    if cx.local_defs.contains(&def_id)
        || cx.em.fn_map.contains_key(&def_id)
        || cx.em.foreign_map.contains_key(&def_id)
        || cx.seen.contains(&def_id)
    {
        return;
    }
    if let Some(&slot) = cx.parent_locals.get(&def_id) {
        let _ = cx.seen.insert(def_id);
        cx.found.push((def_id, CaptureSource::Local(slot)));
    } else if let Some(&idx) = cx.parent_upvalues.get(&def_id) {
        let _ = cx.seen.insert(def_id);
        cx.found.push((def_id, CaptureSource::Upvalue(idx)));
    }
}

fn cfv_walk(cx: &mut CfvCtx<'_, '_>, expr_idx: ExprIdx) {
    match &cx.em.ast.exprs[expr_idx] {
        Expr::Name { .. } => cfv_check_name(cx, expr_idx),
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
        | Expr::Return {
            value: Some(inner), ..
        }
        | Expr::Field { object: inner, .. }
        | Expr::UnaryOp { operand: inner, .. }
        | Expr::Fn { body: inner, .. }
        | Expr::TypeCheck { operand: inner, .. } => cfv_walk(cx, *inner),
        Expr::BinOp { left, right, .. }
        | Expr::Index {
            object: left,
            index: right,
            ..
        } => {
            cfv_walk(cx, *left);
            cfv_walk(cx, *right);
        }
        Expr::Block { stmts, tail, .. } => {
            for &s in stmts {
                cfv_walk(cx, s);
            }
            if let Some(t) = *tail {
                cfv_walk(cx, t);
            }
        }
        Expr::Let { fields, body, .. } => {
            if let Some(v) = fields.value {
                cfv_walk(cx, v);
            }
            if let Some(b) = *body {
                cfv_walk(cx, b);
            }
        }
        Expr::Binding { fields, .. } => {
            if let Some(v) = fields.value {
                cfv_walk(cx, v);
            }
        }
        Expr::Call { callee, args, .. } => {
            cfv_walk(cx, *callee);
            for arg in args {
                let e = match arg {
                    Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => *expr,
                };
                cfv_walk(cx, e);
            }
        }
        Expr::Tuple { elems, .. } => {
            for &e in elems {
                cfv_walk(cx, e);
            }
        }
        Expr::Variant { args, .. } => {
            for &a in args {
                cfv_walk(cx, a);
            }
        }
        Expr::Array { elems, .. } => {
            for e in elems {
                let idx = match e {
                    ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => *expr,
                };
                cfv_walk(cx, idx);
            }
        }
        Expr::Record { fields, .. } => cfv_walk_rec_fields(cx, fields),
        Expr::Update { base, fields, .. } => {
            cfv_walk(cx, *base);
            cfv_walk_rec_fields(cx, fields);
        }
        Expr::Piecewise { arms, .. } => cfv_walk_piecewise(cx, arms),
        Expr::Match {
            scrutinee, arms, ..
        } => cfv_walk_match(cx, *scrutinee, arms),
        Expr::Handle { body, ops, .. } => {
            cfv_walk(cx, *body);
            for op in ops {
                cfv_walk(cx, op.body);
            }
        }
    }
}

/// Bit-width of a numeric type, used for narrow-type truncation after arithmetic.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Width {
    W8,
    W16,
    W32,
    W64,
}

/// Type family used to select the right arithmetic/comparison opcodes.
#[derive(Clone, Copy)]
pub enum TypeFamily {
    Signed(Width),
    Unsigned(Width),
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

/// Emit an expression, returning an error if it produces no stack value.
pub(super) fn emit_require(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    idx: ExprIdx,
    ctx: &str,
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, idx)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: format!("{ctx} produced no value").into(),
        });
    }
    Ok(())
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
#[allow(clippy::too_many_lines)]
pub fn emit_expr_tail(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    match &em.ast.exprs[expr_idx] {
        Expr::Lit { lit, .. } => emit_lit(em, fc, lit),
        Expr::Name { name_ref, span } => {
            let name = em.ast.name_refs[*name_ref].name;
            let span = *span;
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
        Expr::BinOp {
            op, left, right, ..
        } => {
            let (op, left, right) = (*op, *left, *right);
            emit_binop_expr(em, fc, expr_idx, op, left, right)
        }
        Expr::UnaryOp { op, operand, span } => {
            let (op, operand, span) = (*op, *operand, *span);
            if op == UnaryOp::Do {
                emit_do(em, fc, operand, is_tail)
            } else {
                emit_unary(em, fc, op, operand, span)
            }
        }
        Expr::Call { callee, args, .. } => {
            let (callee, args) = (*callee, args.clone());
            emit_call(em, fc, callee, &args, is_tail)
        }
        Expr::Tuple { elems, .. } => emit_tuple(em, fc, &elems.clone()),
        Expr::Record { fields, .. } => emit_record_lit(em, fc, &fields.clone()),
        Expr::Array { elems, .. } => {
            emit_array(em, fc, &elems.clone())?;
            Ok(true)
        }
        Expr::Variant { name, args, .. } => emit_variant(em, fc, *name, &args.clone()),
        Expr::Field {
            object,
            field,
            safe,
            ..
        } => {
            // If the sema resolved this field expression directly to a DefId
            // (e.g. import field chains like `t.runner.report`), emit it as a
            // direct name reference - no runtime field access needed.
            if let Some(&def_id) = em.expr_defs().get(&expr_idx) {
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
            }
            emit_field(em, fc, *object, *field, *safe)
        }
        Expr::Index { object, index, .. } => emit_index(em, fc, *object, *index),
        Expr::Return { value, .. } => emit_return(em, fc, *value),
        Expr::Piecewise { arms, .. } => {
            super::control::emit_piecewise(em, fc, &arms.clone(), is_tail)?;
            Ok(true)
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
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
        Expr::TypeCheck {
            kind, operand, ty, ..
        } => emit_type_check(em, fc, *kind, *operand, *ty, is_tail),
        Expr::Handle {
            effect_ty,
            ops,
            body,
            ..
        } => {
            let (effect_ty, ops, body) = (*effect_ty, ops.clone(), *body);
            emit_handle(em, fc, effect_ty, &ops, body)
        }
    }
}

fn emit_name(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    name: Symbol,
    _span: Span,
) -> Result<bool, EmitError> {
    let Some(&def_id) = em.expr_defs().get(&expr_idx) else {
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
        let idx =
            u8::try_from(upv_idx).map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
        fc.fe.emit_ld_upv(idx);
        if fc.ref_upvalues.contains(&def_id) {
            fc.fe.emit_ld_fld(0)?;
        }
        return Ok(true);
    }
    // Global bindings from dependency modules (non-function module-level lets).
    if let Some(&slot) = em.global_map.get(&def_id) {
        fc.fe.emit_ld_glb(slot);
        return Ok(true);
    }
    // Import aliases: construct a module record from the module's exports.
    let def_info = em.sema.defs.iter().find(|d| d.id == def_id);
    if let Some(info) = def_info
        && info.kind == DefKind::Import
        && let Some(ty_idx) = info.ty_info.ty
    {
        return emit_module_record(em, fc, ty_idx);
    }
    Err(EmitError::UnsupportedFeature {
        desc: format!("unresolved local `{}`", em.interner.resolve(name)).into(),
    })
}

/// Construct a module record at runtime from its type's fields.
/// Each field that maps to a known function or FFI binding becomes a `FnRef`;
/// fields whose types are themselves records (sub-modules) are constructed
/// recursively.
fn emit_module_record(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    ty_idx: music_sema::TypeIdx,
) -> Result<bool, EmitError> {
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    let Type::Record { fields, .. } = &em.sema.types[resolved] else {
        return Ok(false);
    };
    let fields = fields.clone();
    let n = fields.len();
    for field in &fields {
        // Try to find a DefId for this field's name that's a function, FFI, or global.
        let field_name = field.name;
        let mut emitted = false;
        for def in &em.sema.defs {
            if def.name == field_name && def.exported {
                if let Some(&fn_id) = em.fn_map.get(&def.id) {
                    let cv = ConstValue::FnRef(fn_id);
                    let i = em.cp.intern(&cv, em.interner)?;
                    fc.fe.emit_ld_cst(i);
                    emitted = true;
                    break;
                }
                if let Some(&ffi_idx) = em.foreign_map.get(&def.id) {
                    let cv = ConstValue::FnRef(ffi_idx);
                    let i = em.cp.intern(&cv, em.interner)?;
                    fc.fe.emit_ld_cst(i);
                    emitted = true;
                    break;
                }
                // Value bindings from dep modules stored as globals.
                if matches!(def.kind, DefKind::Let | DefKind::Var)
                    && let Some(&slot) = em.global_map.get(&def.id)
                {
                    fc.fe.emit_ld_glb(slot);
                    emitted = true;
                    break;
                }
            }
        }
        if !emitted {
            // Try constructing a sub-module record from the field's type.
            let field_resolved = em.sema.unify.resolve(field.ty, &em.sema.types);
            if matches!(&em.sema.types[field_resolved], Type::Record { .. }) {
                let sub = emit_module_record(em, fc, field.ty)?;
                if !sub {
                    fc.fe.emit_ld_unit();
                }
            } else {
                fc.fe.emit_ld_unit();
            }
        }
    }
    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("module record fields"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("module record fields"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
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
        if fields.kind != BindKind::Mut
            && let Expr::Fn {
                body: fn_body,
                params,
                ..
            } = &em.ast.exprs[val_idx]
        {
            let fn_body = *fn_body;
            let params = params.clone();
            if let Pat::Bind { span, .. } = &em.ast.pats[fields.pat] {
                let span = *span;
                if let Some(&def_id) = em.pat_defs().get(&span) {
                    // Already registered by scan phase - compiled as top-level fn.
                    if em.fn_map.contains_key(&def_id) {
                        return body.map_or(Ok(false), |body_idx| {
                            emit_expr_tail(em, fc, body_idx, is_tail)
                        });
                    }
                    // Recursive lambda: use ref-cell letrec pattern.
                    if body_references_def(em, fn_body, def_id) {
                        emit_letrec_fn(em, fc, def_id, fields.pat, val_idx, &params, fn_body)?;
                        return body.map_or(Ok(false), |body_idx| {
                            emit_expr_tail(em, fc, body_idx, is_tail)
                        });
                    }
                }
            }
        }
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

/// Emit a recursive local closure via the ref-cell letrec pattern:
/// 1. Pre-allocate a ref cell with a placeholder value
/// 2. Emit the closure (which captures the ref cell as an upvalue)
/// 3. Patch the ref cell with the actual closure value
fn emit_letrec_fn(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    def_id: DefId,
    _pat_idx: PatIdx,
    _val_idx: ExprIdx,
    params: &[Param],
    fn_body: ExprIdx,
) -> Result<(), EmitError> {
    // 1. Pre-allocate ref cell: LD_UNIT → ALC_REF → ST_LOC
    let wk = &em.sema.well_known;
    let type_id = em.tp.lower_well_known_def(wk.any, wk).unwrap_or(0);
    fc.fe.emit_ld_unit();
    fc.fe.emit_alc_ref(type_id);
    let slot = fc.alloc_local();
    fc.fe.emit_st_loc(slot);

    // Register the binding before emitting the closure so collect_free_vars
    // finds it in fc.local_map and captures the ref cell.
    let prev = fc.local_map.insert(def_id, slot);
    debug_assert!(prev.is_none(), "duplicate local slot for letrec def");
    let _ = fc.ref_locals.insert(def_id);

    // 2. Emit the closure (standard path - now finds self in parent_locals).
    let produced = emit_fn(em, fc, params, fn_body)?;
    if !produced {
        return Ok(());
    }

    // 3. Patch: store closure into the ref cell's field 0.
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);
    fc.fe.emit_ld_loc(slot);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_st_fld(0)?;
    Ok(())
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
    _span: Span,
) -> Result<bool, EmitError> {
    emit_require(em, fc, operand, "unary operand")?;
    match op {
        UnaryOp::Neg => {
            let family = classify_type_family(em, operand);
            let opcode = match family {
                Some(TypeFamily::Float) => Opcode::FLT_NEG,
                _ => Opcode::INT_NEG,
            };
            fc.fe.emit_unop(opcode);
            Ok(true)
        }
        UnaryOp::Not => {
            fc.fe.emit_unop(Opcode::BIT_NOT);
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
        UnaryOp::Defer => Ok(emit_defer(fc, operand)),
        UnaryOp::Do => {
            // emit_expr_tail routes Do before calling emit_unary; reaching here is a bug.
            Err(EmitError::UnsupportedFeature {
                desc: "UnaryOp::Do reached emit_unary - caller must handle Do before dispatch"
                    .into(),
            })
        }
    }
}

fn emit_tuple(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ExprIdx]) -> Result<bool, EmitError> {
    let n = elems.len();
    for &e in elems {
        emit_require(em, fc, e, "tuple element")?;
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
    safe: bool,
) -> Result<bool, EmitError> {
    emit_require(em, fc, object, "field object")?;

    if safe {
        let tmp = fc.alloc_local();
        fc.fe.emit_st_loc(tmp);

        let none_label = fc.fresh_label();
        let end_label = fc.fresh_label();

        fc.fe.emit_ld_loc(tmp);
        fc.fe.emit_cmp_tag(em.some_tag)?;
        fc.fe.emit_jmp_f(none_label);

        // Some path: extract payload, load field, wrap in Some
        fc.fe.emit_ld_loc(tmp);
        fc.fe.emit_ld_pay(0);
        let index = match field {
            FieldKey::Pos { index, .. } => index,
            FieldKey::Name { name, .. } => resolve_field_name(em, object, name)?,
        };
        fc.fe.emit_ld_fld(index)?;
        fc.fe.emit_mk_var(em.some_tag, 1);
        fc.fe.emit_jmp(end_label);

        // None path: receiver was not Some - produce a zero-payload variant as None
        fc.fe.emit_label(none_label);
        fc.fe.emit_ld_loc(tmp);

        fc.fe.emit_label(end_label);
    } else {
        let index = match field {
            FieldKey::Pos { index, .. } => index,
            FieldKey::Name { name, .. } => resolve_field_name(em, object, name)?,
        };
        fc.fe.emit_ld_fld(index)?;
    }

    Ok(true)
}

fn emit_index(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    object: ExprIdx,
    index: ExprIdx,
) -> Result<bool, EmitError> {
    emit_require(em, fc, object, "index object")?;
    emit_require(em, fc, index, "index value")?;
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
        Ty::Named { name_ref, .. } | Ty::Var { name_ref } => em.ast.name_refs[*name_ref].name,
        _ => return None,
    };
    let wk = &em.sema.well_known;
    let name_str = em.interner.resolve(name);
    // Well-known primitive types - match by name so they work even without sema ty_info.
    match name_str {
        "Bool" => return em.tp.lower_well_known_def(wk.bool, wk),
        "Int" | "Int64" => return em.tp.lower_well_known_def(wk.ints.int, wk),
        "Int8" => return em.tp.lower_well_known_def(wk.ints.int8, wk),
        "Int16" => return em.tp.lower_well_known_def(wk.ints.int16, wk),
        "Int32" => return em.tp.lower_well_known_def(wk.ints.int32, wk),
        "Nat" | "Nat64" => return em.tp.lower_well_known_def(wk.nats.nat, wk),
        "Nat8" => return em.tp.lower_well_known_def(wk.nats.nat8, wk),
        "Nat16" => return em.tp.lower_well_known_def(wk.nats.nat16, wk),
        "Nat32" => return em.tp.lower_well_known_def(wk.nats.nat32, wk),
        "Float" => return em.tp.lower_well_known_def(wk.float, wk),
        "Float32" => return em.tp.lower_well_known_def(wk.floats.float32, wk),
        "Float64" => return em.tp.lower_well_known_def(wk.floats.float64, wk),
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
        // Type couldn't be resolved - fall back to always-true.
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
        // Can't resolve type - leave value on stack, assume caller knows what they're doing.
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
    if let Some(&def_id) = em.active_binop_dispatch().get(&expr_idx)
        && let Some(&fn_id) = em.fn_map.get(&def_id)
    {
        emit_require(em, fc, left, "dispatched binop left operand")?;
        emit_require(em, fc, right, "dispatched binop right operand")?;
        fc.fe.emit_inv(fn_id, false, 2);
        return Ok(true);
    }
    // Polymorphic dispatch: load method from dictionary, call via INV_DYN
    if let Some(dict_lookup) = em.active_binop_dict_dispatch().get(&expr_idx) {
        let dict_slot = fc
            .dict_slots
            .get(&dict_lookup.class)
            .copied()
            .ok_or_else(|| EmitError::UnsupportedFeature {
                desc: "no dictionary slot for class constraint".into(),
            })?;
        let method_idx = class_method_index(em, dict_lookup.class, dict_lookup.method_sym)?;
        fc.fe.emit_ld_loc(dict_slot); // load dictionary product
        fc.fe.emit_ld_fld(method_idx)?; // extract method fn value
        emit_require(em, fc, left, "dict-dispatched binop left operand")?;
        emit_require(em, fc, right, "dict-dispatched binop right operand")?;
        fc.fe.emit_inv_dyn(2)?; // call through fn value with 2 args
        return Ok(true);
    }
    emit_primitive_binop(em, fc, expr_idx, op, left, right)
}

fn emit_primitive_binop(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    op: BinOp,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<bool, EmitError> {
    match op {
        BinOp::And => {
            let family = classify_type_family(em, left).or_else(|| classify_type_family(em, right));
            if is_logical_family(family) {
                desugar::emit_and(em, fc, left, right)?;
            } else {
                emit_require(em, fc, left, "binop left operand")?;
                emit_require(em, fc, right, "binop right operand")?;
                fc.fe.emit_binop(Opcode::BIT_AND);
            }
            Ok(true)
        }
        BinOp::Or => {
            let family = classify_type_family(em, left).or_else(|| classify_type_family(em, right));
            if is_logical_family(family) {
                desugar::emit_or(em, fc, left, right)?;
            } else {
                emit_require(em, fc, left, "binop left operand")?;
                emit_require(em, fc, right, "binop right operand")?;
                fc.fe.emit_binop(Opcode::BIT_OR);
            }
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
        BinOp::In => {
            desugar::emit_in_op(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::RangeInc | BinOp::RangeExc => {
            desugar::emit_range(em, fc, left, right)?;
            Ok(true)
        }
        BinOp::Cons => {
            desugar::emit_cons(em, fc, left, right)?;
            Ok(true)
        }
        _ => {
            // String `+` → call str_cat FFI instead of I_ADD.
            if op == BinOp::Add
                && let Some(ty_idx) = em.expr_types().get(&left).copied()
            {
                let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
                if let Type::Named { def, .. } = &em.sema.types[resolved]
                    && *def == em.sema.well_known.string
                    && let Some(ffi_idx) = em.str_cat_ffi_idx
                {
                    emit_require(em, fc, left, "binop left operand")?;
                    emit_require(em, fc, right, "binop right operand")?;
                    fc.fe.emit_inv_ffi(ffi_idx, 2);
                    return Ok(true);
                }
            }
            emit_require(em, fc, left, "binop left operand")?;
            emit_require(em, fc, right, "binop right operand")?;
            let family = classify_type_family(em, left);
            let opcode = map_binop(op, family)?;
            fc.fe.emit_binop(opcode);
            // Comparison operators produce bool, not int - skip narrow truncation.
            if !matches!(
                op,
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
            ) {
                emit_narrow_truncation(em, fc, family)?;
            }
            Ok(true)
        }
    }
}

/// Emit masking/sign-extension instructions for narrow integer types after arithmetic.
///
/// For signed narrow types: shift left by (64-width), then arithmetic shift right by the same amount.
/// For unsigned narrow types: AND with the width's bitmask.
/// 64-bit and float types need no truncation.
fn emit_narrow_truncation(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    family: Option<TypeFamily>,
) -> Result<(), EmitError> {
    let (signed, shift_amount, mask) = match family {
        Some(TypeFamily::Signed(Width::W8)) => (true, 56i64, 0u64),
        Some(TypeFamily::Signed(Width::W16)) => (true, 48, 0),
        Some(TypeFamily::Signed(Width::W32)) => (true, 32, 0),
        Some(TypeFamily::Unsigned(Width::W8)) => (false, 0, 0xFF),
        Some(TypeFamily::Unsigned(Width::W16)) => (false, 0, 0xFFFF),
        Some(TypeFamily::Unsigned(Width::W32)) => (false, 0, 0xFFFF_FFFF),
        _ => return Ok(()),
    };
    if signed {
        let cv = ConstValue::Int(shift_amount);
        let idx = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(idx);
        fc.fe.emit_binop(Opcode::BIT_SHL);
        let idx2 = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(idx2);
        fc.fe.emit_binop(Opcode::BIT_SHR);
    } else {
        let cv = ConstValue::Int(mask.cast_signed());
        let idx = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(idx);
        fc.fe.emit_binop(Opcode::BIT_AND);
    }
    Ok(())
}

fn emit_call(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    callee: ExprIdx,
    args: &[Arg],
    is_tail: bool,
) -> Result<bool, EmitError> {
    // Direct dispatch: if the callee resolves to a known function, emit a
    // static call (INV / INV_FFI) instead of dynamic dispatch (INV_DYN).
    // This handles both simple names (`foo(...)`) and resolved import field
    // chains (`rt.writeln(...)`, `t.assertions.assert_eq(...)`).
    if let Some(&def_id) = em.expr_defs().get(&callee) {
        let dict_count = emit_dict_for_call(em, fc, def_id, callee)?;
        let explicit_count = emit_call_args(em, fc, args)?;
        let total_count = dict_count + explicit_count;

        if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
            let ac = i32::try_from(total_count).map_err(|_| EmitError::overflow("arg count"))?;
            fc.fe.emit_inv_ffi(ffi_idx, ac);
            return Ok(true);
        }
        if let Some(&fn_id) = em.fn_map.get(&def_id) {
            if is_tail {
                fc.fe.emit_inv_tail(fn_id, false);
                return Ok(false);
            }
            let ac = i32::try_from(total_count).map_err(|_| EmitError::overflow("arg count"))?;
            fc.fe.emit_inv(fn_id, false, ac);
            return Ok(true);
        }
        // Dynamic dispatch via local, upvalue, or global requires callee below args.
        // Args were already emitted above; save them to temp slots, push callee,
        // then restore args on top.
        if fc.local_map.contains_key(&def_id)
            || fc.upvalue_map.contains_key(&def_id)
            || em.global_map.contains_key(&def_id)
        {
            // Spill args to temp slots.
            let mut arg_temps = Vec::with_capacity(total_count);
            for _ in 0..total_count {
                let t = fc.alloc_local();
                fc.fe.emit_st_loc(t);
                arg_temps.push(t);
            }
            // Push callee.
            if let Some(&slot) = fc.local_map.get(&def_id) {
                fc.fe.emit_ld_loc(slot);
                if fc.ref_locals.contains(&def_id) {
                    fc.fe.emit_ld_fld(0)?;
                }
            } else if let Some(&upv_idx) = fc.upvalue_map.get(&def_id) {
                let idx = u8::try_from(upv_idx)
                    .map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
                fc.fe.emit_ld_upv(idx);
                if fc.ref_upvalues.contains(&def_id) {
                    fc.fe.emit_ld_fld(0)?;
                }
            } else if let Some(&slot) = em.global_map.get(&def_id) {
                fc.fe.emit_ld_glb(slot);
            }
            // Restore args on top (reverse order - first spilled is last restored).
            for &t in arg_temps.iter().rev() {
                fc.fe.emit_ld_loc(t);
            }
            let ac_i = i32::try_from(total_count).map_err(|_| EmitError::overflow("arg count"))?;
            fc.fe.emit_inv_dyn(ac_i)?;
            return Ok(true);
        }
        // Name callees that don't resolve are an error.
        if matches!(&em.ast.exprs[callee], Expr::Name { .. }) {
            return Err(EmitError::UnsupportedFeature {
                desc: "unresolved callee".into(),
            });
        }
    } else if matches!(&em.ast.exprs[callee], Expr::Name { .. }) {
        return Err(EmitError::UnsupportedFeature {
            desc: "unresolved callee".into(),
        });
    }
    {
        // Callee must be below args on the stack for resolve_inv_dyn.
        emit_require(em, fc, callee, "callee")?;
        let arg_count = emit_call_args(em, fc, args)?;
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
    let has_spread = fields.iter().any(|f| matches!(f, RecField::Spread { .. }));

    if has_spread {
        emit_record_lit_with_spread(em, fc, fields)
    } else {
        emit_record_lit_fixed(em, fc, fields)
    }
}

fn emit_record_lit_fixed(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
) -> Result<bool, EmitError> {
    // Collect (name, value) pairs and sort by name for canonical field ordering.
    // This ordering must match `resolve_field_in_type` and the type checker's
    // canonical sort so that field indices are consistent across modules.
    let mut named_fields: Vec<(music_shared::Symbol, Option<music_ast::ExprIdx>)> = fields
        .iter()
        .filter_map(|f| match f {
            RecField::Named { name, value, .. } => Some((*name, *value)),
            RecField::Spread { .. } => None,
        })
        .collect();
    named_fields.sort_by(|(a, _), (b, _)| em.interner.resolve(*a).cmp(em.interner.resolve(*b)));
    let n = named_fields.len();
    for (_, val_opt) in named_fields {
        if let Some(val_idx) = val_opt {
            emit_require(em, fc, val_idx, "record field")?;
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

/// Emit a record literal that contains a spread (`{...base, name: val}`).
///
/// Records are heap-allocated products. The spread provides the base object;
/// named fields override specific positions in that object via `ST_FLD`.
/// Only a single leading spread followed by named overrides is supported
/// (the common `{...base, key: val}` pattern). Multiple spreads or spreads
/// mixed with overrides in arbitrary order would require full type-layout
/// knowledge that isn't available here.
fn emit_record_lit_with_spread(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
) -> Result<bool, EmitError> {
    let mut spread_expr: Option<ExprIdx> = None;
    for f in fields {
        if let RecField::Spread { expr, .. } = f {
            if spread_expr.is_some() {
                return Err(EmitError::UnsupportedFeature {
                    desc: "multiple spread elements in record literal".into(),
                });
            }
            spread_expr = Some(*expr);
        }
    }
    let base_expr = spread_expr.ok_or_else(|| EmitError::UnsupportedFeature {
        desc: "record spread without base expression".into(),
    })?;

    emit_require(em, fc, base_expr, "record spread base")?;
    let rec_slot = fc.alloc_local();
    fc.fe.emit_st_loc(rec_slot);

    for f in fields {
        if let RecField::Named { name, value, .. } = f {
            let val_idx = value.ok_or_else(|| EmitError::UnsupportedFeature {
                desc: "record spread override field has no value".into(),
            })?;

            let field_idx = resolve_field_name_by_symbol(em, base_expr, *name)?;

            fc.fe.emit_ld_loc(rec_slot);
            emit_require(em, fc, val_idx, "record spread override field")?;
            fc.fe.emit_st_fld(field_idx)?;
        }
    }

    fc.fe.emit_ld_loc(rec_slot);
    Ok(true)
}

fn resolve_field_name_by_symbol(
    em: &Emitter<'_>,
    object_expr: ExprIdx,
    name: Symbol,
) -> Result<u32, EmitError> {
    let Some(&ty_idx) = em.expr_types().get(&object_expr) else {
        return Err(EmitError::NoTypeInfo {
            desc: "record spread base".into(),
        });
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { fields, .. } => {
            let mut sorted: Vec<_> = fields.clone();
            sorted.sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            for (i, f) in sorted.iter().enumerate() {
                if f.name == name {
                    return u32::try_from(i).map_err(|_| {
                        EmitError::overflow(format!(
                            "record field index for `{}`",
                            em.interner.resolve(name)
                        ))
                    });
                }
            }
            Err(EmitError::FieldNotFound {
                desc: format!("record field `{}`", em.interner.resolve(name)).into(),
            })
        }
        _ => Err(EmitError::FieldNotFound {
            desc: format!(
                "spread base is not a record type (field `{}`)",
                em.interner.resolve(name)
            )
            .into(),
        }),
    }
}

fn emit_variant(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    name: Symbol,
    args: &[ExprIdx],
) -> Result<bool, EmitError> {
    let name_str = em.interner.resolve(name);
    if args.is_empty() {
        if name_str == "True" {
            let cv = ConstValue::Bool(true);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            return Ok(true);
        }
        if name_str == "False" {
            let cv = ConstValue::Bool(false);
            let i = em.cp.intern(&cv, em.interner)?;
            fc.fe.emit_ld_cst(i);
            return Ok(true);
        }
    }

    for &arg_idx in args {
        emit_require(em, fc, arg_idx, "variant arg")?;
    }
    let tag = resolve_variant_tag(em, name)?;
    let arity = u8::try_from(args.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "variant arity exceeds 255".into(),
    })?;
    fc.fe.emit_mk_var(tag, arity);
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
        dep_idx: em.active_dep,
    };

    let mut nested_fc = FnCtx::new(nested_param_count);

    // Build the set of locally-defined DefIds (params).
    let mut local_defs = HashSet::new();
    for (i, param) in params.iter().enumerate() {
        let slot = u32::try_from(i).map_err(|_| EmitError::overflow("nested fn param index"))?;
        if let Some(&did) = em.pat_defs().get(&param.span) {
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
    let upvalue_count = u16::try_from(captures.len())
        .map_err(|_| EmitError::overflow("too many captured variables"))?;

    // Populate the nested function's upvalue_map and propagate ref info.
    for (upv_idx, &(def_id, _)) in captures.iter().enumerate() {
        let idx = u16::try_from(upv_idx)
            .map_err(|_| EmitError::overflow("upvalue index in populate loop"))?;
        let _ = nested_fc.upvalue_map.insert(def_id, idx);
        if fc.ref_locals.contains(&def_id) || fc.ref_upvalues.contains(&def_id) {
            let _ = nested_fc.ref_upvalues.insert(def_id);
        }
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
    let type_id = if let Some(&ty_idx) = em.expr_types().get(&body) {
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
                    let u8_idx = u8::try_from(idx)
                        .map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
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
pub fn bind_pat(em: &mut Emitter<'_>, fc: &mut FnCtx, pat_idx: PatIdx) -> Result<(), EmitError> {
    match &em.ast.pats[pat_idx] {
        Pat::Wild { .. } => {
            fc.fe.emit_pop();
            Ok(())
        }
        Pat::Bind { span, .. } => {
            let span = *span;
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.pat_defs().get(&span) {
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
                let idx =
                    u32::try_from(i).map_err(|_| EmitError::overflow("tuple destructure index"))?;
                fc.fe.emit_ld_loc(tmp);
                fc.fe.emit_ld_fld(idx)?;
                bind_pat(em, fc, *elem)?;
            }
            Ok(())
        }
        Pat::Array { elems, .. } => {
            let tmp = fc.alloc_local();
            fc.fe.emit_st_loc(tmp);
            for (i, elem) in elems.iter().enumerate() {
                let elem_idx =
                    i64::try_from(i).map_err(|_| EmitError::overflow("array destructure index"))?;
                let elem_cst_idx = em.cp.intern(&ConstValue::Int(elem_idx), em.interner)?;
                fc.fe.emit_ld_loc(tmp);
                fc.fe.emit_ld_cst(elem_cst_idx);
                fc.fe.emit_ld_idx();
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
fn bind_pat_ref(em: &mut Emitter<'_>, fc: &mut FnCtx, pat_idx: PatIdx) -> Result<(), EmitError> {
    match &em.ast.pats[pat_idx] {
        Pat::Bind { span, .. } => {
            let span = *span;
            let wk = &em.sema.well_known;
            let type_id = em.tp.lower_well_known_def(wk.any, wk).unwrap_or(0);
            fc.fe.emit_alc_ref(type_id);
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.pat_defs().get(&span) {
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
                emit_require(em, fc, expr, "call arg")?;
                count += 1;
            }
            Arg::Spread { .. } => {
                // Spread in call args requires knowing the argument count at the call
                // site to emit INV/INV_DYN with the correct count. Since array length
                // is only known at runtime, this would require rewriting the entire
                // call emission to use INV_DYN with a dynamically-computed arg count.
                // That restructuring is out of scope for the emitter today.
                return Err(EmitError::UnsupportedFeature {
                    desc: "spread in call arguments is not yet supported".into(),
                });
            }
        }
    }
    Ok(count)
}

fn emit_array(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> Result<(), EmitError> {
    let has_spread = elems.iter().any(|e| matches!(e, ArrayElem::Spread { .. }));

    if has_spread {
        emit_array_with_spread(em, fc, elems)
    } else {
        emit_array_fixed(em, fc, elems)
    }
}

fn emit_array_fixed(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    elems: &[ArrayElem],
) -> Result<(), EmitError> {
    let count =
        u32::try_from(elems.len()).map_err(|_| EmitError::overflow("array element count"))?;
    let len_cv = ConstValue::Int(i64::from(count));
    let li = em.cp.intern(&len_cv, em.interner)?;
    fc.fe.emit_ld_cst(li);
    fc.fe.emit_mk_arr(0);

    let arr_slot = fc.alloc_local();
    fc.fe.emit_st_loc(arr_slot);

    for (i, &elem) in elems.iter().enumerate() {
        let expr_idx = match elem {
            ArrayElem::Elem { expr, .. } => expr,
            ArrayElem::Spread { .. } => {
                return Err(EmitError::UnsupportedFeature {
                    desc: "spread element in fixed-size array path".into(),
                });
            }
        };
        let idx_u32 = u32::try_from(i).map_err(|_| EmitError::overflow("array index"))?;
        fc.fe.emit_ld_loc(arr_slot);
        let idx_cv = ConstValue::Int(i64::from(idx_u32));
        let ci = em.cp.intern(&idx_cv, em.interner)?;
        fc.fe.emit_ld_cst(ci);
        emit_require(em, fc, expr_idx, "array element")?;
        fc.fe.emit_st_idx();
    }

    fc.fe.emit_ld_loc(arr_slot);
    Ok(())
}

/// Emit an array literal that contains at least one spread element.
///
/// Strategy:
/// 1. Compute total size: fixed element count + `LD_LEN` for each spread.
/// 2. Push total size, `MK_ARR` to allocate.
/// 3. Walk elements: for fixed, write at current index; for spread, loop over it.
fn emit_array_with_spread(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    elems: &[ArrayElem],
) -> Result<(), EmitError> {
    // Count the fixed elements (non-spread) to use as the base in the size sum.
    let fixed_count = elems
        .iter()
        .filter(|e| matches!(e, ArrayElem::Elem { .. }))
        .count();
    let fixed_count_i64 =
        i64::try_from(fixed_count).map_err(|_| EmitError::overflow("array fixed element count"))?;

    // Compute total length: start with the fixed count constant, then add LD_LEN for each spread.
    let base_cv = ConstValue::Int(fixed_count_i64);
    let bi = em.cp.intern(&base_cv, em.interner)?;
    fc.fe.emit_ld_cst(bi);

    // Store spread arrays in locals so we can re-read them after computing the size.
    let mut spread_slots: Vec<u32> = vec![];
    for &elem in elems {
        if let ArrayElem::Spread { expr, .. } = elem {
            emit_require(em, fc, expr, "spread expression")?;
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            spread_slots.push(slot);
            // Add this spread's length to the running total.
            fc.fe.emit_ld_loc(slot);
            fc.fe.emit_ld_len();
            fc.fe.emit_i_add();
        }
    }

    // Stack: [total_length]. MK_ARR pops it, pushes array ref.
    fc.fe.emit_mk_arr(0);
    let arr_slot = fc.alloc_local();
    fc.fe.emit_st_loc(arr_slot);

    // Write index local, starts at 0.
    let widx_slot = fc.alloc_local();
    let zero_cv = ConstValue::Int(0);
    let zi = em.cp.intern(&zero_cv, em.interner)?;
    fc.fe.emit_ld_cst(zi);
    fc.fe.emit_st_loc(widx_slot);

    let one_cv = ConstValue::Int(1);
    let one_i = em.cp.intern(&one_cv, em.interner)?;

    let mut spread_iter = spread_slots.iter().copied();

    for &elem in elems {
        match elem {
            ArrayElem::Elem { expr, .. } => {
                fc.fe.emit_ld_loc(arr_slot);
                fc.fe.emit_ld_loc(widx_slot);
                emit_require(em, fc, expr, "array element")?;
                fc.fe.emit_st_idx();
                // widx += 1
                fc.fe.emit_ld_loc(widx_slot);
                fc.fe.emit_ld_cst(one_i);
                fc.fe.emit_i_add();
                fc.fe.emit_st_loc(widx_slot);
            }
            ArrayElem::Spread { .. } => {
                let spread_slot = spread_iter.next().expect("spread_slots exhausted");

                // Loop index for reading from spread array.
                let ridx_slot = fc.alloc_local();
                fc.fe.emit_ld_cst(zi);
                fc.fe.emit_st_loc(ridx_slot);

                // Get spread length into a local.
                let slen_slot = fc.alloc_local();
                fc.fe.emit_ld_loc(spread_slot);
                fc.fe.emit_ld_len();
                fc.fe.emit_st_loc(slen_slot);

                // loop_start: if ridx >= slen, jump to loop_end
                let loop_start = fc.fresh_label();
                let loop_end = fc.fresh_label();

                fc.fe.emit_label(loop_start);
                // ridx < slen?
                fc.fe.emit_ld_loc(ridx_slot);
                fc.fe.emit_ld_loc(slen_slot);
                // CMP_LT: pops two, pushes bool. We need to jump if ridx >= slen, i.e. NOT (ridx < slen).
                fc.fe.emit_binop(Opcode::CMP_LT);
                fc.fe.emit_jmp_f(loop_end);

                // arr[widx] = spread[ridx]
                fc.fe.emit_ld_loc(arr_slot);
                fc.fe.emit_ld_loc(widx_slot);
                fc.fe.emit_ld_loc(spread_slot);
                fc.fe.emit_ld_loc(ridx_slot);
                fc.fe.emit_ld_idx();
                fc.fe.emit_st_idx();

                // widx += 1
                fc.fe.emit_ld_loc(widx_slot);
                fc.fe.emit_ld_cst(one_i);
                fc.fe.emit_i_add();
                fc.fe.emit_st_loc(widx_slot);

                // ridx += 1
                fc.fe.emit_ld_loc(ridx_slot);
                fc.fe.emit_ld_cst(one_i);
                fc.fe.emit_i_add();
                fc.fe.emit_st_loc(ridx_slot);

                fc.fe.emit_jmp(loop_start);
                fc.fe.emit_label(loop_end);
            }
        }
    }

    fc.fe.emit_ld_loc(arr_slot);
    Ok(())
}

/// Resolve a variant's tag by scanning the def table for sibling variants.
pub fn resolve_variant_tag(em: &Emitter<'_>, name: Symbol) -> Result<u32, EmitError> {
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
            return u32::try_from(pos)
                .map_err(|_| EmitError::overflow(format!("variant tag index for `{name_str}`")));
        }
    }
    // Fall back to well-known tags for open variants not in the def table.
    let name_str_owned = em.interner.resolve(name).to_owned();
    if name_str_owned == "Some" {
        return Ok(em.some_tag);
    }
    if name_str_owned == "None" {
        return Ok(u32::from(em.some_tag == 0));
    }
    if name_str_owned == "Ok" {
        return Ok(em.ok_tag);
    }
    if name_str_owned == "Err" {
        return Ok(u32::from(em.ok_tag == 0));
    }
    // Ordering variants: Less + Equal + Greater (defined in @std/cmp/ordering)
    if name_str_owned == "Less" {
        return Ok(0);
    }
    if name_str_owned == "Equal" {
        return Ok(1);
    }
    if name_str_owned == "Greater" {
        return Ok(2);
    }
    Err(EmitError::FieldNotFound {
        desc: format!("variant `{name_str}` not found in any choice type").into(),
    })
}

/// Resolve a named field to its positional index by inspecting the object's type.
pub(super) fn resolve_field_name(
    em: &Emitter<'_>,
    object_expr: ExprIdx,
    name: Symbol,
) -> Result<u32, EmitError> {
    let Some(&ty_idx) = em.expr_types().get(&object_expr) else {
        return Err(EmitError::NoTypeInfo {
            desc: "field access object".into(),
        });
    };
    // Try the expression type first, then fall back to resolving through the
    // object's definition type if the field isn't found (handles partially
    // unified row types in cross-module emission).
    if let Ok(idx) = resolve_field_in_type(em, ty_idx, name) {
        return Ok(idx);
    }
    // Fallback: look up the object's definition type (the declared type of
    // the variable, which may be fully concrete).
    if let Some(&def_id) = em.expr_defs().get(&object_expr)
        && let Some(def_info) = em.sema.defs.iter().find(|d| d.id == def_id)
        && let Some(def_ty) = def_info.ty_info.ty
        && let Ok(idx) = resolve_field_in_type(em, def_ty, name)
    {
        return Ok(idx);
    }
    Err(EmitError::FieldNotFound {
        desc: format!("record field `{}`", em.interner.resolve(name)).into(),
    })
}

fn collect_record_fields(em: &Emitter<'_>, ty_idx: TypeIdx, out: &mut Vec<RecordField>) -> bool {
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    if let Type::Record { fields, rest } = &em.sema.types[resolved] {
        for f in fields {
            if !out.iter().any(|ef| ef.name == f.name) {
                out.push(f.clone());
            }
        }
        rest.is_none_or(|rest_idx| collect_record_fields(em, rest_idx, out))
    } else {
        false // rest resolved to non-record (unbound var) - incomplete
    }
}

/// Search for a closed record type definition whose fields are a superset
/// of `partial_fields`. Returns the full field list if found.
fn find_complete_record_type(
    em: &Emitter<'_>,
    partial_fields: &[RecordField],
) -> Option<Vec<RecordField>> {
    for def in &em.sema.defs {
        // Look at type defs and let bindings that define record types.
        if !matches!(def.kind, DefKind::Type | DefKind::Let) {
            continue;
        }
        let Some(ty_idx) = def.ty_info.ty else {
            continue;
        };
        let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
        let ty = &em.sema.types[resolved];

        // Unwrap Type::Named to its underlying type (e.g., Deque['a] → Record).
        let record_resolved = match ty {
            Type::Record { .. } => resolved,
            Type::Named { def: named_def, .. } => {
                let inner_def = em.sema.defs.iter().find(|d| d.id == *named_def);
                if let Some(d) = inner_def {
                    if let Some(inner_ty) = d.ty_info.ty {
                        em.sema.unify.resolve(inner_ty, &em.sema.types)
                    } else {
                        continue;
                    }
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        if let Type::Record { rest, .. } = &em.sema.types[record_resolved] {
            if rest.is_some() {
                continue;
            }
            let mut full_fields = vec![];
            let _ = collect_record_fields(em, record_resolved, &mut full_fields);
            let is_superset = partial_fields
                .iter()
                .all(|pf| full_fields.iter().any(|ff| ff.name == pf.name));
            if is_superset && full_fields.len() >= partial_fields.len() {
                return Some(full_fields);
            }
        }
    }
    None
}

fn resolve_field_in_type(
    em: &Emitter<'_>,
    ty_idx: TypeIdx,
    name: Symbol,
) -> Result<u32, EmitError> {
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { .. } => {
            let mut all_fields: Vec<RecordField> = vec![];
            let complete = collect_record_fields(em, resolved, &mut all_fields);
            if !complete {
                all_fields
                    .sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            }
            // If the record is open (incomplete from dep module inference),
            // find the canonical closed record type definition.
            if !complete && let Some(full) = find_complete_record_type(em, &all_fields) {
                all_fields = full;
            }
            // Sort by name string for canonical ordering - matches emit_record_lit_fixed.
            all_fields.sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            for (i, f) in all_fields.iter().enumerate() {
                if f.name == name {
                    return u32::try_from(i).map_err(|_| {
                        EmitError::overflow(format!(
                            "record field index for `{}`",
                            em.interner.resolve(name)
                        ))
                    });
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
            u32::try_from(n)
                .map_err(|_| EmitError::overflow(format!("tuple field index `{name_str}`")))
        }
        _ => Err(EmitError::FieldNotFound {
            desc: format!("field `{}` on non-record type", em.interner.resolve(name)).into(),
        }),
    }
}

/// Returns true if the family indicates logical (short-circuit) semantics.
/// Bool → logical; known numeric → bitwise; None → logical (safe default).
const fn is_logical_family(family: Option<TypeFamily>) -> bool {
    match family {
        Some(TypeFamily::Bool) | None => true,
        Some(_) => false,
    }
}

/// Classify the type family of an expression for opcode selection.
pub fn classify_type_family(em: &Emitter<'_>, expr_idx: ExprIdx) -> Option<TypeFamily> {
    let ty_idx = em.expr_types().get(&expr_idx).copied()?;
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    let ty = &em.sema.types[resolved];
    let wk = &em.sema.well_known;
    match ty {
        Type::Named { def, .. } => {
            let d = *def;
            if d == wk.ints.int || d == wk.ints.int64 {
                return Some(TypeFamily::Signed(Width::W64));
            }
            if d == wk.ints.int32 {
                return Some(TypeFamily::Signed(Width::W32));
            }
            if d == wk.ints.int16 {
                return Some(TypeFamily::Signed(Width::W16));
            }
            if d == wk.ints.int8 {
                return Some(TypeFamily::Signed(Width::W8));
            }
            if d == wk.nats.nat || d == wk.nats.nat64 {
                return Some(TypeFamily::Unsigned(Width::W64));
            }
            if d == wk.nats.nat32 {
                return Some(TypeFamily::Unsigned(Width::W32));
            }
            if d == wk.nats.nat16 {
                return Some(TypeFamily::Unsigned(Width::W16));
            }
            if d == wk.nats.nat8 {
                return Some(TypeFamily::Unsigned(Width::W8));
            }
            if d == wk.float || d == wk.floats.float64 {
                return Some(TypeFamily::Float);
            }
            if d == wk.floats.float32 {
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
        (BinOp::Add, Some(TypeFamily::Unsigned(_))) => Opcode::NAT_ADD,
        (BinOp::Add, Some(TypeFamily::Float)) => Opcode::FLT_ADD,
        (BinOp::Add, _) => Opcode::INT_ADD,
        (BinOp::Sub, Some(TypeFamily::Unsigned(_))) => Opcode::NAT_SUB,
        (BinOp::Sub, Some(TypeFamily::Float)) => Opcode::FLT_SUB,
        (BinOp::Sub, _) => Opcode::INT_SUB,
        (BinOp::Mul, Some(TypeFamily::Unsigned(_))) => Opcode::NAT_MUL,
        (BinOp::Mul, Some(TypeFamily::Float)) => Opcode::FLT_MUL,
        (BinOp::Mul, _) => Opcode::INT_MUL,
        (BinOp::Div, Some(TypeFamily::Float)) => Opcode::FLT_DIV,
        (BinOp::Div, Some(TypeFamily::Unsigned(_))) => Opcode::NAT_DIV,
        (BinOp::Div, _) => Opcode::INT_DIV,
        (BinOp::Rem, Some(TypeFamily::Float)) => Opcode::FLT_REM,
        (BinOp::Rem, Some(TypeFamily::Unsigned(_))) => Opcode::NAT_REM,
        (BinOp::Rem, _) => Opcode::INT_REM,
        (BinOp::Eq, _) => Opcode::CMP_EQ,
        (BinOp::Ne, _) => Opcode::CMP_NE,
        (BinOp::Lt, Some(TypeFamily::Float)) => Opcode::CMP_FLT,
        (BinOp::Lt, Some(TypeFamily::Unsigned(_))) => Opcode::CMP_LTU,
        (BinOp::Lt, _) => Opcode::CMP_LT,
        (BinOp::Le, Some(TypeFamily::Float)) => Opcode::CMP_FLE,
        (BinOp::Le, Some(TypeFamily::Unsigned(_))) => Opcode::CMP_LEU,
        (BinOp::Le, _) => Opcode::CMP_LE,
        (BinOp::Gt, Some(TypeFamily::Float)) => Opcode::CMP_FGT,
        (BinOp::Gt, Some(TypeFamily::Unsigned(_))) => Opcode::CMP_GTU,
        (BinOp::Gt, _) => Opcode::CMP_GT,
        (BinOp::Ge, Some(TypeFamily::Float)) => Opcode::CMP_FGE,
        (BinOp::Ge, Some(TypeFamily::Unsigned(_))) => Opcode::CMP_GEU,
        (BinOp::Ge, _) => Opcode::CMP_GE,
        (BinOp::And, _) => Opcode::BIT_AND,
        (BinOp::Or, _) => Opcode::BIT_OR,
        (BinOp::Xor, _) => Opcode::BIT_XOR,
        (BinOp::Shl, _) => Opcode::BIT_SHL,
        (BinOp::Shr, Some(TypeFamily::Unsigned(_))) => Opcode::BIT_SRU,
        (BinOp::Shr, _) => Opcode::BIT_SHR,
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
    emit_require(em, fc, operand, "force-unwrap operand")?;
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
        Ty::Named { name_ref, .. } | Ty::Var { name_ref } => em.ast.name_refs[*name_ref].name,
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
        let param_count = u16::try_from(op.params.len())
            .map_err(|_| EmitError::overflow("handler op param count"))?;
        let mut handler_fc = FnCtx::new(param_count);
        for (i, param) in op.params.iter().enumerate() {
            let slot = u32::try_from(i).map_err(|_| EmitError::overflow("handler param index"))?;
            if let Some(&did) = em.pat_defs().get(&param.span) {
                let prev = handler_fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
            }
        }
        let had_value = emit_expr(em, &mut handler_fc, op.body)?;
        if had_value {
            handler_fc.fe.emit_cont_resume();
        } else {
            handler_fc.fe.emit_ret_u();
        }
        handler_fc
            .fe
            .resolve_fixups(&format!("<handler@{handler_fn_id}>"))?;
        let _code_len = handler_fc.fe.validate_code_len()?;
        let type_id = if let Some(&ty_idx) = em.expr_types().get(&op.body) {
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
        fc.fe.emit_cont_mark(u32::from(effect_id), handler_fn_id)?;
    }

    let produced = emit_expr(em, fc, body)?;

    for _ in ops {
        fc.fe.emit_cont_unmark(u32::from(effect_id))?;
    }

    Ok(produced)
}

/// Emit a `do expr` expression. When the body is a call to an effect operation,
/// emit `CONT_SAVE` instead of a regular call.
fn emit_do(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    body: ExprIdx,
    is_tail: bool,
) -> Result<bool, EmitError> {
    if let Expr::Call { callee, args, .. } = &em.ast.exprs[body] {
        let callee = *callee;
        let args: Vec<_> = args.clone();
        if let Some(&def_id) = em.expr_defs().get(&callee) {
            let is_effect_op = em
                .sema
                .defs
                .iter()
                .any(|d| d.id == def_id && d.kind == DefKind::EffectOp);
            if is_effect_op {
                let op_def = em.sema.defs.iter().find(|d| d.id == def_id);
                let is_async =
                    op_def.and_then(|d| d.parent) == Some(em.sema.well_known.effects.async_eff);

                if is_async {
                    let op_name = op_def
                        .map(|d| em.interner.resolve(d.name).to_owned())
                        .unwrap_or_default();
                    return emit_async_op(em, fc, &op_name, &args);
                }

                let op_index = resolve_effect_op_index(em, def_id);
                let arg_count = emit_call_args(em, fc, &args)?;
                let ac = i32::try_from(arg_count)
                    .map_err(|_| EmitError::overflow("effect op arg count"))?;
                fc.fe.emit_cont_save(op_index, ac);
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
            let fn_def_id = em
                .sema
                .resolution
                .expr_defs
                .get(&fn_expr)
                .copied()
                .ok_or_else(|| EmitError::UnsupportedFeature {
                    desc: "spawn argument is not a resolved name".into(),
                })?;
            let fn_id = em.fn_map.get(&fn_def_id).copied().ok_or_else(|| {
                EmitError::UnsupportedFeature {
                    desc: "spawn argument must be a statically known function".into(),
                }
            })?;
            let extra_args = args.get(1..).unwrap_or(&[]);
            let extra_count = emit_call_args(em, fc, extra_args)?;
            let ac = i32::try_from(extra_count)
                .map_err(|_| EmitError::overflow("spawn extra arg count"))?;
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
pub fn emit_deferred_cleanup(em: &mut Emitter<'_>, fc: &mut FnCtx) -> Result<(), EmitError> {
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
fn emit_defer(fc: &mut FnCtx, operand: ExprIdx) -> bool {
    fc.deferred.push(operand);
    false
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
    emit_require(em, fc, base, "update base")?;
    let base_slot = fc.alloc_local();
    fc.fe.emit_st_loc(base_slot);

    // Look up the record type to determine field layout.
    let Some(&ty_idx) = em.expr_types().get(&base) else {
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
            if let RecField::Named {
                name,
                value: Some(val_idx),
                ..
            } = f
                && *name == type_field.name
            {
                return Some(*val_idx);
            }
            None
        });

        if let Some(val_idx) = update_val {
            emit_require(em, fc, val_idx, "update field value")?;
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

/// Returns the field index of a method within a class's member list (declaration order).
fn class_method_index(
    em: &Emitter<'_>,
    class_def: DefId,
    method_sym: Symbol,
) -> Result<u32, EmitError> {
    let members: Vec<(Symbol, DefId)> = em
        .sema
        .defs
        .iter()
        .filter(|d| d.parent == Some(class_def) && d.kind == DefKind::Fn)
        .map(|d| (d.name, d.id))
        .collect();

    for (i, &(sym, _)) in members.iter().enumerate() {
        if sym == method_sym {
            return u32::try_from(i).map_err(|_| EmitError::overflow("class method index"));
        }
    }

    Err(EmitError::UnsupportedFeature {
        desc: "class method not found for dictionary lookup".into(),
    })
}

/// Emits dictionary construction at a call site for a constrained function.
/// For each constraint, builds a product of method fn-id values from the
/// concrete instance that satisfies the constraint at this call site.
fn emit_dict_for_call(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    callee_def: DefId,
    callee_expr: ExprIdx,
) -> Result<usize, EmitError> {
    let constraints = match em.active_fn_constraints().get(&callee_def) {
        Some(c) => c.clone(),
        None => return Ok(0),
    };

    let mut dict_count = 0;
    for constraint in &constraints {
        // Resolve the concrete type at the call site.
        let concrete_ty = constraint.args.first().and_then(|&arg| {
            let resolved = em.sema.unify.resolve(arg, &em.sema.types);
            match &em.sema.types[resolved] {
                Type::Var(_) | Type::Rigid(_) => None,
                _ => Some(resolved),
            }
        });

        if let Some(concrete_ty) = concrete_ty {
            // Find the instance that satisfies this constraint for the concrete type
            let instance = em.sema.instances.iter().find(|inst| {
                inst.class == constraint.class
                    && types_match(&em.sema.types, &em.sema.unify, inst.target, concrete_ty)
            });

            if let Some(inst) = instance {
                // Build ordered method list from the class
                let class_members: Vec<(Symbol, DefId)> = em
                    .sema
                    .defs
                    .iter()
                    .filter(|d| d.parent == Some(constraint.class) && d.kind == DefKind::Fn)
                    .map(|d| (d.name, d.id))
                    .collect();

                let method_count = class_members.len();
                for (class_sym, _) in &class_members {
                    let inst_method = inst.members.iter().find(|(s, _)| s == class_sym);
                    if let Some((_, method_def)) = inst_method {
                        if let Some(&fn_id) = em.fn_map.get(method_def) {
                            let cst_idx = em.cp.intern(&ConstValue::FnRef(fn_id), em.interner)?;
                            fc.fe.emit_ld_cst(cst_idx);
                        } else {
                            return Err(EmitError::UnsupportedFeature {
                                desc: "instance method not compiled for dict construction".into(),
                            });
                        }
                    } else {
                        return Err(EmitError::UnsupportedFeature {
                            desc: "instance missing method required by class".into(),
                        });
                    }
                }
                let mc =
                    u32::try_from(method_count).map_err(|_| EmitError::overflow("method count"))?;
                let sp =
                    i32::try_from(method_count).map_err(|_| EmitError::overflow("method count"))?;
                fc.fe.emit_mk_prd(mc, sp)?;
            } else {
                return Err(EmitError::UnsupportedFeature {
                    desc: "no instance found for call-site constraint".into(),
                });
            }
        } else {
            // Type is still a variable - forward our own dictionary
            if let Some(&dict_slot) = fc.dict_slots.get(&constraint.class) {
                fc.fe.emit_ld_loc(dict_slot);
            } else {
                return Err(EmitError::UnsupportedFeature {
                    desc: "no dictionary to forward for nested generic call".into(),
                });
            }
        }
        let _ = callee_expr;
        dict_count += 1;
    }

    Ok(dict_count)
}
