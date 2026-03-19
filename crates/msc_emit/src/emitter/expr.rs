//! Expression emission: tree-walks AST expressions, leaving results on the stack.

use msc_ast::ExprIdx;
use msc_ast::Pat;
use msc_ast::PatIdx;
use msc_ast::expr::{
    Arg, ArrayElem, BinOp, BindKind, Expr, FieldKey, LetFields, TypeCheckKind, UnaryOp,
};
use msc_ast::lit::Lit;
use msc_bc::Opcode;
use msc_sema::def::DefKind;
use msc_sema::types::Type;
use msc_shared::Span;
use msc_shared::Symbol;

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use crate::error::EmitResult;

use super::Emitter;
use super::FnCtx;
use super::capture;
use super::closure;
use super::desugar;
use super::effects;
use super::record;
use super::type_query;
use super::type_query::{TypeFamily, Width};
use super::typeclass;

/// Emit bytecode for `expr_idx`. Returns `true` if a value was pushed onto the stack.
///
/// `is_tail` indicates whether this expression is in tail position of a function body.
/// When true, direct calls can be emitted as tail calls.
pub fn emit_expr(em: &mut Emitter<'_>, fc: &mut FnCtx, expr_idx: ExprIdx) -> EmitResult<bool> {
    emit_expr_tail(em, fc, expr_idx, false)
}

/// Emit an expression, returning an error if it produces no stack value.
pub(super) fn emit_require(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    idx: ExprIdx,
    ctx: &str,
) -> EmitResult {
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
    ty: ExprIdx,
    is_tail: bool,
) -> EmitResult<bool> {
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
) -> EmitResult<bool> {
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
            emit_unary(em, fc, op, operand, span)
        }
        Expr::Need { operand, .. } => {
            let operand = *operand;
            effects::emit_need(em, fc, operand, is_tail)
        }
        Expr::Resume { value, .. } => {
            let value = *value;
            effects::emit_resume(em, fc, value)
        }
        Expr::Call { callee, args, .. } => {
            let (callee, args) = (*callee, args.clone());
            emit_call(em, fc, callee, &args, is_tail)
        }
        Expr::Tuple { elems, .. } => emit_tuple(em, fc, &elems.clone()),
        Expr::Record { fields, .. } => record::emit_record_lit(em, fc, &fields.clone(), None),
        Expr::Array { elems, .. } => {
            emit_array(em, fc, &elems.clone())?;
            Ok(true)
        }
        Expr::Variant { name, args, .. } => record::emit_variant(em, fc, *name, &args.clone()),
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
                    let cv = ConstValue::Int(i64::from(fn_id));
                    let i = em.cp.intern(&cv)?;
                    fc.fe.emit_ld_cst(i);
                    return Ok(true);
                }
                if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
                    let cv = ConstValue::Int(i64::from(ffi_idx));
                    let i = em.cp.intern(&cv)?;
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
        Expr::Fn { params, body, .. } => closure::emit_fn(em, fc, &params.clone(), *body),
        Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Choice { .. }
        | Expr::RecordDef { .. }
        | Expr::Class { .. }
        | Expr::Instance { .. }
        | Expr::Effect { .. }
        | Expr::Foreign { .. }
        | Expr::Error { .. }
        | Expr::TypeApp { .. }
        | Expr::FnType { .. }
        | Expr::OptionType { .. }
        | Expr::ProductType { .. }
        | Expr::SumType { .. }
        | Expr::ArrayType { .. }
        | Expr::PiType { .. } => Ok(false),
        Expr::Update { base, fields, .. } => record::emit_update(em, fc, *base, &fields.clone()),
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
            effects::emit_handle(em, fc, effect_ty, &ops, body)
        }
    }
}

fn emit_name(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
    name: Symbol,
    _span: Span,
) -> EmitResult<bool> {
    let Some(&def_id) = em.expr_defs().get(&expr_idx) else {
        return Err(EmitError::UnsupportedFeature {
            desc: format!("unresolved name `{}`", em.interner.resolve(name)).into(),
        });
    };
    if let Some(&fn_id) = em.fn_map.get(&def_id) {
        let cv = ConstValue::Int(i64::from(fn_id));
        let i = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
        let cv = ConstValue::Int(i64::from(ffi_idx));
        let i = em.cp.intern(&cv)?;
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
    ty_idx: msc_sema::TypeIdx,
) -> EmitResult<bool> {
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
                    let cv = ConstValue::Int(i64::from(fn_id));
                    let i = em.cp.intern(&cv)?;
                    fc.fe.emit_ld_cst(i);
                    emitted = true;
                    break;
                }
                if let Some(&ffi_idx) = em.foreign_map.get(&def.id) {
                    let cv = ConstValue::Int(i64::from(ffi_idx));
                    let i = em.cp.intern(&cv)?;
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
) -> EmitResult<bool> {
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
) -> EmitResult<bool> {
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
                    if capture::body_references_def(em, fn_body, def_id) {
                        closure::emit_letrec_fn(
                            em, fc, def_id, fields.pat, val_idx, &params, fn_body,
                        )?;
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

fn emit_binding(em: &mut Emitter<'_>, fc: &mut FnCtx, fields: &LetFields) -> EmitResult<bool> {
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
) -> EmitResult<bool> {
    emit_require(em, fc, operand, "unary operand")?;
    match op {
        UnaryOp::Neg => {
            fc.fe.emit_unop(Opcode::NEG);
            Ok(true)
        }
        UnaryOp::Not => {
            fc.fe.emit_unop(Opcode::NOT);
            Ok(true)
        }
        UnaryOp::ForceUnwrap => effects::emit_force_unwrap(em, fc, operand),
        UnaryOp::Propagate => {
            desugar::emit_propagate(em, fc, operand)?;
            Ok(true)
        }
        UnaryOp::Try => {
            desugar::emit_try(em, fc, operand)?;
            Ok(true)
        }
        UnaryOp::Defer => Ok(emit_defer(fc, operand)),
    }
}

fn emit_tuple(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ExprIdx]) -> EmitResult<bool> {
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
) -> EmitResult<bool> {
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
            FieldKey::Name { name, .. } => type_query::resolve_field_name(em, object, name)?,
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
            FieldKey::Name { name, .. } => type_query::resolve_field_name(em, object, name)?,
        };
        if type_query::object_type_is_tuple(em, object) {
            fc.fe.emit_tup_get(index)?;
        } else {
            fc.fe.emit_ld_fld(index)?;
        }
    }

    Ok(true)
}

fn emit_index(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    object: ExprIdx,
    index: ExprIdx,
) -> EmitResult<bool> {
    emit_require(em, fc, object, "index object")?;
    emit_require(em, fc, index, "index value")?;
    fc.fe.emit_ld_idx();
    Ok(true)
}

fn emit_return(em: &mut Emitter<'_>, fc: &mut FnCtx, value: Option<ExprIdx>) -> EmitResult<bool> {
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

fn emit_type_test(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
    ty: ExprIdx,
) -> EmitResult<bool> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        let cv = ConstValue::Int(1);
        let i = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(i);
        return Ok(true);
    }
    if let Some(type_id) = type_query::lower_ast_ty_to_type_id(em, ty) {
        fc.fe.emit_type_chk(type_id);
    } else {
        // Type couldn't be resolved - fall back to always-true.
        fc.fe.emit_pop();
        let cv = ConstValue::Int(1);
        let i = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(i);
    }
    Ok(true)
}

/// Emit `:?>` (type cast): evaluate operand, check type, UNR on mismatch.
fn emit_type_cast(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
    ty: ExprIdx,
    is_tail: bool,
) -> EmitResult<bool> {
    let produced = emit_expr_tail(em, fc, operand, is_tail)?;
    if !produced {
        return Ok(false);
    }
    let Some(type_id) = type_query::lower_ast_ty_to_type_id(em, ty) else {
        // Can't resolve type - leave value on stack, assume caller knows what they're doing.
        return Ok(true);
    };
    fc.fe.emit_dup();
    fc.fe.emit_type_chk(type_id);
    let ok_label = fc.fresh_label();
    fc.fe.emit_jmp_t(ok_label);
    fc.fe.emit_unop(msc_bc::Opcode::PANIC);
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
) -> EmitResult<bool> {
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
        let method_idx =
            typeclass::class_method_index(em, dict_lookup.class, dict_lookup.method_sym)?;
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
) -> EmitResult<bool> {
    match op {
        BinOp::And => {
            let family = type_query::classify_type_family(em, left)
                .or_else(|| type_query::classify_type_family(em, right));
            if type_query::is_logical_family(family) {
                desugar::emit_and(em, fc, left, right)?;
            } else {
                emit_require(em, fc, left, "binop left operand")?;
                emit_require(em, fc, right, "binop right operand")?;
                fc.fe.emit_binop(Opcode::AND);
            }
            Ok(true)
        }
        BinOp::Or => {
            let family = type_query::classify_type_family(em, left)
                .or_else(|| type_query::classify_type_family(em, right));
            if type_query::is_logical_family(family) {
                desugar::emit_or(em, fc, left, right)?;
            } else {
                emit_require(em, fc, left, "binop left operand")?;
                emit_require(em, fc, right, "binop right operand")?;
                fc.fe.emit_binop(Opcode::OR);
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
            let family = type_query::classify_type_family(em, left);
            let opcode = type_query::map_binop(op, family)?;
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
) -> EmitResult {
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
        let idx = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(idx);
        fc.fe.emit_binop(Opcode::SHL);
        let idx2 = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(idx2);
        fc.fe.emit_binop(Opcode::SHR);
    } else {
        let cv = ConstValue::Int(mask.cast_signed());
        let idx = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(idx);
        fc.fe.emit_binop(Opcode::AND);
    }
    Ok(())
}

fn emit_call(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    callee: ExprIdx,
    args: &[Arg],
    is_tail: bool,
) -> EmitResult<bool> {
    // Direct dispatch: if the callee resolves to a known function, emit a
    // static call (INV / INV_FFI) instead of dynamic dispatch (INV_DYN).
    // This handles both simple names (`foo(...)`) and resolved import field
    // chains (`rt.writeln(...)`, `t.assertions.assert_eq(...)`).
    if let Some(&def_id) = em.expr_defs().get(&callee) {
        let dict_count = typeclass::emit_dict_for_call(em, fc, def_id, callee)?;
        let explicit_count = emit_call_args(em, fc, args)?;
        let explicit_count_u32 =
            u32::try_from(explicit_count).map_err(|_| EmitError::overflow("arg count"))?;
        let total_count = dict_count + explicit_count_u32;

        if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
            let ac = i32::try_from(total_count).map_err(|_| EmitError::overflow("arg count"))?;
            fc.fe.emit_inv_ffi(ffi_idx, ac);
            return Ok(true);
        }
        if let Some(&fn_id) = em.fn_map.get(&def_id) {
            let ac = i32::try_from(total_count).map_err(|_| EmitError::overflow("arg count"))?;
            if is_tail {
                fc.fe.emit_inv_tail(fn_id, false, ac);
                return Ok(false);
            }
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
            let mut arg_temps = Vec::with_capacity(
                total_count
                    .try_into()
                    .map_err(|_| EmitError::overflow("arg count"))?,
            );
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

fn emit_lit(em: &mut Emitter<'_>, fc: &mut FnCtx, lit: &Lit) -> EmitResult<bool> {
    match lit {
        Lit::Unit { .. } => Ok(false),
        Lit::Int { value, .. } => {
            if let Ok(smi) = i16::try_from(*value) {
                fc.fe.emit_ld_smi(smi);
            } else {
                let cv = ConstValue::Int(*value);
                let i = em.cp.intern(&cv)?;
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }
        Lit::Float { value, .. } => {
            let cv = ConstValue::Float(*value);
            let i = em.cp.intern(&cv)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::Rune { codepoint, .. } => {
            let cv = ConstValue::Int(i64::from(*codepoint));
            let i = em.cp.intern(&cv)?;
            fc.fe.emit_ld_cst(i);
            Ok(true)
        }
        Lit::Str { value, .. } => {
            let stridx = em.string_table.intern(*value, em.interner)?;
            let cv = ConstValue::Str(stridx);
            let i = em.cp.intern(&cv)?;
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
pub fn bind_pat(em: &mut Emitter<'_>, fc: &mut FnCtx, pat_idx: PatIdx) -> EmitResult {
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
                let elem_cst_idx = em.cp.intern(&ConstValue::Int(elem_idx))?;
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
fn bind_pat_ref(em: &mut Emitter<'_>, fc: &mut FnCtx, pat_idx: PatIdx) -> EmitResult {
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

pub(super) fn emit_call_args(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    args: &[Arg],
) -> EmitResult<usize> {
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

fn emit_array(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> EmitResult {
    let has_spread = elems.iter().any(|e| matches!(e, ArrayElem::Spread { .. }));

    if has_spread {
        emit_array_with_spread(em, fc, elems)
    } else {
        emit_array_fixed(em, fc, elems)
    }
}

fn emit_array_fixed(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> EmitResult {
    let count =
        u32::try_from(elems.len()).map_err(|_| EmitError::overflow("array element count"))?;
    let len_cv = ConstValue::Int(i64::from(count));
    let li = em.cp.intern(&len_cv)?;
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
        let ci = em.cp.intern(&idx_cv)?;
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
fn emit_array_with_spread(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> EmitResult {
    // Count the fixed elements (non-spread) to use as the base in the size sum.
    let fixed_count = elems
        .iter()
        .filter(|e| matches!(e, ArrayElem::Elem { .. }))
        .count();
    let fixed_count_i64 =
        i64::try_from(fixed_count).map_err(|_| EmitError::overflow("array fixed element count"))?;

    // Compute total length: start with the fixed count constant, then add LD_LEN for each spread.
    let base_cv = ConstValue::Int(fixed_count_i64);
    let bi = em.cp.intern(&base_cv)?;
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
            fc.fe.emit_binop(Opcode::ADD);
        }
    }

    // Stack: [total_length]. MK_ARR pops it, pushes array ref.
    fc.fe.emit_mk_arr(0);
    let arr_slot = fc.alloc_local();
    fc.fe.emit_st_loc(arr_slot);

    // Write index local, starts at 0.
    let widx_slot = fc.alloc_local();
    let zero_cv = ConstValue::Int(0);
    let zi = em.cp.intern(&zero_cv)?;
    fc.fe.emit_ld_cst(zi);
    fc.fe.emit_st_loc(widx_slot);

    let one_cv = ConstValue::Int(1);
    let one_i = em.cp.intern(&one_cv)?;

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
                fc.fe.emit_binop(Opcode::ADD);
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
                fc.fe.emit_binop(Opcode::ADD);
                fc.fe.emit_st_loc(widx_slot);

                // ridx += 1
                fc.fe.emit_ld_loc(ridx_slot);
                fc.fe.emit_ld_cst(one_i);
                fc.fe.emit_binop(Opcode::ADD);
                fc.fe.emit_st_loc(ridx_slot);

                fc.fe.emit_jmp(loop_start);
                fc.fe.emit_label(loop_end);
            }
        }
    }

    fc.fe.emit_ld_loc(arr_slot);
    Ok(())
}

/// Emit deferred expressions (in reverse order) for cleanup before function exit.
pub fn emit_deferred_cleanup(em: &mut Emitter<'_>, fc: &mut FnCtx) -> EmitResult {
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
