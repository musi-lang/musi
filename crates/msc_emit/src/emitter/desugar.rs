//! Desugaring: short-circuit operators, assignment, pipe, f-string, and
//! operator desugarings (nil-coalesce, err-coal, propagate, try, range, cons).

use msc_ast::expr::{Expr, FieldKey};
use msc_ast::lit::FStrPart;

use crate::const_pool::ConstValue;
use crate::error::{EmitError, EmitResult};
use msc_ast::ExprIdx;
use super::Emitter;
use super::FnCtx;
use super::expr::{emit_expr, emit_require};
use super::type_query::resolve_field_name;

/// Emit `left and right` with short-circuit evaluation. Leaves bool on stack.
///
/// If left is false, result is false without evaluating right.
pub fn emit_and(em: &mut Emitter<'_>, fc: &mut FnCtx, left: ExprIdx, right: ExprIdx) -> EmitResult {
    let false_label = fc.fresh_label();
    let end_label = fc.fresh_label();

    let produced = emit_expr(em, fc, left)?;
    if produced {
        fc.fe.emit_dup();
        fc.fe.emit_jmp_f(false_label);
        // Left was true - discard it and evaluate right
        fc.fe.emit_pop();
        emit_require(em, fc, right, "short-circuit `and` right operand")?;
        fc.fe.emit_jmp(end_label);
        fc.fe.emit_label(false_label);
        fc.fe.emit_label(end_label);
    }
    Ok(())
}

/// Emit `left or right` with short-circuit evaluation. Leaves bool on stack.
///
/// If left is true, result is true without evaluating right.
pub fn emit_or(em: &mut Emitter<'_>, fc: &mut FnCtx, left: ExprIdx, right: ExprIdx) -> EmitResult {
    let end_label = fc.fresh_label();

    let produced = emit_expr(em, fc, left)?;
    if produced {
        fc.fe.emit_dup();
        // If left is true, jump to end (result is already on stack via dup)
        fc.fe.emit_jmp_t(end_label);
        // Left was false - discard it and evaluate right
        fc.fe.emit_pop();
        emit_require(em, fc, right, "short-circuit `or` right operand")?;
        fc.fe.emit_label(end_label);
    }
    Ok(())
}

/// Emit `left <- right` (assignment). Stores the right value to the left's local slot.
/// Assignment produces no stack value.
pub fn emit_assign(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    let produced = emit_expr(em, fc, right)?;
    if !produced {
        return Ok(());
    }

    let left_expr = em.ast.exprs[left].clone();
    if let Expr::Name { .. } = left_expr
        && let Some(&def_id) = em.expr_defs().get(&left)
    {
        if let Some(&slot) = fc.local_map.get(&def_id) {
            if fc.ref_locals.contains(&def_id) {
                let val_slot = fc.alloc_local();
                fc.fe.emit_st_loc(val_slot);
                fc.fe.emit_ld_loc(slot);
                fc.fe.emit_ld_loc(val_slot);
                fc.fe.emit_st_fld(0)?;
            } else {
                fc.fe.emit_st_loc(slot);
            }
            return Ok(());
        }
        if let Some(&upv_idx) = fc.upvalue_map.get(&def_id)
            && fc.ref_upvalues.contains(&def_id)
        {
            let idx = u8::try_from(upv_idx)
                .map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
            let val_slot = fc.alloc_local();
            fc.fe.emit_st_loc(val_slot);
            fc.fe.emit_ld_upv(idx);
            fc.fe.emit_ld_loc(val_slot);
            fc.fe.emit_st_fld(0)?;
            return Ok(());
        }
        if let Some(&slot) = em.global_map.get(&def_id) {
            fc.fe.emit_st_glb(slot);
            return Ok(());
        }
    }
    // Array element assignment: arr.[idx] <- value
    // ST_IDX expects [array, index, value] on stack.
    if let Expr::Index { object, index, .. } = left_expr {
        let val_slot = fc.alloc_local();
        fc.fe.emit_st_loc(val_slot);
        emit_require(em, fc, object, "index assign: object")?;
        emit_require(em, fc, index, "index assign: index")?;
        fc.fe.emit_ld_loc(val_slot);
        fc.fe.emit_st_idx();
        return Ok(());
    }
    // Record field assignment: obj.field <- value
    // ST_FLD expects [obj, value] on stack.
    if let Expr::Field { object, field, .. } = left_expr {
        let val_slot = fc.alloc_local();
        fc.fe.emit_st_loc(val_slot);
        emit_require(em, fc, object, "field assign: object")?;
        fc.fe.emit_ld_loc(val_slot);
        let field_idx = match field {
            FieldKey::Pos { index, .. } => index,
            FieldKey::Name { name, .. } => resolve_field_name(em, object, name)?,
        };
        fc.fe.emit_st_fld(field_idx)?;
        return Ok(());
    }
    // Discard if we can't resolve the target.
    fc.fe.emit_pop();
    Ok(())
}

/// Emit `left |> right` (pipe). Calls right with left as first argument.
pub fn emit_pipe(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    _pipe_expr_idx: ExprIdx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    emit_require(em, fc, left, "pipe left operand")?;

    let right_expr = em.ast.exprs[right].clone();
    if let Expr::Name { .. } = right_expr {
        if let Some(&def_id) = em.expr_defs().get(&right) {
            if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
                fc.fe.emit_inv_ffi(ffi_idx, 1);
                return Ok(());
            }
            if let Some(&fn_id) = em.fn_map.get(&def_id) {
                fc.fe.emit_inv(fn_id, false, 1);
                return Ok(());
            }
            if let Some(&slot) = fc.local_map.get(&def_id) {
                fc.fe.emit_ld_loc(slot);
                fc.fe.emit_inv_dyn(1)?;
                return Ok(());
            }
        }
        Err(EmitError::UnsupportedFeature {
            desc: "unresolved pipe callee".into(),
        })
    } else {
        // General case: spill piped value, emit callee expr, restore arg, INV_DYN.
        let tmp = fc.alloc_local();
        fc.fe.emit_st_loc(tmp);
        emit_require(em, fc, right, "pipe callee")?;
        fc.fe.emit_ld_loc(tmp);
        fc.fe.emit_inv_dyn(1)?;
        Ok(())
    }
}

/// Emit an f-string by concatenating each part. Leaves the result string on stack.
pub fn emit_fstr(em: &mut Emitter<'_>, fc: &mut FnCtx, parts: &[FStrPart]) -> EmitResult {
    if parts.is_empty() {
        let stridx = em.string_table.intern_str("")?;
        let cv = ConstValue::Str(stridx);
        let i = em.cp.intern(&cv)?;
        fc.fe.emit_ld_cst(i);
        return Ok(());
    }

    let str_cat_idx = em
        .str_cat_ffi_idx
        .ok_or_else(|| EmitError::UnsupportedFeature {
            desc: "str_cat FFI not registered".into(),
        })?;
    let show_ffi_idx = em
        .sema
        .lang_items
        .get("show")
        .and_then(|def| em.foreign_map.get(&def).copied());

    let mut first = true;
    for part in parts {
        match part {
            FStrPart::Text { raw, .. } => {
                let text = em.interner.resolve(*raw);
                let sym = if text.contains('\\') {
                    let unescaped = unescape_str(text);
                    em.interner.intern(&unescaped)
                } else {
                    *raw
                };
                let stridx = em.string_table.intern(sym, em.interner)?;
                let cv = ConstValue::Str(stridx);
                let i = em.cp.intern(&cv)?;
                fc.fe.emit_ld_cst(i);
            }
            FStrPart::Interpolated { expr, .. } => {
                let produced = emit_expr(em, fc, *expr)?;
                if produced {
                    if let Some(show_idx) = show_ffi_idx {
                        fc.fe.emit_inv_ffi(show_idx, 1);
                    }
                } else {
                    let stridx = em.string_table.intern_str("")?;
                    let cv = ConstValue::Str(stridx);
                    let i = em.cp.intern(&cv)?;
                    fc.fe.emit_ld_cst(i);
                }
            }
        }
        if !first {
            fc.fe.emit_inv_ffi(str_cat_idx, 2);
        }
        first = false;
    }
    Ok(())
}

/// Emit `left ?? right` (nil-coalescing). If left is Some, extract payload;
/// otherwise evaluate right as fallback.
pub fn emit_nil_coal(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    emit_coalesce(em, fc, left, right, em.some_tag, "nil-coalesce")
}

/// Emit `operand?` (propagate). If operand is Some, extract payload;
/// if None, early-return None from the current function.
pub fn emit_propagate(em: &mut Emitter<'_>, fc: &mut FnCtx, operand: ExprIdx) -> EmitResult {
    emit_require(em, fc, operand, "propagate operand")?;
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    let some_label = fc.fresh_label();

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_cmp_tag(em.some_tag)?;
    fc.fe.emit_jmp_t(some_label);

    // None path: run deferred cleanup and early-return None
    let deferred = fc.deferred.clone();
    for &def_expr in deferred.iter().rev() {
        let produced = emit_expr(em, fc, def_expr)?;
        if produced {
            fc.fe.emit_pop();
        }
    }
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ret();

    // Some path: extract payload
    fc.fe.emit_label(some_label);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ld_pay(0);
    Ok(())
}

/// Emit `left..right` or `left..<right` (range construction as a 2-tuple).
pub fn emit_range(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    emit_require(em, fc, left, "range start")?;
    emit_require(em, fc, right, "range end")?;
    fc.fe.emit_mk_prd(2, 1)?; // (start, end) product, pops 2 pushes 1 -> net pop 1
    Ok(())
}

/// Emit `left !! right` (error coalescing). If left is Ok, extract payload;
/// otherwise evaluate right as fallback.
pub fn emit_err_coal(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    emit_coalesce(em, fc, left, right, em.ok_tag, "err-coalesce")
}

/// Shared implementation for nil-coalesce (`??`) and err-coalesce (`!!`).
///
/// Checks left against `success_tag` (Some for `??`, Ok for `!!`).
/// If the tag matches, extracts payload; otherwise evaluates right as fallback.
fn emit_coalesce(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
    success_tag: u32,
    op_name: &str,
) -> EmitResult {
    emit_require(em, fc, left, &format!("{op_name} left operand"))?;
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    let success_label = fc.fresh_label();
    let end_label = fc.fresh_label();

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_cmp_tag(success_tag)?;
    fc.fe.emit_jmp_t(success_label);

    // Fallback path: evaluate right
    emit_require(em, fc, right, &format!("{op_name} right operand"))?;
    fc.fe.emit_jmp(end_label);

    // Success path: extract payload
    fc.fe.emit_label(success_label);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ld_pay(0);

    fc.fe.emit_label(end_label);
    Ok(())
}

/// Emit `x :: xs` (cons). Constructs a (head, tail) product.
pub fn emit_cons(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> EmitResult {
    emit_require(em, fc, left, "cons head")?;
    emit_require(em, fc, right, "cons tail")?;
    fc.fe.emit_mk_prd(2, 1)?; // (head, tail)
    Ok(())
}

/// Process backslash escape sequences in a string literal.
fn unescape_str(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('0') => out.push('\0'),
                Some('\\') | None => out.push('\\'),
                Some('"') => out.push('"'),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}
