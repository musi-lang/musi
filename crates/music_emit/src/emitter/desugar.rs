//! Desugaring: short-circuit operators, assignment, pipe, and f-string emission.

use music_ast::expr::Expr;
use music_ast::lit::FStrPart;

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use music_ast::ExprIdx;

use super::super::emitter::Emitter;
use super::FnCtx;
use super::expr::emit_expr;

/// Emit `left and right` with short-circuit evaluation. Leaves bool on stack.
///
/// If left is false, result is false without evaluating right.
pub fn emit_and(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    let false_label = fc.fresh_label();
    let end_label = fc.fresh_label();

    let produced = emit_expr(em, fc, left)?;
    if produced {
        fc.fe.emit_dup();
        fc.fe.emit_jmp_f(false_label);
        // Left was true — discard it and evaluate right
        fc.fe.emit_pop();
        let produced_right = emit_expr(em, fc, right)?;
        if !produced_right {
            return Err(EmitError::UnsupportedFeature {
                desc: "short-circuit `and` right operand produced no value".into(),
            });
        }
        fc.fe.emit_jmp(end_label);
        fc.fe.emit_label(false_label);
        fc.fe.emit_label(end_label);
    }
    Ok(())
}

/// Emit `left or right` with short-circuit evaluation. Leaves bool on stack.
///
/// If left is true, result is true without evaluating right.
pub fn emit_or(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    let end_label = fc.fresh_label();

    let produced = emit_expr(em, fc, left)?;
    if produced {
        fc.fe.emit_dup();
        // If left is true, jump to end (result is already on stack via dup)
        fc.fe.emit_jmp_t(end_label);
        // Left was false — discard it and evaluate right
        fc.fe.emit_pop();
        let produced_right = emit_expr(em, fc, right)?;
        if !produced_right {
            return Err(EmitError::UnsupportedFeature {
                desc: "short-circuit `or` right operand produced no value".into(),
            });
        }
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
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, right)?;
    if !produced {
        return Ok(());
    }

    let left_expr = em.ast.exprs[left].clone();
    if let Expr::Name { .. } = left_expr
        && let Some(&def_id) = em.sema.resolution.expr_defs.get(&left)
        && let Some(&slot) = fc.local_map.get(&def_id)
    {
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
    // Discard if we can't resolve the target or it's not a name.
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
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, left)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "pipe left operand produced no value".into(),
        });
    }

    let right_expr = em.ast.exprs[right].clone();
    match right_expr {
        Expr::Name { .. } => {
            if let Some(&def_id) = em.sema.resolution.expr_defs.get(&right) {
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
        }
        _ => Err(EmitError::UnsupportedFeature {
            desc: "complex pipe callee".into(),
        }),
    }
}

/// Emit an f-string by concatenating each part. Leaves the result string on stack.
pub fn emit_fstr(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    parts: &[FStrPart],
) -> Result<(), EmitError> {
    if parts.is_empty() {
        let empty_sym = em.interner.intern("");
        let cv = ConstValue::Str(empty_sym);
        let i = em.cp.intern(&cv, em.interner)?;
        fc.fe.emit_ld_cst(i);
        return Ok(());
    }

    let str_cat_idx = em
        .str_cat_ffi_idx
        .ok_or_else(|| EmitError::UnsupportedFeature {
            desc: "str_cat FFI not registered".into(),
        })?;
    let show_def = em.sema.well_known.fns.show;
    let show_ffi_idx = em.foreign_map.get(&show_def).copied();

    let mut first = true;
    for part in parts {
        match part {
            FStrPart::Text { raw, .. } => {
                let cv = ConstValue::Str(*raw);
                let i = em.cp.intern(&cv, em.interner)?;
                fc.fe.emit_ld_cst(i);
            }
            FStrPart::Interpolated { expr, .. } => {
                let produced = emit_expr(em, fc, *expr)?;
                if produced {
                    if let Some(show_idx) = show_ffi_idx {
                        fc.fe.emit_inv_ffi(show_idx, 1);
                    }
                } else {
                    let empty_sym = em.interner.intern("");
                    let cv = ConstValue::Str(empty_sym);
                    let i = em.cp.intern(&cv, em.interner)?;
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
