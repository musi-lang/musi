//! Desugaring: short-circuit operators, assignment, pipe, f-string, and
//! operator desugarings (nil-coalesce, err-coal, propagate, try, range, cons).

use music_ast::expr::Expr;
use music_ast::lit::FStrPart;

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use musi_bc::Opcode;
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

/// Emit `left ?? right` (nil-coalescing). If left is Some, extract payload;
/// otherwise evaluate right as fallback.
pub fn emit_nil_coal(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, left)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "nil-coalesce left operand produced no value".into(),
        });
    }
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    let some_label = fc.fresh_label();
    let end_label = fc.fresh_label();

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_cmp_tag(em.some_tag)?;
    fc.fe.emit_jmp_t(some_label);

    // None path: evaluate fallback
    let produced_right = emit_expr(em, fc, right)?;
    if !produced_right {
        return Err(EmitError::UnsupportedFeature {
            desc: "nil-coalesce right operand produced no value".into(),
        });
    }
    fc.fe.emit_jmp(end_label);

    // Some path: extract payload
    fc.fe.emit_label(some_label);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ld_pay(0);

    fc.fe.emit_label(end_label);
    Ok(())
}

/// Emit `operand?` (propagate). If operand is Some, extract payload;
/// if None, early-return None from the current function.
pub fn emit_propagate(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "propagate operand produced no value".into(),
        });
    }
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

/// Emit `try operand`. Evaluates operand and wraps result in Some(value).
pub fn emit_try(em: &mut Emitter<'_>, fc: &mut FnCtx, operand: ExprIdx) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, operand)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "try operand produced no value".into(),
        });
    }
    fc.fe.emit_mk_var(em.some_tag)?;
    Ok(())
}

/// Emit `left..right` or `left..<right` (range construction as a 2-tuple).
pub fn emit_range(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    let produced_left = emit_expr(em, fc, left)?;
    if !produced_left {
        return Err(EmitError::UnsupportedFeature {
            desc: "range start produced no value".into(),
        });
    }
    let produced_right = emit_expr(em, fc, right)?;
    if !produced_right {
        return Err(EmitError::UnsupportedFeature {
            desc: "range end produced no value".into(),
        });
    }
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
) -> Result<(), EmitError> {
    let produced = emit_expr(em, fc, left)?;
    if !produced {
        return Err(EmitError::UnsupportedFeature {
            desc: "err-coalesce left operand produced no value".into(),
        });
    }
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    let ok_label = fc.fresh_label();
    let end_label = fc.fresh_label();

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_cmp_tag(em.ok_tag)?;
    fc.fe.emit_jmp_t(ok_label);

    // Err path: evaluate fallback
    let produced_right = emit_expr(em, fc, right)?;
    if !produced_right {
        return Err(EmitError::UnsupportedFeature {
            desc: "err-coalesce right operand produced no value".into(),
        });
    }
    fc.fe.emit_jmp(end_label);

    // Ok path: extract payload
    fc.fe.emit_label(ok_label);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_ld_pay(0);

    fc.fe.emit_label(end_label);
    Ok(())
}

/// Emit `left in right` (membership test). Linear scan over the array `right`,
/// returning `true` if any element equals `left`, `false` otherwise.
pub fn emit_in_op(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    // Evaluate operands into temp slots so we can reload them inside the loop.
    let produced_right = emit_expr(em, fc, right)?;
    if !produced_right {
        return Err(EmitError::UnsupportedFeature {
            desc: "`in` right operand produced no value".into(),
        });
    }
    let arr_slot = fc.alloc_local();
    fc.fe.emit_st_loc(arr_slot);

    let produced_left = emit_expr(em, fc, left)?;
    if !produced_left {
        return Err(EmitError::UnsupportedFeature {
            desc: "`in` left operand produced no value".into(),
        });
    }
    let needle_slot = fc.alloc_local();
    fc.fe.emit_st_loc(needle_slot);

    // Get array length once.
    fc.fe.emit_ld_loc(arr_slot);
    fc.fe.emit_ld_len();
    let len_slot = fc.alloc_local();
    fc.fe.emit_st_loc(len_slot);

    // i = 0
    let zero_cv = ConstValue::Int(0);
    let zero_idx = em.cp.intern(&zero_cv, em.interner)?;
    fc.fe.emit_ld_cst(zero_idx);
    let i_slot = fc.alloc_local();
    fc.fe.emit_st_loc(i_slot);

    let loop_start = fc.fresh_label();
    let found_true = fc.fresh_label();
    let loop_exit = fc.fresh_label();
    let end_label = fc.fresh_label();

    fc.fe.emit_label(loop_start);

    // Check i < len; if not, exit loop and push false.
    fc.fe.emit_ld_loc(i_slot);
    fc.fe.emit_ld_loc(len_slot);
    fc.fe.emit_binop(Opcode::CMP_LT);
    fc.fe.emit_jmp_f(loop_exit);

    // Load arr[i] and compare with needle.
    fc.fe.emit_ld_loc(arr_slot);
    fc.fe.emit_ld_loc(i_slot);
    fc.fe.emit_ld_idx();
    fc.fe.emit_ld_loc(needle_slot);
    fc.fe.emit_cmp_eq();
    fc.fe.emit_jmp_t(found_true);

    // i += 1
    fc.fe.emit_ld_loc(i_slot);
    let one_cv = ConstValue::Int(1);
    let one_idx = em.cp.intern(&one_cv, em.interner)?;
    fc.fe.emit_ld_cst(one_idx);
    fc.fe.emit_binop(Opcode::I_ADD);
    fc.fe.emit_st_loc(i_slot);

    fc.fe.emit_jmp(loop_start);

    // found_true: push true and jump past the false path.
    fc.fe.emit_label(found_true);
    let true_cv = ConstValue::Bool(true);
    let true_idx = em.cp.intern(&true_cv, em.interner)?;
    fc.fe.emit_ld_cst(true_idx);
    fc.fe.emit_jmp(end_label);

    // loop_exit: exhausted array without a match, push false.
    fc.fe.emit_label(loop_exit);
    let false_cv = ConstValue::Bool(false);
    let false_idx = em.cp.intern(&false_cv, em.interner)?;
    fc.fe.emit_ld_cst(false_idx);

    fc.fe.emit_label(end_label);

    Ok(())
}

/// Emit `x :: xs` (cons). Constructs a (head, tail) product.
pub fn emit_cons(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    left: ExprIdx,
    right: ExprIdx,
) -> Result<(), EmitError> {
    let produced_left = emit_expr(em, fc, left)?;
    if !produced_left {
        return Err(EmitError::UnsupportedFeature {
            desc: "cons head produced no value".into(),
        });
    }
    let produced_right = emit_expr(em, fc, right)?;
    if !produced_right {
        return Err(EmitError::UnsupportedFeature {
            desc: "cons tail produced no value".into(),
        });
    }
    fc.fe.emit_mk_prd(2, 1)?; // (head, tail)
    Ok(())
}
