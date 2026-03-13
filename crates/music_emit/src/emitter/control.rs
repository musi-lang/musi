//! Control-flow instruction emission: piecewise and match expressions.

use music_ast::PatIdx;
use music_ast::expr::{MatchArm, PwArm, PwGuard};
use music_ast::lit::Lit;
use music_ast::{AstArenas, Pat};

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use music_ast::ExprIdx;

use super::super::emitter::Emitter;
use super::FnCtx;
use super::expr::{emit_expr, emit_expr_tail, resolve_variant_tag_by_name};

/// Emit a piecewise expression. Leaves result on stack.
pub fn emit_piecewise(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    arms: &[PwArm],
    is_tail: bool,
) -> Result<(), EmitError> {
    if arms.is_empty() {
        return Err(EmitError::UnsupportedFeature {
            desc: "empty piecewise expression".into(),
        });
    }

    let result_slot = fc.alloc_local();
    let merge_label = fc.fresh_label();

    let n = arms.len();
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        if is_last || matches!(arm.guard, PwGuard::Any { .. }) {
            let produced = emit_expr_tail(em, fc, arm.result, is_tail)?;
            if produced {
                fc.fe.emit_st_loc(result_slot);
            }
            fc.fe.emit_jmp(merge_label);
        } else if let PwGuard::When {
            expr: guard_expr, ..
        } = arm.guard
        {
            let then_label = fc.fresh_label();
            let next_label = fc.fresh_label();
            let produced_guard = emit_expr(em, fc, guard_expr)?;
            if produced_guard {
                fc.fe.emit_jmp_t(then_label);
            }
            fc.fe.emit_jmp(next_label);
            fc.fe.emit_label(then_label);
            let produced = emit_expr_tail(em, fc, arm.result, is_tail)?;
            if produced {
                fc.fe.emit_st_loc(result_slot);
            }
            fc.fe.emit_jmp(merge_label);
            fc.fe.emit_label(next_label);
        }
    }

    fc.fe.emit_label(merge_label);
    fc.fe.emit_ld_loc(result_slot);
    Ok(())
}

/// Emit a match expression. Leaves result on stack.
pub fn emit_match(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    scrutinee: ExprIdx,
    arms: &[MatchArm],
    is_tail: bool,
) -> Result<(), EmitError> {
    let produced_scrutinee = emit_expr(em, fc, scrutinee)?;
    let scrutinee_slot = fc.alloc_local();
    if produced_scrutinee {
        fc.fe.emit_st_loc(scrutinee_slot);
    }

    let has_variant_arms = arms.iter().any(|arm| is_variant_pat(arm.pat, em.ast));
    let tag_slot = if has_variant_arms {
        let slot = fc.alloc_local();
        fc.fe.emit_ld_loc(scrutinee_slot);
        fc.fe.emit_ld_tag();
        fc.fe.emit_st_loc(slot);
        Some(slot)
    } else {
        None
    };

    let result_slot = fc.alloc_local();
    let merge_label = fc.fresh_label();

    let n = arms.len();
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        let next_label = if is_last {
            None
        } else {
            Some(fc.fresh_label())
        };

        if let Some(fail_label) = next_label {
            emit_pat_test(em, fc, arm.pat, scrutinee_slot, tag_slot, fail_label)?;
        }

        emit_pat_bind(em, fc, arm.pat, scrutinee_slot)?;

        if let Some(guard_expr) = arm.guard {
            let produced_guard = emit_expr(em, fc, guard_expr)?;
            if let Some(fail_label) = next_label {
                if produced_guard {
                    fc.fe.emit_jmp_f(fail_label);
                }
            } else if produced_guard {
                fc.fe.emit_pop();
            }
        }

        let produced = emit_expr_tail(em, fc, arm.result, is_tail)?;
        if produced {
            fc.fe.emit_st_loc(result_slot);
        }
        fc.fe.emit_jmp(merge_label);

        if let Some(fail_label) = next_label {
            fc.fe.emit_label(fail_label);
        }
    }

    fc.fe.emit_label(merge_label);
    fc.fe.emit_ld_loc(result_slot);
    Ok(())
}

/// Test whether scrutinee at `scrutinee_slot` matches `pat`.
/// Jumps to `fail_label` if not matched.
/// `tag_slot` holds the cached tag value when variant arms are present.
fn emit_pat_test(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    pat_idx: PatIdx,
    scrutinee_slot: u32,
    tag_slot: Option<u32>,
    fail_label: u32,
) -> Result<(), EmitError> {
    let pat = em.ast.pats[pat_idx].clone();
    match pat {
        Pat::Bind {
            inner: Some(inner_pat),
            ..
        } => emit_pat_test(em, fc, inner_pat, scrutinee_slot, tag_slot, fail_label),
        Pat::Lit { lit, .. } => emit_lit_pat_test(em, fc, scrutinee_slot, &lit, fail_label),
        Pat::Variant { name, .. } => {
            let tag = resolve_variant_tag_by_name(em, name)?;
            if let Some(ts) = tag_slot {
                let tag_cv = ConstValue::Int(i64::from(tag));
                let ci = em.cp.intern(&tag_cv, em.interner)?;
                fc.fe.emit_ld_loc(ts);
                fc.fe.emit_ld_cst(ci);
                fc.fe.emit_cmp_eq();
            } else {
                fc.fe.emit_ld_loc(scrutinee_slot);
                fc.fe.emit_cmp_tag(tag)?;
            }
            fc.fe.emit_jmp_f(fail_label);
            Ok(())
        }
        Pat::Or { left, right, .. } => {
            let try_right = fc.fresh_label();
            let pass_label = fc.fresh_label();
            emit_pat_test(em, fc, left, scrutinee_slot, tag_slot, try_right)?;
            fc.fe.emit_jmp(pass_label);
            fc.fe.emit_label(try_right);
            emit_pat_test(em, fc, right, scrutinee_slot, tag_slot, fail_label)?;
            fc.fe.emit_label(pass_label);
            Ok(())
        }
        Pat::Array { elems, .. } => {
            let expected_len = i64::try_from(elems.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "array pattern element count".into(),
            })?;
            let len_const = ConstValue::Int(expected_len);
            let len_cst_idx = em.cp.intern(&len_const, em.interner)?;
            fc.fe.emit_ld_loc(scrutinee_slot);
            fc.fe.emit_ld_len();
            fc.fe.emit_ld_cst(len_cst_idx);
            fc.fe.emit_cmp_eq();
            fc.fe.emit_jmp_f(fail_label);
            Ok(())
        }
        Pat::Wild { .. }
        | Pat::Bind { inner: None, .. }
        | Pat::Tuple { .. }
        | Pat::Record { .. }
        | Pat::Error { .. } => Ok(()),
    }
}

fn emit_lit_pat_test(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    scrutinee_slot: u32,
    lit: &Lit,
    fail_label: u32,
) -> Result<(), EmitError> {
    let cv = match lit {
        Lit::Int { value, .. } => ConstValue::Int(*value),
        Lit::Rune { codepoint, .. } => ConstValue::Rune(*codepoint),
        Lit::Str { value, .. } => ConstValue::Str(*value),
        Lit::Float { value, .. } => ConstValue::Float(*value),
        Lit::Unit { .. } | Lit::FStr { .. } => return Ok(()),
    };

    fc.fe.emit_ld_loc(scrutinee_slot);
    let ci = em.cp.intern(&cv, em.interner)?;
    fc.fe.emit_ld_cst(ci);
    fc.fe.emit_cmp_eq();
    fc.fe.emit_jmp_f(fail_label);
    Ok(())
}

/// Bind pattern variables by loading from `value_slot`.
fn emit_pat_bind(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    pat_idx: PatIdx,
    value_slot: u32,
) -> Result<(), EmitError> {
    let pat = em.ast.pats[pat_idx].clone();
    match pat {
        Pat::Bind { span, inner, .. } => {
            let slot = fc.alloc_local();
            fc.fe.emit_ld_loc(value_slot);
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.sema.resolution.pat_defs.get(&span) {
                let prev = fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
            }
            if let Some(inner_pat) = inner {
                emit_pat_bind(em, fc, inner_pat, slot)?;
            }
            Ok(())
        }
        Pat::Tuple { elems, .. } => {
            for (i, &elem) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| EmitError::OperandOverflow {
                    desc: "tuple pattern index".into(),
                })?;
                let field_slot = fc.alloc_local();
                fc.fe.emit_ld_loc(value_slot);
                fc.fe.emit_ld_fld(idx)?;
                fc.fe.emit_st_loc(field_slot);
                emit_pat_bind(em, fc, elem, field_slot)?;
            }
            Ok(())
        }
        Pat::Variant { args, .. } => {
            if args.is_empty() {
                return Ok(());
            }
            let payload_slot = fc.alloc_local();
            fc.fe.emit_ld_loc(value_slot);
            fc.fe.emit_ld_pay(0);
            fc.fe.emit_st_loc(payload_slot);
            if args.len() == 1 {
                emit_pat_bind(em, fc, args[0], payload_slot)?;
            } else {
                for (i, &elem) in args.iter().enumerate() {
                    let idx = u32::try_from(i).map_err(|_| EmitError::OperandOverflow {
                        desc: "variant payload index".into(),
                    })?;
                    let field_slot = fc.alloc_local();
                    fc.fe.emit_ld_loc(payload_slot);
                    fc.fe.emit_ld_fld(idx)?;
                    fc.fe.emit_st_loc(field_slot);
                    emit_pat_bind(em, fc, elem, field_slot)?;
                }
            }
            Ok(())
        }
        Pat::Record { fields, .. } => {
            for (i, field) in fields.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| EmitError::OperandOverflow {
                    desc: "record pattern field index".into(),
                })?;
                let field_slot = fc.alloc_local();
                fc.fe.emit_ld_loc(value_slot);
                fc.fe.emit_ld_fld(idx)?;
                fc.fe.emit_st_loc(field_slot);
                if let Some(sub_pat) = field.pat {
                    emit_pat_bind(em, fc, sub_pat, field_slot)?;
                } else if let Some(&did) = em.sema.resolution.pat_defs.get(&field.span) {
                    let prev = fc.local_map.insert(did, field_slot);
                    debug_assert!(prev.is_none(), "duplicate local slot for def");
                }
            }
            Ok(())
        }
        Pat::Array { elems, .. } => {
            for (i, &elem) in elems.iter().enumerate() {
                let elem_idx = i64::try_from(i).map_err(|_| EmitError::OperandOverflow {
                    desc: "array pattern element index".into(),
                })?;
                let elem_cst_idx = em.cp.intern(&ConstValue::Int(elem_idx), em.interner)?;
                let elem_slot = fc.alloc_local();
                fc.fe.emit_ld_loc(value_slot);
                fc.fe.emit_ld_cst(elem_cst_idx);
                fc.fe.emit_ld_idx();
                fc.fe.emit_st_loc(elem_slot);
                emit_pat_bind(em, fc, elem, elem_slot)?;
            }
            Ok(())
        }
        Pat::Or { left, .. } => emit_pat_bind(em, fc, left, value_slot),
        Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => Ok(()),
    }
}

fn is_variant_pat(pat_idx: PatIdx, ast: &AstArenas) -> bool {
    match &ast.pats[pat_idx] {
        Pat::Variant { .. } => true,
        Pat::Bind {
            inner: Some(inner), ..
        } => is_variant_pat(*inner, ast),
        Pat::Or { left, right, .. } => is_variant_pat(*left, ast) || is_variant_pat(*right, ast),
        _ => false,
    }
}
