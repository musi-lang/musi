#![allow(clippy::too_many_lines)]

use musi_ast::{BinOp, Cond, ElifBranch, Expr, LitValue, Pat};
use musi_shared::Idx;

use crate::error::CodegenError;
use crate::{ConstEntry, Module, Opcode};

use super::expr::{emit_expr, emit_expr_or_unit};
use super::pattern::{emit_pattern_bindings, emit_pattern_test};
use super::state::{EmitArenas, EmitState, FnEmitter};

pub(super) fn emit_case_cond(
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

/// Emits the jump condition for an `if`/`elif`/`while` branch, returning the
/// fixup index for the "condition was false" jump.  For `Cond::Case`, also
/// pushes a new scope and emits pattern bindings; the caller must call
/// `out.pop_scope()` after the branch body.
fn emit_branch_cond(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<usize, CodegenError> {
    match cond {
        Cond::Expr(_) => {
            emit_cond(arenas, state, cond, module, out)?;
            Ok(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
        }
        Cond::Case { pat, init, .. } => {
            let (tmp_slot, test_fixup) = emit_case_cond(arenas, state, pat, *init, module, out)?;
            let fixup = test_fixup
                .map_or_else(|| Ok(out.emit_jump_placeholder(FnEmitter::BR_FALSE)), Ok)?;
            out.push_scope();
            emit_pattern_bindings(arenas, state, pat, tmp_slot, out)?;
            Ok(fixup)
        }
    }
}

pub(super) fn emit_cond(
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

#[derive(Clone, Copy)]
pub(super) struct ForData<'a> {
    pub(super) pat: &'a Pat,
    pub(super) iter: Idx<Expr>,
    pub(super) guard: Option<Idx<Expr>>,
    pub(super) body: Idx<Expr>,
}

#[derive(Clone, Copy)]
pub(super) struct IfData<'a> {
    pub(super) cond: &'a Cond,
    pub(super) then_body: Idx<Expr>,
    pub(super) elif_chains: &'a [ElifBranch],
    pub(super) else_body: Option<Idx<Expr>>,
}

pub(super) fn emit_if(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    data: IfData<'_>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let else_fixup = emit_branch_cond(arenas, state, data.cond, module, out)?;
    let then = arenas.exprs.get(data.then_body).clone();
    emit_expr(arenas, state, &then, module, out)?;
    if matches!(data.cond, Cond::Case { .. }) {
        out.pop_scope();
    }
    let first_end_fixup = out.emit_jump_placeholder(FnEmitter::BR);
    out.patch_jump_to_here(else_fixup)?;

    let mut end_fixups = vec![first_end_fixup];
    for branch in data.elif_chains {
        let next_fixup = emit_branch_cond(arenas, state, &branch.cond, module, out)?;
        let guard_fixup = if let Some(guard_idx) = branch.guard {
            let guard_expr = arenas.exprs.get(guard_idx).clone();
            emit_expr(arenas, state, &guard_expr, module, out)?;
            Some(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
        } else {
            None
        };
        let branch_body = arenas.exprs.get(branch.body).clone();
        emit_expr(arenas, state, &branch_body, module, out)?;
        if matches!(branch.cond.as_ref(), Cond::Case { .. }) {
            out.pop_scope();
        }
        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));
        if let Some(gf) = guard_fixup {
            out.patch_jump_to_here(gf)?;
        }
        out.patch_jump_to_here(next_fixup)?;
    }

    emit_expr_or_unit(arenas, state, data.else_body, module, out)?;
    for fixup in end_fixups {
        out.patch_jump_to_here(fixup)?;
    }
    Ok(())
}

pub(super) fn emit_while(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    cond: &Cond,
    guard: Option<Idx<Expr>>,
    body: Idx<Expr>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();
    let end_fixup = emit_branch_cond(arenas, state, cond, module, out)?;

    // Guard: if false, skip body but still loop back (after popping Case scope if needed)
    let guard_fixup = if let Some(guard_idx) = guard {
        let guard_expr = arenas.exprs.get(guard_idx).clone();
        emit_expr(arenas, state, &guard_expr, module, out)?;
        Some(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
    } else {
        None
    };

    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;
    if matches!(cond, Cond::Case { .. }) {
        out.pop_scope();
    }
    out.push(&Opcode::Drop);
    out.emit_br_back(start_pos)?;

    // Guard-false path: exit loop (same semantics as condition-false)
    if let Some(gf) = guard_fixup {
        out.patch_jump_to_here(gf)?;
        if matches!(cond, Cond::Case { .. }) {
            out.pop_scope();
        }
        // fall through to end_fixup — guard false exits the loop
    }

    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()
}

pub(super) fn emit_loop(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    body: Idx<Expr>,
    post_cond: Option<&Cond>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();
    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;
    out.push(&Opcode::Drop);
    if let Some(cond) = post_cond {
        emit_cond(arenas, state, cond, module, out)?;
        let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
        out.emit_br_back(start_pos)?;
        out.patch_jump_to_here(end_fixup)?;
    } else {
        out.emit_br_back(start_pos)?;
    }
    out.close_loop()
}

pub(super) fn emit_for(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    data: ForData<'_>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let ForData {
        pat,
        iter,
        guard,
        body,
    } = data;
    let is_ident = matches!(pat, Pat::Ident { suffix: None, .. });
    let iter_expr = arenas.exprs.get(iter).clone();

    // -- Range path (BinOp::Range / BinOp::RangeExcl) --
    if let Expr::Binary { op, lhs, rhs, .. } = &iter_expr
        && matches!(op, BinOp::Range | BinOp::RangeExcl)
    {
        let cmp_op = if matches!(op, BinOp::RangeExcl) {
            Opcode::LtI64
        } else {
            Opcode::LeqI64
        };

        out.push_scope();
        let counter_slot = if is_ident {
            let Pat::Ident { name, .. } = pat else {
                return Err(CodegenError::UnsupportedExpr);
            };
            out.define_local(arenas.interner.resolve(*name))?
        } else {
            out.define_local("$range_i")?
        };
        let limit_slot = out.define_local("$limit")?;

        let lhs_expr = arenas.exprs.get(*lhs).clone();
        emit_expr(arenas, state, &lhs_expr, module, out)?;
        out.push(&Opcode::StLoc(counter_slot));
        let rhs_expr = arenas.exprs.get(*rhs).clone();
        emit_expr(arenas, state, &rhs_expr, module, out)?;
        out.push(&Opcode::StLoc(limit_slot));

        let start_pos = out.start_loop();
        out.push(&Opcode::LdLoc(counter_slot));
        out.push(&Opcode::LdLoc(limit_slot));
        out.push(&cmp_op);
        let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

        // Guard only supported for Ident patterns on range (non-Ident + guard = UnsupportedExpr)
        if !is_ident && guard.is_some() {
            return Err(CodegenError::UnsupportedExpr);
        }

        let guard_fixup = if let Some(guard_idx) = guard {
            let guard_expr = arenas.exprs.get(guard_idx).clone();
            emit_expr(arenas, state, &guard_expr, module, out)?;
            Some(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
        } else {
            None
        };

        let body_expr = arenas.exprs.get(body).clone();
        emit_expr(arenas, state, &body_expr, module, out)?;
        out.push(&Opcode::Drop);

        // Guard-false lands after Drop (at counter increment)
        if let Some(gf) = guard_fixup {
            out.patch_jump_to_here(gf)?;
        }

        out.push(&Opcode::LdLoc(counter_slot));
        out.push(&Opcode::LdImmI64(1));
        out.push(&Opcode::AddI64);
        out.push(&Opcode::StLoc(counter_slot));
        out.emit_br_back(start_pos)?;
        out.patch_jump_to_here(end_fixup)?;
        out.close_loop()?;
        out.pop_scope();
        return Ok(());
    }

    // -- General array path --
    let iter_len_idx = module.add_string_const("iter_len")?;
    let iter_get_idx = module.add_string_const("iter_get")?;
    out.push_scope();
    let arr_slot = out.define_local("$arr")?;
    let len_slot = out.define_local("$len")?;
    let i_slot = out.define_local("$i")?;

    emit_expr(arenas, state, &iter_expr, module, out)?;
    out.push(&Opcode::StLoc(arr_slot));
    out.push(&Opcode::LdLoc(arr_slot));
    out.push(&Opcode::CallMethod {
        method_idx: iter_len_idx,
        arg_count: 1,
    });
    out.push(&Opcode::StLoc(len_slot));
    out.push(&Opcode::LdImmI64(0));
    out.push(&Opcode::StLoc(i_slot));

    let start_pos = out.start_loop();
    out.push(&Opcode::LdLoc(i_slot));
    out.push(&Opcode::LdLoc(len_slot));
    out.push(&Opcode::LtI64);
    let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

    // Fetch element
    out.push(&Opcode::LdLoc(arr_slot));
    out.push(&Opcode::LdLoc(i_slot));
    out.push(&Opcode::CallMethod {
        method_idx: iter_get_idx,
        arg_count: 2,
    });

    // Bind element slot (Ident fast-path uses pat name; else spill to $for_elem)
    let elem_slot = if is_ident {
        let Pat::Ident { name, .. } = pat else {
            return Err(CodegenError::UnsupportedExpr);
        };
        let slot = out.define_local(arenas.interner.resolve(*name))?;
        out.push(&Opcode::StLoc(slot));
        slot
    } else {
        let slot = out.define_local("$for_elem")?;
        out.push(&Opcode::StLoc(slot));
        slot
    };

    // Guard: if false, skip body and jump straight to counter increment
    let guard_fixup = if let Some(guard_idx) = guard {
        let guard_expr = arenas.exprs.get(guard_idx).clone();
        emit_expr(arenas, state, &guard_expr, module, out)?;
        Some(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
    } else {
        None
    };

    // For non-Ident patterns, bind into an inner scope
    if !is_ident {
        out.push_scope();
        emit_pattern_bindings(arenas, state, pat, elem_slot, out)?;
    }

    let body_expr = arenas.exprs.get(body).clone();
    emit_expr(arenas, state, &body_expr, module, out)?;

    if !is_ident {
        out.pop_scope();
    }
    out.push(&Opcode::Drop);

    // Guard-false lands here (after Drop, before counter increment)
    if let Some(gf) = guard_fixup {
        out.patch_jump_to_here(gf)?;
    }

    out.push(&Opcode::LdLoc(i_slot));
    out.push(&Opcode::LdImmI64(1));
    out.push(&Opcode::AddI64);
    out.push(&Opcode::StLoc(i_slot));
    out.emit_br_back(start_pos)?;
    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()?;
    out.pop_scope();
    Ok(())
}

pub(super) fn emit_short_circuit(
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
    // Dup so the value survives on the short-circuit path.
    out.push(&Opcode::Dup);
    let fixup = out.emit_jump_placeholder(branch_tag);
    // On the fall-through path the dup'd value is no longer needed.
    out.push(&Opcode::Drop);
    let rhs_expr = arenas.exprs.get(rhs).clone();
    emit_expr(arenas, state, &rhs_expr, module, out)?;
    out.patch_jump_to_here(fixup)
}

pub(super) fn emit_binary(
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

pub(super) fn emit_lit(
    value: &LitValue,
    arenas: &EmitArenas<'_>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match value {
        LitValue::Str(sym) => {
            let raw = arenas.interner.resolve(*sym);
            let unquoted = raw
                .strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or(raw);
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
