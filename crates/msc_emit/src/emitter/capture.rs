//! Free-variable collection for closure capture analysis.

use std::collections::{HashMap, HashSet};

use msc_ast::expr::{Arg, ArrayElem, Expr, HandlerOp, MatchArm, PwArm, PwGuard, RecField};
use msc_ast::ExprIdx;
use msc_sema::DefId;

use super::Emitter;

/// Where a captured variable lives in the parent scope.
#[derive(Clone, Copy)]
pub(super) enum CaptureSource {
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
pub(super) fn collect_free_vars(
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
pub(super) fn body_references_def(em: &Emitter<'_>, body: ExprIdx, target: DefId) -> bool {
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

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match on all expression kinds"
)]
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
        | Expr::Return { value: None, .. }
        | Expr::TypeApp { .. }
        | Expr::FnType { .. }
        | Expr::OptionType { .. }
        | Expr::ProductType { .. }
        | Expr::SumType { .. }
        | Expr::ArrayType { .. }
        | Expr::PiType { .. }
        | Expr::Resume { value: None, .. } => {}
        Expr::Paren { inner, .. }
        | Expr::Annotated { inner, .. }
        | Expr::Return {
            value: Some(inner), ..
        }
        | Expr::Field { object: inner, .. }
        | Expr::UnaryOp { operand: inner, .. }
        | Expr::Fn { body: inner, .. }
        | Expr::TypeCheck { operand: inner, .. }
        | Expr::Need { operand: inner, .. }
        | Expr::Resume {
            value: Some(inner), ..
        } => cfv_walk(cx, *inner),
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
        Expr::Let { fields, .. } => {
            if let Some(v) = fields.value {
                cfv_walk(cx, v);
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
        Expr::Handle { body, ops, .. } => cfv_walk_handle(cx, *body, ops),
    }
}

fn cfv_walk_handle(cx: &mut CfvCtx<'_, '_>, body: ExprIdx, ops: &[HandlerOp]) {
    cfv_walk(cx, body);
    for op in ops {
        cfv_walk(cx, op.body);
    }
}
