//! Instance/dictionary dispatch, constraint scope management, and handler op coverage.

use std::collections::HashSet;
use std::hash::BuildHasher;

use msc_ast::ExprIdx;
use msc_ast::expr::{Expr, Expr as AstExpr, HandlerOp, LetFields};
use msc_ast::pat::Pat;
use msc_ast::ty_param::{Constraint, Rel, TyParam};
use msc_shared::{Idx, Span, Symbol};

use crate::checker::Checker;
use crate::def::DefId;
use crate::error::SemaError;
use crate::types::{DictLookup, Obligation, Type, TypeIdx};
use crate::unify::types_match;

pub(super) fn find_instance_method<S: BuildHasher>(
    ck: &Checker<'_, S>,
    target_ty: TypeIdx,
    op_name: &str,
) -> Option<DefId> {
    let op_sym = ck.ctx.interner.get(op_name)?;
    for inst in &ck.store.instances {
        if types_match(&ck.store.types, &ck.store.unify, inst.target, target_ty)
            && let Some(&def_id) = inst
                .members
                .iter()
                .find(|(s, _)| *s == op_sym)
                .map(|(_, id)| id)
        {
            return Some(def_id);
        }
    }
    None
}

pub(super) fn check_handler_op_coverage<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    effect_ty_idx: ExprIdx,
    ops: &[HandlerOp],
) {
    let effect_name = match &ck.ctx.ast.exprs[effect_ty_idx] {
        AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
        AstExpr::TypeApp { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
            AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
            _ => return,
        },
        _ => return,
    };

    let required_ops = find_effect_required_ops(ck, effect_name);
    if required_ops.is_empty() {
        return;
    }

    let effect_name_str = ck.ctx.interner.resolve(effect_name).to_owned();
    let handled: HashSet<Symbol> = ops.iter().map(|op| op.name).collect();

    for (op_sym, op_span) in &required_ops {
        if !handled.contains(op_sym) {
            let op_name_str = ck.ctx.interner.resolve(*op_sym);
            let _d = ck.diags.report(
                &SemaError::MissingHandlerOp {
                    effect: Box::from(effect_name_str.as_str()),
                    op: Box::from(op_name_str),
                },
                *op_span,
                ck.ctx.file_id,
            );
        }
    }
}

/// Processes AST constraints into active obligations for the current scope.
/// Returns the previous obligations so they can be restored.
pub(super) fn enter_constraint_scope<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    constraints: &[Constraint],
    params: &[TyParam],
) -> Vec<Obligation> {
    if constraints.is_empty() {
        return ck.store.active_obligations.clone();
    }

    let prev = ck.store.active_obligations.clone();
    let mut new_obligations = prev.clone();

    for constraint in constraints {
        if constraint.rel == Rel::Super {
            continue;
        }
        let bound_name = match &ck.ctx.ast.exprs[constraint.bound] {
            AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
            AstExpr::TypeApp { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
                AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
                _ => continue,
            },
            _ => continue,
        };
        let class_def = ck.scopes.lookup(ck.current_scope, bound_name);
        let Some(class_def) = class_def else { continue };

        // Find the type variable for constraint.param among the ty params
        let param_ty = params
            .iter()
            .find(|p| p.name == constraint.param)
            .and_then(|p| {
                ck.ctx.pat_defs.get(&p.span).or_else(|| {
                    // Type params are defined via enter_ty_param_scope, look up in current scope
                    ck.scopes
                        .lookup(ck.current_scope, p.name)
                        .as_ref()
                        .copied()
                        .map(|_| &p.name)
                        .and(None)
                })
            })
            .copied();

        // Fall back: look up the param name in current scope to get its DefId, then get its type
        let param_type_idx = if let Some(&def_id) = param_ty.as_ref() {
            ck.defs
                .get(def_id)
                .ty_info
                .ty
                .unwrap_or_else(|| ck.named_ty(def_id))
        } else if let Some(def_id) = ck.scopes.lookup(ck.current_scope, constraint.param) {
            ck.named_ty(def_id)
        } else {
            continue;
        };

        let ob = Obligation {
            class: class_def,
            args: vec![param_type_idx],
            span: constraint.span,
        };
        new_obligations.push(ob.clone());
        ck.store.obligations.push(ob);
    }

    ck.store.active_obligations = new_obligations;
    prev
}

/// Records `fn_constraints` for a constrained function.
pub(super) fn record_fn_constraints<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields) {
    if fields.constraints.is_empty() {
        return;
    }

    let pat = &ck.ctx.ast.pats[fields.pat];
    let pat_span = match pat {
        Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
        _ => return,
    };

    let Some(&def_id) = ck.ctx.pat_defs.get(&pat_span) else {
        return;
    };

    // Collect the obligations that were added for this function's constraints
    let fn_obs: Vec<Obligation> = ck
        .store
        .active_obligations
        .iter()
        .filter(|ob| {
            fields.constraints.iter().any(|c| {
                let bname = match &ck.ctx.ast.exprs[c.bound] {
                    AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
                    AstExpr::TypeApp { callee, .. } => {
                        if let AstExpr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                            ck.ctx.ast.name_refs[*name_ref].name
                        } else {
                            return false;
                        }
                    }
                    _ => return false,
                };
                ck.scopes.lookup(ck.current_scope, bname) == Some(ob.class)
            })
        })
        .cloned()
        .collect();

    if !fn_obs.is_empty() {
        let _prev = ck.store.fn_constraints.insert(def_id, fn_obs);
    }
}

/// Searches active obligations for a method that matches the operator on a type variable.
pub(super) fn find_dict_method<S: BuildHasher>(
    ck: &Checker<'_, S>,
    target_ty: TypeIdx,
    op_name: &str,
) -> Option<DictLookup> {
    let op_sym = ck.ctx.interner.get(op_name)?;
    let resolved = ck.store.unify.resolve(target_ty, &ck.store.types);
    let resolved_ty = &ck.store.types[resolved];

    if !matches!(resolved_ty, Type::Var(_) | Type::Rigid(_)) {
        return None;
    }

    for ob in &ck.store.active_obligations {
        let ob_ty = ck.store.unify.resolve(*ob.args.first()?, &ck.store.types);
        if ob_ty != resolved {
            continue;
        }
        // Check if this class has a method matching op_sym
        if ck.ctx.class_op_members.contains_key(&(ob.class, op_sym)) {
            return Some(DictLookup {
                class: ob.class,
                method_sym: op_sym,
            });
        }
    }
    None
}

fn find_effect_required_ops<S: BuildHasher>(
    ck: &Checker<'_, S>,
    effect_name: Symbol,
) -> Vec<(Symbol, Span)> {
    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let Expr::Effect { name, ops, .. } = &ck.ctx.ast.exprs[idx]
            && *name == effect_name
        {
            return ops.iter().map(|op| (op.name, op.span)).collect();
        }
    }
    vec![]
}
