//! Declaration type checking (class, given, effect, foreign).

use music_ast::decl::{ClassMember, ForeignDecl};
use music_ast::expr::{Expr, Param};
use music_ast::ty::TyParam;
use music_ast::util::collect_ty_var_nodes;
use music_shared::{Idx, Span};
use std::hash::BuildHasher;

use crate::checker::Checker;
use crate::checker::expr::{check, synth};
use crate::checker::ty::lower_ty;
use crate::def::DefKind;

/// Checks a class/given member's default body with sig params in scope.
fn check_member_fn<S: BuildHasher>(ck: &mut Checker<'_, S>, member: &ClassMember) {
    let ClassMember::Fn { sig, default, .. } = member else {
        return;
    };
    let Some(body) = default else {
        return;
    };

    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    for param in &sig.params {
        let param_ty = if let Some(ty) = param.ty {
            lower_ty(ck, ty)
        } else {
            ck.fresh_var(param.span)
        };
        let id = if let Some(&existing) = ck.ctx.pat_defs.get(&param.span) {
            existing
        } else {
            ck.defs.alloc(param.name, DefKind::Param, param.span)
        };
        let _prev = ck.scopes.define(ck.current_scope, param.name, id);
        ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
    }

    if let Some(ret) = sig.ret {
        let expected = lower_ty(ck, ret);
        check(ck, *body, expected);
    } else {
        let _ty = synth(ck, *body);
    }

    ck.current_scope = parent;
}

fn check_member_law<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    params: &[Param],
    body: Idx<music_ast::Expr>,
    span: Span,
    class_ty_params: &[TyParam],
) {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    if params.is_empty() {
        // Implicit: law vars get the first class `over` type param as their type.
        let class_ty = match class_ty_params
            .first()
            .and_then(|p| ck.scopes.lookup(ck.current_scope, p.name))
        {
            Some(def_id) => ck.named_ty(def_id),
            None => ck.fresh_var(span),
        };

        if let Some(law_vars) = ck.ctx.law_inferred_vars.get(&span) {
            for &(sym, def_id) in law_vars {
                let _prev = ck.scopes.define(ck.current_scope, sym, def_id);
                ck.defs.get_mut(def_id).ty_info.ty = Some(class_ty);
            }
        }
    } else {
        // Explicit: lower type annotations for each param.
        for param in params {
            let param_ty = if let Some(ty) = param.ty {
                lower_ty(ck, ty)
            } else {
                ck.fresh_var(param.span)
            };
            let id = if let Some(&existing) = ck.ctx.pat_defs.get(&param.span) {
                existing
            } else {
                ck.defs.alloc(param.name, DefKind::LawVar, param.span)
            };
            let _prev = ck.scopes.define(ck.current_scope, param.name, id);
            ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
        }
    }

    // Law bodies are propositions — check as Bool.
    let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
    check(ck, body, bool_ty);

    ck.current_scope = parent;
}

fn check_class_members<S: BuildHasher>(ck: &mut Checker<'_, S>, members: &[ClassMember], ty_params: &[TyParam]) {
    for member in members {
        match member {
            ClassMember::Fn { .. } => check_member_fn(ck, member),
            ClassMember::Law {
                params, body, span, ..
            } => check_member_law(ck, params, *body, *span, ty_params),
        }
    }
}

/// Checks a declaration expression (class, given, effect, foreign).
pub fn check_stmt<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: Idx<music_ast::Expr>) {
    match ck.ctx.ast.exprs[expr_idx].clone() {
        Expr::Class {
            params, members, ..
        } => {
            let parent = if params.is_empty() {
                None
            } else {
                let (p, _ids) = ck.enter_ty_param_scope(&params);
                Some(p)
            };
            check_class_members(ck, &members, &params);
            if let Some(p) = parent {
                ck.current_scope = p;
            }
        }
        Expr::Instance {
            target,
            params,
            members,
            ..
        } => {
            let mut all_params: Vec<TyParam> = params;
            for &arg in &target.args {
                collect_ty_var_nodes(arg, ck.ctx.ast, &mut all_params);
            }
            let parent = if all_params.is_empty() {
                None
            } else {
                let (p, _ids) = ck.enter_ty_param_scope(&all_params);
                Some(p)
            };
            check_class_members(ck, &members, &all_params);
            if let Some(p) = parent {
                ck.current_scope = p;
            }
        }
        Expr::Effect { ops, .. } => {
            for op in &ops {
                let _op_ty = lower_ty(ck, op.ty);
            }
        }
        Expr::Foreign { decls, .. } => {
            for decl in &decls {
                if let ForeignDecl::Fn { ty, .. } = decl {
                    let _fn_ty = lower_ty(ck, *ty);
                }
            }
        }
        _ => {}
    }
}
