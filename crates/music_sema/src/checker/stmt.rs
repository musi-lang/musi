//! Declaration type checking (class, given, effect, foreign).

use music_ast::decl::{ClassMember, ForeignDecl};
use music_ast::expr::Expr;
use music_ast::ty::{Ty, TyParam};
use music_shared::Idx;

use crate::checker::Checker;
use crate::checker::expr::{check, synth};
use crate::checker::ty::lower_ty;
use crate::def::DefKind;

/// Checks a class/given member's default body with sig params in scope.
fn check_member_fn(ck: &mut Checker<'_>, member: &ClassMember) {
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
        let id = ck.defs.alloc(param.name, DefKind::Param, param.span);
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

/// Collects type variables from `Ty::Var` nodes in a type tree.
fn collect_ty_var_nodes(
    ty_idx: Idx<music_ast::Ty>,
    ast: &music_ast::AstArenas,
    out: &mut Vec<TyParam>,
) {
    match &ast.tys[ty_idx] {
        Ty::Var { name, span } => {
            if !out.iter().any(|p| p.name == *name) {
                out.push(TyParam {
                    name: *name,
                    span: *span,
                });
            }
        }
        Ty::Named { args, .. } => {
            for &arg in args {
                collect_ty_var_nodes(arg, ast, out);
            }
        }
        Ty::Option { inner, .. } | Ty::Ref { inner, .. } | Ty::Array { elem: inner, .. } => {
            collect_ty_var_nodes(*inner, ast, out);
        }
        Ty::Fn { params, ret, .. } => {
            for &p in params {
                collect_ty_var_nodes(p, ast, out);
            }
            collect_ty_var_nodes(*ret, ast, out);
        }
        Ty::Product { fields, .. }
        | Ty::Sum {
            variants: fields, ..
        } => {
            for &f in fields {
                collect_ty_var_nodes(f, ast, out);
            }
        }
        _ => {}
    }
}

/// Checks a declaration expression (class, given, effect, foreign).
pub(crate) fn check_stmt(ck: &mut Checker<'_>, expr_idx: Idx<music_ast::Expr>) {
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
            for member in &members {
                check_member_fn(ck, member);
            }
            if let Some(p) = parent {
                ck.current_scope = p;
            }
        }
        Expr::Given {
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
            for member in &members {
                check_member_fn(ck, member);
            }
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
