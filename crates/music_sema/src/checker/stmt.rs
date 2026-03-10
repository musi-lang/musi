//! Declaration type checking (class, given, effect, foreign).

use music_ast::decl::{ClassMember, ForeignDecl};
use music_ast::expr::Expr;
use music_shared::Idx;

use crate::checker::Checker;
use crate::checker::expr::{check, synth};
use crate::checker::ty::lower_ty;

/// Checks a declaration expression (class, given, effect, foreign).
pub(crate) fn check_stmt(ck: &mut Checker<'_>, expr_idx: Idx<music_ast::Expr>) {
    match ck.ctx.ast.exprs[expr_idx].clone() {
        Expr::Class { members, .. } => {
            for member in &members {
                match member {
                    ClassMember::Fn { default, .. } => {
                        if let Some(body) = default {
                            let _ty = synth(ck, *body);
                        }
                    }
                    ClassMember::Law { body, .. } => {
                        let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
                        check(ck, *body, bool_ty);
                    }
                }
            }
        }
        Expr::Given { members, .. } => {
            for member in &members {
                match member {
                    ClassMember::Fn { default, .. } => {
                        if let Some(body) = default {
                            let _ty = synth(ck, *body);
                        }
                    }
                    ClassMember::Law { body, .. } => {
                        let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
                        check(ck, *body, bool_ty);
                    }
                }
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
