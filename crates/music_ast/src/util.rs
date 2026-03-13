//! Utility functions for AST traversal.

use crate::ty::{Ty, TyParam};
use crate::{AstArenas, TyIdx};

/// Collects type variables from `Ty::Var` nodes in a type tree.
pub fn collect_ty_var_nodes(ty_idx: TyIdx, arenas: &AstArenas, out: &mut Vec<TyParam>) {
    match &arenas.tys[ty_idx] {
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
                collect_ty_var_nodes(arg, arenas, out);
            }
        }
        Ty::Option { inner, .. } | Ty::Array { elem: inner, .. } => {
            collect_ty_var_nodes(*inner, arenas, out);
        }
        Ty::Fn { params, ret, .. } => {
            for &p in params {
                collect_ty_var_nodes(p, arenas, out);
            }
            collect_ty_var_nodes(*ret, arenas, out);
        }
        Ty::Product { fields, .. }
        | Ty::Sum {
            variants: fields, ..
        } => {
            for &f in fields {
                collect_ty_var_nodes(f, arenas, out);
            }
        }
        _ => {}
    }
}
