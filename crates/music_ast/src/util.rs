//! Utility functions for AST traversal.

use crate::expr::Expr;
use crate::ty_param::TyParam;
use crate::{AstArenas, ExprIdx};

/// Collects type variables from type expression nodes.
///
/// Walks `Expr` type-expression variants looking for `Expr::Name` nodes
/// that reference type variables (detected by the caller).
pub fn collect_ty_var_nodes(expr_idx: ExprIdx, arenas: &AstArenas, out: &mut Vec<TyParam>) {
    match &arenas.exprs[expr_idx] {
        Expr::Name { name_ref, .. } => {
            let nr = &arenas.name_refs[*name_ref];
            if !out.iter().any(|p| p.name == nr.name) {
                out.push(TyParam {
                    name: nr.name,
                    span: nr.span,
                });
            }
        }
        Expr::TypeApp { callee, args, .. } => {
            collect_ty_var_nodes(*callee, arenas, out);
            for &arg in args {
                collect_ty_var_nodes(arg, arenas, out);
            }
        }
        Expr::OptionType { inner, .. } | Expr::ArrayType { elem: inner, .. } => {
            collect_ty_var_nodes(*inner, arenas, out);
        }
        Expr::FnType { params, ret, .. } => {
            for &p in params {
                collect_ty_var_nodes(p, arenas, out);
            }
            collect_ty_var_nodes(*ret, arenas, out);
        }
        Expr::ProductType { fields, .. }
        | Expr::SumType {
            variants: fields, ..
        } => {
            for &f in fields {
                collect_ty_var_nodes(f, arenas, out);
            }
        }
        // Field access for qualified types (M.Type)
        Expr::Field { object, .. } => {
            collect_ty_var_nodes(*object, arenas, out);
        }
        Expr::PiType { param_ty, body, .. } => {
            collect_ty_var_nodes(*param_ty, arenas, out);
            collect_ty_var_nodes(*body, arenas, out);
        }
        _ => {}
    }
}
