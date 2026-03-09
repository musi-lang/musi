//! Lowering AST type nodes (`Ty`) to semantic `Type`.

use music_ast::expr::Arrow;
use music_ast::ty::{Quantifier as AstQuantifier, Ty};
use music_shared::Idx;

use crate::checker::Checker;
use crate::error::SemaError;
use crate::types::{EffectRow, Quantifier, RecordField, Type};

/// Lowers an AST `Ty` node to a semantic `Type` in the checker's arena.
#[allow(clippy::too_many_lines)]
pub(crate) fn lower_ty(ck: &mut Checker<'_>, ty_idx: Idx<Ty>) -> Idx<Type> {
    match ck.ast.tys[ty_idx].clone() {
        Ty::Var { name, span } => {
            // Look up the type variable in scope.
            if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name) {
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: Vec::new(),
                })
            } else {
                let name_str = ck.interner.resolve(name);
                let _d = ck.diags.report(
                    &SemaError::UndefinedName {
                        name: Box::from(name_str),
                    },
                    span,
                    ck.file_id,
                );
                ck.error_ty()
            }
        }
        Ty::Named { name, args, span } => {
            if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name) {
                let lowered_args: Vec<_> = args.iter().map(|&a| lower_ty(ck, a)).collect();
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: lowered_args,
                })
            } else {
                let name_str = ck.interner.resolve(name);
                let _d = ck.diags.report(
                    &SemaError::UndefinedName {
                        name: Box::from(name_str),
                    },
                    span,
                    ck.file_id,
                );
                ck.error_ty()
            }
        }
        Ty::Option { inner, .. } => {
            let inner_ty = lower_ty(ck, inner);
            ck.alloc_ty(Type::Named {
                def: ck.well_known.option,
                args: vec![inner_ty],
            })
        }
        Ty::Ref { inner, .. } => {
            let inner_ty = lower_ty(ck, inner);
            ck.alloc_ty(Type::Ref { inner: inner_ty })
        }
        Ty::Fn {
            params, ret, arrow, ..
        } => {
            let param_tys: Vec<_> = params.iter().map(|&p| lower_ty(ck, p)).collect();
            let ret_ty = lower_ty(ck, ret);
            let effect_row = match arrow {
                Arrow::Pure => EffectRow::PURE,
                Arrow::Effectful => {
                    // TODO: lower effect set from AST
                    EffectRow {
                        effects: Vec::new(),
                        row_var: None,
                    }
                }
            };
            ck.alloc_ty(Type::Fn {
                params: param_tys,
                ret: ret_ty,
                effects: effect_row,
            })
        }
        Ty::Product { fields, .. } => {
            let elems: Vec<_> = fields.iter().map(|&f| lower_ty(ck, f)).collect();
            ck.alloc_ty(Type::Tuple { elems })
        }
        Ty::Sum { variants, .. } => {
            // Each variant is a Ty — lower them.
            let variant_tys: Vec<_> = variants.iter().map(|&v| lower_ty(ck, v)).collect();
            // For now, represent as a tuple of variants.
            ck.alloc_ty(Type::Tuple { elems: variant_tys })
        }
        Ty::Record { fields, open, .. } => {
            let rec_fields: Vec<_> = fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    ty: lower_ty(ck, f.ty),
                })
                .collect();
            ck.alloc_ty(Type::Record {
                fields: rec_fields,
                open,
            })
        }
        Ty::Refine { base, .. } => {
            // Refinement types: lower the base, ignore predicate for now.
            lower_ty(ck, base)
        }
        Ty::Array { elem, len, .. } => {
            let elem_ty = lower_ty(ck, elem);
            ck.alloc_ty(Type::Array { elem: elem_ty, len })
        }
        Ty::Quantified {
            kind, params, body, ..
        } => {
            let q = match kind {
                AstQuantifier::Forall => Quantifier::Forall,
                AstQuantifier::Exists => Quantifier::Exists,
            };
            // Create rigid type variables for the quantified params.
            let ty_vars: Vec<_> = params
                .iter()
                .map(|p| {
                    let (var_id, _idx) = ck.unify.fresh_rigid(p.span, &mut ck.types);
                    var_id
                })
                .collect();
            let body_ty = lower_ty(ck, body);
            ck.alloc_ty(Type::Quantified {
                kind: q,
                params: ty_vars,
                constraints: Vec::new(),
                body: body_ty,
            })
        }
        Ty::Error { .. } => ck.error_ty(),
    }
}
