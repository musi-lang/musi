//! Lowering AST type nodes (`Ty`) to semantic `Type`.

use music_ast::TyIdx;
use music_ast::expr::Arrow;
use music_ast::ty::{EffectItem, EffectSet, Ty};
use music_shared::{Span, Symbol};
use std::hash::BuildHasher;

use crate::checker::Checker;
use crate::def::DefId;
use crate::error::SemaError;
use crate::types::{EffectEntry, EffectRow, Type, TypeIdx};

/// Looks up `name` in scope, reporting `UndefinedName` if missing.
fn lookup_name_or_error<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    name: Symbol,
    span: Span,
) -> Option<DefId> {
    if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name) {
        Some(def_id)
    } else {
        let name_str = ck.ctx.interner.resolve(name);
        let _d = ck.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            ck.ctx.file_id,
        );
        None
    }
}

/// Lowers an AST `Ty` node to a semantic `Type` in the checker's arena.
pub fn lower_ty<S: BuildHasher>(ck: &mut Checker<'_, S>, ty_idx: TyIdx) -> TypeIdx {
    match ck.ctx.ast.tys[ty_idx].clone() {
        Ty::Var { name, span } => {
            if let Some(def_id) = lookup_name_or_error(ck, name, span) {
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: vec![],
                })
            } else {
                ck.error_ty()
            }
        }
        Ty::Named { name, args, span } => {
            if let Some(def_id) = lookup_name_or_error(ck, name, span) {
                let lowered_args: Vec<_> = args.iter().map(|&a| lower_ty(ck, a)).collect();
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: lowered_args,
                })
            } else {
                ck.error_ty()
            }
        }
        Ty::Option { inner, span } => {
            let inner_ty = lower_ty(ck, inner);
            if let Some(def) = ck
                .ctx
                .interner
                .get("Option")
                .and_then(|sym| ck.scopes.lookup(ck.current_scope, sym))
            {
                ck.alloc_ty(Type::Named {
                    def,
                    args: vec![inner_ty],
                })
            } else {
                let _d = ck.diags.report(
                    &SemaError::UndefinedName {
                        name: Box::from("Option"),
                    },
                    span,
                    ck.ctx.file_id,
                );
                ck.error_ty()
            }
        }
        Ty::Fn {
            params,
            ret,
            arrow,
            effects,
            ..
        } => lower_ty_fn(ck, &params, ret, arrow, effects.as_ref()),
        Ty::Product { fields, .. } => {
            if fields.is_empty() {
                return ck.named_ty(ck.ctx.well_known.unit);
            }
            let elems: Vec<_> = fields.iter().map(|&f| lower_ty(ck, f)).collect();
            ck.alloc_ty(Type::Tuple { elems })
        }
        Ty::Sum { variants, .. } => {
            let variant_tys: Vec<_> = variants.iter().map(|&v| lower_ty(ck, v)).collect();
            ck.alloc_ty(Type::AnonSum {
                variants: variant_tys,
            })
        }
        Ty::Array { elem, len, .. } => {
            let elem_ty = lower_ty(ck, elem);
            ck.alloc_ty(Type::Array { elem: elem_ty, len })
        }
        Ty::Error { .. } => ck.error_ty(),
    }
}

fn lower_ty_fn<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    params: &[TyIdx],
    ret: TyIdx,
    arrow: Arrow,
    effects: Option<&EffectSet>,
) -> TypeIdx {
    let param_tys: Vec<_> = params.iter().map(|&p| lower_ty(ck, p)).collect();
    let ret_ty = lower_ty(ck, ret);
    let effect_row = match (arrow, effects) {
        (Arrow::Pure, _) => EffectRow::PURE,
        (Arrow::Effectful, Some(eff_set)) => lower_effect_set(ck, eff_set),
        (Arrow::Effectful, None) => EffectRow {
            effects: vec![],
            row_var: None,
        },
    };
    ck.alloc_ty(Type::Fn {
        params: param_tys,
        ret: ret_ty,
        effects: effect_row,
    })
}

fn lower_effect_set<S: BuildHasher>(ck: &mut Checker<'_, S>, eff_set: &EffectSet) -> EffectRow {
    let mut effects = vec![];
    let mut row_var = None;
    for item in &eff_set.effects {
        match item {
            EffectItem::Named { name, arg, span } => {
                if let Some(def_id) = lookup_name_or_error(ck, *name, *span) {
                    let args = arg.iter().map(|&ty_idx| lower_ty(ck, ty_idx)).collect();
                    effects.push(EffectEntry { def: def_id, args });
                }
            }
            EffectItem::Var { span, .. } => {
                let var_id = ck.store.unify.fresh_var_id(*span);
                row_var = Some(var_id);
            }
        }
    }
    EffectRow { effects, row_var }
}
