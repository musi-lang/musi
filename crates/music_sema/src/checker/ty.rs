//! Lowering AST type nodes (`Ty`) to semantic `Type`.

use music_ast::expr::Arrow;
use music_ast::ty::{EffectItem, EffectSet, Quantifier as AstQuantifier, Ty, TyParam};
use music_shared::{Idx, Span, Symbol};

use crate::checker::Checker;
use crate::def::DefId;
use crate::error::SemaError;
use crate::types::{EffectEntry, EffectRow, Quantifier, RecordField, Type};

/// Looks up `name` in scope, reporting `UndefinedName` if missing.
fn lookup_name_or_error(ck: &mut Checker<'_>, name: Symbol, span: Span) -> Option<DefId> {
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
pub(crate) fn lower_ty(ck: &mut Checker<'_>, ty_idx: Idx<Ty>) -> Idx<Type> {
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
        Ty::Option { inner, .. } => {
            let inner_ty = lower_ty(ck, inner);
            ck.alloc_ty(Type::Named {
                def: ck.ctx.well_known.option,
                args: vec![inner_ty],
            })
        }
        Ty::Ref { inner, .. } => {
            let inner_ty = lower_ty(ck, inner);
            ck.alloc_ty(Type::Ref { inner: inner_ty })
        }
        Ty::Fn {
            params,
            ret,
            arrow,
            effects,
            ..
        } => lower_ty_fn(ck, &params, ret, arrow, effects.as_ref()),
        Ty::Product { fields, .. } => {
            let elems: Vec<_> = fields.iter().map(|&f| lower_ty(ck, f)).collect();
            ck.alloc_ty(Type::Tuple { elems })
        }
        Ty::Sum { variants, .. } => {
            let variant_tys: Vec<_> = variants.iter().map(|&v| lower_ty(ck, v)).collect();
            ck.alloc_ty(Type::AnonSum {
                variants: variant_tys,
            })
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
            // Refinement type predicates are intentionally not checked.
            // Full refinement checking requires constraint solving (SMT)
            // and is deferred to a future milestone.
            lower_ty(ck, base)
        }
        Ty::Array { elem, len, .. } => {
            let elem_ty = lower_ty(ck, elem);
            ck.alloc_ty(Type::Array { elem: elem_ty, len })
        }
        Ty::Quantified {
            kind, params, body, ..
        } => lower_ty_quantified(ck, kind, &params, body),
        Ty::Error { .. } => ck.error_ty(),
    }
}

fn lower_ty_fn(
    ck: &mut Checker<'_>,
    params: &[Idx<Ty>],
    ret: Idx<Ty>,
    arrow: Arrow,
    effects: Option<&EffectSet>,
) -> Idx<Type> {
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

fn lower_effect_set(ck: &mut Checker<'_>, eff_set: &EffectSet) -> EffectRow {
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

fn lower_ty_quantified(
    ck: &mut Checker<'_>,
    kind: AstQuantifier,
    params: &[TyParam],
    body: Idx<Ty>,
) -> Idx<Type> {
    let q = match kind {
        AstQuantifier::Forall => Quantifier::Forall,
        AstQuantifier::Exists => Quantifier::Exists,
    };
    let ty_vars: Vec<_> = params
        .iter()
        .map(|p| {
            let (var_id, _idx) = ck.store.unify.fresh_rigid(p.span, &mut ck.store.types);
            var_id
        })
        .collect();
    let body_ty = lower_ty(ck, body);
    ck.alloc_ty(Type::Quantified {
        kind: q,
        params: ty_vars,
        constraints: vec![],
        body: body_ty,
    })
}
