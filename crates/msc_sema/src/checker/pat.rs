//! Pattern type checking.

use std::collections::HashMap;
use std::hash::BuildHasher;

use msc_ast::PatIdx;
use msc_ast::expr::BindKind;
use msc_ast::lit::Lit;
use msc_ast::pat::Pat;
use msc_shared::{Span, Symbol};

use crate::checker::Checker;
use crate::checker::instantiate::substitute_ty;
use crate::types::{Type, TypeIdx};

/// Checks a pattern against an expected type.
pub fn check_pat<S: BuildHasher>(ck: &mut Checker<'_, S>, pat_idx: PatIdx, expected: TypeIdx) {
    match ck.ctx.ast.pats[pat_idx].clone() {
        Pat::Lit { lit, span } => {
            let lit_ty = match &lit {
                Lit::Int { .. } => ck.named_ty(ck.ctx.well_known.ints.int),
                Lit::Float { .. } => ck.named_ty(ck.ctx.well_known.floats.float64),
                Lit::Str { .. } | Lit::FStr { .. } => ck.named_ty(ck.ctx.well_known.string),
                Lit::Rune { .. } => ck.named_ty(ck.ctx.well_known.rune),
                Lit::Unit { .. } => ck.named_ty(ck.ctx.well_known.unit),
            };
            ck.unify_or_report(expected, lit_ty, span, None);
        }
        Pat::Bind {
            span, inner, kind, ..
        } => {
            if let Some(&def_id) = ck.ctx.pat_defs.get(&span) {
                let stored_ty = if kind == BindKind::Mut {
                    ck.alloc_ty(Type::Ref { inner: expected })
                } else {
                    expected
                };
                ck.defs.get_mut(def_id).ty_info.ty = Some(stored_ty);
            }
            // If there's an inner pattern (`x @ pat`), check it too.
            if let Some(inner) = inner {
                check_pat(ck, inner, expected);
            }
        }
        Pat::Tuple { elems, span } => {
            let resolved = ck.resolve_ty(expected);
            match &ck.store.types[resolved] {
                Type::Tuple {
                    elems: expected_elems,
                } => {
                    if elems.len() == expected_elems.len() {
                        let pairs: Vec<_> = elems
                            .iter()
                            .copied()
                            .zip(expected_elems.iter().copied())
                            .collect();
                        for (pat_elem, ty_elem) in pairs {
                            check_pat(ck, pat_elem, ty_elem);
                        }
                    }
                }
                Type::Error => {}
                _ => {
                    let fresh_elems: Vec<_> = elems.iter().map(|_| ck.fresh_var(span)).collect();
                    let tup_ty = ck.alloc_ty(Type::Tuple { elems: fresh_elems });
                    ck.unify_or_report(expected, tup_ty, span, None);
                }
            }
        }
        Pat::Record { fields, .. } => {
            let resolved = ck.resolve_ty(expected);
            if let Type::Record {
                fields: expected_fields,
                ..
            } = &ck.store.types[resolved]
            {
                let expected_fields = expected_fields.clone();
                for field in &fields {
                    if let Some(ef) = expected_fields.iter().find(|ef| ef.name == field.name)
                        && let Some(pat) = field.pat
                    {
                        check_pat(ck, pat, ef.ty);
                    }
                }
            }
        }
        Pat::Array { elems, .. } => {
            let resolved = ck.resolve_ty(expected);
            if let Type::Array { elem, .. } = &ck.store.types[resolved] {
                let elem = *elem;
                for &pat_elem in &elems {
                    check_pat(ck, pat_elem, elem);
                }
            }
        }
        Pat::Variant {
            name, span, args, ..
        } => {
            if let Some(&def_id) = ck.ctx.pat_defs.get(&span) {
                ck.defs.get_mut(def_id).ty_info.ty = Some(expected);
            }
            let resolved = ck.resolve_ty(expected);
            let param_tys: Vec<_> = if let Type::Fn { params, .. } = &ck.store.types[resolved] {
                params.clone()
            } else {
                extract_variant_payload_types(ck, resolved, name)
                    .unwrap_or_else(|| args.iter().map(|_| ck.fresh_var(Span::DUMMY)).collect())
            };
            for (&arg, param_ty) in args.iter().zip(param_tys) {
                check_pat(ck, arg, param_ty);
            }
        }
        Pat::Or { left, right, .. } => {
            check_pat(ck, left, expected);
            check_pat(ck, right, expected);
        }
        Pat::Wild { .. } | Pat::Error { .. } => {}
    }
}

/// Expand a `Named` type to its underlying `Sum` and extract the payload types
/// for the given variant. Mirrors the expansion logic in `lookup_field`.
fn extract_variant_payload_types<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    variant_name: Symbol,
) -> Option<Vec<TypeIdx>> {
    let expanded = match &ck.store.types[ty] {
        Type::Sum { variants } => {
            return variants
                .iter()
                .find(|v| v.name == variant_name)
                .map(|v| v.fields.clone());
        }
        Type::Named { def, args } => {
            let (def, args) = (*def, args.clone());
            let underlying = ck.defs.get(def).ty_info.ty?;
            let ty_params = ck.defs.get(def).ty_info.ty_params.clone();
            if !ty_params.is_empty() && ty_params.len() == args.len() {
                let mut subst = HashMap::with_capacity(ty_params.len());
                for (param_def, &arg_ty) in ty_params.iter().zip(&args) {
                    let _ = subst.insert(*param_def, arg_ty);
                }
                substitute_ty(ck, underlying, &subst)
            } else {
                underlying
            }
        }
        _ => return None,
    };
    let expanded = ck.resolve_ty(expanded);
    if let Type::Sum { variants } = &ck.store.types[expanded] {
        variants
            .iter()
            .find(|v| v.name == variant_name)
            .map(|v| v.fields.clone())
    } else {
        None
    }
}
