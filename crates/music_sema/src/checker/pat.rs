//! Pattern type checking.

use music_ast::PatIdx;
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_shared::Span;

use crate::checker::Checker;
use crate::types::{Type, TypeIdx};

/// Checks a pattern against an expected type.
pub(crate) fn check_pat(ck: &mut Checker<'_>, pat_idx: PatIdx, expected: TypeIdx) {
    match ck.ctx.ast.pats[pat_idx].clone() {
        Pat::Lit { lit, span } => {
            let lit_ty = match &lit {
                Lit::Int { .. } => ck.named_ty(ck.ctx.well_known.ints.int),
                Lit::Float { .. } => ck.named_ty(ck.ctx.well_known.floats.float64),
                Lit::Str { .. } | Lit::FStr { .. } => ck.named_ty(ck.ctx.well_known.string),
                Lit::Rune { .. } => ck.named_ty(ck.ctx.well_known.rune),
                Lit::Unit { .. } => ck.named_ty(ck.ctx.well_known.unit),
            };
            ck.unify_or_report(expected, lit_ty, span);
        }
        Pat::Bind { span, inner, .. } => {
            if let Some(&def_id) = ck.ctx.pat_defs.get(&span) {
                ck.defs.get_mut(def_id).ty_info.ty = Some(expected);
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
                    ck.unify_or_report(expected, tup_ty, span);
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
        Pat::Variant { span, args, .. } => {
            if let Some(&def_id) = ck.ctx.pat_defs.get(&span) {
                ck.defs.get_mut(def_id).ty_info.ty = Some(expected);
            }
            let resolved = ck.resolve_ty(expected);
            let param_tys: Vec<_> = if let Type::Fn { params, .. } = &ck.store.types[resolved] {
                params.clone()
            } else {
                args.iter().map(|_| ck.fresh_var(Span::DUMMY)).collect()
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
