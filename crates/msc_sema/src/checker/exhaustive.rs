//! Match exhaustiveness checking.

use std::collections::HashSet;
use std::hash::BuildHasher;

use msc_ast::expr::MatchArm;
use msc_ast::pat::Pat;
use msc_shared::{Span, Symbol};

use crate::checker::Checker;
use crate::error::SemaError;
use crate::types::{Type, TypeIdx};

pub(super) fn check_match_exhaustiveness<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    scrut_ty: TypeIdx,
    arms: &[MatchArm],
    span: Span,
) {
    let has_wildcard = arms.iter().any(|arm| {
        matches!(
            &ck.ctx.ast.pats[arm.pat],
            Pat::Wild { .. } | Pat::Bind { inner: None, .. }
        )
    });
    if has_wildcard {
        return;
    }

    let resolved = ck.resolve_ty(scrut_ty);
    if matches!(ck.store.types[resolved], Type::Error | Type::Var(_)) {
        return;
    }
    match ck.store.types[resolved].clone() {
        Type::Sum { variants } => {
            let covered: HashSet<Symbol> = arms
                .iter()
                .filter_map(|arm| {
                    if let Pat::Variant { name, .. } = &ck.ctx.ast.pats[arm.pat] {
                        Some(*name)
                    } else {
                        None
                    }
                })
                .collect();
            for variant in &variants {
                if !covered.contains(&variant.name) {
                    let name_str = ck.ctx.interner.resolve(variant.name);
                    let _d = ck.diags.report(
                        &SemaError::NonExhaustiveMatch {
                            missing: Box::from(name_str),
                        },
                        span,
                        ck.ctx.file_id,
                    );
                }
            }
        }
        Type::Named { def, .. } => {
            let covered: HashSet<String> = arms
                .iter()
                .filter_map(|arm| {
                    if let Pat::Variant { name, .. } = &ck.ctx.ast.pats[arm.pat] {
                        Some(ck.ctx.interner.resolve(*name).to_owned())
                    } else {
                        None
                    }
                })
                .collect();

            // Well-known types with known variant sets.
            let option_def = ck
                .ctx
                .interner
                .get("Option")
                .and_then(|sym| ck.scopes.lookup(ck.current_scope, sym));
            let well_known_variants: Option<&[&str]> = if option_def == Some(def) {
                Some(&["Some", "None"])
            } else if def == ck.ctx.well_known.bool {
                Some(&["True", "False"])
            } else {
                None
            };

            if let Some(expected) = well_known_variants {
                for case in expected {
                    if !covered.contains(*case) {
                        let _d = ck.diags.report(
                            &SemaError::NonExhaustiveMatch {
                                missing: Box::from(*case),
                            },
                            span,
                            ck.ctx.file_id,
                        );
                    }
                }
            } else if let Some(dt) = ck.defs.get(def).ty_info.ty {
                let resolved_def = ck.resolve_ty(dt);
                if let Type::Sum { variants } = ck.store.types[resolved_def].clone() {
                    for variant in &variants {
                        let name_str = ck.ctx.interner.resolve(variant.name);
                        if !covered.contains(name_str) {
                            let _d = ck.diags.report(
                                &SemaError::NonExhaustiveMatch {
                                    missing: Box::from(name_str),
                                },
                                span,
                                ck.ctx.file_id,
                            );
                        }
                    }
                } else {
                    report_missing_wildcard(ck, span);
                }
            } else {
                report_missing_wildcard(ck, span);
            }
        }
        _ => {
            report_missing_wildcard(ck, span);
        }
    }
}

fn report_missing_wildcard<S: BuildHasher>(ck: &mut Checker<'_, S>, span: Span) {
    let _d = ck.diags.report(
        &SemaError::NonExhaustiveMatch {
            missing: Box::from("_"),
        },
        span,
        ck.ctx.file_id,
    );
}
