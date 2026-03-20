//! Match exhaustiveness checking.

use std::collections::HashSet;
use std::hash::BuildHasher;

use msc_ast::Lit;
use msc_ast::expr::MatchArm;
use msc_ast::pat::Pat;
use msc_shared::{Span, Symbol};

use crate::DefId;
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
            check_named_exhaustiveness(ck, def, arms, span);
        }
        _ => {
            report_missing_wildcard(ck, span);
        }
    }
}

fn check_named_exhaustiveness<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    def: DefId,
    arms: &[MatchArm],
    span: Span,
) {
    let covered = collect_covered_variants(ck, def, arms);
    check_covered_against_expected(ck, def, &covered, span);
}

fn collect_covered_variants<S: BuildHasher>(
    ck: &Checker<'_, S>,
    def: DefId,
    arms: &[MatchArm],
) -> HashSet<String> {
    let mut covered: HashSet<String> = arms
        .iter()
        .filter_map(|arm| {
            if let Pat::Variant { name, .. } = &ck.ctx.ast.pats[arm.pat] {
                Some(ck.ctx.interner.resolve(*name).to_owned())
            } else {
                None
            }
        })
        .collect();

    if def == ck.ctx.well_known.bool {
        for arm in arms {
            if let Pat::Lit {
                lit: Lit::Int { value, .. },
                ..
            } = &ck.ctx.ast.pats[arm.pat]
            {
                match *value {
                    0 => {
                        _ = covered.insert("False".to_owned());
                    }
                    1 => {
                        _ = covered.insert("True".to_owned());
                    }
                    _ => {}
                }
            }
        }
    }

    covered
}

fn check_covered_against_expected<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    def: DefId,
    covered: &HashSet<String>,
    span: Span,
) {
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

fn report_missing_wildcard<S: BuildHasher>(ck: &mut Checker<'_, S>, span: Span) {
    let _d = ck.diags.report(
        &SemaError::NonExhaustiveMatch {
            missing: Box::from("_"),
        },
        span,
        ck.ctx.file_id,
    );
}
