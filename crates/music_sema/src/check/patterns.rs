use std::collections::HashSet;

use music_ast::PatId;
use music_ast::expr::CaseArm;
use music_ast::pat::PatKind;
use music_shared::Symbol;

use super::SemaDb;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::SemaTypeId;
use crate::types::Ty;
use crate::unify::unify;
use music_ast::ExprId;
use music_shared::Span;

impl SemaDb {
    pub(super) fn synth_case(
        &mut self,
        scrutinee: ExprId,
        arms: &[CaseArm],
        span: Span,
    ) -> SemaTypeId {
        let _ = self.synth(scrutinee);

        if arms.is_empty() {
            return self.env.intern(Ty::Empty);
        }

        let mut wildcard_seen = false;
        let mut result_ty = self.env.intern(Ty::Empty);

        for (i, arm) in arms.iter().enumerate() {
            if wildcard_seen {
                let arm_span = self.db.ast.exprs.get(arm.body).span;
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UnreachablePattern,
                    span: arm_span,
                    context: None,
                });
            }

            self.check_or_pattern_bindings(arm.pat);

            let pat_kind = &self.db.ast.pats.get(arm.pat).kind;
            if matches!(pat_kind, PatKind::Wildcard | PatKind::Bind(_)) {
                wildcard_seen = true;
            }

            let arm_ty = self.synth(arm.body);
            if i == 0 {
                result_ty = arm_ty;
            } else {
                match unify(&mut self.env, result_ty, arm_ty, span) {
                    Ok(ty) => result_ty = ty,
                    Err(e) => self.errors.push(e),
                }
            }
        }

        if !wildcard_seen {
            self.errors.push(SemaError {
                kind: SemaErrorKind::NonExhaustiveMatch,
                span,
                context: None,
            });
        }

        result_ty
    }

    pub(super) fn collect_pat_bindings(&self, pat_id: PatId, names: &mut HashSet<Symbol>) {
        let pat = self.db.ast.pats.get(pat_id);
        match &pat.kind {
            PatKind::Bind(ident) => {
                let _inserted = names.insert(ident.name);
            }
            PatKind::As { name, pat } => {
                let _inserted = names.insert(name.name);
                self.collect_pat_bindings(*pat, names);
            }
            PatKind::Variant { fields, .. } => {
                for &f in fields {
                    self.collect_pat_bindings(f, names);
                }
            }
            PatKind::Record(fields) => {
                for f in fields {
                    if let Some(p) = f.pat {
                        self.collect_pat_bindings(p, names);
                    } else {
                        let _inserted = names.insert(f.name.name);
                    }
                }
            }
            PatKind::Tuple(pats) | PatKind::Array(pats) | PatKind::Or(pats) => {
                for &p in pats {
                    self.collect_pat_bindings(p, names);
                }
            }
            PatKind::Wildcard | PatKind::Lit(_) => {}
        }
    }

    pub(super) fn check_or_pattern_bindings(&mut self, pat_id: PatId) {
        let pat = self.db.ast.pats.get(pat_id);
        if let PatKind::Or(alts) = &pat.kind {
            let alts = alts.clone();
            if alts.len() < 2 {
                return;
            }
            let mut first_names = HashSet::new();
            self.collect_pat_bindings(alts[0], &mut first_names);

            for &alt in &alts[1..] {
                let mut alt_names = HashSet::new();
                self.collect_pat_bindings(alt, &mut alt_names);
                if alt_names != first_names {
                    let span = self.db.ast.pats.get(alt).span;
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::OrPatternMismatch,
                        span,
                        context: None,
                    });
                }
            }
        }
    }
}
