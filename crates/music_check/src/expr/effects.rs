use music_hir::{
    HirArg, HirArrowFlavor, HirExprId, HirExprKind, HirHandleClause, HirMemberKey, HirOrigin,
};
use music_names::{Ident, Symbol};

use std::collections::HashMap;

use crate::SemaErrorKind;

use crate::checker::{Checker, ResumeCtx};
use crate::env::{EffectFamily, EffectOpSig, substitute_generics};
use crate::{EffectKey, EffectRow, SemTy, SemTyId};

type PerformFallback = (SemTyId, EffectRow);
type PerformParts = (Ident, Ident, Box<[HirArg]>);
type PerformPartsResult = Result<PerformParts, PerformFallback>;

impl Checker<'_> {
    pub(super) fn synth_perform(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
    ) -> (SemTyId, EffectRow) {
        let (effect_ident, op_ident, args) = match self.perform_parts(origin, expr) {
            Ok(parts) => parts,
            Err(out) => return out,
        };

        let Some((sig, eff_arg)) = self.perform_sig(origin, effect_ident, op_ident) else {
            return (self.state.builtins.unknown, EffectRow::empty());
        };

        let mut effs = EffectRow::empty();
        self.check_perform_args(origin, effect_ident, op_ident, &args, &sig, &mut effs);
        effs.add(EffectKey {
            name: effect_ident.name,
            arg: eff_arg,
        });
        (sig.ret, effs)
    }

    fn perform_parts(&mut self, origin: HirOrigin, expr: HirExprId) -> PerformPartsResult {
        let HirExprKind::Call { callee, args } = self.ctx.store.exprs.get(expr).kind.clone() else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return Err((self.state.builtins.unknown, effs));
        };

        let HirExprKind::Member { base, key, .. } = self.ctx.store.exprs.get(callee).kind.clone()
        else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return Err((self.state.builtins.unknown, effs));
        };

        let HirMemberKey::Name(op_ident) = key else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return Err((self.state.builtins.unknown, effs));
        };

        let HirExprKind::Named {
            ident: effect_ident,
        } = self.ctx.store.exprs.get(base).kind.clone()
        else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return Err((self.state.builtins.unknown, effs));
        };

        Ok((effect_ident, op_ident, args))
    }

    fn perform_sig(
        &mut self,
        origin: HirOrigin,
        effect: Ident,
        op: Ident,
    ) -> Option<(EffectOpSig, Option<SemTyId>)> {
        let Some(effect_binding) = self.binding_for_use(effect.span) else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(effect.name).to_owned(),
                },
            );
            return None;
        };

        let Some(family) = self.state.env.get_effect_family(effect_binding) else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(effect.name).to_owned(),
                },
            );
            return None;
        };

        let Some(sig) = family.ops.get(&op.name).cloned() else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffectOp {
                    effect: self.ctx.interner.resolve(effect.name).to_owned(),
                    op: self.ctx.interner.resolve(op.name).to_owned(),
                },
            );
            return None;
        };

        self.instantiate_effect_sig(origin, family.generic_count, sig)
    }

    fn instantiate_effect_sig(
        &mut self,
        origin: HirOrigin,
        generic_count: u32,
        sig: EffectOpSig,
    ) -> Option<(EffectOpSig, Option<SemTyId>)> {
        match generic_count {
            0 => Some((sig, None)),
            1 => {
                let eff_arg = self.state.semtys.fresh_infer_var(origin.span);
                let subst = [eff_arg];
                let params: Vec<_> = sig
                    .params
                    .iter()
                    .copied()
                    .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
                    .collect();
                let ret = substitute_generics(&mut self.state.semtys, sig.ret, &subst);
                Some((
                    EffectOpSig {
                        params: params.into_boxed_slice(),
                        ret,
                    },
                    Some(eff_arg),
                ))
            }
            other => {
                self.error(
                    origin.span,
                    SemaErrorKind::EffectTypeParamCountUnsupported { count: other },
                );
                None
            }
        }
    }

    fn check_perform_args(
        &mut self,
        origin: HirOrigin,
        effect: Ident,
        op: Ident,
        args: &[HirArg],
        sig: &EffectOpSig,
        effs: &mut EffectRow,
    ) {
        let has_spread = args.iter().any(|a| matches!(a, HirArg::Spread { .. }));
        if !has_spread && args.len() != sig.params.len() {
            self.error(
                origin.span,
                SemaErrorKind::PerformArgCountMismatch {
                    effect: self.ctx.interner.resolve(effect.name).to_owned(),
                    op: self.ctx.interner.resolve(op.name).to_owned(),
                    expected: u32::try_from(sig.params.len()).unwrap_or(0),
                    found: u32::try_from(args.len()).unwrap_or(0),
                },
            );
        }

        for (i, arg) in args.iter().enumerate() {
            match arg {
                HirArg::Expr(id) => {
                    let expected = sig
                        .params
                        .get(i)
                        .copied()
                        .unwrap_or(self.state.builtins.unknown);
                    let (_t, e) = self.check_expr(*id, expected);
                    effs.union_with(&e);
                }
                HirArg::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
            }
        }
    }

    pub(super) fn synth_handle(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        handler: Ident,
        clauses: &[HirHandleClause],
    ) -> (SemTyId, EffectRow) {
        let (handled_ty, mut handled_effs) = self.synth_expr(expr);

        let result = self.state.semtys.fresh_infer_var(origin.span);
        let mut clause_effs = EffectRow::empty();

        let Some(handler_binding) = self.binding_for_use(handler.span) else {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_owned(),
                },
            );
            return (result, handled_effs);
        };

        let handler_family = self.state.env.get_effect_family(handler_binding).cloned();
        if handler_family.is_none() {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_owned(),
                },
            );
        }

        let effect_name = self.ctx.interner.resolve(handler.name).to_owned();
        if let Some(family) = handler_family.as_ref() {
            self.validate_handle_clauses(handler, &effect_name, clauses, family);
        }

        let mut handled_arg = handled_effs
            .items
            .iter()
            .find(|k| k.name == handler.name)
            .and_then(|k| k.arg);

        let mut value_clause_count = 0usize;
        for clause in clauses {
            let (body_ty, body_effs) = if clause.is_value {
                value_clause_count += 1;
                self.synth_handle_value_clause(clause, handled_ty)
            } else {
                self.synth_handle_op_clause(
                    origin,
                    clause,
                    handler_family.as_ref(),
                    &effect_name,
                    &mut handled_arg,
                    result,
                )
            };

            clause_effs.union_with(&body_effs);
            let _ = self.unify_or_report(origin.span, result, body_ty);
        }

        if value_clause_count != 1 {
            self.error(origin.span, SemaErrorKind::HandleValueClauseRequired);
        }

        handled_effs.remove_by_name(handler.name);
        handled_effs.union_with(&clause_effs);
        (result, handled_effs)
    }

    fn validate_handle_clauses(
        &mut self,
        handler: Ident,
        effect_name: &str,
        clauses: &[HirHandleClause],
        family: &EffectFamily,
    ) {
        let mut counts = HashMap::<Symbol, u32>::new();
        for clause in clauses.iter().filter(|c| !c.is_value) {
            *counts.entry(clause.name.name).or_insert(0) += 1;
        }

        for &op in family.ops.keys() {
            let count = counts.get(&op).copied().unwrap_or(0);
            if count == 0 {
                self.error(
                    handler.span,
                    SemaErrorKind::HandleClauseMissingOp {
                        effect: effect_name.to_owned(),
                        op: self.ctx.interner.resolve(op).to_owned(),
                    },
                );
            } else if count > 1 {
                self.error(
                    handler.span,
                    SemaErrorKind::HandleClauseDuplicateOp {
                        effect: effect_name.to_owned(),
                        op: self.ctx.interner.resolve(op).to_owned(),
                    },
                );
            }
        }

        for clause in clauses.iter().filter(|c| !c.is_value) {
            if !family.ops.contains_key(&clause.name.name) {
                self.error(
                    clause.name.span,
                    SemaErrorKind::UnknownEffectOp {
                        effect: effect_name.to_owned(),
                        op: self.ctx.interner.resolve(clause.name.name).to_owned(),
                    },
                );
            }
        }
    }

    fn synth_handle_value_clause(
        &mut self,
        clause: &HirHandleClause,
        handled_ty: SemTyId,
    ) -> (SemTyId, EffectRow) {
        self.bind_value_clause_binder(clause.name, handled_ty);
        self.synth_expr(clause.body)
    }

    fn synth_handle_op_clause(
        &mut self,
        origin: HirOrigin,
        clause: &HirHandleClause,
        handler_family: Option<&EffectFamily>,
        effect_name: &str,
        handled_arg: &mut Option<SemTyId>,
        result: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let mut op_ret = self.state.builtins.unknown;
        let mut param_tys = Vec::<SemTyId>::new();

        if let Some(family) = handler_family {
            if let Some(sig) = family.ops.get(&clause.name.name).cloned() {
                let (sig, _eff_arg) = match family.generic_count {
                    0 => (sig, None),
                    1 => {
                        let arg = handled_arg
                            .get_or_insert_with(|| self.state.semtys.fresh_infer_var(origin.span));
                        let subst = [*arg];
                        let params: Vec<_> = sig
                            .params
                            .iter()
                            .copied()
                            .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
                            .collect();
                        let ret = substitute_generics(&mut self.state.semtys, sig.ret, &subst);
                        (
                            EffectOpSig {
                                params: params.into_boxed_slice(),
                                ret,
                            },
                            Some(*arg),
                        )
                    }
                    other => {
                        self.error(
                            clause.origin.span,
                            SemaErrorKind::EffectTypeParamCountUnsupported { count: other },
                        );
                        (sig, None)
                    }
                };

                op_ret = sig.ret;
                param_tys = sig.params.to_vec();

                let expected_params = u32::try_from(param_tys.len() + 1).unwrap_or(u32::MAX);
                let found = u32::try_from(clause.params.len()).unwrap_or(0);
                if expected_params != found {
                    self.error(
                        clause.origin.span,
                        SemaErrorKind::HandleClauseParamCountMismatch {
                            effect: effect_name.to_owned(),
                            op: self.ctx.interner.resolve(clause.name.name).to_owned(),
                            expected: expected_params,
                            found,
                        },
                    );
                }
            }
        }

        let k_ty = self.state.semtys.alloc(SemTy::Arrow {
            flavor: HirArrowFlavor::Pure,
            input: op_ret,
            output: result,
        });

        for (i, p) in clause.params.iter().copied().enumerate() {
            let ty = if i + 1 == clause.params.len() {
                k_ty
            } else {
                param_tys
                    .get(i)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown)
            };
            self.bind_handle_param(p, ty);
        }

        self.state.flow.resume_stack.push(ResumeCtx {
            arg: op_ret,
            result,
        });
        let out = self.synth_expr(clause.body);
        let _ = self.state.flow.resume_stack.pop();
        out
    }

    pub(super) fn synth_resume(
        &mut self,
        origin: HirOrigin,
        value: Option<HirExprId>,
    ) -> (SemTyId, EffectRow) {
        let Some(ctx) = self.state.flow.resume_stack.last().cloned() else {
            self.error(origin.span, SemaErrorKind::ResumeOutsideOpClause);
            return (self.state.builtins.error, EffectRow::empty());
        };

        let mut effs = EffectRow::empty();
        if let Some(value) = value {
            let (t, e) = self.synth_expr(value);
            effs.union_with(&e);
            let _ = self.unify_or_report(origin.span, ctx.arg, t);
        } else {
            let _ = self.unify_or_report(origin.span, ctx.arg, self.state.builtins.unit);
        }

        (ctx.result, effs)
    }
}
