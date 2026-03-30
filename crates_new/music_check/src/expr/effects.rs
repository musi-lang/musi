use music_hir::{
    HirArg, HirArrowFlavor, HirExprId, HirExprKind, HirHandleClause, HirMemberKey, HirOrigin,
};
use music_names::{Ident, Symbol};

use std::collections::HashMap;

use crate::SemaErrorKind;

use crate::checker::{Checker, ResumeCtx};
use crate::env::{EffectOpSig, substitute_generics};
use crate::{EffectKey, EffectRow, SemTy, SemTyId};

impl<'a> Checker<'a> {
    pub(super) fn synth_perform(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
    ) -> (SemTyId, EffectRow) {
        // Must be `perform Effect.op(args...)`.
        let HirExprKind::Call { callee, args } = self.ctx.store.exprs.get(expr).kind.clone() else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return (self.state.builtins.unknown, effs);
        };

        let HirExprKind::Member { base, key, .. } = self.ctx.store.exprs.get(callee).kind.clone()
        else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return (self.state.builtins.unknown, effs);
        };

        let HirMemberKey::Name(op_ident) = key else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return (self.state.builtins.unknown, effs);
        };

        let HirExprKind::Name {
            ident: effect_ident,
        } = self.ctx.store.exprs.get(base).kind.clone()
        else {
            self.error(origin.span, SemaErrorKind::InvalidPerformTarget);
            let (_t, effs) = self.synth_expr(expr);
            return (self.state.builtins.unknown, effs);
        };

        let Some(effect_binding) = self.binding_for_use(effect_ident.span) else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(effect_ident.name).to_string(),
                },
            );
            return (self.state.builtins.unknown, EffectRow::empty());
        };

        let sig = {
            let Some(family) = self.state.env.get_effect_family(effect_binding) else {
                self.error(
                    origin.span,
                    SemaErrorKind::UnknownEffect {
                        name: self.ctx.interner.resolve(effect_ident.name).to_string(),
                    },
                );
                return (self.state.builtins.unknown, EffectRow::empty());
            };
            family
                .ops
                .get(&op_ident.name)
                .cloned()
                .map(|sig| (family.generic_count, sig))
        };

        let Some((generic_count, sig)) = sig else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffectOp {
                    effect: self.ctx.interner.resolve(effect_ident.name).to_string(),
                    op: self.ctx.interner.resolve(op_ident.name).to_string(),
                },
            );
            return (self.state.builtins.unknown, EffectRow::empty());
        };

        let (sig, eff_arg) = match generic_count {
            0 => (sig, None),
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
                (
                    EffectOpSig {
                        params: params.into_boxed_slice(),
                        ret,
                    },
                    Some(eff_arg),
                )
            }
            other => {
                self.error(
                    origin.span,
                    SemaErrorKind::EffectTypeParamCountUnsupported { count: other },
                );
                return (self.state.builtins.unknown, EffectRow::empty());
            }
        };

        let mut effs = EffectRow::empty();
        let has_spread = args.iter().any(|a| matches!(a, HirArg::Spread { .. }));
        if !has_spread && args.len() != sig.params.len() {
            self.error(
                origin.span,
                SemaErrorKind::PerformArgCountMismatch {
                    effect: self.ctx.interner.resolve(effect_ident.name).to_string(),
                    op: self.ctx.interner.resolve(op_ident.name).to_string(),
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

        effs.add(EffectKey {
            name: effect_ident.name,
            arg: eff_arg,
        });
        (sig.ret, effs)
    }

    pub(super) fn synth_handle(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        handler: Ident,
        clauses: Box<[HirHandleClause]>,
    ) -> (SemTyId, EffectRow) {
        let (handled_ty, mut handled_effs) = self.synth_expr(expr);

        let result = self.state.semtys.fresh_infer_var(origin.span);
        let mut clause_effs = EffectRow::empty();

        let Some(handler_binding) = self.binding_for_use(handler.span) else {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_string(),
                },
            );
            return (result, handled_effs);
        };

        let handler_family = self.state.env.get_effect_family(handler_binding).cloned();
        if handler_family.is_none() {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_string(),
                },
            );
        }

        let effect_name = self.ctx.interner.resolve(handler.name).to_string();
        if let Some(family) = handler_family.as_ref() {
            let mut counts = HashMap::<Symbol, u32>::new();
            for clause in clauses.iter().filter(|c| !c.is_value) {
                *counts.entry(clause.name.name).or_insert(0) += 1;
            }

            for (&op, _sig) in family.ops.iter() {
                let count = counts.get(&op).copied().unwrap_or(0);
                if count == 0 {
                    self.error(
                        handler.span,
                        SemaErrorKind::HandleClauseMissingOp {
                            effect: effect_name.clone(),
                            op: self.ctx.interner.resolve(op).to_string(),
                        },
                    );
                } else if count > 1 {
                    self.error(
                        handler.span,
                        SemaErrorKind::HandleClauseDuplicateOp {
                            effect: effect_name.clone(),
                            op: self.ctx.interner.resolve(op).to_string(),
                        },
                    );
                }
            }

            for clause in clauses.iter().filter(|c| !c.is_value) {
                if !family.ops.contains_key(&clause.name.name) {
                    self.error(
                        clause.name.span,
                        SemaErrorKind::UnknownEffectOp {
                            effect: effect_name.clone(),
                            op: self.ctx.interner.resolve(clause.name.name).to_string(),
                        },
                    );
                }
            }
        }

        let mut handled_arg = handled_effs
            .items
            .iter()
            .find(|k| k.name == handler.name)
            .and_then(|k| k.arg);

        let mut value_clause_count = 0usize;
        for clause in clauses.iter() {
            let (body_ty, body_effs) = if clause.is_value {
                value_clause_count += 1;
                self.bind_value_clause_binder(clause.name, handled_ty);
                self.synth_expr(clause.body)
            } else {
                let mut op_ret = self.state.builtins.unknown;
                let mut param_tys = Vec::<SemTyId>::new();

                if let Some(family) = handler_family.as_ref() {
                    if let Some(sig) = family.ops.get(&clause.name.name).cloned() {
                        let (sig, _eff_arg) = match family.generic_count {
                            0 => (sig, None),
                            1 => {
                                let arg = handled_arg.get_or_insert_with(|| {
                                    self.state.semtys.fresh_infer_var(origin.span)
                                });
                                let subst = [*arg];
                                let params: Vec<_> = sig
                                    .params
                                    .iter()
                                    .copied()
                                    .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
                                    .collect();
                                let ret =
                                    substitute_generics(&mut self.state.semtys, sig.ret, &subst);
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

                        let expected_params =
                            u32::try_from(param_tys.len() + 1).unwrap_or(u32::MAX);
                        let found = u32::try_from(clause.params.len()).unwrap_or(0);
                        if expected_params != found {
                            self.error(
                                clause.origin.span,
                                SemaErrorKind::HandleClauseParamCountMismatch {
                                    effect: effect_name.clone(),
                                    op: self.ctx.interner.resolve(clause.name.name).to_string(),
                                    expected: expected_params,
                                    found,
                                },
                            );
                        }
                    }
                }

                // Bind params, last param is continuation.
                let k_ty = self.state.semtys.alloc(SemTy::Arrow {
                    flavor: HirArrowFlavor::Pure,
                    input: op_ret,
                    output: result,
                });

                if !clause.params.is_empty() {
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
                }

                self.state.flow.resume_stack.push(ResumeCtx {
                    arg: op_ret,
                    result,
                });
                let out = self.synth_expr(clause.body);
                let _ = self.state.flow.resume_stack.pop();
                out
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
