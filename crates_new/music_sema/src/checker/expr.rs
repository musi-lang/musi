use std::collections::HashMap;

use music_hir::{
    HirArg, HirArrowFlavor, HirBinaryOp, HirEffectSet, HirExprId, HirExprKind, HirHandleClause,
    HirLitKind, HirMemberKey, HirOrigin, HirParam, HirPatId, HirRecordItem, HirTyId,
};
use music_names::Symbol;

use crate::SemaErrorKind;

use super::{
    EffectKey, EffectRow, SemTy, SemTyId,
    env::ValueScheme,
};

use super::check::Checker;
use super::check::ResumeCtx;

impl<'a> Checker<'a> {

    fn union_call_effects(&mut self, callee: HirExprId, effs: &mut EffectRow) {
        // Named bindings may carry declared `with { ... }` effects (or inferred effects when
        // aliases are created via `let x := f;` and `let x := (..) => ...;`).
        if let HirExprKind::Name { ident } = self.ctx.store.exprs.get(callee).kind.clone() {
            if let Some(binding) = self.binding_for_use(ident.span) {
                if let Some(scheme) = self.state.env.get_value(binding) {
                    if let Some(declared) = scheme.declared_effects.as_ref() {
                        effs.union_with(declared);
                        return;
                    }
                }
            }
        }

        // Inline lambdas carry latent effects that only happen when called.
        if let Some(latent) = self.state.flow.callable_effs.get(&callee).cloned() {
            effs.union_with(&latent);
        }
    }

    pub(super) fn synth_expr(&mut self, expr_id: HirExprId) -> (SemTyId, EffectRow) {
        let expr = self.ctx.store.exprs.get(expr_id).clone();
        let origin = expr.origin;

        let (ty, effs) = match expr.kind {
            HirExprKind::Error => (self.state.builtins.error, EffectRow::empty()),
            HirExprKind::Sequence { exprs, yields_unit } => {
                let mut effs = EffectRow::empty();
                let mut last_ty = self.state.builtins.unit;
                for id in exprs.iter().copied() {
                    let (t, e) = self.synth_expr(id);
                    last_ty = t;
                    effs.union_with(&e);
                }
                let ty = if yields_unit { self.state.builtins.unit } else { last_ty };
                (ty, effs)
            }
            HirExprKind::Let {
                mods: _mods,
                mutable: _mutable,
                pat,
                has_params,
                params,
                type_params,
                where_: _where,
                effects,
                annot,
                value,
            } => self.synth_let(origin, pat, has_params, params, type_params, effects, annot, value),
            HirExprKind::Import { .. } => (self.state.builtins.unknown, EffectRow::empty()),
            HirExprKind::ForeignBlock { items, .. } => {
                let mut effs = EffectRow::empty();
                for id in items.iter().copied() {
                    let (_t, e) = self.synth_expr(id);
                    effs.union_with(&e);
                }
                (self.state.builtins.unit, effs)
            }
            HirExprKind::Data { .. } | HirExprKind::Effect { .. } | HirExprKind::Class { .. } => {
                (self.state.builtins.type_, EffectRow::empty())
            }
            HirExprKind::Instance { .. } => (self.state.builtins.unit, EffectRow::empty()),
            HirExprKind::Name { ident } => {
                let ty = self
                    .binding_for_use(ident.span)
                    .and_then(|binding| self.state.env.get_value(binding))
                    .map(|scheme| self.state.env.instantiate(&mut self.state.semtys, scheme, ident.span))
                    .unwrap_or(self.state.builtins.unknown);
                (ty, EffectRow::empty())
            }
            HirExprKind::Lit { lit } => {
                let ty = match lit.kind {
                    HirLitKind::Int { .. } => self.state.builtins.int_,
                    HirLitKind::Float { .. } => self.state.builtins.float_,
                    HirLitKind::Rune { .. } => self.state.builtins.int_,
                    HirLitKind::String(_) | HirLitKind::FString { .. } => self.state.builtins.string_,
                };
                (ty, EffectRow::empty())
            }
            HirExprKind::Tuple { items } => {
                if items.is_empty() {
                    (self.state.builtins.unit, EffectRow::empty())
                } else {
                    let mut effs = EffectRow::empty();
                    let mut tys = Vec::with_capacity(items.len());
                    for item in items.iter().copied() {
                        let (t, e) = self.synth_expr(item);
                        tys.push(t);
                        effs.union_with(&e);
                    }
                    let ty = self.state.semtys.alloc(SemTy::Tuple {
                        items: tys.into_boxed_slice(),
                    });
                    (ty, effs)
                }
            }
            HirExprKind::Array { items } => {
                let mut effs = EffectRow::empty();
                let elem = self.state.semtys.fresh_infer_var(origin.span);
                for item in items {
                    match item {
                        music_hir::HirArrayItem::Expr(id) => {
                            let (t, e) = self.synth_expr(id);
                            effs.union_with(&e);
                            let _ = self.unify_or_report(origin.span, elem, t);
                        }
                        music_hir::HirArrayItem::Spread { expr, .. } => {
                            let (_t, e) = self.synth_expr(expr);
                            effs.union_with(&e);
                        }
                    }
                }
                let ty = self.state.semtys.alloc(SemTy::Array {
                    dims: Box::new([]),
                    elem,
                });
                (ty, effs)
            }
            HirExprKind::Record { items } => {
                let mut effs = EffectRow::empty();
                for item in items.iter() {
                    match item {
                        HirRecordItem::Field { value, .. } => {
                            if let Some(expr) = value {
                                let (_t, e) = self.synth_expr(*expr);
                                effs.union_with(&e);
                            }
                        }
                        HirRecordItem::Spread { expr, .. } => {
                            let (_t, e) = self.synth_expr(*expr);
                            effs.union_with(&e);
                        }
                    }
                }
                (self.state.builtins.unknown, effs)
            }
            HirExprKind::Variant { payload, .. } => {
                let mut effs = EffectRow::empty();
                if let Some(payload) = payload {
                    let (_t, e) = self.synth_expr(payload);
                    effs.union_with(&e);
                }
                (self.state.builtins.unknown, effs)
            }
            HirExprKind::Lambda { params, ret, body } => {
                // Lambda bodies are not evaluated at definition time; their effects are latent and
                // only occur when the function is called.
                let (fn_ty, latent) = self.synth_lambda(origin, params, ret, body);
                let _prev = self.state.flow.callable_effs.insert(expr_id, latent);
                (fn_ty, EffectRow::empty())
            }
            HirExprKind::Call { callee, args } => self.synth_call(origin, callee, args),
            HirExprKind::Member { base, .. } => {
                let (_t, effs) = self.synth_expr(base);
                (self.state.builtins.unknown, effs)
            }
            HirExprKind::Index { base, indices } => {
                let (_t, mut effs) = self.synth_expr(base);
                for idx in indices.iter().copied() {
                    let (_t, e) = self.synth_expr(idx);
                    effs.union_with(&e);
                }
                (self.state.builtins.unknown, effs)
            }
            HirExprKind::RecordUpdate { base, items } => {
                let (_t, mut effs) = self.synth_expr(base);
                for item in items.iter() {
                    match item {
                        HirRecordItem::Field { value, .. } => {
                            if let Some(expr) = value {
                                let (_t, e) = self.synth_expr(*expr);
                                effs.union_with(&e);
                            }
                        }
                        HirRecordItem::Spread { expr, .. } => {
                            let (_t, e) = self.synth_expr(*expr);
                            effs.union_with(&e);
                        }
                    }
                }
                (self.state.builtins.unknown, effs)
            }
            HirExprKind::TypeTest { expr, .. } => {
                let (_t, effs) = self.synth_expr(expr);
                (self.state.builtins.bool_, effs)
            }
            HirExprKind::TypeCast { expr, ty } => {
                let (_t, effs) = self.synth_expr(expr);
                let ty = self.lower_hir_ty(ty, &HashMap::new());
                (ty, effs)
            }
            HirExprKind::Prefix { op, expr } => {
                let (inner, effs) = self.synth_expr(expr);
                let ty = match op {
                    music_hir::HirPrefixOp::Negate => {
                        let _ = self.unify_or_report(origin.span, inner, self.state.builtins.int_);
                        self.state.builtins.int_
                    }
                    music_hir::HirPrefixOp::Not => self.state.builtins.bool_,
                    music_hir::HirPrefixOp::Mut => self.state.builtins.unknown,
                };
                (ty, effs)
            }
            HirExprKind::Binary { op, left, right } => self.synth_binary(origin, op, left, right),
            HirExprKind::Case { scrut, arms } => self.synth_case(origin, scrut, arms),
            HirExprKind::Perform { expr } => self.synth_perform(origin, expr),
            HirExprKind::Handle {
                expr,
                handler,
                clauses,
            } => self.synth_handle(origin, expr, handler, clauses),
            HirExprKind::Resume { value } => self.synth_resume(origin, value),
            HirExprKind::Quote { .. } | HirExprKind::Splice { .. } => {
                (self.state.builtins.any, EffectRow::empty())
            }
        };

        self.record_type(expr_id, ty);
        (ty, effs)
    }

    fn synth_let(
        &mut self,
        origin: HirOrigin,
        pat: HirPatId,
        has_params: bool,
        params: Box<[HirParam]>,
        type_params: Box<[music_hir::HirTypeParam]>,
        effects: Option<HirEffectSet>,
        annot: Option<HirTyId>,
        value: Option<HirExprId>,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();

        let mut ty_params = HashMap::<Symbol, u32>::new();
        for (i, tp) in type_params.iter().enumerate() {
            let _prev = ty_params.insert(tp.name.name, u32::try_from(i).unwrap_or(0));
        }

        let declared_effects = effects.as_ref().map(|set| self.lower_effect_set(set, &ty_params));

        // Function-like let.
        if has_params {
            let (fn_ty, body_effs) = self.typecheck_callable(
                origin,
                &params,
                &ty_params,
                annot,
                value,
                declared_effects.as_ref(),
            );

            if let Some(allowed) = declared_effects.as_ref() {
                if !allowed.is_open {
                    for extra in body_effs.items.iter() {
                        if allowed.items.contains(extra) {
                            continue;
                        }
                        self.error(
                            origin.span,
                            SemaErrorKind::EffectNotDeclared {
                                name: self.ctx.interner.resolve(extra.name).to_string(),
                            },
                        );
                    }
                    if body_effs.is_open {
                        self.error(origin.span, SemaErrorKind::EffectRemainderNotDeclared);
                    }
                }
            } else if !body_effs.is_pure() {
                self.error(origin.span, SemaErrorKind::MissingWithClause);
            }

            self.bind_pat_to_scheme(pat, ValueScheme {
                generic_count: type_params.len().try_into().unwrap_or(0),
                ty: fn_ty,
                declared_effects,
            });
            // Function definitions do not evaluate their bodies at binding time.
            return (self.state.builtins.unit, EffectRow::empty());
        }

        // Non-function let.
        if let Some(value) = value {
            let (rhs_ty, rhs_effs) = self.synth_expr(value);
            effs.union_with(&rhs_effs);

            let rhs_ty = if let Some(annot) = annot {
                let expected = self.lower_hir_ty(annot, &ty_params);
                let _ = self.unify_or_report(origin.span, rhs_ty, expected);
                expected
            } else {
                rhs_ty
            };

            // Special-case: effect definitions produce operation tables.
            if let HirExprKind::Effect { members } = self.ctx.store.exprs.get(value).kind.clone() {
                self.register_effect_def(pat, &ty_params, &members);
            }

            // Preserve latent effects when binding function values.
            let mut latent = None;
            if let HirExprKind::Name { ident } = self.ctx.store.exprs.get(value).kind.clone() {
                if let Some(binding) = self.binding_for_use(ident.span) {
                    latent = self.state.env.get_value(binding).and_then(|s| s.declared_effects.clone());
                }
            }
            if latent.is_none() {
                latent = self.state.flow.callable_effs.get(&value).cloned();
            }

            self.bind_pat_to_scheme(pat, ValueScheme {
                generic_count: type_params.len().try_into().unwrap_or(0),
                ty: rhs_ty,
                declared_effects: latent,
            });
        }

        (self.state.builtins.unit, effs)
    }

    fn synth_call(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: Box<[HirArg]>,
    ) -> (SemTyId, EffectRow) {
        let (callee_ty, mut effs) = self.synth_expr(callee);

        let mut arg_tys = Vec::new();
        for arg in args.iter() {
            match arg {
                HirArg::Expr(id) => {
                    let (t, e) = self.synth_expr(*id);
                    arg_tys.push(t);
                    effs.union_with(&e);
                }
                HirArg::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                    arg_tys.push(self.state.builtins.unknown);
                }
            }
        }

        let expected_input = match arg_tys.len() {
            0 => self.state.builtins.unit,
            1 => arg_tys[0],
            _ => self.state.semtys.alloc(SemTy::Tuple {
                items: arg_tys.into_boxed_slice(),
            }),
        };

        let out = self.state.semtys.fresh_infer_var(origin.span);
        let expect_fn = self.state.semtys.alloc(SemTy::Arrow {
            flavor: HirArrowFlavor::Effectful,
            input: expected_input,
            output: out,
        });

        let _ = self.unify_or_report(origin.span, callee_ty, expect_fn);

        self.union_call_effects(callee, &mut effs);

        (out, effs)
    }

    fn synth_binary(
        &mut self,
        origin: HirOrigin,
        op: HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> (SemTyId, EffectRow) {
        match op {
            HirBinaryOp::Pipe => {
                // `x |> f` is `f(x)`
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);

                let out = self.state.semtys.fresh_infer_var(origin.span);
                let expect_fn = self.state.semtys.alloc(SemTy::Arrow {
                    flavor: HirArrowFlavor::Effectful,
                    input: l_ty,
                    output: out,
                });
                let _ = self.unify_or_report(origin.span, r_ty, expect_fn);
                self.union_call_effects(right, &mut effs);
                (out, effs)
            }
            HirBinaryOp::Assign => {
                let (_l_ty, mut effs) = self.synth_expr(left);
                let (_r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                (self.state.builtins.unit, effs)
            }
            HirBinaryOp::Eq
            | HirBinaryOp::NotEq
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::LtEq
            | HirBinaryOp::GtEq => {
                let (_l_ty, mut effs) = self.synth_expr(left);
                let (_r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                (self.state.builtins.bool_, effs)
            }
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Mod => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.int_);
                let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.int_);
                (self.state.builtins.int_, effs)
            }
            _ => {
                let (_l_ty, mut effs) = self.synth_expr(left);
                let (_r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                (self.state.builtins.unknown, effs)
            }
        }
    }

    fn synth_case(
        &mut self,
        origin: HirOrigin,
        scrut: HirExprId,
        arms: Box<[music_hir::HirCaseArm]>,
    ) -> (SemTyId, EffectRow) {
        let (scrut_ty, mut effs) = self.synth_expr(scrut);
        let result = self.state.semtys.fresh_infer_var(origin.span);

        for arm in arms.iter() {
            self.bind_pat_for_scrut(arm.pat, scrut_ty);

            if let Some(guard) = arm.guard {
                let (_t, e) = self.synth_expr(guard);
                effs.union_with(&e);
            }

            let (body_ty, body_effs) = self.synth_expr(arm.body);
            effs.union_with(&body_effs);
            let _ = self.unify_or_report(origin.span, body_ty, result);
        }

        (result, effs)
    }

    fn synth_perform(&mut self, origin: HirOrigin, expr: HirExprId) -> (SemTyId, EffectRow) {
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

        let HirExprKind::Name { ident: effect_ident } = self.ctx.store.exprs.get(base).kind.clone()
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
            let Some(ops) = self.state.env.get_effect_ops(effect_binding) else {
                self.error(
                    origin.span,
                    SemaErrorKind::UnknownEffect {
                        name: self.ctx.interner.resolve(effect_ident.name).to_string(),
                    },
                );
                return (self.state.builtins.unknown, EffectRow::empty());
            };
            ops.get(&op_ident.name).cloned()
        };

        let Some(sig) = sig else {
            self.error(
                origin.span,
                SemaErrorKind::UnknownEffectOp {
                    effect: self.ctx.interner.resolve(effect_ident.name).to_string(),
                    op: self.ctx.interner.resolve(op_ident.name).to_string(),
                },
            );
            return (self.state.builtins.unknown, EffectRow::empty());
        };

        let mut effs = EffectRow::empty();
        for (arg, expected) in args.iter().zip(sig.params.iter().copied()) {
            match arg {
                HirArg::Expr(id) => {
                    let (t, e) = self.synth_expr(*id);
                    effs.union_with(&e);
                    let _ = self.unify_or_report(origin.span, t, expected);
                }
                HirArg::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
            }
        }

        effs.add(EffectKey {
            name: effect_ident.name,
            arg: None,
        });
        (sig.ret, effs)
    }

    fn synth_handle(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        handler: music_names::Ident,
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

        let handler_ops = self.state.env.get_effect_ops(handler_binding).cloned();
        if handler_ops.is_none() {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_string(),
                },
            );
        }

        let mut value_clause_count = 0usize;
        for clause in clauses.iter() {
            let (body_ty, body_effs) = if clause.is_value {
                value_clause_count += 1;
                self.bind_value_clause_binder(clause.name, handled_ty);
                self.synth_expr(clause.body)
            } else {
                let sig = handler_ops
                    .as_ref()
                    .and_then(|ops| ops.get(&clause.name.name).cloned());

                let (op_ret, param_tys) = sig.map_or_else(
                    || (self.state.builtins.unknown, Vec::new()),
                    |sig| (sig.ret, sig.params.to_vec()),
                );

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
                            param_tys.get(i).copied().unwrap_or(self.state.builtins.unknown)
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
            let _ = self.unify_or_report(origin.span, body_ty, result);
        }

        if value_clause_count != 1 {
            self.error(origin.span, SemaErrorKind::HandleValueClauseRequired);
        }

        handled_effs.remove_by_name(handler.name);
        handled_effs.union_with(&clause_effs);
        (result, handled_effs)
    }

    fn synth_resume(&mut self, origin: HirOrigin, value: Option<HirExprId>) -> (SemTyId, EffectRow) {
        let Some(ctx) = self.state.flow.resume_stack.last().cloned() else {
            self.error(origin.span, SemaErrorKind::ResumeOutsideOpClause);
            return (self.state.builtins.error, EffectRow::empty());
        };

        let mut effs = EffectRow::empty();
        if let Some(value) = value {
            let (t, e) = self.synth_expr(value);
            effs.union_with(&e);
            let _ = self.unify_or_report(origin.span, t, ctx.arg);
        } else {
            let _ = self.unify_or_report(origin.span, self.state.builtins.unit, ctx.arg);
        }

        (ctx.result, effs)
    }
}
