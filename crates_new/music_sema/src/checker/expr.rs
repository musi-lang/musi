use std::collections::HashMap;

use music_hir::{
    HirArg, HirArrowFlavor, HirBinaryOp, HirDeclMods, HirEffectSet, HirExprId, HirExprKind,
    HirHandleClause, HirLitKind, HirMemberKey, HirOrigin, HirParam, HirPatId, HirRecordItem,
    HirTyId,
};
use music_names::Symbol;

use crate::SemaErrorKind;

use super::unify;
use super::{
    EffectKey, EffectRow, SemTy, SemTyId,
    env::{ValueScheme, substitute_generics},
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

    pub(super) fn check_expr(
        &mut self,
        expr_id: HirExprId,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let expr = self.ctx.store.exprs.get(expr_id).clone();
        let expected = unify::resolve(&self.state.semtys, expected);

        let (ty, effs) = match expr.kind {
            HirExprKind::Record { items } => self.check_record_expr(expr.origin, &items, expected),
            HirExprKind::Variant { name, payload } => {
                self.check_variant_expr(expr.origin, name, payload, expected)
            }
            _ => {
                let (ty, effs) = self.synth_expr(expr_id);
                let ty = self.unify_or_report(expr.origin.span, ty, expected);
                (ty, effs)
            }
        };

        self.record_type(expr_id, ty);
        (ty, effs)
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
                let ty = if yields_unit {
                    self.state.builtins.unit
                } else {
                    last_ty
                };
                (ty, effs)
            }
            HirExprKind::Let {
                mods,
                mutable: _mutable,
                pat,
                has_params,
                params,
                type_params,
                where_: _where,
                effects,
                annot,
                value,
            } => self.synth_let(
                origin,
                mods,
                pat,
                has_params,
                params,
                type_params,
                effects,
                annot,
                value,
            ),
            HirExprKind::Import { exports, .. } => {
                let mut fields = std::collections::BTreeMap::new();
                for sym in exports.iter().copied() {
                    let _prev = fields.insert(sym, self.state.builtins.unknown);
                }
                let ty = self.state.semtys.alloc(SemTy::Record { fields });
                (ty, EffectRow::empty())
            }
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
            HirExprKind::Instance {
                mods,
                type_params,
                where_,
                target,
                members,
            } => {
                let _ = mods;
                let _ = where_;
                self.synth_instance(origin, type_params, target, members)
            }
            HirExprKind::Name { ident } => {
                let ty = self
                    .binding_for_use(ident.span)
                    .and_then(|binding| self.state.env.get_value(binding))
                    .map(|scheme| {
                        self.state
                            .env
                            .instantiate(&mut self.state.semtys, scheme, ident.span)
                    })
                    .unwrap_or(self.state.builtins.unknown);
                (ty, EffectRow::empty())
            }
            HirExprKind::Lit { lit } => {
                let ty = match lit.kind {
                    HirLitKind::Int { .. } => self.state.builtins.int_,
                    HirLitKind::Float { .. } => self.state.builtins.float_,
                    HirLitKind::Rune { .. } => self.state.builtins.int_,
                    HirLitKind::String(_) | HirLitKind::FString { .. } => {
                        self.state.builtins.string_
                    }
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
            HirExprKind::Record { items } => self.synth_record_lit(origin, items),
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
            HirExprKind::Member { base, chain, key } => self.synth_member(origin, base, chain, key),
            HirExprKind::Index { base, indices } => self.synth_index(origin, base, indices),
            HirExprKind::RecordUpdate { base, items } => {
                self.synth_record_update(origin, base, items)
            }
            HirExprKind::TypeTest { expr, ty, alias } => {
                let (_t, effs) = self.synth_expr(expr);
                let ty = self.lower_hir_ty(ty, &HashMap::new());
                if let Some(alias) = alias {
                    if let Some(binding) = self.binding_for_def(alias.span) {
                        self.state.env.insert_value(
                            binding,
                            ValueScheme {
                                generic_count: 0,
                                ty,
                                declared_effects: None,
                            },
                        );
                    }
                }
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
                    music_hir::HirPrefixOp::Not => {
                        if self.is_named_zero_arg(inner, self.state.builtins.int_) {
                            let _ =
                                self.unify_or_report(origin.span, inner, self.state.builtins.int_);
                            self.state.builtins.int_
                        } else {
                            let _ =
                                self.unify_or_report(origin.span, inner, self.state.builtins.bool_);
                            self.state.builtins.bool_
                        }
                    }
                    music_hir::HirPrefixOp::Mut => {
                        self.state.semtys.alloc(SemTy::Mut { base: inner })
                    }
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
            HirExprKind::Quote { .. } => (self.state.builtins.any, EffectRow::empty()),
            HirExprKind::Splice { .. } => {
                self.error(origin.span, SemaErrorKind::SpliceOutsideQuote);
                (self.state.builtins.any, EffectRow::empty())
            }
        };

        self.record_type(expr_id, ty);
        (ty, effs)
    }

    fn synth_record_lit(
        &mut self,
        origin: HirOrigin,
        items: Box<[HirRecordItem]>,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        let mut fields = std::collections::BTreeMap::<Symbol, SemTyId>::new();
        let mut open = false;

        for item in items.iter() {
            match item {
                HirRecordItem::Spread { expr, .. } => {
                    let (t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                    let t = unify::resolve(&self.state.semtys, t);
                    match self.state.semtys.get(t).clone() {
                        SemTy::Record {
                            fields: spread_fields,
                        } => {
                            for (k, v) in spread_fields {
                                let _prev = fields.insert(k, v);
                            }
                        }
                        SemTy::Unknown | SemTy::Any | SemTy::Error => {
                            open = true;
                        }
                        _ => {
                            open = true;
                        }
                    }
                }
                HirRecordItem::Field { name, value, .. } => {
                    let (t, e) = if let Some(expr) = value {
                        self.synth_expr(*expr)
                    } else {
                        let ty = self
                            .binding_for_use(name.span)
                            .and_then(|binding| self.state.env.get_value(binding))
                            .map(|scheme| {
                                self.state.env.instantiate(
                                    &mut self.state.semtys,
                                    scheme,
                                    name.span,
                                )
                            })
                            .unwrap_or(self.state.builtins.unknown);
                        (ty, EffectRow::empty())
                    };
                    effs.union_with(&e);
                    let _prev = fields.insert(name.name, t);
                }
            }
        }

        let ty = if open {
            let _ = origin;
            self.state.builtins.unknown
        } else {
            self.state.semtys.alloc(SemTy::Record { fields })
        };
        (ty, effs)
    }

    fn synth_let(
        &mut self,
        origin: HirOrigin,
        mods: HirDeclMods,
        pat: HirPatId,
        has_params: bool,
        params: Box<[HirParam]>,
        type_params: Box<[music_hir::HirTypeParam]>,
        effects: Option<HirEffectSet>,
        annot: Option<HirTyId>,
        value: Option<HirExprId>,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();

        self.register_lang_items_on_let(&mods, pat, value);

        let mut ty_params = HashMap::<Symbol, u32>::new();
        for (i, tp) in type_params.iter().enumerate() {
            let _prev = ty_params.insert(tp.name.name, u32::try_from(i).unwrap_or(0));
        }

        let declared_effects = effects
            .as_ref()
            .map(|set| self.lower_effect_set(set, &ty_params));

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
                        if self.effect_row_allows(allowed, extra) {
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

            self.bind_pat_to_scheme(
                pat,
                ValueScheme {
                    generic_count: type_params.len().try_into().unwrap_or(0),
                    ty: fn_ty,
                    declared_effects,
                },
            );
            // Function definitions do not evaluate their bodies at binding time.
            return (self.state.builtins.unit, EffectRow::empty());
        }

        // Non-function let.
        if let Some(value) = value {
            let expected = annot.map(|annot| self.lower_hir_ty(annot, &ty_params));
            let (rhs_ty, rhs_effs) = if let Some(expected) = expected {
                self.check_expr(value, expected)
            } else {
                self.synth_expr(value)
            };
            effs.union_with(&rhs_effs);

            // Special-case: effect definitions produce operation tables.
            match self.ctx.store.exprs.get(value).kind.clone() {
                HirExprKind::Effect { members } => {
                    self.register_effect_def(
                        pat,
                        u32::try_from(type_params.len()).ok().unwrap_or(0),
                        &ty_params,
                        &members,
                    );
                }
                HirExprKind::Class { members, .. } => {
                    self.register_class_def(
                        pat,
                        u32::try_from(type_params.len()).ok().unwrap_or(0),
                        &ty_params,
                        &members,
                    );
                }
                HirExprKind::Data { variants, fields } => {
                    self.register_data_def(
                        pat,
                        u32::try_from(type_params.len()).ok().unwrap_or(0),
                        &ty_params,
                        variants.as_deref(),
                        fields.as_deref(),
                    );
                }
                _ => {}
            }

            // Preserve latent effects when binding function values.
            let mut latent = None;
            if let HirExprKind::Name { ident } = self.ctx.store.exprs.get(value).kind.clone() {
                if let Some(binding) = self.binding_for_use(ident.span) {
                    latent = self
                        .state
                        .env
                        .get_value(binding)
                        .and_then(|s| s.declared_effects.clone());
                }
            }
            if latent.is_none() {
                latent = self.state.flow.callable_effs.get(&value).cloned();
            }

            self.bind_pat_to_scheme(
                pat,
                ValueScheme {
                    generic_count: type_params.len().try_into().unwrap_or(0),
                    ty: rhs_ty,
                    declared_effects: latent,
                },
            );
        }

        (self.state.builtins.unit, effs)
    }

    fn synth_instance(
        &mut self,
        origin: HirOrigin,
        type_params: Box<[music_hir::HirTypeParam]>,
        target: HirTyId,
        members: Box<[music_hir::HirMemberDef]>,
    ) -> (SemTyId, EffectRow) {
        let mut ty_params_map = HashMap::<Symbol, u32>::new();
        for (i, tp) in type_params.iter().enumerate() {
            let _prev = ty_params_map.insert(tp.name.name, u32::try_from(i).unwrap_or(0));
        }

        let target_ty = self.ctx.store.tys.get(target).clone();
        let music_hir::HirTyKind::Named {
            name: class_name,
            args: class_args,
        } = target_ty.kind
        else {
            self.error(origin.span, SemaErrorKind::InvalidInstanceTarget);
            return (self.state.builtins.unit, EffectRow::empty());
        };

        let Some(class_binding) = self.binding_for_use(class_name.span) else {
            self.error(
                class_name.span,
                SemaErrorKind::UnknownClass {
                    name: self.ctx.interner.resolve(class_name.name).to_string(),
                },
            );
            return (self.state.builtins.unit, EffectRow::empty());
        };

        let Some(class_family) = self.state.env.get_class_family(class_binding).cloned() else {
            self.error(
                class_name.span,
                SemaErrorKind::UnknownClass {
                    name: self.ctx.interner.resolve(class_name.name).to_string(),
                },
            );
            return (self.state.builtins.unit, EffectRow::empty());
        };

        let arg_tys: Vec<SemTyId> = class_args
            .iter()
            .copied()
            .map(|t| self.lower_hir_ty(t, &ty_params_map))
            .collect();

        match class_family.generic_count {
            0 => {}
            1 if arg_tys.len() == 1 => {}
            other if u32::try_from(arg_tys.len()).ok() != Some(other) => {
                self.error(
                    class_name.span,
                    SemaErrorKind::ClassTypeParamCountUnsupported {
                        count: u32::try_from(arg_tys.len()).ok().unwrap_or(0),
                    },
                );
            }
            _ => {}
        }

        let mut member_by_name: HashMap<Symbol, &music_hir::HirMemberDef> = HashMap::new();
        for member in members.iter() {
            let music_hir::HirMemberDef::Let { name, .. } = member else {
                continue;
            };
            let _prev = member_by_name.insert(name.name.name, member);
        }

        for (op, sig) in class_family.ops.iter() {
            let Some(member) = member_by_name.get(op).copied() else {
                self.error(
                    origin.span,
                    SemaErrorKind::InstanceMissingOp {
                        class: self.ctx.interner.resolve(class_name.name).to_string(),
                        op: self.ctx.interner.resolve(*op).to_string(),
                    },
                );
                continue;
            };

            let music_hir::HirMemberDef::Let {
                origin: member_origin,
                params,
                ret,
                value,
                ..
            } = member
            else {
                continue;
            };

            let subst = arg_tys.clone();
            let expected_params: Vec<_> = sig
                .params
                .iter()
                .copied()
                .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
                .collect();
            let expected_ret = substitute_generics(&mut self.state.semtys, sig.ret, &subst);

            // Bind params using expected types as defaults.
            for (i, p) in params.iter().enumerate() {
                let expected = expected_params
                    .get(i)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown);
                let actual = p
                    .annot
                    .map(|t| {
                        let t = self.lower_hir_ty(t, &ty_params_map);
                        let _ = self.unify_or_report(p.origin.span, t, expected);
                        t
                    })
                    .unwrap_or(expected);

                if let Some(binding) = self.binding_for_def(p.name.span) {
                    self.state.env.insert_value(
                        binding,
                        ValueScheme {
                            generic_count: 0,
                            ty: actual,
                            declared_effects: None,
                        },
                    );
                }

                if let Some(default) = p.default {
                    let _ = self.check_expr(default, actual);
                }
            }

            let actual_ret = ret
                .map(|t| {
                    let t = self.lower_hir_ty(t, &ty_params_map);
                    let _ = self.unify_or_report(member_origin.span, t, expected_ret);
                    t
                })
                .unwrap_or(expected_ret);

            if let Some(body) = *value {
                let _ = self.check_expr(body, actual_ret);
            } else {
                self.error(
                    member_origin.span,
                    SemaErrorKind::InstanceMemberValueRequired,
                );
            }
        }

        for member in members.iter() {
            let music_hir::HirMemberDef::Let { name, .. } = member else {
                continue;
            };
            if !class_family.ops.contains_key(&name.name.name) {
                self.error(
                    name.name.span,
                    SemaErrorKind::UnknownClassOp {
                        class: self.ctx.interner.resolve(class_name.name).to_string(),
                        op: self.ctx.interner.resolve(name.name.name).to_string(),
                    },
                );
            }
        }

        self.state
            .env
            .insert_instance(class_binding, arg_tys.into_boxed_slice());

        (self.state.builtins.unit, EffectRow::empty())
    }

    fn effect_row_allows(&mut self, allowed: &EffectRow, eff: &EffectKey) -> bool {
        for a in allowed.items.iter().filter(|a| a.name == eff.name) {
            match (a.arg, eff.arg) {
                (None, _) => return true,
                (Some(_), None) => continue,
                (Some(a_arg), Some(e_arg)) => {
                    if unify::unify(&mut self.state.semtys, a_arg, e_arg).is_ok() {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn check_record_expr(
        &mut self,
        origin: HirOrigin,
        items: &Box<[HirRecordItem]>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let expected = unify::resolve(&self.state.semtys, expected);
        if let SemTy::Record { fields } = self.state.semtys.get(expected).clone() {
            let mut effs = EffectRow::empty();
            for item in items.iter() {
                match item {
                    HirRecordItem::Spread { expr, .. } => {
                        let (_t, e) = self.synth_expr(*expr);
                        effs.union_with(&e);
                    }
                    HirRecordItem::Field { name, value, .. } => {
                        let expected_field = fields.get(&name.name).copied();
                        let (val_ty, e) = if let Some(expr) = value {
                            if let Some(expected_field) = expected_field {
                                self.check_expr(*expr, expected_field)
                            } else {
                                self.synth_expr(*expr)
                            }
                        } else {
                            let ty = self
                                .binding_for_use(name.span)
                                .and_then(|binding| self.state.env.get_value(binding))
                                .map(|scheme| {
                                    self.state.env.instantiate(
                                        &mut self.state.semtys,
                                        scheme,
                                        name.span,
                                    )
                                })
                                .unwrap_or(self.state.builtins.unknown);
                            (ty, EffectRow::empty())
                        };
                        effs.union_with(&e);
                        if let Some(expected_field) = expected_field {
                            let _ = self.unify_or_report(name.span, val_ty, expected_field);
                        }
                    }
                }
            }
            return (expected, effs);
        }

        let SemTy::Named { name, args } = self.state.semtys.get(expected).clone() else {
            return self.synth_and_unify_record(origin, items, expected);
        };

        let Some(def) = self.state.env.get_data_def(name).cloned() else {
            return self.synth_and_unify_record(origin, items, expected);
        };

        let Some(fields) = def.fields.as_ref() else {
            return self.synth_and_unify_record(origin, items, expected);
        };

        let mut subst = Vec::with_capacity(def.generic_count as usize);
        for i in 0..def.generic_count {
            subst.push(
                args.get(i as usize)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        let mut effs = EffectRow::empty();
        for item in items.iter() {
            match item {
                HirRecordItem::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
                HirRecordItem::Field { name, value, .. } => {
                    let expected_field = fields
                        .get(&name.name)
                        .map(|f| substitute_generics(&mut self.state.semtys, f.ty, &subst));

                    let (val_ty, e) = if let Some(expr) = value {
                        if let Some(expected_field) = expected_field {
                            self.check_expr(*expr, expected_field)
                        } else {
                            self.synth_expr(*expr)
                        }
                    } else {
                        let ty = self
                            .binding_for_use(name.span)
                            .and_then(|binding| self.state.env.get_value(binding))
                            .map(|scheme| {
                                self.state.env.instantiate(
                                    &mut self.state.semtys,
                                    scheme,
                                    name.span,
                                )
                            })
                            .unwrap_or(self.state.builtins.unknown);
                        (ty, EffectRow::empty())
                    };
                    effs.union_with(&e);

                    if let Some(expected_field) = expected_field {
                        let _ = self.unify_or_report(name.span, val_ty, expected_field);
                    }
                }
            }
        }

        (expected, effs)
    }

    fn synth_and_unify_record(
        &mut self,
        origin: HirOrigin,
        items: &Box<[HirRecordItem]>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
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
        let ty = self.unify_or_report(origin.span, self.state.builtins.unknown, expected);
        (ty, effs)
    }

    fn check_variant_expr(
        &mut self,
        origin: HirOrigin,
        name: music_names::Ident,
        payload: Option<HirExprId>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let expected = unify::resolve(&self.state.semtys, expected);
        let SemTy::Named {
            name: ty_name,
            args,
        } = self.state.semtys.get(expected).clone()
        else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        let Some(def) = self.state.env.get_data_def(ty_name).cloned() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };
        let Some(variants) = def.variants.as_ref() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        let Some(payload_ty) = variants.get(&name.name).copied() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        let mut subst = Vec::with_capacity(def.generic_count as usize);
        for i in 0..def.generic_count {
            subst.push(
                args.get(i as usize)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        let mut effs = EffectRow::empty();
        match (payload_ty, payload) {
            (None, None) => {}
            (None, Some(payload)) => {
                let (_t, e) = self.synth_expr(payload);
                effs.union_with(&e);
            }
            (Some(payload_ty), Some(payload)) => {
                let payload_expected =
                    substitute_generics(&mut self.state.semtys, payload_ty, &subst);
                let (_t, e) = self.check_expr(payload, payload_expected);
                effs.union_with(&e);
            }
            (Some(_), None) => {}
        };

        (expected, effs)
    }

    fn synth_and_unify_variant(
        &mut self,
        origin: HirOrigin,
        payload: Option<HirExprId>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        if let Some(payload) = payload {
            let (_t, e) = self.synth_expr(payload);
            effs.union_with(&e);
        }
        let ty = self.unify_or_report(origin.span, self.state.builtins.unknown, expected);
        (ty, effs)
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

    fn synth_member(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        chain: music_hir::HirChainKind,
        key: HirMemberKey,
    ) -> (SemTyId, EffectRow) {
        let (base_ty, effs) = self.synth_expr(base);
        let base_ty = unify::resolve(&self.state.semtys, base_ty);

        match (chain, self.state.semtys.get(base_ty).clone(), key) {
            (music_hir::HirChainKind::Optional, base, key) => {
                self.synth_optional_member(origin, base, key, effs)
            }
            (music_hir::HirChainKind::Forced, base, key) => {
                self.synth_forced_member(origin, base, key, effs)
            }
            (music_hir::HirChainKind::Normal, SemTy::Tuple { .. }, HirMemberKey::IntLit { .. }) => {
                (self.state.builtins.unknown, effs)
            }
            (
                music_hir::HirChainKind::Normal,
                SemTy::Record { fields },
                HirMemberKey::Name(field),
            ) => (
                fields
                    .get(&field.name)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
                effs,
            ),
            (
                music_hir::HirChainKind::Normal,
                SemTy::Named { name, args },
                HirMemberKey::Name(field),
            ) => {
                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return (self.state.builtins.unknown, effs);
                };
                let Some(fields) = def.fields.as_ref() else {
                    return (self.state.builtins.unknown, effs);
                };
                let Some(field_def) = fields.get(&field.name) else {
                    return (self.state.builtins.unknown, effs);
                };

                let mut subst = Vec::with_capacity(def.generic_count as usize);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(i as usize)
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                let ty = substitute_generics(&mut self.state.semtys, field_def.ty, &subst);
                (ty, effs)
            }
            _ => (self.state.builtins.unknown, effs),
        }
    }

    fn synth_optional_member(
        &mut self,
        origin: HirOrigin,
        base: SemTy,
        key: HirMemberKey,
        effs: EffectRow,
    ) -> (SemTyId, EffectRow) {
        let Some(option_name) = self.state.lang.option_ty else {
            self.error(origin.span, SemaErrorKind::OptionLangItemRequired);
            return (self.state.builtins.unknown, effs);
        };

        let payload_ty = match base {
            SemTy::Named { name, args } if name == option_name && args.len() == 1 => args[0],
            SemTy::Unknown | SemTy::Any | SemTy::Error => {
                return (self.state.builtins.unknown, effs);
            }
            _ => {
                self.error(origin.span, SemaErrorKind::OptionalChainRequiresOption);
                return (self.state.builtins.unknown, effs);
            }
        };

        let field_ty = self.type_member(payload_ty, &key);
        let ty = self.state.semtys.alloc(SemTy::Named {
            name: option_name,
            args: Box::new([field_ty]),
        });
        (ty, effs)
    }

    fn synth_forced_member(
        &mut self,
        origin: HirOrigin,
        base: SemTy,
        key: HirMemberKey,
        effs: EffectRow,
    ) -> (SemTyId, EffectRow) {
        let Some(option_name) = self.state.lang.option_ty else {
            self.error(origin.span, SemaErrorKind::OptionLangItemRequired);
            return (self.state.builtins.unknown, effs);
        };

        let payload_ty = match base {
            SemTy::Named { name, args } if name == option_name && args.len() == 1 => args[0],
            SemTy::Unknown | SemTy::Any | SemTy::Error => {
                return (self.state.builtins.unknown, effs);
            }
            _ => {
                self.error(origin.span, SemaErrorKind::ForcedChainRequiresOption);
                return (self.state.builtins.unknown, effs);
            }
        };

        let field_ty = self.type_member(payload_ty, &key);
        let mut effs = effs;
        effs.add(EffectKey {
            name: self.state.known.abort,
            arg: None,
        });
        (field_ty, effs)
    }

    fn type_member(&mut self, ty: SemTyId, key: &HirMemberKey) -> SemTyId {
        let ty = unify::resolve(&self.state.semtys, ty);
        match (self.state.semtys.get(ty).clone(), key) {
            (SemTy::Record { fields }, HirMemberKey::Name(field)) => fields
                .get(&field.name)
                .copied()
                .unwrap_or(self.state.builtins.unknown),
            (SemTy::Named { name, args }, HirMemberKey::Name(field)) => {
                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return self.state.builtins.unknown;
                };
                let Some(fields) = def.fields.as_ref() else {
                    return self.state.builtins.unknown;
                };
                let Some(field_def) = fields.get(&field.name) else {
                    return self.state.builtins.unknown;
                };

                let mut subst = Vec::with_capacity(def.generic_count as usize);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(i as usize)
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                substitute_generics(&mut self.state.semtys, field_def.ty, &subst)
            }
            _ => self.state.builtins.unknown,
        }
    }

    fn synth_index(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        indices: Box<[HirExprId]>,
    ) -> (SemTyId, EffectRow) {
        let (base_ty, mut effs) = self.synth_expr(base);
        for idx in indices.iter().copied() {
            let (idx_ty, e) = self.synth_expr(idx);
            effs.union_with(&e);
            let _ = self.unify_or_report(origin.span, idx_ty, self.state.builtins.int_);
        }

        let base_ty = unify::resolve(&self.state.semtys, base_ty);
        match self.state.semtys.get(base_ty).clone() {
            SemTy::Array { elem, .. } => (elem, effs),
            _ => (self.state.builtins.unknown, effs),
        }
    }

    fn synth_record_update(
        &mut self,
        _origin: HirOrigin,
        base: HirExprId,
        items: Box<[HirRecordItem]>,
    ) -> (SemTyId, EffectRow) {
        let (base_ty, mut effs) = self.synth_expr(base);
        let base_resolved = unify::resolve(&self.state.semtys, base_ty);
        if let SemTy::Record { fields } = self.state.semtys.get(base_resolved).clone() {
            let mut out_fields = fields;
            let mut open = false;

            for item in items.iter() {
                match item {
                    HirRecordItem::Spread { expr, .. } => {
                        let (t, e) = self.synth_expr(*expr);
                        effs.union_with(&e);
                        let t = unify::resolve(&self.state.semtys, t);
                        match self.state.semtys.get(t).clone() {
                            SemTy::Record {
                                fields: spread_fields,
                            } => {
                                for (k, v) in spread_fields {
                                    let _prev = out_fields.insert(k, v);
                                }
                            }
                            SemTy::Unknown | SemTy::Any | SemTy::Error => open = true,
                            _ => open = true,
                        }
                    }
                    HirRecordItem::Field {
                        name: field, value, ..
                    } => {
                        let expected = out_fields.get(&field.name).copied();
                        let (t, e) = if let Some(expr) = value {
                            if let Some(expected) = expected {
                                self.check_expr(*expr, expected)
                            } else {
                                self.synth_expr(*expr)
                            }
                        } else {
                            let ty = self
                                .binding_for_use(field.span)
                                .and_then(|binding| self.state.env.get_value(binding))
                                .map(|scheme| {
                                    self.state.env.instantiate(
                                        &mut self.state.semtys,
                                        scheme,
                                        field.span,
                                    )
                                })
                                .unwrap_or(self.state.builtins.unknown);
                            (ty, EffectRow::empty())
                        };
                        effs.union_with(&e);
                        let _prev = out_fields.insert(field.name, t);
                    }
                }
            }

            let ty = if open {
                self.state.builtins.unknown
            } else {
                self.state
                    .semtys
                    .alloc(SemTy::Record { fields: out_fields })
            };
            return (ty, effs);
        }

        let SemTy::Named { name, args } = self.state.semtys.get(base_resolved).clone() else {
            // Fallback: evaluate item expressions for errors/effects, no structural typing.
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
            return (self.state.builtins.unknown, effs);
        };

        let Some(def) = self.state.env.get_data_def(name).cloned() else {
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
            return (self.state.builtins.unknown, effs);
        };

        let Some(fields) = def.fields.as_ref() else {
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
            return (self.state.builtins.unknown, effs);
        };

        let mut subst = Vec::with_capacity(def.generic_count as usize);
        for i in 0..def.generic_count {
            subst.push(
                args.get(i as usize)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        for item in items.iter() {
            match item {
                HirRecordItem::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
                HirRecordItem::Field {
                    name: field, value, ..
                } => {
                    let expected_field = fields
                        .get(&field.name)
                        .map(|f| substitute_generics(&mut self.state.semtys, f.ty, &subst));

                    let (val_ty, e) = if let Some(expr) = value {
                        if let Some(expected_field) = expected_field {
                            self.check_expr(*expr, expected_field)
                        } else {
                            self.synth_expr(*expr)
                        }
                    } else {
                        let ty = self
                            .binding_for_use(field.span)
                            .and_then(|binding| self.state.env.get_value(binding))
                            .map(|scheme| {
                                self.state.env.instantiate(
                                    &mut self.state.semtys,
                                    scheme,
                                    field.span,
                                )
                            })
                            .unwrap_or(self.state.builtins.unknown);
                        (ty, EffectRow::empty())
                    };
                    effs.union_with(&e);

                    if let Some(expected_field) = expected_field {
                        let _ = self.unify_or_report(field.span, val_ty, expected_field);
                    }
                }
            }
        }

        (base_ty, effs)
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
            HirBinaryOp::Or | HirBinaryOp::Xor | HirBinaryOp::And => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);

                let ty = if self.is_named_zero_arg(l_ty, self.state.builtins.int_)
                    || self.is_named_zero_arg(r_ty, self.state.builtins.int_)
                {
                    let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.int_);
                    let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.int_);
                    self.state.builtins.int_
                } else {
                    let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.bool_);
                    let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.bool_);
                    self.state.builtins.bool_
                };
                (ty, effs)
            }
            HirBinaryOp::Eq
            | HirBinaryOp::NotEq
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::LtEq
            | HirBinaryOp::GtEq => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                let _ = self.unify_or_report(origin.span, l_ty, r_ty);
                (self.state.builtins.bool_, effs)
            }
            HirBinaryOp::Shl | HirBinaryOp::Shr => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.int_);
                let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.int_);
                (self.state.builtins.int_, effs)
            }
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Mod => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                if self.is_named_zero_arg(l_ty, self.state.builtins.float_)
                    || self.is_named_zero_arg(r_ty, self.state.builtins.float_)
                {
                    let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.float_);
                    let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.float_);
                    (self.state.builtins.float_, effs)
                } else {
                    let _ = self.unify_or_report(origin.span, l_ty, self.state.builtins.int_);
                    let _ = self.unify_or_report(origin.span, r_ty, self.state.builtins.int_);
                    (self.state.builtins.int_, effs)
                }
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
                let (t, e) = self.synth_expr(guard);
                effs.union_with(&e);
                let _ = self.unify_or_report(origin.span, t, self.state.builtins.bool_);
            }

            let (body_ty, body_effs) = self.synth_expr(arm.body);
            effs.union_with(&body_effs);
            let _ = self.unify_or_report(origin.span, body_ty, result);
        }

        (result, effs)
    }

    fn is_named_zero_arg(&self, ty: SemTyId, builtin: SemTyId) -> bool {
        let builtin = match self.state.semtys.get(builtin) {
            SemTy::Named { name, args } if args.is_empty() => *name,
            _ => return false,
        };
        let ty = unify::resolve(&self.state.semtys, ty);
        matches!(
            self.state.semtys.get(ty),
            SemTy::Named { name, args } if *name == builtin && args.is_empty()
        )
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
                    super::env::EffectOpSig {
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
            arg: eff_arg,
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

        let handler_family = self.state.env.get_effect_family(handler_binding).cloned();
        if handler_family.is_none() {
            self.error(
                handler.span,
                SemaErrorKind::UnknownEffect {
                    name: self.ctx.interner.resolve(handler.name).to_string(),
                },
            );
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
                                    super::env::EffectOpSig {
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
            let _ = self.unify_or_report(origin.span, body_ty, result);
        }

        if value_clause_count != 1 {
            self.error(origin.span, SemaErrorKind::HandleValueClauseRequired);
        }

        handled_effs.remove_by_name(handler.name);
        handled_effs.union_with(&clause_effs);
        (result, handled_effs)
    }

    fn synth_resume(
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
            let _ = self.unify_or_report(origin.span, t, ctx.arg);
        } else {
            let _ = self.unify_or_report(origin.span, self.state.builtins.unit, ctx.arg);
        }

        (ctx.result, effs)
    }
}
