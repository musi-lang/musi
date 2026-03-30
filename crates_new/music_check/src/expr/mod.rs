use std::collections::{BTreeMap, HashMap};

use music_basic::{Span, int_lit};
use music_hir::{
    HirArg, HirArrayItem, HirArrowFlavor, HirBinaryOp, HirCaseArm, HirChainKind, HirDeclMods,
    HirDim, HirEffectSet, HirExprId, HirExprKind, HirFStringPart, HirLitKind, HirMemberDef,
    HirMemberKey, HirOrigin, HirParam, HirPatId, HirPrefixOp, HirRecordItem, HirTyId, HirTyKind,
    HirTyParam,
};
use music_names::{Ident, NameBindingId, Symbol, SymbolSlice};

use crate::SemaErrorKind;

use super::unify;
use super::{
    EffectKey, EffectRow, SemTy, SemTyId,
    env::{ClassFamily, ValueScheme, generalize_infer_vars, substitute_generics},
    ty::SemTyIds,
};

use super::checker::Checker;

mod assign;
mod effects;
mod quote;
mod variant;

#[derive(Clone, Copy)]
struct LetExpr<'a> {
    origin: HirOrigin,
    mods: &'a HirDeclMods,
    mutable: bool,
    pat: HirPatId,
    has_params: bool,
    params: &'a [HirParam],
    ty_params: &'a [HirTyParam],
    effects: Option<&'a HirEffectSet>,
    annot: Option<HirTyId>,
    value: Option<HirExprId>,
}

impl Checker<'_> {
    fn union_call_effects(&self, callee: HirExprId, effs: &mut EffectRow) {
        // Named bindings may carry declared `with { ... }` effects (or inferred effects when
        // aliases are created via `let x := f;` and `let x := (..) => ...;`).
        if let HirExprKind::Named { ident } = self.ctx.store.exprs.get(callee).kind.clone() {
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

    pub(crate) fn check_expr(
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
                let ty = self.unify_or_report(expr.origin.span, expected, ty);
                (ty, effs)
            }
        };

        self.record_type(expr_id, ty);
        (ty, effs)
    }

    pub(crate) fn synth_expr(&mut self, expr_id: HirExprId) -> (SemTyId, EffectRow) {
        let expr = self.ctx.store.exprs.get(expr_id).clone();
        let origin = expr.origin;

        let (ty, effs) = match expr.kind {
            HirExprKind::Error => (self.state.builtins.error, EffectRow::empty()),
            HirExprKind::Sequence { exprs, yields_unit } => {
                self.synth_sequence(&exprs, yields_unit)
            }
            HirExprKind::Let {
                mods,
                mutable,
                pat,
                has_params,
                params,
                ty_params,
                where_: _where,
                effects,
                annot,
                value,
            } => self.synth_let(LetExpr {
                origin,
                mods: &mods,
                mutable,
                pat,
                has_params,
                params: &params,
                ty_params: &ty_params,
                effects: effects.as_ref(),
                annot,
                value,
            }),
            HirExprKind::Import { exports, .. } => self.synth_import(&exports),
            HirExprKind::ForeignBlock { items, .. } => self.synth_foreign_block(&items),
            HirExprKind::Data { .. } | HirExprKind::Effect { .. } | HirExprKind::Class { .. } => {
                (self.state.builtins.type_, EffectRow::empty())
            }
            HirExprKind::Instance {
                mods,
                ty_params,
                where_,
                target,
                members,
            } => {
                let _ = mods;
                let _ = where_;
                self.synth_instance(origin, &ty_params, target, &members)
            }
            HirExprKind::Named { ident } => self.synth_named(ident),
            HirExprKind::Lit { lit } => self.synth_lit(lit.kind),
            HirExprKind::Tuple { items } => self.synth_tuple(origin, &items),
            HirExprKind::Array { items } => self.synth_array(origin, &items),
            HirExprKind::Record { items } => self.synth_record_lit(origin, &items),
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
                let (fn_ty, latent) = self.synth_lambda(origin, &params, ret, body);
                let _prev = self.state.flow.callable_effs.insert(expr_id, latent);
                (fn_ty, EffectRow::empty())
            }
            HirExprKind::Call { callee, args } => self.synth_call(origin, callee, &args),
            HirExprKind::Member { base, chain, key } => self.synth_member(origin, base, chain, key),
            HirExprKind::Index { base, indices } => self.synth_index(origin, base, &indices),
            HirExprKind::RecordUpdate { base, items } => {
                self.synth_record_update(origin, base, &items)
            }
            HirExprKind::TypeTest { expr, ty, alias } => {
                self.synth_type_test(origin, expr, ty, alias)
            }
            HirExprKind::TypeCast { expr, ty } => self.synth_type_cast(expr, ty),
            HirExprKind::Prefix { op, expr } => self.synth_prefix(origin, op, expr),
            HirExprKind::Binary { op, left, right } => self.synth_binary(origin, op, left, right),
            HirExprKind::Case { scrut, arms } => self.synth_case(origin, scrut, &arms),
            HirExprKind::Perform { expr } => self.synth_perform(origin, expr),
            HirExprKind::Handle {
                expr,
                handler,
                clauses,
            } => self.synth_handle(origin, expr, handler, &clauses),
            HirExprKind::Resume { value } => self.synth_resume(origin, value),
            HirExprKind::Quote { splices, .. } => self.synth_quote(origin, &splices),
            HirExprKind::Splice { .. } => (self.state.builtins.syntax, EffectRow::empty()),
        };

        self.record_type(expr_id, ty);
        (ty, effs)
    }

    fn synth_sequence(&mut self, exprs: &[HirExprId], yields_unit: bool) -> (SemTyId, EffectRow) {
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

    fn synth_import(&mut self, exports: &SymbolSlice) -> (SemTyId, EffectRow) {
        let mut fields = BTreeMap::new();
        for sym in exports.iter().copied() {
            let _prev = fields.insert(sym, self.state.builtins.unknown);
        }
        let ty = self.state.semtys.alloc(SemTy::Record { fields });
        (ty, EffectRow::empty())
    }

    fn synth_foreign_block(&mut self, items: &[HirExprId]) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        for id in items.iter().copied() {
            let (_t, e) = self.synth_expr(id);
            effs.union_with(&e);
        }
        (self.state.builtins.unit, effs)
    }

    fn synth_named(&mut self, ident: Ident) -> (SemTyId, EffectRow) {
        let ty = self
            .binding_for_use(ident.span)
            .and_then(|binding| self.state.env.get_value(binding))
            .map_or(self.state.builtins.unknown, |scheme| {
                scheme.instantiate(&mut self.state.semtys, ident.span)
            });
        (ty, EffectRow::empty())
    }

    fn synth_lit(&mut self, kind: HirLitKind) -> (SemTyId, EffectRow) {
        match kind {
            HirLitKind::Float { .. } => (self.state.builtins.float_, EffectRow::empty()),
            HirLitKind::Int { .. } | HirLitKind::Rune { .. } => {
                (self.state.builtins.int_, EffectRow::empty())
            }
            HirLitKind::String(_) => (self.state.builtins.string_, EffectRow::empty()),
            HirLitKind::FString { parts, .. } => {
                let mut effs = EffectRow::empty();
                for part in &parts {
                    match part {
                        HirFStringPart::Literal { .. } => {}
                        HirFStringPart::Expr { expr, origin } => {
                            let (t, e) = self.synth_expr(*expr);
                            effs.union_with(&e);
                            let _ =
                                self.unify_or_report(origin.span, self.state.builtins.string_, t);
                        }
                    }
                }
                (self.state.builtins.string_, effs)
            }
        }
    }

    fn synth_tuple(&mut self, origin: HirOrigin, items: &[HirExprId]) -> (SemTyId, EffectRow) {
        if items.is_empty() {
            return (self.state.builtins.unit, EffectRow::empty());
        }

        let mut effs = EffectRow::empty();
        let mut tys = Vec::with_capacity(items.len());
        for item in items.iter().copied() {
            let (t, e) = self.synth_expr(item);
            tys.push(t);
            effs.union_with(&e);
        }
        let _ = origin;
        let ty = self.state.semtys.alloc(SemTy::Tuple {
            items: tys.into_boxed_slice(),
        });
        (ty, effs)
    }

    fn synth_array(&mut self, origin: HirOrigin, items: &[HirArrayItem]) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        let elem = self.state.semtys.fresh_infer_var(origin.span);
        for item in items {
            match item {
                HirArrayItem::Expr(id) => {
                    let (t, e) = self.synth_expr(*id);
                    effs.union_with(&e);
                    let _ = self.unify_or_report(origin.span, elem, t);
                }
                HirArrayItem::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
            }
        }
        let ty = self.state.semtys.alloc(SemTy::Array {
            dims: Box::new([HirDim::Inferred { span: origin.span }]),
            elem,
        });
        (ty, effs)
    }

    fn synth_type_test(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        ty: HirTyId,
        alias: Option<Ident>,
    ) -> (SemTyId, EffectRow) {
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
        let _ = origin;
        (self.state.builtins.bool_, effs)
    }

    fn synth_type_cast(&mut self, expr: HirExprId, ty: HirTyId) -> (SemTyId, EffectRow) {
        let (_t, effs) = self.synth_expr(expr);
        let ty = self.lower_hir_ty(ty, &HashMap::new());
        (ty, effs)
    }

    fn synth_prefix(
        &mut self,
        origin: HirOrigin,
        op: HirPrefixOp,
        expr: HirExprId,
    ) -> (SemTyId, EffectRow) {
        let (inner, effs) = self.synth_expr(expr);
        let ty = match op {
            HirPrefixOp::Negate => {
                let _ = self.unify_or_report(origin.span, self.state.builtins.int_, inner);
                self.state.builtins.int_
            }
            HirPrefixOp::Not => {
                if self.is_named_zero_arg(inner, self.state.builtins.int_) {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, inner);
                    self.state.builtins.int_
                } else {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.bool_, inner);
                    self.state.builtins.bool_
                }
            }
            HirPrefixOp::Mut => self.state.semtys.alloc(SemTy::Mut { base: inner }),
        };
        (ty, effs)
    }

    fn synth_record_lit(
        &mut self,
        origin: HirOrigin,
        items: &[HirRecordItem],
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        let mut fields = BTreeMap::<Symbol, SemTyId>::new();
        let mut open = false;

        for item in items {
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
                        _ => open = true,
                    }
                }
                HirRecordItem::Field { name, value, .. } => {
                    let (t, e) = if let Some(expr) = value {
                        self.synth_expr(*expr)
                    } else {
                        let ty = self
                            .binding_for_use(name.span)
                            .and_then(|binding| self.state.env.get_value(binding))
                            .map_or(self.state.builtins.unknown, |scheme| {
                                scheme.instantiate(&mut self.state.semtys, name.span)
                            });
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

    fn synth_let(&mut self, let_: LetExpr<'_>) -> (SemTyId, EffectRow) {
        self.register_lang_items_on_let(let_.mods, let_.pat, let_.value);
        self.mark_pat_bindings_mut(let_.pat, let_.mutable);

        let ty_params_map = ty_params_map(let_.ty_params);
        let declared_effects = let_
            .effects
            .map(|set| self.lower_effect_set(set, &ty_params_map));

        if let_.has_params {
            return self.synth_fn_let(&let_, &ty_params_map, declared_effects);
        }

        self.synth_value_let(&let_, &ty_params_map)
    }

    fn synth_fn_let(
        &mut self,
        let_: &LetExpr<'_>,
        ty_params_map: &HashMap<Symbol, u32>,
        declared_effects: Option<EffectRow>,
    ) -> (SemTyId, EffectRow) {
        let (fn_ty, body_effs) = self.check_callable(
            let_.origin,
            let_.params,
            ty_params_map,
            let_.annot,
            let_.value,
            declared_effects.as_ref(),
        );

        self.check_declared_effects(let_.origin.span, declared_effects.as_ref(), &body_effs);

        let base_generic_count = u32::try_from(let_.ty_params.len()).unwrap_or(0);
        let (extra_generic_count, fn_ty) =
            generalize_infer_vars(&mut self.state.semtys, fn_ty, base_generic_count);

        let scheme = ValueScheme {
            generic_count: base_generic_count + extra_generic_count,
            ty: fn_ty,
            declared_effects,
        };
        self.bind_pat_to_scheme(let_.pat, &scheme);

        // Function definitions do not evaluate their bodies at binding time.
        (self.state.builtins.unit, EffectRow::empty())
    }

    fn check_declared_effects(
        &mut self,
        span: Span,
        declared: Option<&EffectRow>,
        actual: &EffectRow,
    ) {
        let Some(declared) = declared else {
            if !actual.is_pure() {
                self.error(span, SemaErrorKind::MissingWithClause);
            }
            return;
        };

        if declared.is_open {
            return;
        }

        for extra in &actual.items {
            if self.effect_row_allows(declared, extra) {
                continue;
            }
            self.error(
                span,
                SemaErrorKind::EffectNotDeclared {
                    name: self.ctx.interner.resolve(extra.name).to_owned(),
                },
            );
        }
        if actual.is_open {
            self.error(span, SemaErrorKind::EffectRemainderNotDeclared);
        }
    }

    fn synth_value_let(
        &mut self,
        let_: &LetExpr<'_>,
        ty_params_map: &HashMap<Symbol, u32>,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();

        let Some(value) = let_.value else {
            return (self.state.builtins.unit, effs);
        };

        let expected = let_
            .annot
            .map(|annot| self.lower_hir_ty(annot, ty_params_map));
        let (rhs_ty, rhs_effs) = if let Some(expected) = expected {
            self.check_expr(value, expected)
        } else {
            self.synth_expr(value)
        };
        effs.union_with(&rhs_effs);

        self.register_def_on_value(let_, ty_params_map, value);

        let latent = self.latent_effects_from_value(let_, value);

        let base_generic_count = u32::try_from(let_.ty_params.len()).unwrap_or(0);
        let (scheme_generic_count, scheme_ty) =
            self.generalize_value_let(let_, base_generic_count, rhs_ty, rhs_effs.is_pure(), value);

        let scheme = ValueScheme {
            generic_count: scheme_generic_count,
            ty: scheme_ty,
            declared_effects: latent,
        };
        self.bind_pat_to_scheme(let_.pat, &scheme);

        (self.state.builtins.unit, effs)
    }

    fn register_def_on_value(
        &mut self,
        let_: &LetExpr<'_>,
        ty_params_map: &HashMap<Symbol, u32>,
        value: HirExprId,
    ) {
        let generic_count = u32::try_from(let_.ty_params.len()).unwrap_or(0);
        match self.ctx.store.exprs.get(value).kind.clone() {
            HirExprKind::Effect { members } => {
                self.register_effect_def(let_.pat, generic_count, ty_params_map, members.as_ref());
            }
            HirExprKind::Class { members, .. } => {
                self.register_class_def(let_.pat, generic_count, ty_params_map, members.as_ref());
            }
            HirExprKind::Data { variants, fields } => {
                self.register_data_def(
                    let_.pat,
                    generic_count,
                    ty_params_map,
                    variants.as_deref(),
                    fields.as_deref(),
                );
            }
            _ => {}
        }
    }

    fn latent_effects_from_value(&self, let_: &LetExpr<'_>, value: HirExprId) -> Option<EffectRow> {
        let _ = let_;
        if let HirExprKind::Named { ident } = self.ctx.store.exprs.get(value).kind.clone() {
            if let Some(binding) = self.binding_for_use(ident.span) {
                return self
                    .state
                    .env
                    .get_value(binding)
                    .and_then(|s| s.declared_effects.clone());
            }
        }
        self.state.flow.callable_effs.get(&value).cloned()
    }

    fn generalize_value_let(
        &mut self,
        let_: &LetExpr<'_>,
        base_generic_count: u32,
        rhs_ty: SemTyId,
        rhs_is_pure: bool,
        value: HirExprId,
    ) -> (u32, SemTyId) {
        if let_.annot.is_none()
            && let_.ty_params.is_empty()
            && matches!(
                self.ctx.store.exprs.get(value).kind,
                HirExprKind::Named { .. }
            )
        {
            return self.forward_scheme_from_rhs(value, base_generic_count, rhs_ty);
        }

        if rhs_is_pure {
            let (extra, generalized) =
                generalize_infer_vars(&mut self.state.semtys, rhs_ty, base_generic_count);
            return (base_generic_count + extra, generalized);
        }

        (base_generic_count, rhs_ty)
    }

    fn forward_scheme_from_rhs(
        &self,
        value: HirExprId,
        base_generic_count: u32,
        rhs_ty: SemTyId,
    ) -> (u32, SemTyId) {
        let HirExprKind::Named { ident } = self.ctx.store.exprs.get(value).kind.clone() else {
            return (base_generic_count, rhs_ty);
        };
        let Some(binding) = self.binding_for_use(ident.span) else {
            return (base_generic_count, rhs_ty);
        };
        let Some(scheme) = self.state.env.get_value(binding).cloned() else {
            return (base_generic_count, rhs_ty);
        };
        (scheme.generic_count, scheme.ty)
    }

    fn synth_instance(
        &mut self,
        origin: HirOrigin,
        ty_params: &[HirTyParam],
        target: HirTyId,
        members: &[HirMemberDef],
    ) -> (SemTyId, EffectRow) {
        let ty_params_map = ty_params_map(ty_params);
        let Some((class_name, class_binding, class_family, arg_tys)) =
            self.instance_target(origin, target, &ty_params_map)
        else {
            return (self.state.builtins.unit, EffectRow::empty());
        };

        self.check_instance_arg_count(class_name, &class_family, &arg_tys);

        let member_by_name = Self::instance_members_by_name(members);
        self.check_instance_ops(
            origin,
            class_name,
            &class_family,
            &arg_tys,
            &ty_params_map,
            &member_by_name,
        );
        self.check_unknown_instance_ops(class_name, &class_family, members);

        self.state
            .env
            .insert_instance(class_binding, arg_tys.into_boxed_slice());
        (self.state.builtins.unit, EffectRow::empty())
    }

    fn instance_target(
        &mut self,
        origin: HirOrigin,
        target: HirTyId,
        ty_params_map: &HashMap<Symbol, u32>,
    ) -> Option<(Ident, NameBindingId, ClassFamily, Vec<SemTyId>)> {
        let target_ty = self.ctx.store.tys.get(target).clone();
        let HirTyKind::Named {
            name: class_name,
            args: class_args,
        } = target_ty.kind
        else {
            self.error(origin.span, SemaErrorKind::InvalidInstanceTarget);
            return None;
        };

        let Some(class_binding) = self.binding_for_use(class_name.span) else {
            self.error(
                class_name.span,
                SemaErrorKind::UnknownClass {
                    name: self.ctx.interner.resolve(class_name.name).to_owned(),
                },
            );
            return None;
        };

        let Some(class_family) = self.state.env.get_class_family(class_binding).cloned() else {
            self.error(
                class_name.span,
                SemaErrorKind::UnknownClass {
                    name: self.ctx.interner.resolve(class_name.name).to_owned(),
                },
            );
            return None;
        };

        let arg_tys: Vec<SemTyId> = class_args
            .iter()
            .copied()
            .map(|t| self.lower_hir_ty(t, ty_params_map))
            .collect();

        Some((class_name, class_binding, class_family, arg_tys))
    }

    fn check_instance_arg_count(
        &mut self,
        class_name: Ident,
        family: &ClassFamily,
        arg_tys: &[SemTyId],
    ) {
        match family.generic_count {
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
    }

    fn instance_members_by_name(members: &[HirMemberDef]) -> HashMap<Symbol, &HirMemberDef> {
        let mut out = HashMap::new();
        for member in members {
            let HirMemberDef::Let { name, .. } = member else {
                continue;
            };
            let _prev = out.insert(name.name.name, member);
        }
        out
    }

    fn check_instance_ops(
        &mut self,
        origin: HirOrigin,
        class_name: Ident,
        family: &ClassFamily,
        arg_tys: &[SemTyId],
        ty_params_map: &HashMap<Symbol, u32>,
        member_by_name: &HashMap<Symbol, &HirMemberDef>,
    ) {
        for (op, sig) in &family.ops {
            let Some(member) = member_by_name.get(op).copied() else {
                self.error(
                    origin.span,
                    SemaErrorKind::InstanceMissingOp {
                        class: self.ctx.interner.resolve(class_name.name).to_owned(),
                        op: self.ctx.interner.resolve(*op).to_owned(),
                    },
                );
                continue;
            };

            let HirMemberDef::Let {
                origin: member_origin,
                params,
                ret,
                value,
                ..
            } = member
            else {
                continue;
            };

            let subst = arg_tys.to_vec();
            let expected_params: Vec<_> = sig
                .params
                .iter()
                .copied()
                .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
                .collect();
            let expected_ret = substitute_generics(&mut self.state.semtys, sig.ret, &subst);

            self.bind_instance_params(params, &expected_params, ty_params_map);

            let actual_ret = ret
                .map(|t| {
                    let t = self.lower_hir_ty(t, ty_params_map);
                    let _ = self.unify_or_report(member_origin.span, expected_ret, t);
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
    }

    fn bind_instance_params(
        &mut self,
        params: &[HirParam],
        expected_params: &[SemTyId],
        ty_params_map: &HashMap<Symbol, u32>,
    ) {
        for (i, p) in params.iter().enumerate() {
            let expected = expected_params
                .get(i)
                .copied()
                .unwrap_or(self.state.builtins.unknown);
            let actual = p.annot.map_or(expected, |t| {
                let t = self.lower_hir_ty(t, ty_params_map);
                let _ = self.unify_or_report(p.origin.span, expected, t);
                t
            });

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
    }

    fn check_unknown_instance_ops(
        &mut self,
        class_name: Ident,
        family: &ClassFamily,
        members: &[HirMemberDef],
    ) {
        for member in members {
            let HirMemberDef::Let { name, .. } = member else {
                continue;
            };
            if !family.ops.contains_key(&name.name.name) {
                self.error(
                    name.name.span,
                    SemaErrorKind::UnknownClassOp {
                        class: self.ctx.interner.resolve(class_name.name).to_owned(),
                        op: self.ctx.interner.resolve(name.name.name).to_owned(),
                    },
                );
            }
        }
    }

    fn effect_row_allows(&mut self, allowed: &EffectRow, eff: &EffectKey) -> bool {
        for a in allowed.items.iter().filter(|a| a.name == eff.name) {
            match (a.arg, eff.arg) {
                (None, _) => return true,
                (Some(_), None) => {}
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
        items: &[HirRecordItem],
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let expected = unify::resolve(&self.state.semtys, expected);
        if let SemTy::Record { fields } = self.state.semtys.get(expected).clone() {
            let mut effs = EffectRow::empty();
            for item in items {
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
                                .map_or(self.state.builtins.unknown, |scheme| {
                                    scheme.instantiate(&mut self.state.semtys, name.span)
                                });
                            (ty, EffectRow::empty())
                        };
                        effs.union_with(&e);
                        if let Some(expected_field) = expected_field {
                            let _ = self.unify_or_report(name.span, expected_field, val_ty);
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

        let cap = usize::try_from(def.generic_count).unwrap_or(0);
        let mut subst = Vec::with_capacity(cap);
        for i in 0..def.generic_count {
            subst.push(
                args.get(usize::try_from(i).unwrap_or(usize::MAX))
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        let mut effs = EffectRow::empty();
        for item in items {
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
                            .map_or(self.state.builtins.unknown, |scheme| {
                                scheme.instantiate(&mut self.state.semtys, name.span)
                            });
                        (ty, EffectRow::empty())
                    };
                    effs.union_with(&e);

                    if let Some(expected_field) = expected_field {
                        let _ = self.unify_or_report(name.span, expected_field, val_ty);
                    }
                }
            }
        }

        (expected, effs)
    }

    fn synth_and_unify_record(
        &mut self,
        origin: HirOrigin,
        items: &[HirRecordItem],
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        for item in items {
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
        let ty = self.unify_or_report(origin.span, expected, self.state.builtins.unknown);
        (ty, effs)
    }

    fn synth_call(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: &[HirArg],
    ) -> (SemTyId, EffectRow) {
        let (callee_ty, mut effs) = self.synth_expr(callee);

        let mut arg_tys = vec![];
        for arg in args {
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

        let _ = self.unify_or_report(origin.span, expect_fn, callee_ty);

        self.union_call_effects(callee, &mut effs);

        (out, effs)
    }

    fn synth_member(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        chain: HirChainKind,
        key: HirMemberKey,
    ) -> (SemTyId, EffectRow) {
        let (mut base_ty, effs) = self.synth_expr(base);
        base_ty = unify::resolve(&self.state.semtys, base_ty);
        loop {
            let SemTy::Mut { base } = self.state.semtys.get(base_ty).clone() else {
                break;
            };
            base_ty = unify::resolve(&self.state.semtys, base);
        }

        match (chain, self.state.semtys.get(base_ty).clone(), key) {
            (HirChainKind::Optional, base, key) => {
                self.synth_optional_member(origin, base, &key, effs)
            }
            (HirChainKind::Forced, base, key) => self.synth_forced_member(origin, base, &key, effs),
            (HirChainKind::Normal, SemTy::Tuple { items }, HirMemberKey::IntLit { span, .. }) => {
                let idx = parse_int_lit_u32(self.slice(span));
                let len = u32::try_from(items.len()).unwrap_or(0);
                let ty = idx
                    .and_then(|i| usize::try_from(i).ok())
                    .and_then(|i| items.get(i).copied());
                if let Some(ty) = ty {
                    (ty, effs)
                } else {
                    self.error(
                        span,
                        SemaErrorKind::TupleIndexOutOfRange {
                            index: idx.unwrap_or(0),
                            len,
                        },
                    );
                    (self.state.builtins.error, effs)
                }
            }
            (HirChainKind::Normal, SemTy::Record { fields }, HirMemberKey::Name(field)) => (
                fields
                    .get(&field.name)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
                effs,
            ),
            (HirChainKind::Normal, SemTy::Named { name, args }, HirMemberKey::Name(field)) => {
                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return (self.state.builtins.unknown, effs);
                };
                let Some(fields) = def.fields.as_ref() else {
                    return (self.state.builtins.unknown, effs);
                };
                let Some(field_def) = fields.get(&field.name) else {
                    return (self.state.builtins.unknown, effs);
                };

                let cap = usize::try_from(def.generic_count).unwrap_or(0);
                let mut subst = Vec::with_capacity(cap);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(usize::try_from(i).unwrap_or(usize::MAX))
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
        key: &HirMemberKey,
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

        let field_ty = self.ty_member(payload_ty, key);
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
        key: &HirMemberKey,
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

        let field_ty = self.ty_member(payload_ty, key);
        let mut effs = effs;
        effs.add(EffectKey {
            name: self.state.known.abort,
            arg: None,
        });
        (field_ty, effs)
    }

    fn ty_member(&mut self, ty: SemTyId, key: &HirMemberKey) -> SemTyId {
        let ty = unify::resolve(&self.state.semtys, ty);
        match (self.state.semtys.get(ty).clone(), key) {
            (SemTy::Mut { base }, key) => self.ty_member(base, key),
            (SemTy::Record { fields }, HirMemberKey::Name(field)) => fields
                .get(&field.name)
                .copied()
                .unwrap_or(self.state.builtins.unknown),
            (SemTy::Named { name, args }, HirMemberKey::Name(field)) => {
                if self.error_if_opaque_repr_access(field.span, name) {
                    return self.state.builtins.unknown;
                }

                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return self.state.builtins.unknown;
                };
                let Some(fields) = def.fields.as_ref() else {
                    return self.state.builtins.unknown;
                };
                let Some(field_def) = fields.get(&field.name) else {
                    return self.state.builtins.unknown;
                };

                let cap = usize::try_from(def.generic_count).unwrap_or(0);
                let mut subst = Vec::with_capacity(cap);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(usize::try_from(i).unwrap_or(usize::MAX))
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                substitute_generics(&mut self.state.semtys, field_def.ty, &subst)
            }
            _ => self.state.builtins.unknown,
        }
    }

    fn project_indices(
        &mut self,
        origin: HirOrigin,
        mut ty: SemTyId,
        indices: &[HirExprId],
        effs: &mut EffectRow,
    ) -> SemTyId {
        let mut array_chain = false;
        for idx_expr in indices.iter().copied() {
            let (idx_ty, idx_effs) = self.synth_expr(idx_expr);
            effs.union_with(&idx_effs);
            match self.project_one_index(origin, ty, idx_expr, idx_ty, &mut array_chain) {
                Ok(next) => ty = next,
                Err(err) => return err,
            }
        }
        ty
    }

    fn project_one_index(
        &mut self,
        origin: HirOrigin,
        ty: SemTyId,
        idx_expr: HirExprId,
        idx_ty: SemTyId,
        array_chain: &mut bool,
    ) -> Result<SemTyId, SemTyId> {
        let resolved = self.strip_mut_chain(ty);
        match self.state.semtys.get(resolved).clone() {
            SemTy::Array { dims, elem } => {
                *array_chain = true;
                let _ = self.unify_or_report(origin.span, self.state.builtins.int_, idx_ty);

                if dims.len() == 1 {
                    Ok(elem)
                } else {
                    let remaining = dims[1..].to_vec().into_boxed_slice();
                    Ok(self.state.semtys.alloc(SemTy::Array {
                        dims: remaining,
                        elem,
                    }))
                }
            }
            SemTy::Tuple { items } => {
                self.error_if_array_chain(origin, *array_chain)?;
                if let Some(i) = int_lit_expr_u32(self, idx_expr) {
                    let Some(item) = usize::try_from(i).ok().and_then(|i| items.get(i).copied())
                    else {
                        self.error(
                            origin.span,
                            SemaErrorKind::TupleIndexOutOfRange {
                                index: i,
                                len: u32::try_from(items.len()).unwrap_or(0),
                            },
                        );
                        return Err(self.state.builtins.error);
                    };
                    Ok(item)
                } else {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, idx_ty);
                    Ok(self.state.builtins.unknown)
                }
            }
            SemTy::Record { fields } => {
                self.error_if_array_chain(origin, *array_chain)?;
                if let Some(name) = string_lit_expr_text(self, idx_expr) {
                    let sym = self.ctx.interner.intern(&name);
                    if let Some(field_ty) = fields.get(&sym).copied() {
                        Ok(field_ty)
                    } else {
                        self.error(origin.span, SemaErrorKind::FieldNotFound { name });
                        Err(self.state.builtins.error)
                    }
                } else {
                    Ok(self.state.builtins.unknown)
                }
            }
            SemTy::Named { name, args } => {
                self.error_if_array_chain(origin, *array_chain)?;
                if self.error_if_opaque_repr_access(origin.span, name) {
                    return Ok(self.state.builtins.unknown);
                }
                let Some(field_name) = string_lit_expr_text(self, idx_expr) else {
                    return Ok(self.state.builtins.unknown);
                };
                let field_sym = self.ctx.interner.intern(&field_name);

                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return Ok(self.state.builtins.unknown);
                };
                let Some(fields) = def.fields.as_ref() else {
                    return Ok(self.state.builtins.unknown);
                };
                let Some(field_def) = fields.get(&field_sym) else {
                    self.error(
                        origin.span,
                        SemaErrorKind::FieldNotFound { name: field_name },
                    );
                    return Err(self.state.builtins.error);
                };

                let cap = usize::try_from(def.generic_count).unwrap_or(0);
                let mut subst = Vec::with_capacity(cap);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(usize::try_from(i).unwrap_or(usize::MAX))
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                Ok(substitute_generics(
                    &mut self.state.semtys,
                    field_def.ty,
                    &subst,
                ))
            }
            SemTy::Unknown | SemTy::Any | SemTy::Error => Ok(self.state.builtins.unknown),
            _ => {
                self.error_if_array_chain(origin, *array_chain)?;
                Ok(self.state.builtins.unknown)
            }
        }
    }

    fn strip_mut_chain(&self, ty: SemTyId) -> SemTyId {
        let mut resolved = unify::resolve(&self.state.semtys, ty);
        loop {
            let SemTy::Mut { base } = self.state.semtys.get(resolved).clone() else {
                return resolved;
            };
            resolved = unify::resolve(&self.state.semtys, base);
        }
    }

    fn error_if_array_chain(
        &mut self,
        origin: HirOrigin,
        array_chain: bool,
    ) -> Result<(), SemTyId> {
        if array_chain {
            self.error(origin.span, SemaErrorKind::IndexExceedsArrayNesting);
            return Err(self.state.builtins.error);
        }
        Ok(())
    }

    fn synth_index(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        indices: &[HirExprId],
    ) -> (SemTyId, EffectRow) {
        let (ty, mut effs) = self.synth_expr(base);
        let ty = self.project_indices(origin, ty, indices, &mut effs);
        (ty, effs)
    }

    fn synth_record_update(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        items: &[HirRecordItem],
    ) -> (SemTyId, EffectRow) {
        let (base_ty, mut effs) = self.synth_expr(base);
        let base_resolved = self.strip_mut_chain(base_ty);
        match self.state.semtys.get(base_resolved).clone() {
            SemTy::Record { fields } => {
                let ty = self.synth_record_update_structural(fields, items, &mut effs);
                (ty, effs)
            }
            SemTy::Named { name, args } => {
                let ty =
                    self.synth_record_update_named(origin, base_ty, name, &args, items, &mut effs);
                (ty, effs)
            }
            _ => {
                self.synth_record_update_item_exprs(items, &mut effs);
                (self.state.builtins.unknown, effs)
            }
        }
    }

    fn synth_record_update_structural(
        &mut self,
        mut out_fields: BTreeMap<Symbol, SemTyId>,
        items: &[HirRecordItem],
        effs: &mut EffectRow,
    ) -> SemTyId {
        let mut open = false;

        for item in items {
            match item {
                HirRecordItem::Spread { expr, .. } => {
                    let (t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                    let t = unify::resolve(&self.state.semtys, t);
                    match self.state.semtys.get(t).clone() {
                        SemTy::Record { fields } => {
                            for (k, v) in fields {
                                let _prev = out_fields.insert(k, v);
                            }
                        }
                        _ => open = true,
                    }
                }
                HirRecordItem::Field { name, value, .. } => {
                    let expected = out_fields.get(&name.name).copied();
                    let (t, e) = if let Some(expr) = value {
                        if let Some(expected) = expected {
                            self.check_expr(*expr, expected)
                        } else {
                            self.synth_expr(*expr)
                        }
                    } else {
                        (self.record_shorthand_value(*name), EffectRow::empty())
                    };
                    effs.union_with(&e);
                    let _prev = out_fields.insert(name.name, t);
                }
            }
        }

        if open {
            self.state.builtins.unknown
        } else {
            self.state
                .semtys
                .alloc(SemTy::Record { fields: out_fields })
        }
    }

    fn synth_record_update_named(
        &mut self,
        origin: HirOrigin,
        base_ty: SemTyId,
        name: Symbol,
        args: &SemTyIds,
        items: &[HirRecordItem],
        effs: &mut EffectRow,
    ) -> SemTyId {
        if self.error_if_opaque_repr_access(origin.span, name) {
            self.synth_record_update_item_exprs(items, effs);
            return self.state.builtins.unknown;
        }

        let Some(def) = self.state.env.get_data_def(name).cloned() else {
            self.synth_record_update_item_exprs(items, effs);
            return self.state.builtins.unknown;
        };
        let Some(fields) = def.fields.as_ref() else {
            self.synth_record_update_item_exprs(items, effs);
            return self.state.builtins.unknown;
        };

        let cap = usize::try_from(def.generic_count).unwrap_or(0);
        let mut subst = Vec::with_capacity(cap);
        for i in 0..def.generic_count {
            subst.push(
                args.get(usize::try_from(i).unwrap_or(usize::MAX))
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        for item in items {
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
                        (self.record_shorthand_value(*name), EffectRow::empty())
                    };
                    effs.union_with(&e);

                    if let Some(expected_field) = expected_field {
                        let _ = self.unify_or_report(name.span, expected_field, val_ty);
                    }
                }
            }
        }

        base_ty
    }

    fn synth_record_update_item_exprs(&mut self, items: &[HirRecordItem], effs: &mut EffectRow) {
        for item in items {
            match item {
                HirRecordItem::Field { value: None, .. } => {}
                HirRecordItem::Field {
                    value: Some(expr), ..
                }
                | HirRecordItem::Spread { expr, .. } => {
                    let (_t, e) = self.synth_expr(*expr);
                    effs.union_with(&e);
                }
            }
        }
    }

    fn record_shorthand_value(&mut self, name: Ident) -> SemTyId {
        self.binding_for_use(name.span)
            .and_then(|binding| self.state.env.get_value(binding))
            .map_or(self.state.builtins.unknown, |scheme| {
                scheme.instantiate(&mut self.state.semtys, name.span)
            })
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
                let _ = self.unify_or_report(origin.span, expect_fn, r_ty);
                self.union_call_effects(right, &mut effs);
                (out, effs)
            }
            HirBinaryOp::Assign => self.synth_assign(origin, left, right),
            HirBinaryOp::Or | HirBinaryOp::Xor | HirBinaryOp::And => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);

                let ty = if self.is_named_zero_arg(l_ty, self.state.builtins.int_)
                    || self.is_named_zero_arg(r_ty, self.state.builtins.int_)
                {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, l_ty);
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, r_ty);
                    self.state.builtins.int_
                } else {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.bool_, l_ty);
                    let _ = self.unify_or_report(origin.span, self.state.builtins.bool_, r_ty);
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
                let _ = self.unify_read_or_report(origin.span, l_ty, r_ty);
                (self.state.builtins.bool_, effs)
            }
            HirBinaryOp::Shl | HirBinaryOp::Shr => {
                let (l_ty, mut effs) = self.synth_expr(left);
                let (r_ty, r_effs) = self.synth_expr(right);
                effs.union_with(&r_effs);
                let _ = self.unify_or_report(origin.span, self.state.builtins.int_, l_ty);
                let _ = self.unify_or_report(origin.span, self.state.builtins.int_, r_ty);
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
                    let _ = self.unify_or_report(origin.span, self.state.builtins.float_, l_ty);
                    let _ = self.unify_or_report(origin.span, self.state.builtins.float_, r_ty);
                    (self.state.builtins.float_, effs)
                } else {
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, l_ty);
                    let _ = self.unify_or_report(origin.span, self.state.builtins.int_, r_ty);
                    (self.state.builtins.int_, effs)
                }
            }
            HirBinaryOp::Symbolic(op_ident) => {
                self.synth_symbolic_infix(origin, op_ident, left, right)
            }
        }
    }

    fn synth_symbolic_infix(
        &mut self,
        origin: HirOrigin,
        op_ident: Ident,
        left: HirExprId,
        right: HirExprId,
    ) -> (SemTyId, EffectRow) {
        let (l_ty, mut effs) = self.synth_expr(left);
        let (r_ty, r_effs) = self.synth_expr(right);
        effs.union_with(&r_effs);

        let mut declared = None::<EffectRow>;
        let callee_ty = self
            .binding_for_use(op_ident.span)
            .and_then(|binding| self.state.env.get_value(binding))
            .map_or(self.state.builtins.unknown, |scheme| {
                declared.clone_from(&scheme.declared_effects);
                scheme.instantiate(&mut self.state.semtys, op_ident.span)
            });

        if let Some(declared) = declared.as_ref() {
            effs.union_with(declared);
        }

        let input = self.state.semtys.alloc(SemTy::Tuple {
            items: Box::new([l_ty, r_ty]),
        });
        let out = self.state.semtys.fresh_infer_var(origin.span);
        let expect_fn = self.state.semtys.alloc(SemTy::Arrow {
            flavor: HirArrowFlavor::Effectful,
            input,
            output: out,
        });
        let _ = self.unify_or_report(origin.span, expect_fn, callee_ty);

        (out, effs)
    }

    fn synth_case(
        &mut self,
        origin: HirOrigin,
        scrut: HirExprId,
        arms: &[HirCaseArm],
    ) -> (SemTyId, EffectRow) {
        let (scrut_ty, mut effs) = self.synth_expr(scrut);
        let result = self.state.semtys.fresh_infer_var(origin.span);

        for arm in arms {
            self.bind_pat_for_scrut(arm.pat, scrut_ty);

            if let Some(guard) = arm.guard {
                let (t, e) = self.synth_expr(guard);
                effs.union_with(&e);
                let _ = self.unify_or_report(origin.span, self.state.builtins.bool_, t);
            }

            let (body_ty, body_effs) = self.synth_expr(arm.body);
            effs.union_with(&body_effs);
            let _ = self.unify_or_report(origin.span, result, body_ty);
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
}

fn ty_params_map(ty_params: &[HirTyParam]) -> HashMap<Symbol, u32> {
    let mut out = HashMap::<Symbol, u32>::new();
    for (i, tp) in ty_params.iter().enumerate() {
        let _prev = out.insert(tp.name.name, u32::try_from(i).unwrap_or(0));
    }
    out
}

fn int_lit_expr_u32(checker: &Checker<'_>, expr_id: HirExprId) -> Option<u32> {
    match checker.ctx.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => match lit.kind {
            HirLitKind::Int { span, .. } => parse_int_lit_u32(checker.slice(span)),
            _ => None,
        },
        _ => None,
    }
}

fn string_lit_expr_text(checker: &Checker<'_>, expr_id: HirExprId) -> Option<String> {
    match checker.ctx.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => match lit.kind {
            HirLitKind::String(s) => Some(checker.decode_string_span(s.span)),
            _ => None,
        },
        _ => None,
    }
}

fn parse_int_lit_u32(text: &str) -> Option<u32> {
    let v = int_lit::parse_u64(text)?;
    u32::try_from(v).ok()
}
