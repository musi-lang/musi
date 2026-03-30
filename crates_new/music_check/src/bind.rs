use std::collections::{BTreeMap, HashMap};

use music_basic::Span;
use music_hir::{
    HirFieldDef, HirLitKind, HirMemberDef, HirPat, HirPatId, HirPatKind, HirRecordPatField,
    HirVariantDef,
};
use music_names::{Ident, Symbol};

use crate::SemaErrorKind;

use super::checker::Checker;
use super::env::{
    ClassOpSig, DataDef, DataFieldDef, EffectOpSig, ValueScheme, substitute_generics,
};
use super::ty::SemTyIds;
use super::unify;
use super::{SemTy, SemTyId};

#[derive(Debug, Clone, Copy)]
enum VariantPayloadTy {
    Unknown,
    NoPayload,
    Payload(SemTyId),
}

impl Checker<'_> {
    pub(crate) fn bind_pat_to_scheme(&mut self, pat: HirPatId, scheme: &ValueScheme) {
        let mut stack = vec![pat];
        while let Some(pat_id) = stack.pop() {
            let pat = self.ctx.store.pats.get(pat_id).clone();
            match pat.kind {
                HirPatKind::Bind { name, sub } => {
                    if let Some(binding) = self.binding_for_def(name.span) {
                        self.state.env.insert_value(binding, scheme.clone());
                    }
                    if let Some(sub) = sub {
                        stack.push(sub);
                    }
                }
                HirPatKind::Variant { args, .. }
                | HirPatKind::Tuple { items: args }
                | HirPatKind::Array { items: args }
                | HirPatKind::Or { alts: args } => stack.extend(args.iter().copied()),
                HirPatKind::Record { fields } => {
                    for f in &fields {
                        if f.sub.is_none() {
                            if let Some(binding) = self.binding_for_def(f.name.span) {
                                self.state.env.insert_value(binding, scheme.clone());
                            }
                        }
                        if let Some(sub) = f.sub {
                            stack.push(sub);
                        }
                    }
                }
                HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
            }
        }
    }

    pub(crate) fn bind_pat_for_scrut(&mut self, pat: HirPatId, scrut_ty: SemTyId) {
        let scrut_ty = self.strip_mut_wrappers(scrut_ty);
        let pat = self.ctx.store.pats.get(pat).clone();
        self.bind_pat_for_scrut_kind(pat, scrut_ty);
    }

    fn strip_mut_wrappers(&self, ty: SemTyId) -> SemTyId {
        let mut ty = unify::resolve(&self.state.semtys, ty);
        loop {
            let SemTy::Mut { base } = self.state.semtys.get(ty).clone() else {
                return ty;
            };
            ty = unify::resolve(&self.state.semtys, base);
        }
    }

    fn bind_pat_for_scrut_kind(&mut self, pat: HirPat, scrut_ty: SemTyId) {
        match pat.kind {
            HirPatKind::Bind { name, sub } => self.bind_bind_pat(name, sub, scrut_ty),
            HirPatKind::Lit { lit } => self.bind_lit_pat(pat.origin.span, &lit.kind, scrut_ty),
            HirPatKind::Tuple { items } => self.bind_tuple_pat(pat.origin.span, &items, scrut_ty),
            HirPatKind::Array { items } => self.bind_array_pat(pat.origin.span, &items, scrut_ty),
            HirPatKind::Record { fields } => {
                self.bind_record_pat(pat.origin.span, &fields, scrut_ty);
            }
            HirPatKind::Or { alts } => self.bind_or_pat(&alts, scrut_ty),
            HirPatKind::Variant { name, args } => {
                self.bind_variant_pat(pat.origin.span, name, &args, scrut_ty);
            }
            HirPatKind::Wildcard | HirPatKind::Error => {}
        }
    }

    fn bind_bind_pat(&mut self, name: Ident, sub: Option<HirPatId>, scrut_ty: SemTyId) {
        if let Some(binding) = self.binding_for_def(name.span) {
            self.state.env.insert_value(
                binding,
                ValueScheme {
                    generic_count: 0,
                    ty: scrut_ty,
                    declared_effects: None,
                },
            );
        }
        if let Some(sub) = sub {
            self.bind_pat_for_scrut(sub, scrut_ty);
        }
    }

    fn bind_lit_pat(&mut self, span: Span, kind: &HirLitKind, scrut_ty: SemTyId) {
        let expected = match kind {
            HirLitKind::Int { .. } | HirLitKind::Rune { .. } => self.state.builtins.int_,
            HirLitKind::Float { .. } => self.state.builtins.float_,
            HirLitKind::String(_) | HirLitKind::FString { .. } => self.state.builtins.string_,
        };
        let _ = self.unify_or_report(span, expected, scrut_ty);
    }

    fn bind_tuple_pat(&mut self, span: Span, items: &[HirPatId], scrut_ty: SemTyId) {
        match self.state.semtys.get(scrut_ty).clone() {
            SemTy::Tuple { items: tys } => {
                if tys.len() != items.len() {
                    self.error(
                        span,
                        SemaErrorKind::TuplePatternArityMismatch {
                            expected: u32::try_from(tys.len()).unwrap_or(0),
                            found: u32::try_from(items.len()).unwrap_or(0),
                        },
                    );
                }

                for (i, p) in items.iter().copied().enumerate() {
                    let t = tys.get(i).copied().unwrap_or(self.state.builtins.unknown);
                    self.bind_pat_for_scrut(p, t);
                }
            }
            SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => {
                for p in items.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
            }
            _ => {
                self.error(span, SemaErrorKind::TuplePatternRequiresTuple);
                for p in items.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
            }
        }
    }

    fn bind_array_pat(&mut self, span: Span, items: &[HirPatId], scrut_ty: SemTyId) {
        let elem = match self.state.semtys.get(scrut_ty).clone() {
            SemTy::Array { elem, .. } => elem,
            SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => {
                self.state.builtins.unknown
            }
            _ => {
                self.error(span, SemaErrorKind::ArrayPatternRequiresArray);
                self.state.builtins.unknown
            }
        };
        for p in items.iter().copied() {
            self.bind_pat_for_scrut(p, elem);
        }
    }

    fn bind_record_pat(&mut self, span: Span, fields: &[HirRecordPatField], scrut_ty: SemTyId) {
        let record_def = self.record_pat_fields(span, scrut_ty);

        for f in fields {
            let field_ty = record_def
                .as_ref()
                .and_then(|fields| fields.get(&f.name.name).copied());

            if record_def.is_some() && field_ty.is_none() {
                self.error(
                    f.name.span,
                    SemaErrorKind::FieldNotFound {
                        name: self.ctx.interner.resolve(f.name.name).to_owned(),
                    },
                );
            }

            if f.mutable {
                if let Some(sub) = f.sub {
                    self.mark_pat_bindings_mut(sub, true);
                } else if let Some(binding) = self.binding_for_def(f.name.span) {
                    self.mark_binding_mut(binding, true);
                }
            }

            if f.sub.is_none() {
                if let Some(binding) = self.binding_for_def(f.name.span) {
                    self.state.env.insert_value(
                        binding,
                        ValueScheme {
                            generic_count: 0,
                            ty: field_ty.unwrap_or(self.state.builtins.unknown),
                            declared_effects: None,
                        },
                    );
                }
            }

            if let Some(sub) = f.sub {
                self.bind_pat_for_scrut(sub, field_ty.unwrap_or(self.state.builtins.unknown));
            }
        }
    }

    fn record_pat_fields(
        &mut self,
        span: Span,
        scrut_ty: SemTyId,
    ) -> Option<BTreeMap<Symbol, SemTyId>> {
        match self.state.semtys.get(scrut_ty).clone() {
            SemTy::Record { fields } => Some(fields),
            SemTy::Named { name, args } => {
                if self.error_if_opaque_repr_access(span, name) {
                    return None;
                }
                let def = self.state.env.get_data_def(name).cloned();
                def.and_then(|def| {
                    let field_defs = def.fields?;
                    let generic_count = def.generic_count;

                    let mut out = BTreeMap::new();
                    for (k, v) in field_defs {
                        let mut subst =
                            Vec::with_capacity(usize::try_from(generic_count).unwrap_or(0));
                        for i in 0..generic_count {
                            subst.push(
                                args.get(usize::try_from(i).unwrap_or(0))
                                    .copied()
                                    .unwrap_or(self.state.builtins.unknown),
                            );
                        }
                        let ty = substitute_generics(&mut self.state.semtys, v.ty, &subst);
                        let _prev = out.insert(k, ty);
                    }
                    Some(out)
                })
                .or_else(|| {
                    self.error(span, SemaErrorKind::RecordPatternRequiresRecord);
                    None
                })
            }
            SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => None,
            _ => {
                self.error(span, SemaErrorKind::RecordPatternRequiresRecord);
                None
            }
        }
    }

    fn bind_or_pat(&mut self, alts: &[HirPatId], scrut_ty: SemTyId) {
        for p in alts.iter().copied() {
            self.bind_pat_for_scrut(p, scrut_ty);
        }
    }

    fn bind_variant_pat(&mut self, span: Span, name: Ident, args: &[HirPatId], scrut_ty: SemTyId) {
        let (payload_ty, generic_count, ty_args) = self.variant_payload_ty(span, name, scrut_ty);

        let payload_ty = match payload_ty {
            VariantPayloadTy::Unknown => {
                for p in args.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
                return;
            }
            VariantPayloadTy::NoPayload => None,
            VariantPayloadTy::Payload(payload_ty) => {
                let mut subst = Vec::with_capacity(usize::try_from(generic_count).unwrap_or(0));
                for i in 0..generic_count {
                    subst.push(
                        ty_args
                            .get(usize::try_from(i).unwrap_or(0))
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                Some(substitute_generics(
                    &mut self.state.semtys,
                    payload_ty,
                    &subst,
                ))
            }
        };

        self.bind_variant_payload_args(name, args, payload_ty);
    }

    fn variant_payload_ty(
        &mut self,
        span: Span,
        name: Ident,
        scrut_ty: SemTyId,
    ) -> (VariantPayloadTy, u32, SemTyIds) {
        let mut payload_ty = VariantPayloadTy::Unknown;
        let mut generic_count = 0u32;
        let mut ty_args: SemTyIds = Box::new([]);

        match self.state.semtys.get(scrut_ty).clone() {
            SemTy::Named {
                name: ty_name,
                args,
            } => {
                if self.error_if_opaque_repr_access(span, ty_name) {
                    return (payload_ty, generic_count, ty_args);
                }

                if let Some(def) = self.state.env.get_data_def(ty_name).cloned() {
                    generic_count = def.generic_count;
                    ty_args = args;
                    if let Some(variants) = def.variants.as_ref() {
                        let Some(found) = variants.get(&name.name).copied() else {
                            self.error(
                                name.span,
                                SemaErrorKind::VariantNotFound {
                                    name: self.ctx.interner.resolve(name.name).to_owned(),
                                },
                            );
                            return (payload_ty, generic_count, ty_args);
                        };

                        payload_ty = found.map_or(VariantPayloadTy::NoPayload, |ty| {
                            VariantPayloadTy::Payload(ty)
                        });
                    } else {
                        self.error(span, SemaErrorKind::VariantPatternRequiresData);
                    }
                }
            }
            SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => {}
            _ => {
                self.error(span, SemaErrorKind::VariantPatternRequiresData);
            }
        }

        (payload_ty, generic_count, ty_args)
    }

    fn bind_variant_payload_args(
        &mut self,
        name: Ident,
        args: &[HirPatId],
        payload_ty: Option<SemTyId>,
    ) {
        match payload_ty {
            None => {
                if !args.is_empty() {
                    self.error(
                        name.span,
                        SemaErrorKind::VariantPatternArgCountMismatch {
                            variant: self.ctx.interner.resolve(name.name).to_owned(),
                            expected: "0".to_owned(),
                            found: u32::try_from(args.len()).unwrap_or(0),
                        },
                    );
                }
            }
            Some(payload_ty) => {
                let resolved = unify::resolve(&self.state.semtys, payload_ty);
                let tuple_items = match self.state.semtys.get(resolved).clone() {
                    SemTy::Tuple { items } => Some(items),
                    _ => None,
                };

                let found = u32::try_from(args.len()).unwrap_or(0);
                let ok = if args.len() == 1 {
                    self.bind_pat_for_scrut(args[0], payload_ty);
                    true
                } else if let Some(items) = tuple_items.as_ref() {
                    if items.len() == args.len() {
                        for (p, t) in args.iter().copied().zip(items.iter().copied()) {
                            self.bind_pat_for_scrut(p, t);
                        }
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                if ok {
                    return;
                }

                let expected = tuple_items
                    .as_ref()
                    .map_or_else(|| "1".to_owned(), |items| format!("1 or {}", items.len()));

                self.error(
                    name.span,
                    SemaErrorKind::VariantPatternArgCountMismatch {
                        variant: self.ctx.interner.resolve(name.name).to_owned(),
                        expected,
                        found,
                    },
                );
                for p in args.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
            }
        }
    }

    pub(crate) fn bind_value_clause_binder(&mut self, binder: Ident, ty: SemTyId) {
        if let Some(binding) = self.binding_for_def(binder.span) {
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

    pub(crate) fn bind_handle_param(&mut self, param: Ident, ty: SemTyId) {
        if let Some(binding) = self.binding_for_def(param.span) {
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

    pub(crate) fn register_effect_def(
        &mut self,
        pat: HirPatId,
        generic_count: u32,
        ty_params: &HashMap<Symbol, u32>,
        members: &[HirMemberDef],
    ) {
        // Only supports `let EffectName := effect { ... }` bindings for now.
        let mut bind_sites = vec![];
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };

        let mut ops = HashMap::<Symbol, EffectOpSig>::new();
        for member in members {
            let HirMemberDef::Let {
                name: callable_name,
                params,
                ret,
                ..
            } = member
            else {
                continue;
            };
            let mut param_tys = vec![];
            for p in params {
                let ty = p.annot.map_or(self.state.builtins.unknown, |t| {
                    self.lower_hir_ty(t, ty_params)
                });
                param_tys.push(ty);
            }
            let ret = ret
                .map(|t| self.lower_hir_ty(t, ty_params))
                .unwrap_or(self.state.builtins.unknown);
            let _prev = ops.insert(
                callable_name.name.name,
                EffectOpSig {
                    params: param_tys.into_boxed_slice(),
                    ret,
                },
            );
        }
        self.state
            .env
            .insert_effect_family(binding, generic_count, ops);
    }

    pub(crate) fn register_class_def(
        &mut self,
        pat: HirPatId,
        generic_count: u32,
        ty_params: &HashMap<Symbol, u32>,
        members: &[HirMemberDef],
    ) {
        // Only supports `let ClassName := class { ... }` bindings for now.
        let mut bind_sites = vec![];
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };

        let mut ops = HashMap::<Symbol, ClassOpSig>::new();
        for member in members {
            let HirMemberDef::Let {
                name,
                params,
                ret,
                value,
                ..
            } = member
            else {
                continue;
            };

            let mut param_tys = vec![];
            for p in params {
                let ty = p.annot.map_or(self.state.builtins.unknown, |t| {
                    self.lower_hir_ty(t, ty_params)
                });
                param_tys.push(ty);
            }
            let ret_ty = ret
                .map(|t| self.lower_hir_ty(t, ty_params))
                .unwrap_or(self.state.builtins.unknown);

            let _prev = ops.insert(
                name.name.name,
                ClassOpSig {
                    params: param_tys.clone().into_boxed_slice(),
                    ret: ret_ty,
                },
            );

            // Default bodies are validated for type correctness but do not contribute effects at
            // definition site.
            if let Some(body) = *value {
                for (p, p_ty) in params.iter().zip(param_tys.iter().copied()) {
                    if let Some(binding) = self.binding_for_def(p.name.span) {
                        self.state.env.insert_value(
                            binding,
                            ValueScheme {
                                generic_count: 0,
                                ty: p_ty,
                                declared_effects: None,
                            },
                        );
                    }
                }
                for (p, p_ty) in params.iter().zip(param_tys.iter().copied()) {
                    if let Some(default) = p.default {
                        let _ = self.check_expr(default, p_ty);
                    }
                }
                let _ = self.check_expr(body, ret_ty);
            }
        }

        self.state
            .env
            .insert_class_family(binding, generic_count, ops);
    }

    pub(crate) fn register_data_def(
        &mut self,
        pat: HirPatId,
        generic_count: u32,
        ty_params: &HashMap<Symbol, u32>,
        variants: Option<&[HirVariantDef]>,
        fields: Option<&[HirFieldDef]>,
    ) {
        // Only supports `let TypeName := data { ... }` bindings for now.
        let mut bind_sites = vec![];
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };
        let ty_name = self.ctx.names.bindings[binding].name;

        let variants_map = variants.map(|variants| {
            let mut out = HashMap::<Symbol, Option<SemTyId>>::new();
            for v in variants {
                let payload_ty = v.payload_ty.map(|t| self.lower_hir_ty(t, ty_params));
                let _prev = out.insert(v.name.name, payload_ty);

                if let Some(value) = v.value {
                    if let Some(payload_ty) = payload_ty {
                        let _ = self.check_expr(value, payload_ty);
                    } else {
                        let _ = self.synth_expr(value);
                    }
                }
            }
            out
        });

        let fields_map = fields.map(|fields| {
            let mut out = HashMap::<Symbol, DataFieldDef>::new();
            for f in fields {
                let ty = self.lower_hir_ty(f.ty, ty_params);
                let _prev = out.insert(f.name.name, DataFieldDef { ty });

                if let Some(value) = f.value {
                    let _ = self.check_expr(value, ty);
                }
            }
            out
        });

        self.state.env.insert_data_def(
            ty_name,
            DataDef {
                generic_count,
                variants: variants_map,
                fields: fields_map,
            },
        );

        self.validate_option_lang_item(ty_name);
    }

    fn validate_option_lang_item(&mut self, ty_name: Symbol) {
        if self.state.lang.option_ty != Some(ty_name) {
            return;
        }

        let span = self.state.lang.option_span.unwrap_or(Span::DUMMY);

        let Some(def) = self.state.env.get_data_def(ty_name).cloned() else {
            return;
        };

        if def.generic_count != 1 {
            self.error(
                span,
                SemaErrorKind::OptionLangItemTypeParamCountUnsupported {
                    count: def.generic_count,
                },
            );
        }

        if def.fields.is_some() {
            self.error(span, SemaErrorKind::OptionLangItemFieldsNotAllowed);
        }

        let Some(variants) = def.variants.as_ref() else {
            self.error(span, SemaErrorKind::OptionLangItemVariantsRequired);
            return;
        };

        if variants.len() != 2 {
            self.error(span, SemaErrorKind::OptionLangItemVariantCountInvalid);
        }

        let some_payload = variants.get(&self.state.known.some).copied().flatten();
        let none_payload = variants.get(&self.state.known.none).copied();

        let Some(some_payload) = some_payload else {
            self.error(span, SemaErrorKind::OptionLangItemSomeRequired);
            return;
        };
        if none_payload.is_none() {
            self.error(span, SemaErrorKind::OptionLangItemNoneRequired);
            return;
        }

        if none_payload != Some(None) {
            self.error(span, SemaErrorKind::OptionLangItemNoneMustBeNullary);
        }

        if !matches!(self.state.semtys.get(some_payload), SemTy::Generic(0)) {
            self.error(span, SemaErrorKind::OptionLangItemSomePayloadInvalid);
        }
    }

    pub(crate) fn collect_bind_sites(&self, pat: HirPatId, out: &mut Vec<Span>) {
        let mut stack = vec![pat];
        while let Some(pat_id) = stack.pop() {
            let pat = self.ctx.store.pats.get(pat_id);
            match &pat.kind {
                HirPatKind::Bind { name, sub } => {
                    out.push(name.span);
                    if let Some(sub) = sub {
                        stack.push(*sub);
                    }
                }
                HirPatKind::Variant { args, .. }
                | HirPatKind::Tuple { items: args }
                | HirPatKind::Array { items: args }
                | HirPatKind::Or { alts: args } => stack.extend(args.iter().copied()),
                HirPatKind::Record { fields } => {
                    for f in fields {
                        if let Some(sub) = f.sub {
                            stack.push(sub);
                        } else {
                            out.push(f.name.span);
                        }
                    }
                }
                HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
            }
        }
    }
}
