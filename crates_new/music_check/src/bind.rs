use std::collections::HashMap;

use music_basic::Span;
use music_hir::{HirFieldDef, HirLitKind, HirMemberDef, HirPatId, HirPatKind, HirVariantDef};
use music_names::Symbol;

use crate::SemaErrorKind;

use super::checker::Checker;
use super::env::{
    ClassOpSig, DataDef, DataFieldDef, EffectOpSig, ValueScheme, substitute_generics,
};
use super::unify;
use super::{SemTy, SemTyId};

impl<'a> Checker<'a> {
    pub(crate) fn bind_pat_to_scheme(&mut self, pat: HirPatId, scheme: ValueScheme) {
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
                    for f in fields.iter() {
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
        let mut scrut_ty = unify::resolve(&self.state.semtys, scrut_ty);
        loop {
            let SemTy::Mut { base } = self.state.semtys.get(scrut_ty).clone() else {
                break;
            };
            scrut_ty = unify::resolve(&self.state.semtys, base);
        }

        let pat = self.ctx.store.pats.get(pat).clone();
        match pat.kind {
            HirPatKind::Bind { name, sub } => {
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
            HirPatKind::Lit { lit } => {
                let expected = match lit.kind {
                    HirLitKind::Int { .. } | HirLitKind::Rune { .. } => self.state.builtins.int_,
                    HirLitKind::Float { .. } => self.state.builtins.float_,
                    HirLitKind::String(_) | HirLitKind::FString { .. } => {
                        self.state.builtins.string_
                    }
                };
                let _ = self.unify_or_report(pat.origin.span, expected, scrut_ty);
            }
            HirPatKind::Tuple { items } => match self.state.semtys.get(scrut_ty).clone() {
                SemTy::Tuple { items: tys } => {
                    if tys.len() != items.len() {
                        self.error(
                            pat.origin.span,
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
                    self.error(pat.origin.span, SemaErrorKind::TuplePatternRequiresTuple);
                    for p in items.iter().copied() {
                        self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                    }
                }
            },
            HirPatKind::Array { items } => {
                let elem = match self.state.semtys.get(scrut_ty).clone() {
                    SemTy::Array { elem, .. } => elem,
                    SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => {
                        self.state.builtins.unknown
                    }
                    _ => {
                        self.error(pat.origin.span, SemaErrorKind::ArrayPatternRequiresArray);
                        self.state.builtins.unknown
                    }
                };
                for p in items.iter().copied() {
                    self.bind_pat_for_scrut(p, elem);
                }
            }
            HirPatKind::Record { fields } => {
                let record_def = match self.state.semtys.get(scrut_ty).clone() {
                    SemTy::Record { fields } => Some(fields),
                    SemTy::Named { name, args } => {
                        if self.error_if_opaque_repr_access(pat.origin.span, name) {
                            None
                        } else {
                            self.state
                                .env
                                .get_data_def(name)
                                .cloned()
                                .and_then(|def| {
                                    let Some(field_defs) = def.fields else {
                                        self.error(
                                            pat.origin.span,
                                            SemaErrorKind::RecordPatternRequiresRecord,
                                        );
                                        return None;
                                    };
                                    let generic_count = def.generic_count;

                                    let mut out = std::collections::BTreeMap::new();
                                    for (k, v) in field_defs {
                                        let mut subst = Vec::with_capacity(generic_count as usize);
                                        for i in 0..generic_count {
                                            subst.push(
                                                args.get(i as usize)
                                                    .copied()
                                                    .unwrap_or(self.state.builtins.unknown),
                                            );
                                        }
                                        let ty = substitute_generics(
                                            &mut self.state.semtys,
                                            v.ty,
                                            &subst,
                                        );
                                        let _prev = out.insert(k, ty);
                                    }
                                    Some(out)
                                })
                                .or_else(|| {
                                    self.error(
                                        pat.origin.span,
                                        SemaErrorKind::RecordPatternRequiresRecord,
                                    );
                                    None
                                })
                        }
                    }
                    SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => None,
                    _ => {
                        self.error(pat.origin.span, SemaErrorKind::RecordPatternRequiresRecord);
                        None
                    }
                };

                for f in fields.iter() {
                    let field_ty = record_def
                        .as_ref()
                        .and_then(|fields| fields.get(&f.name.name).copied());

                    if record_def.is_some() && field_ty.is_none() {
                        self.error(
                            f.name.span,
                            SemaErrorKind::FieldNotFound {
                                name: self.ctx.interner.resolve(f.name.name).to_string(),
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
                        self.bind_pat_for_scrut(
                            sub,
                            field_ty.unwrap_or(self.state.builtins.unknown),
                        );
                    }
                }
            }
            HirPatKind::Or { alts } => {
                for p in alts.iter().copied() {
                    self.bind_pat_for_scrut(p, scrut_ty);
                }
            }
            HirPatKind::Variant { name, args } => {
                let mut payload_ty = None::<Option<SemTyId>>;
                let mut generic_count = 0u32;
                let mut ty_args: Box<[SemTyId]> = Box::new([]);

                match self.state.semtys.get(scrut_ty).clone() {
                    SemTy::Named {
                        name: ty_name,
                        args,
                    } => {
                        if !self.error_if_opaque_repr_access(pat.origin.span, ty_name)
                            && let Some(def) = self.state.env.get_data_def(ty_name).cloned()
                        {
                            generic_count = def.generic_count;
                            ty_args = args;
                            if let Some(variants) = def.variants.as_ref() {
                                payload_ty = variants.get(&name.name).copied();
                                if payload_ty.is_none() {
                                    self.error(
                                        name.span,
                                        SemaErrorKind::VariantNotFound {
                                            name: self.ctx.interner.resolve(name.name).to_string(),
                                        },
                                    );
                                }
                            } else {
                                self.error(
                                    pat.origin.span,
                                    SemaErrorKind::VariantPatternRequiresData,
                                );
                            }
                        }
                    }
                    SemTy::Unknown | SemTy::Any | SemTy::Error | SemTy::InferVar(_) => {}
                    _ => {
                        self.error(pat.origin.span, SemaErrorKind::VariantPatternRequiresData);
                    }
                }

                let Some(payload_ty) = payload_ty else {
                    for p in args.iter().copied() {
                        self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                    }
                    return;
                };

                let payload_ty = payload_ty.map(|payload_ty| {
                    let mut subst = Vec::with_capacity(generic_count as usize);
                    for i in 0..generic_count {
                        subst.push(
                            ty_args
                                .get(i as usize)
                                .copied()
                                .unwrap_or(self.state.builtins.unknown),
                        );
                    }
                    substitute_generics(&mut self.state.semtys, payload_ty, &subst)
                });

                match payload_ty {
                    None => {
                        if !args.is_empty() {
                            self.error(
                                name.span,
                                SemaErrorKind::VariantPatternArgCountMismatch {
                                    variant: self.ctx.interner.resolve(name.name).to_string(),
                                    expected: "0".to_string(),
                                    found: u32::try_from(args.len()).unwrap_or(0),
                                },
                            );
                        }
                    }
                    Some(payload_ty) => {
                        let resolved = unify::resolve(&self.state.semtys, payload_ty);
                        let tuple_len = match self.state.semtys.get(resolved).clone() {
                            SemTy::Tuple { items } => Some(items.len()),
                            _ => None,
                        };

                        let found = u32::try_from(args.len()).unwrap_or(0);
                        let ok = if args.len() == 1 {
                            self.bind_pat_for_scrut(args[0], payload_ty);
                            true
                        } else if let Some(tuple_len) = tuple_len {
                            if tuple_len == args.len() {
                                let SemTy::Tuple { items } =
                                    self.state.semtys.get(resolved).clone()
                                else {
                                    unreachable!();
                                };
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

                        if !ok {
                            let expected = if let Some(tuple_len) = tuple_len {
                                format!("1 or {tuple_len}")
                            } else {
                                "1".to_string()
                            };
                            self.error(
                                name.span,
                                SemaErrorKind::VariantPatternArgCountMismatch {
                                    variant: self.ctx.interner.resolve(name.name).to_string(),
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
            }
            HirPatKind::Wildcard | HirPatKind::Error => {}
        }
    }

    pub(crate) fn bind_value_clause_binder(&mut self, binder: music_names::Ident, ty: SemTyId) {
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

    pub(crate) fn bind_handle_param(&mut self, param: music_names::Ident, ty: SemTyId) {
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
        members: &Box<[music_hir::HirMemberDef]>,
    ) {
        // Only supports `let EffectName := effect { ... }` bindings for now.
        let mut bind_sites = Vec::new();
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };

        let mut ops = HashMap::<Symbol, EffectOpSig>::new();
        for member in members.iter() {
            let music_hir::HirMemberDef::Let {
                name, params, ret, ..
            } = member
            else {
                continue;
            };
            let mut param_tys = Vec::new();
            for p in params.iter() {
                let ty = p
                    .annot
                    .map(|t| self.lower_hir_ty(t, ty_params))
                    .unwrap_or(self.state.builtins.unknown);
                param_tys.push(ty);
            }
            let ret = ret
                .map(|t| self.lower_hir_ty(t, ty_params))
                .unwrap_or(self.state.builtins.unknown);
            let _prev = ops.insert(
                name.name.name,
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
        members: &Box<[HirMemberDef]>,
    ) {
        // Only supports `let ClassName := class { ... }` bindings for now.
        let mut bind_sites = Vec::new();
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };

        let mut ops = HashMap::<Symbol, ClassOpSig>::new();
        for member in members.iter() {
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

            let mut param_tys = Vec::new();
            for p in params.iter() {
                let ty = p
                    .annot
                    .map(|t| self.lower_hir_ty(t, ty_params))
                    .unwrap_or(self.state.builtins.unknown);
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
        let mut bind_sites = Vec::new();
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };
        let type_name = self.ctx.names.bindings[binding].name;

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
            type_name,
            DataDef {
                generic_count,
                variants: variants_map,
                fields: fields_map,
            },
        );

        self.validate_option_lang_item(type_name);
    }

    fn validate_option_lang_item(&mut self, type_name: Symbol) {
        if self.state.lang.option_ty != Some(type_name) {
            return;
        }

        let span = self.state.lang.option_span.unwrap_or(Span::DUMMY);

        let Some(def) = self.state.env.get_data_def(type_name).cloned() else {
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
                    for f in fields.iter() {
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
