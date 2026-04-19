use std::collections::{BTreeMap, HashSet};

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirConstraint, HirExprId, HirExprKind, HirFieldDef, HirLitId, HirLitKind,
    HirMemberDef, HirMemberKind, HirPatKind, HirTyId, HirTyKind, HirVariantDef, HirVariantFieldDef,
};
use music_names::{Ident, NameBindingId, Symbol};

use super::super::const_eval::{data_variant_tag, record_data_variant_tag};
use super::super::exprs::check_expr;
use super::super::schemes::BindingScheme;
use super::super::surface::surface_key;
use super::super::variant_payload::lower_variant_payload;
use super::super::{CheckPass, DiagKind, EffectDef, EffectOpDef, PassBase};
use crate::api::{
    ClassFacts, ClassMemberFacts, ExprFacts, ForeignLinkInfo, LawFacts, LawParamFacts, TargetInfo,
    normalize_arch_text, normalize_target_text,
};
use crate::effects::EffectRow;

type VariantDefRange = SliceRange<HirVariantDef>;
type FieldDefRange = SliceRange<HirFieldDef>;
type ConstraintRange = SliceRange<HirConstraint>;
type MemberDefRange = SliceRange<HirMemberDef>;

fn member_has_attr(ctx: &CheckPass<'_, '_, '_>, member: &HirMemberDef, name: &str) -> bool {
    ctx.attrs(member.attrs.clone()).iter().any(|attr| {
        let parts = ctx.idents(attr.path);
        parts.len() == 1 && ctx.resolve_symbol(parts[0].name) == name
    })
}

fn matches_target_value(target: Option<&str>, values: &[String]) -> bool {
    target.is_some_and(|target| {
        values
            .iter()
            .any(|value| normalize_target_text(value) == target)
    })
}

fn matches_arch_value(target: Option<&str>, values: &[String]) -> bool {
    target.is_some_and(|target| {
        values
            .iter()
            .any(|value| normalize_arch_text(value) == target)
    })
}

pub(in super::super) fn member_signature(
    ctx: &mut PassBase<'_, '_, '_>,
    member: &HirMemberDef,
    bind_name: bool,
) -> ClassMemberFacts {
    let builtins = ctx.builtins();
    let params = ctx
        .params(member.params.clone())
        .into_iter()
        .map(|param| {
            param.ty.map_or(builtins.unknown, |expr| {
                let origin = ctx.expr(expr).origin;
                ctx.lower_type_expr(expr, origin)
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let result = member.sig.map_or(builtins.unknown, |expr| {
        let origin = ctx.expr(expr).origin;
        ctx.lower_type_expr(expr, origin)
    });
    if bind_name {
        let params_list = ctx.alloc_ty_list(params.iter().copied());
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params: params_list,
            ret: result,
            is_effectful: false,
        });
        if let Some(binding) = ctx.binding_id_for_decl(member.name) {
            ctx.insert_binding_type(binding, ty);
        }
    }
    ClassMemberFacts::new(member.name.name, params, result)
}

pub(in super::super) fn member_law_facts(
    ctx: &mut PassBase<'_, '_, '_>,
    member: &HirMemberDef,
) -> LawFacts {
    let builtins = ctx.builtins();
    let params = ctx
        .params(member.params.clone())
        .into_iter()
        .map(|param| {
            LawParamFacts::new(
                param.name.name,
                param.ty.map_or(builtins.unknown, |expr| {
                    let origin = ctx.expr(expr).origin;
                    ctx.lower_type_expr(expr, origin)
                }),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    LawFacts::new(member.name.name, params)
}

pub(in super::super) fn check_foreign_let(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    type_params: Box<[Symbol]>,
    type_param_kinds: Box<[HirTyId]>,
) -> Option<HirTyId> {
    ctx.check_foreign_let(expr_id, type_params, type_param_kinds)
}

impl CheckPass<'_, '_, '_> {
    fn check_data_expr(&mut self, variants: VariantDefRange, fields: FieldDefRange) -> ExprFacts {
        let builtins = self.builtins();
        for variant in self.variants(variants) {
            for field in self.variant_fields(variant.fields) {
                let origin = self.expr(field.ty).origin;
                let _ = self.lower_type_expr(field.ty, origin);
            }
            if let Some(value) = variant.value {
                let _ = check_expr(self, value);
            }
        }
        for field in self.fields(fields) {
            let origin = self.expr(field.ty).origin;
            let _ = self.lower_type_expr(field.ty, origin);
            if let Some(value) = field.value {
                let _ = check_expr(self, value);
            }
        }
        ExprFacts::new(builtins.type_, EffectRow::empty())
    }

    fn check_class_expr(
        &mut self,
        expr_id: HirExprId,
        constraints: ConstraintRange,
        members: MemberDefRange,
    ) -> ExprFacts {
        let builtins = self.builtins();
        if let Some(facts) = self.class_facts(expr_id).cloned() {
            for member in self.members(members) {
                if member.kind == HirMemberKind::Law
                    && let Some(value) = member.value
                {
                    let law_facts = check_expr(self, value);
                    let origin = self.expr(value).origin;
                    self.type_mismatch(origin, builtins.bool_, law_facts.ty);
                    if !law_facts.effects.is_pure() {
                        self.diag(origin.span, DiagKind::LawMustBePure, "");
                    }
                } else {
                    let _ = member_signature(self, &member, true);
                }
            }
            let _ = self.lower_constraints(constraints);
            self.insert_class_facts(expr_id, facts);
        } else {
            for member in self.members(members) {
                let _ = member_signature(self, &member, true);
            }
        }
        ExprFacts::new(self.builtins().type_, EffectRow::empty())
    }

    fn check_foreign_let(
        &mut self,
        expr_id: HirExprId,
        type_params: Box<[Symbol]>,
        type_param_kinds: Box<[HirTyId]>,
    ) -> Option<HirTyId> {
        let builtins = self.builtins();
        let abi: Box<str> = self
            .expr(expr_id)
            .mods
            .foreign
            .as_ref()
            .and_then(|m| m.abi)
            .map_or_else(|| "c".into(), |sym| self.resolve_symbol(sym).into());
        let attrs = self.attrs(self.expr(expr_id).mods.attrs);
        for attr in &attrs {
            let path = super::super::attrs::attr_path(self, attr);
            match path.as_slice() {
                ["link"] => self.validate_link_attr(attr, self.expr(expr_id).origin),
                ["when"] => self.validate_when_attr(attr, self.expr(expr_id).origin),
                _ => {}
            }
        }
        if !self.when_attrs_match(&attrs) {
            if let Some((binding, _)) = self.foreign_binding_from_let(expr_id) {
                self.mark_gated_binding(binding);
            }
            return None;
        }
        if let Some((binding, _)) = self.foreign_binding_from_let(expr_id) {
            self.mark_unsafe_binding(binding);
            let link = self.link_info_from_attrs(&attrs);
            if link.name.is_some() || link.symbol.is_some() {
                self.set_foreign_link(binding, link);
            }
        }
        let origin = self.expr(expr_id).origin;
        let HirExprKind::Let { params, sig, .. } = self.expr(expr_id).kind else {
            self.diag(origin.span, DiagKind::ForeignSignatureRequired, "");
            return None;
        };
        let param_names = self
            .params(params.clone())
            .into_iter()
            .map(|param| param.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let params = self.lower_params(params);
        let result = sig.map_or(builtins.unknown, |sig| {
            let origin = self.expr(sig).origin;
            self.lower_type_expr(sig, origin)
        });
        let params = self.alloc_ty_list(params.iter().copied());
        let ty = self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: result,
            is_effectful: false,
        });
        if let Some((binding, _)) = self.foreign_binding_from_let(expr_id) {
            let scheme = BindingScheme {
                type_params,
                type_param_kinds,
                param_names,
                comptime_params: Box::default(),
                constraints: Box::default(),
                ty,
                effects: EffectRow::empty(),
            };
            let value_ty = self.scheme_value_ty(&scheme);
            self.insert_binding_type(binding, value_ty);
            self.insert_binding_effects(binding, EffectRow::empty());
            self.insert_binding_scheme(binding, scheme);
            self.validate_foreign_let(expr_id, abi.as_ref());
            return Some(value_ty);
        }
        self.validate_foreign_let(expr_id, abi.as_ref());
        Some(ty)
    }

    fn foreign_binding_from_let(&self, expr: HirExprId) -> Option<(NameBindingId, Ident)> {
        let HirExprKind::Let { pat, .. } = self.expr(expr).kind else {
            return None;
        };
        let HirPatKind::Bind { name } = self.pat(pat).kind else {
            return None;
        };
        let binding = self.binding_id_for_decl(name)?;
        Some((binding, name))
    }

    fn when_attrs_match(&self, attrs: &[HirAttr]) -> bool {
        let target = self.target();
        for attr in attrs {
            let path = super::super::attrs::attr_path(self, attr);
            if path.as_slice() != ["when"] {
                continue;
            }
            if !self.when_attr_matches(target, attr) {
                return false;
            }
        }
        true
    }

    fn when_attr_matches(&self, target: Option<&TargetInfo>, attr: &HirAttr) -> bool {
        let Some(target) = target else {
            return false;
        };

        for arg in self.attr_args(attr.args.clone()) {
            let Some(name) = arg.name.map(|ident| self.resolve_symbol(ident.name)) else {
                continue;
            };
            let Some(values) = self.when_values(arg.value) else {
                continue;
            };

            let matched = match name {
                "os" => matches_target_value(target.os.as_deref(), &values),
                "arch" => matches_arch_value(target.arch.as_deref(), &values),
                "archFamily" => matches_target_value(target.arch_family.as_deref(), &values),
                "env" => matches_target_value(target.env.as_deref(), &values),
                "abi" => matches_target_value(target.abi.as_deref(), &values),
                "vendor" => matches_target_value(target.vendor.as_deref(), &values),
                "family" => values.iter().any(|value| {
                    target
                        .family
                        .contains(normalize_target_text(value).as_str())
                }),
                "feature" => values.iter().any(|v| target.features.contains(v.as_str())),
                "pointerWidth" => target
                    .pointer_width
                    .is_some_and(|width| values.iter().any(|value| value == &width.to_string())),
                "endian" => matches_target_value(target.endian.as_deref(), &values),
                "jit" => {
                    target.jit.supported
                        && matches_target_value(target.jit.backend.as_deref(), &values)
                }
                "jitIsa" => {
                    target.jit.supported && matches_target_value(target.jit.isa.as_deref(), &values)
                }
                "jitCallConv" => {
                    target.jit.supported
                        && matches_target_value(target.jit.call_conv.as_deref(), &values)
                }
                "jitFeature" => values.iter().any(|value| {
                    target
                        .jit
                        .features
                        .contains(normalize_target_text(value).as_str())
                }),
                _ => true,
            };
            if !matched {
                return false;
            }
        }
        true
    }

    fn when_values(&self, expr: HirExprId) -> Option<Vec<String>> {
        match self.expr(expr).kind {
            HirExprKind::Lit { lit } => self.when_lit_value(lit).map(|s| vec![s]),
            HirExprKind::Array { items } => {
                let mut out = Vec::<String>::new();
                for item in self.array_items(items) {
                    if let HirExprKind::Lit { lit } = self.expr(item.expr).kind {
                        if let Some(value) = self.when_lit_value(lit) {
                            out.push(value);
                        }
                    }
                }
                Some(out)
            }
            _ => None,
        }
    }

    fn when_lit_value(&self, lit: HirLitId) -> Option<String> {
        match self.lit_kind(lit) {
            HirLitKind::String { value } | HirLitKind::Int { raw: value } => Some(value.into()),
            HirLitKind::Float { .. } | HirLitKind::Rune { .. } => None,
        }
    }

    fn link_info_from_attrs(&self, attrs: &[HirAttr]) -> ForeignLinkInfo {
        let mut out = ForeignLinkInfo::new();
        for attr in attrs {
            let path = super::super::attrs::attr_path(self, attr);
            if path.as_slice() != ["link"] {
                continue;
            }
            let mut positional = Vec::<String>::new();
            for arg in self.attr_args(attr.args.clone()) {
                let Some(value) = self.string_lit_value(arg.value) else {
                    continue;
                };
                if let Some(name) = arg.name.map(|ident| self.resolve_symbol(ident.name)) {
                    match name {
                        "name" => out = out.with_name(value),
                        "symbol" => out = out.with_symbol(value),
                        _ => {}
                    }
                } else {
                    positional.push(value);
                }
            }
            if out.name.is_none()
                && let Some(value) = positional.first().cloned()
            {
                out.name = Some(value.into_boxed_str());
            }
            if out.symbol.is_none()
                && let Some(value) = positional.get(1).cloned()
            {
                out.symbol = Some(value.into_boxed_str());
            }
        }
        out
    }

    fn string_lit_value(&self, expr: HirExprId) -> Option<String> {
        match self.expr(expr).kind {
            HirExprKind::Lit { lit } => self.lit_string_value(lit),
            _ => None,
        }
    }
}

impl CheckPass<'_, '_, '_> {
    pub(in super::super) fn check_bound_data(
        &mut self,
        name: Ident,
        variants: VariantDefRange,
        fields: FieldDefRange,
    ) -> ExprFacts {
        let data_name: Box<str> = self.resolve_symbol(name.name).into();
        if self.data_def(&data_name).is_none() {
            let mut variant_map = BTreeMap::<Box<str>, super::super::DataVariantDef>::new();
            let mut seen_variants = BTreeMap::<Box<str>, _>::new();
            let mut seen_tags = HashSet::<i64>::new();
            for (variant_index, variant) in self.variants(variants.clone()).into_iter().enumerate()
            {
                let tag: Box<str> = self.resolve_symbol(variant.name.name).into();
                let tag_value = data_variant_tag(
                    self,
                    variant.value,
                    i64::try_from(variant_index).unwrap_or(i64::MAX),
                );
                if !record_data_variant_tag(&mut seen_tags, tag_value) {
                    self.diag(
                        variant.origin.span,
                        DiagKind::DuplicateDataVariantDiscriminant,
                        "",
                    );
                }
                let variant_fields = self.variant_fields(variant.fields);
                if variant_payload_style_is_mixed(&variant_fields) {
                    self.diag(variant.origin.span, DiagKind::MixedVariantPayloadStyle, "");
                }
                let (payload, field_tys, field_names) =
                    lower_variant_payload(self, &variant_fields);
                let result = variant
                    .result
                    .map(|expr| self.lower_type_expr(expr, variant.origin));
                if let Some(previous_origin) = seen_variants.insert(tag.clone(), variant.origin) {
                    self.diag_message_with_previous(
                        variant.origin.span,
                        previous_origin.span,
                        DiagKind::CollectDuplicateDataVariant,
                        format!("duplicate data variant `{tag}`"),
                        format!("data variant `{tag}` first declared here"),
                    );
                }
                let _previous_variant = variant_map.insert(
                    tag,
                    super::super::DataVariantDef::new(
                        tag_value,
                        payload,
                        result,
                        field_tys,
                        field_names,
                    ),
                );
            }
            if variant_map.is_empty() {
                let record_fields = self.fields(fields.clone());
                let field_names = record_fields
                    .iter()
                    .map(|field| Some(self.resolve_symbol(field.name.name).into()))
                    .collect::<Vec<Option<Box<str>>>>()
                    .into_boxed_slice();
                let field_tys = record_fields
                    .into_iter()
                    .map(|field| {
                        let origin = self.expr(field.ty).origin;
                        self.lower_type_expr(field.ty, origin)
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                if !field_tys.is_empty() {
                    let _ = variant_map.insert(
                        data_name.clone(),
                        super::super::DataVariantDef::new(0, None, None, field_tys, field_names),
                    );
                }
            }
            let key = surface_key(self.module_key(), self.interner(), name.name);
            self.insert_data_def(
                data_name,
                super::super::DataDef::new(key, variant_map, None, None, None, false)
                    .with_record_shape(variants.is_empty() && !fields.is_empty()),
            );
        }
        self.check_data_expr(variants, fields)
    }

    pub(in super::super) fn check_bound_effect(
        &mut self,
        expr_id: HirExprId,
        name: Ident,
        members: MemberDefRange,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let effect_name: Box<str> = self.resolve_symbol(name.name).into();
        if self.effect_def(&effect_name).is_none() {
            let members_vec = self.members(members.clone());
            let ops = members_vec
                .iter()
                .filter(|member| member.kind == HirMemberKind::Let)
                .map(|member| {
                    let facts = member_signature(self, member, false);
                    (
                        Box::<str>::from(self.resolve_symbol(member.name.name)),
                        EffectOpDef::new(
                            facts.params.clone(),
                            self.params(member.params.clone())
                                .into_iter()
                                .map(|param| param.name.name)
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                            facts.result,
                        )
                        .with_comptime_safe(member_has_attr(
                            self,
                            member,
                            "comptimeSafe",
                        )),
                    )
                })
                .collect::<BTreeMap<_, _>>();
            let laws = members_vec
                .iter()
                .filter(|member| member.kind == HirMemberKind::Law)
                .map(|member| member_law_facts(self, member))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let key = surface_key(self.module_key(), self.interner(), name.name);
            self.insert_effect_def(effect_name, EffectDef::new(key, ops, laws));
        }
        let _ = expr_id;
        for member in self.members(members) {
            match member.kind {
                HirMemberKind::Let => {
                    let _ = member_signature(self, &member, true);
                    if let Some(value) = member.value {
                        let _ = check_expr(self, value);
                    }
                }
                HirMemberKind::Law => {
                    if let Some(value) = member.value {
                        let law_facts = check_expr(self, value);
                        let origin = self.expr(value).origin;
                        self.type_mismatch(origin, builtins.bool_, law_facts.ty);
                        if !law_facts.effects.is_pure() {
                            self.diag(origin.span, DiagKind::LawMustBePure, "");
                        }
                    }
                }
            }
        }
        ExprFacts::new(builtins.type_, EffectRow::empty())
    }

    pub(in super::super) fn check_bound_class(
        &mut self,
        expr_id: HirExprId,
        name: Ident,
        type_params: &[Symbol],
        constraints: ConstraintRange,
        members: MemberDefRange,
    ) -> ExprFacts {
        if self.class_id(name.name).is_none() {
            self.insert_class_id(name.name, expr_id);
            let members_vec = self.members(members.clone());
            let class_members = members_vec
                .iter()
                .filter(|member| member.kind == HirMemberKind::Let)
                .map(|member| member_signature(self, member, false))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let laws = members_vec
                .iter()
                .filter(|member| member.kind == HirMemberKind::Law)
                .map(|member| member_law_facts(self, member))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let constraints_facts = self.lower_constraints(constraints.clone());
            let type_params = type_params.to_vec().into_boxed_slice();
            let facts = ClassFacts::new(
                surface_key(self.module_key(), self.interner(), name.name),
                name.name,
                class_members,
                laws,
            )
            .with_type_params(type_params)
            .with_constraints(constraints_facts);
            self.insert_class_facts(expr_id, facts.clone());
            self.insert_class_facts_by_name(name.name, facts);
        }
        self.check_class_expr(expr_id, constraints, members)
    }
}

fn variant_payload_style_is_mixed(fields: &[HirVariantFieldDef]) -> bool {
    let has_named = fields.iter().any(|field| field.name.is_some());
    let has_positional = fields.iter().any(|field| field.name.is_none());
    has_named && has_positional
}

// Attribute and export wrappers live in `HirExpr.mods`, not `HirExprKind`.
