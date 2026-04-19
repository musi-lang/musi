use std::collections::{BTreeMap, HashMap, HashSet};

use music_arena::SliceRange;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirBinder, HirConstraint, HirExprId, HirExprKind, HirFieldDef,
    HirMatchArm, HirMemberDef, HirMemberKind, HirOrigin, HirTemplatePart, HirTyId, HirVariantDef,
    HirVariantFieldDef,
};
use music_names::{Ident, Symbol};

use crate::api::{ClassFacts, ClassMemberFacts, DefinitionKey, LawFacts};

use super::attrs::extract_data_layout_hints;
use super::const_eval::{data_variant_tag, record_data_variant_tag};
use super::decls::{member_law_facts, member_signature};
use super::pats::bound_name_from_pat;
use super::surface::surface_key;
use super::variant_payload::lower_variant_payload;
use super::{CollectPass, DataDef, DataVariantDef, DiagKind, EffectDef, EffectOpDef};

struct DataDeclTypeParams {
    scope: Vec<(Symbol, HirTyId)>,
    names: Box<[Symbol]>,
    kinds: Box<[HirTyId]>,
}

fn data_decl_type_names(type_param_kinds: &[(Symbol, HirTyId)]) -> Box<[Symbol]> {
    type_param_kinds
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn data_decl_type_kinds(type_param_kinds: &[(Symbol, HirTyId)]) -> Box<[HirTyId]> {
    type_param_kinds
        .iter()
        .map(|(_, kind)| *kind)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub fn collect_module(ctx: &mut CollectPass<'_, '_, '_>) {
    ctx.collect_module();
}

fn member_has_attr(ctx: &CollectPass<'_, '_, '_>, member: &HirMemberDef, name: &str) -> bool {
    ctx.attrs(member.attrs.clone()).iter().any(|attr| {
        let parts = ctx.idents(attr.path);
        parts.len() == 1 && ctx.resolve_symbol(parts[0].name) == name
    })
}

impl CollectPass<'_, '_, '_> {
    fn collect_module(&mut self) {
        self.visit_expr(self.root_expr_id());
    }

    fn visit_expr(&mut self, id: HirExprId) {
        if self.visit_expr_trivial(id)
            || self.visit_expr_aggregate(id)
            || self.visit_expr_call_like(id)
            || self.visit_expr_type_ops(id)
            || self.visit_expr_decls(id)
            || self.visit_expr_control(id)
        {}
    }

    fn visit_expr_trivial(&self, id: HirExprId) -> bool {
        matches!(
            self.expr(id).kind,
            HirExprKind::Error
                | HirExprKind::Name { .. }
                | HirExprKind::Lit { .. }
                | HirExprKind::ArrayTy { .. }
                | HirExprKind::HandlerTy { .. }
                | HirExprKind::Variant { .. }
                | HirExprKind::Quote { .. }
                | HirExprKind::Splice { .. }
        )
    }

    fn visit_expr_aggregate(&mut self, id: HirExprId) -> bool {
        match self.expr(id).kind {
            HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
                self.visit_expr_ids(exprs);
            }
            HirExprKind::Array { items } => self.visit_array_items(items),
            HirExprKind::Record { items } => {
                for item in self.record_items(items) {
                    self.visit_expr(item.value);
                }
            }
            HirExprKind::RecordUpdate { base, items } => {
                for item in self.record_items(items) {
                    self.visit_expr(item.value);
                }
                self.visit_expr(base);
            }
            HirExprKind::Template { parts } => self.visit_template_parts(parts),
            _ => return false,
        }
        true
    }

    fn visit_expr_call_like(&mut self, id: HirExprId) -> bool {
        match self.expr(id).kind {
            HirExprKind::Pi { binder_ty, ret, .. } => {
                self.visit_expr(binder_ty);
                self.visit_expr(ret);
            }
            HirExprKind::Lambda { body, .. }
            | HirExprKind::Import { arg: body }
            | HirExprKind::Request { expr: body } => self.visit_expr(body),
            HirExprKind::Call { callee, args } => self.visit_call(callee, args),
            HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
                self.visit_expr(callee);
                self.visit_expr_ids(args);
            }
            HirExprKind::Field { base, .. } => self.visit_expr(base),
            _ => return false,
        }
        true
    }

    fn visit_expr_type_ops(&mut self, id: HirExprId) -> bool {
        match self.expr(id).kind {
            HirExprKind::TypeTest { base, ty, .. } | HirExprKind::TypeCast { base, ty } => {
                self.visit_expr(base);
                self.visit_expr(ty);
            }
            HirExprKind::Prefix { expr, .. } | HirExprKind::PartialRange { expr, .. } => {
                self.visit_expr(expr);
            }
            HirExprKind::Binary { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            _ => return false,
        }
        true
    }

    fn visit_expr_decls(&mut self, id: HirExprId) -> bool {
        match self.expr(id).kind {
            HirExprKind::Let {
                pat,
                value,
                type_params,
                ..
            } => {
                if let Some(name) = bound_name_from_pat(self, pat) {
                    let attrs = self.expr(id).mods.attrs;
                    let outer_attrs = (!attrs.is_empty()).then_some((self.expr(id).origin, attrs));
                    self.collect_bound_decl(value, name, type_params, outer_attrs.as_ref());
                }
                self.visit_expr(value);
            }
            HirExprKind::Data { variants, fields } => self.visit_data(variants, fields),
            HirExprKind::Effect { members } | HirExprKind::Class { members, .. } => {
                for member in self.members(members) {
                    self.visit_member(&member);
                }
            }
            HirExprKind::Instance { class, members, .. } => {
                self.visit_expr(class);
                for member in self.members(members) {
                    self.visit_member(&member);
                }
            }
            _ => return false,
        }
        true
    }

    fn visit_expr_control(&mut self, id: HirExprId) -> bool {
        match self.expr(id).kind {
            HirExprKind::Match { scrutinee, arms } => self.visit_match(scrutinee, arms),
            HirExprKind::HandlerLit { clauses, .. } => {
                for clause in self.handle_clauses(clauses) {
                    self.visit_expr(clause.body);
                }
            }
            HirExprKind::Handle { expr, handler } => {
                self.visit_expr(expr);
                self.visit_expr(handler);
            }
            HirExprKind::Resume { expr } => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            _ => return false,
        }
        true
    }

    fn visit_expr_ids(&mut self, exprs: SliceRange<HirExprId>) {
        for expr in self.expr_ids(exprs) {
            self.visit_expr(expr);
        }
    }

    fn visit_array_items(&mut self, items: SliceRange<HirArrayItem>) {
        for item in self.array_items(items) {
            self.visit_expr(item.expr);
        }
    }

    fn visit_template_parts(&mut self, parts: SliceRange<HirTemplatePart>) {
        for part in self.template_parts(parts) {
            if let HirTemplatePart::Expr { expr } = part {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_call(&mut self, callee: HirExprId, args: SliceRange<HirArg>) {
        self.visit_expr(callee);
        for arg in self.args(args) {
            self.visit_expr(arg.expr);
        }
    }

    fn visit_match(&mut self, scrutinee: HirExprId, arms: SliceRange<HirMatchArm>) {
        self.visit_expr(scrutinee);
        for arm in self.match_arms(arms) {
            if let Some(guard) = arm.guard {
                self.visit_expr(guard);
            }
            self.visit_expr(arm.expr);
        }
    }

    fn visit_data(&mut self, variants: SliceRange<HirVariantDef>, fields: SliceRange<HirFieldDef>) {
        for variant in self.variants(variants) {
            for field in self.variant_fields(variant.fields) {
                self.visit_expr(field.ty);
            }
            if let Some(value) = variant.value {
                self.visit_expr(value);
            }
        }
        for field in self.fields(fields) {
            self.visit_expr(field.ty);
            if let Some(value) = field.value {
                self.visit_expr(value);
            }
        }
    }

    fn collect_bound_decl(
        &mut self,
        value: HirExprId,
        name: Ident,
        type_params: SliceRange<HirBinder>,
        outer_attrs: Option<&(HirOrigin, SliceRange<HirAttr>)>,
    ) {
        let origin = outer_attrs.map_or_else(|| self.expr(value).origin, |(o, _)| *o);
        let attrs = outer_attrs.map_or_else(Vec::new, |(_, range)| vec![range.clone()]);
        match self.expr(value).kind {
            HirExprKind::Data { variants, fields } => {
                self.collect_data_decl(origin, &attrs, name, type_params, variants, fields);
            }
            HirExprKind::Effect { members } => self.collect_effect_decl(name, members),
            HirExprKind::Class {
                constraints,
                members,
            } => self.collect_class_decl(value, name, type_params, constraints, members),
            _ => {}
        }
    }

    fn collect_data_decl(
        &mut self,
        origin: HirOrigin,
        attrs: &[SliceRange<HirAttr>],
        name: Ident,
        type_params: SliceRange<HirBinder>,
        variants: SliceRange<HirVariantDef>,
        fields: SliceRange<HirFieldDef>,
    ) {
        let data_name: Box<str> = self.resolve_symbol(name.name).into();
        if self.data_def(&data_name).is_some() {
            return;
        }

        let (repr_kind, layout_align, layout_pack, frozen) =
            extract_data_layout_hints(self, origin, attrs);
        let type_params = self.data_decl_type_params(type_params);
        let key = surface_key(self.module_key(), self.interner(), name.name);
        self.insert_data_stub(
            &data_name,
            key.clone(),
            (repr_kind.clone(), layout_align, layout_pack, frozen),
            &type_params,
        );
        self.push_type_param_kinds(&type_params.scope);
        let is_record_shape = variants.is_empty() && !fields.is_empty();
        let variant_map = self.collect_data_variants(&data_name, variants, fields);
        self.pop_type_param_kinds();
        self.insert_data_final(
            data_name,
            key,
            variant_map,
            (repr_kind, layout_align, layout_pack, frozen),
            type_params,
            is_record_shape,
        );
    }

    fn data_decl_type_params(&mut self, type_params: SliceRange<HirBinder>) -> DataDeclTypeParams {
        let scope = self.lower_type_param_kinds(type_params);
        let names = scope
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let kinds = scope
            .iter()
            .map(|(_, kind)| *kind)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        DataDeclTypeParams {
            scope,
            names,
            kinds,
        }
    }

    fn insert_data_stub(
        &mut self,
        data_name: &str,
        key: DefinitionKey,
        layout: (Option<Box<str>>, Option<u32>, Option<u32>, bool),
        type_params: &DataDeclTypeParams,
    ) {
        let (repr_kind, layout_align, layout_pack, frozen) = layout;
        self.insert_data_def(
            data_name,
            DataDef::new(
                key,
                BTreeMap::new(),
                repr_kind,
                layout_align,
                layout_pack,
                frozen,
            )
            .with_type_params(type_params.names.clone(), type_params.kinds.clone()),
        );
    }

    fn insert_data_final(
        &mut self,
        data_name: Box<str>,
        key: DefinitionKey,
        variant_map: BTreeMap<Box<str>, DataVariantDef>,
        layout: (Option<Box<str>>, Option<u32>, Option<u32>, bool),
        type_params: DataDeclTypeParams,
        is_record_shape: bool,
    ) {
        let (repr_kind, layout_align, layout_pack, frozen) = layout;
        self.insert_data_def(
            data_name,
            DataDef::new(
                key,
                variant_map,
                repr_kind,
                layout_align,
                layout_pack,
                frozen,
            )
            .with_type_params(type_params.names, type_params.kinds)
            .with_record_shape(is_record_shape),
        );
    }

    fn collect_data_variants(
        &mut self,
        data_name: &str,
        variants: SliceRange<HirVariantDef>,
        fields: SliceRange<HirFieldDef>,
    ) -> BTreeMap<Box<str>, DataVariantDef> {
        let mut variant_map = BTreeMap::<Box<str>, DataVariantDef>::new();
        let mut seen_variants = HashMap::<Box<str>, HirOrigin>::new();
        let mut seen_tags = HashSet::<i64>::new();
        for (variant_index, variant) in self.variants(variants).into_iter().enumerate() {
            self.collect_data_variant(
                variant_index,
                variant,
                &mut seen_tags,
                &mut seen_variants,
                &mut variant_map,
            );
        }
        if variant_map.is_empty() {
            self.insert_record_data_variant(data_name, fields, &mut variant_map);
        }
        variant_map
    }

    fn collect_data_variant(
        &mut self,
        variant_index: usize,
        variant: HirVariantDef,
        seen_tags: &mut HashSet<i64>,
        seen_variants: &mut HashMap<Box<str>, HirOrigin>,
        variant_map: &mut BTreeMap<Box<str>, DataVariantDef>,
    ) {
        let tag: Box<str> = self.resolve_symbol(variant.name.name).into();
        let default_tag = i64::try_from(variant_index).unwrap_or(i64::MAX);
        let tag_value = data_variant_tag(self, variant.value, default_tag);
        self.record_data_variant_tag(seen_tags, tag_value, variant.origin);
        let variant_fields = self.variant_fields(variant.fields);
        if variant_payload_style_is_mixed(&variant_fields) {
            self.diag(variant.origin.span, DiagKind::MixedVariantPayloadStyle, "");
        }
        let (payload, field_tys, field_names) = lower_variant_payload(self, &variant_fields);
        let result = variant
            .result
            .map(|expr| self.lower_type_expr(expr, variant.origin));
        self.record_data_variant_name(seen_variants, &tag, variant.origin);
        let _previous_variant = variant_map.insert(
            tag,
            DataVariantDef::new(tag_value, payload, result, field_tys, field_names),
        );
    }

    fn record_data_variant_tag(
        &mut self,
        seen_tags: &mut HashSet<i64>,
        tag_value: i64,
        origin: HirOrigin,
    ) {
        if !record_data_variant_tag(seen_tags, tag_value) {
            self.diag(origin.span, DiagKind::DuplicateDataVariantDiscriminant, "");
        }
    }

    fn record_data_variant_name(
        &mut self,
        seen_variants: &mut HashMap<Box<str>, HirOrigin>,
        tag: &str,
        origin: HirOrigin,
    ) {
        let Some(previous_origin) = seen_variants.insert(tag.into(), origin) else {
            return;
        };
        self.diag_message_with_previous(
            origin.span,
            previous_origin.span,
            DiagKind::CollectDuplicateDataVariant,
            format!("duplicate data variant `{tag}`"),
            format!("data variant `{tag}` first declared here"),
        );
    }

    fn insert_record_data_variant(
        &mut self,
        data_name: &str,
        fields: SliceRange<HirFieldDef>,
        variant_map: &mut BTreeMap<Box<str>, DataVariantDef>,
    ) {
        let record_fields = self.fields(fields);
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
                data_name.into(),
                DataVariantDef::new(0, None, None, field_tys, field_names),
            );
        }
    }

    fn collect_effect_decl(&mut self, name: Ident, members: SliceRange<HirMemberDef>) {
        let effect_name: Box<str> = self.resolve_symbol(name.name).into();
        if self.effect_def(&effect_name).is_some() {
            return;
        }
        let members_vec = self.members(members);
        let mut seen_ops = HashMap::new();
        let mut seen_laws = HashMap::new();
        for member in &members_vec {
            match member.kind {
                HirMemberKind::Let => {
                    let op_name: Box<str> = self.resolve_symbol(member.name.name).into();
                    if let Some(previous_origin) = seen_ops.insert(op_name.clone(), member.origin) {
                        self.diag_message_with_previous(
                            member.origin.span,
                            previous_origin.span,
                            DiagKind::CollectDuplicateEffectOp,
                            format!("duplicate effect operation `{op_name}`"),
                            format!("effect operation `{op_name}` first declared here"),
                        );
                    }
                }
                HirMemberKind::Law => {
                    let law_name = self.resolve_symbol(member.name.name).to_owned();
                    if let Some(previous_origin) = seen_laws.insert(member.name.name, member.origin)
                    {
                        self.diag_message_with_previous(
                            member.origin.span,
                            previous_origin.span,
                            DiagKind::CollectDuplicateEffectLaw,
                            format!("duplicate effect law `{law_name}`"),
                            format!("effect law `{law_name}` first declared here"),
                        );
                    }
                }
            }
        }
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

    fn collect_class_decl(
        &mut self,
        value: HirExprId,
        name: Ident,
        type_params: SliceRange<HirBinder>,
        constraints: SliceRange<HirConstraint>,
        members: SliceRange<HirMemberDef>,
    ) {
        if self.class_id(name.name).is_some() {
            return;
        }
        let members_vec = self.members(members);
        self.validate_class_decl_members(&members_vec);
        let type_param_kinds = self.lower_type_param_kinds(type_params);
        self.push_type_param_kinds(&type_param_kinds);
        let class_members = self.collect_class_members(&members_vec);
        let laws = self.collect_class_laws(&members_vec);
        self.insert_class_id(name.name, value);
        let type_params = data_decl_type_names(&type_param_kinds);
        let type_param_kind_tys = data_decl_type_kinds(&type_param_kinds);
        let constraints = self.lower_constraints(constraints);
        self.pop_type_param_kinds();
        let facts = ClassFacts::new(
            surface_key(self.module_key(), self.interner(), name.name),
            name.name,
            class_members,
            laws,
        )
        .with_type_params(type_params)
        .with_type_param_kinds(type_param_kind_tys)
        .with_constraints(constraints);
        self.insert_class_facts(value, facts.clone());
        self.insert_class_facts_by_name(name.name, facts);
    }

    fn validate_class_decl_members(&mut self, members: &[HirMemberDef]) {
        let mut seen_members = HashMap::new();
        let mut seen_laws = HashMap::new();
        for member in members {
            match member.kind {
                HirMemberKind::Let
                    if seen_members
                        .insert(member.name.name, member.origin)
                        .is_some() =>
                {
                    let member_name = self.resolve_symbol(member.name.name).to_owned();
                    self.diag_message(
                        member.origin.span,
                        DiagKind::CollectDuplicateClassMember,
                        format!("duplicate class member `{member_name}`"),
                        format!("duplicate class member `{member_name}`"),
                    );
                }
                HirMemberKind::Law
                    if seen_laws.insert(member.name.name, member.origin).is_some() =>
                {
                    let law_name = self.resolve_symbol(member.name.name).to_owned();
                    self.diag_message(
                        member.origin.span,
                        DiagKind::CollectDuplicateClassLaw,
                        format!("duplicate class law `{law_name}`"),
                        format!("duplicate class law `{law_name}`"),
                    );
                }
                _ => {}
            }
        }
    }

    fn collect_class_members(&mut self, members: &[HirMemberDef]) -> Box<[ClassMemberFacts]> {
        members
            .iter()
            .filter(|member| member.kind == HirMemberKind::Let)
            .map(|member| member_signature(self, member, false))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn collect_class_laws(&mut self, members: &[HirMemberDef]) -> Box<[LawFacts]> {
        members
            .iter()
            .filter(|member| member.kind == HirMemberKind::Law)
            .map(|member| member_law_facts(self, member))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn visit_member(&mut self, member: &HirMemberDef) {
        for param in self.params(member.params.clone()) {
            if let Some(ty) = param.ty {
                self.visit_expr(ty);
            }
            if let Some(default) = param.default {
                self.visit_expr(default);
            }
        }
        if let Some(sig) = member.sig {
            self.visit_expr(sig);
        }
        if let Some(value) = member.value {
            self.visit_expr(value);
        }
    }
}

fn variant_payload_style_is_mixed(fields: &[HirVariantFieldDef]) -> bool {
    let has_named = fields.iter().any(|field| field.name.is_some());
    let has_positional = fields.iter().any(|field| field.name.is_none());
    has_named && has_positional
}
