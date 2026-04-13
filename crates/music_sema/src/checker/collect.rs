use std::collections::{BTreeMap, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirBinder, HirCaseArm, HirConstraint, HirExprId, HirExprKind,
    HirFieldDef, HirMemberDef, HirMemberKind, HirOrigin, HirTemplatePart, HirTyId, HirTyKind,
    HirVariantDef,
};
use music_names::Ident;

use crate::api::ClassFacts;

use super::attrs::extract_data_layout_hints;
use super::decls::{member_law_facts, member_signature};
use super::patterns::bound_name_from_pat;
use super::surface::surface_key;
use super::{CollectPass, DataDef, DataVariantDef, DiagKind, EffectDef, EffectOpDef};

pub fn collect_module(ctx: &mut CollectPass<'_, '_, '_>) {
    ctx.collect_module();
}

impl CollectPass<'_, '_, '_> {
    fn collect_module(&mut self) {
        self.visit_expr(self.root_expr_id());
    }

    fn visit_expr(&mut self, id: HirExprId) {
        match self.expr(id).kind {
            HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
                self.visit_expr_ids(exprs);
            }
            HirExprKind::Array { items } => self.visit_array_items(items),
            HirExprKind::Record { items } | HirExprKind::RecordUpdate { items, .. } => {
                for item in self.record_items(items) {
                    self.visit_expr(item.value);
                }
                if let HirExprKind::RecordUpdate { base, .. } = self.expr(id).kind {
                    self.visit_expr(base);
                }
            }
            HirExprKind::Template { parts } => self.visit_template_parts(parts),
            HirExprKind::Pi { binder_ty, ret, .. } => {
                self.visit_expr(binder_ty);
                self.visit_expr(ret);
            }
            HirExprKind::Lambda { body, .. }
            | HirExprKind::Import { arg: body }
            | HirExprKind::Perform { expr: body } => self.visit_expr(body),
            HirExprKind::Call { callee, args } => self.visit_call(callee, args),
            HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
                self.visit_expr(callee);
                self.visit_expr_ids(args);
            }
            HirExprKind::Field { base, .. } => self.visit_expr(base),
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
            HirExprKind::Case { scrutinee, arms } => self.visit_case(scrutinee, arms),
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
            HirExprKind::Error
            | HirExprKind::Name { .. }
            | HirExprKind::Lit { .. }
            | HirExprKind::ArrayTy { .. }
            | HirExprKind::HandlerTy { .. }
            | HirExprKind::Variant { .. }
            | HirExprKind::Quote { .. }
            | HirExprKind::Splice { .. } => {}
        }
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

    fn visit_case(&mut self, scrutinee: HirExprId, arms: SliceRange<HirCaseArm>) {
        self.visit_expr(scrutinee);
        for arm in self.case_arms(arms) {
            if let Some(guard) = arm.guard {
                self.visit_expr(guard);
            }
            self.visit_expr(arm.expr);
        }
    }

    fn visit_data(&mut self, variants: SliceRange<HirVariantDef>, fields: SliceRange<HirFieldDef>) {
        for variant in self.variants(variants) {
            if let Some(arg) = variant.arg {
                self.visit_expr(arg);
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
                self.collect_data_decl(origin, &attrs, name, variants, fields);
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
        variants: SliceRange<HirVariantDef>,
        fields: SliceRange<HirFieldDef>,
    ) {
        let data_name: Box<str> = self.resolve_symbol(name.name).into();
        if self.data_def(&data_name).is_some() {
            return;
        }

        let (repr_kind, layout_align, layout_pack) = extract_data_layout_hints(self, origin, attrs);
        let mut variant_map = BTreeMap::<Box<str>, DataVariantDef>::new();
        for variant in self.variants(variants) {
            let tag: Box<str> = self.resolve_symbol(variant.name.name).into();
            let payload = variant.arg.map(|expr| {
                let origin = self.expr(expr).origin;
                self.lower_type_expr(expr, origin)
            });
            let field_tys =
                payload.map_or_else(Box::<[_]>::default, |ty| self.flatten_data_field_tys(ty));
            let prev = variant_map.insert(tag, DataVariantDef::new(payload, field_tys));
            if prev.is_some() {
                self.diag(
                    variant.origin.span,
                    DiagKind::CollectDuplicateDataVariant,
                    "",
                );
            }
        }
        if variant_map.is_empty() {
            let field_tys = self
                .fields(fields)
                .into_iter()
                .map(|field| {
                    let origin = self.expr(field.ty).origin;
                    self.lower_type_expr(field.ty, origin)
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();
            if !field_tys.is_empty() {
                let _ = variant_map.insert(data_name.clone(), DataVariantDef::new(None, field_tys));
            }
        }

        let key = surface_key(self.module_key(), self.interner(), name.name);
        self.insert_data_def(
            data_name,
            DataDef::new(key, variant_map, repr_kind, layout_align, layout_pack),
        );
    }

    fn flatten_data_field_tys(&self, ty: HirTyId) -> Box<[HirTyId]> {
        match &self.ty(ty).kind {
            HirTyKind::Tuple { items } => self.ty_ids(*items).into_boxed_slice(),
            _ => vec![ty].into_boxed_slice(),
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
                    if seen_ops.insert(op_name, member.origin).is_some() {
                        self.diag(member.origin.span, DiagKind::CollectDuplicateEffectOp, "");
                    }
                }
                HirMemberKind::Law => {
                    if seen_laws.insert(member.name.name, member.origin).is_some() {
                        self.diag(member.origin.span, DiagKind::CollectDuplicateEffectLaw, "");
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
                    EffectOpDef::new(facts.params.clone(), facts.result),
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
        let mut seen_members = HashMap::new();
        let mut seen_laws = HashMap::new();
        for member in &members_vec {
            match member.kind {
                HirMemberKind::Let
                    if seen_members
                        .insert(member.name.name, member.origin)
                        .is_some() =>
                {
                    self.diag(
                        member.origin.span,
                        DiagKind::CollectDuplicateClassMember,
                        "",
                    );
                }
                HirMemberKind::Law
                    if seen_laws.insert(member.name.name, member.origin).is_some() =>
                {
                    self.diag(member.origin.span, DiagKind::CollectDuplicateClassLaw, "");
                }
                _ => {}
            }
        }
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
        self.insert_class_id(name.name, value);
        let type_params = self
            .binders(type_params)
            .into_iter()
            .map(|binder| binder.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let constraints = self.lower_constraints(constraints);
        let facts = ClassFacts::new(
            surface_key(self.module_key(), self.interner(), name.name),
            name.name,
            class_members,
            laws,
        )
        .with_type_params(type_params)
        .with_constraints(constraints);
        self.insert_class_facts(value, facts.clone());
        self.insert_class_facts_by_name(name.name, facts);
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
