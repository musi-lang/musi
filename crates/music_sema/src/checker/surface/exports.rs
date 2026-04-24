use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirExprId, HirExprKind, HirFieldDef, HirHandleClause,
    HirLitKind, HirMatchArm, HirMemberDef, HirParam, HirPatId, HirPatKind, HirRecordItem,
    HirTemplatePart, HirTyId, HirVariantDef,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameBindingKind, NameSite, Symbol};

use super::super::{DeclState, ModuleState, TypingState};
use super::types::{SurfaceTyBuilder, lower_surface_effect_row};
use crate::BindingScheme;
use crate::api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ConstraintFacts, ConstraintSurface, DataSurface,
    DataVariantSurface, EffectOpSurface, EffectSurface, ExportedValue, GivenSurface,
    LawParamSurface, LawSurface, ShapeMemberSurface, ShapeSurface, SurfaceEffectRow, SurfaceTy,
    SurfaceTyId,
};

#[derive(Debug, Clone)]
pub(super) struct ExportBinding {
    pub(super) binding: NameBindingId,
    pub(super) name: Box<str>,
    pub(super) opaque: bool,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Clone)]
pub(super) struct ExportGiven {
    pub(super) span: Span,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Default)]
pub(super) struct ModuleExports {
    pub(super) bindings: Vec<ExportBinding>,
    pub(super) givens: Vec<ExportGiven>,
}

pub(super) struct ExportSurfaceCollector<'a, 'store> {
    module: &'a ModuleState,
    exports: &'a ModuleExports,
    tys: SurfaceTyBuilder<'store>,
}

impl<'a, 'store> ExportSurfaceCollector<'a, 'store> {
    pub(super) const fn new(
        module: &'a ModuleState,
        exports: &'a ModuleExports,
        tys: SurfaceTyBuilder<'store>,
    ) -> Self {
        Self {
            module,
            exports,
            tys,
        }
    }

    pub(super) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.finish()
    }
}

fn attr_is_musi(path: &[Box<str>]) -> bool {
    matches!(path, [head, tail] if head.as_ref() == "musi" && matches!(tail.as_ref(), "known" | "intrinsic"))
        || path.first().is_some_and(|seg| seg.as_ref() == "musi")
}

fn attr_is_reserved(path: &[Box<str>]) -> bool {
    match path {
        [head, tail]
            if head.as_ref() == "musi" && matches!(tail.as_ref(), "known" | "intrinsic") =>
        {
            true
        }
        [head] if head.as_ref() == "native" => true,
        [head] if head.as_ref() == "target" => true,
        [head] if head.as_ref() == "profile" => true,
        [head] if head.as_ref() == "lifecycle" => true,
        [head] if head.as_ref() == "repr" => true,
        [head] if head.as_ref() == "layout" => true,
        [head] if head.as_ref() == "frozen" => true,
        [head, ..] if head.as_ref() == "diag" => true,
        [head, ..] if head.as_ref() == "musi" => true,
        _ => false,
    }
}

fn lower_attr_value(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
) -> Option<AttrValue> {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => match module.resolved.module.store.lits.get(lit).kind.clone() {
            HirLitKind::String { value } => Some(AttrValue::String(value)),
            HirLitKind::Int { raw } => Some(AttrValue::Int(raw)),
            HirLitKind::Rune { value } => Some(AttrValue::Rune(value)),
            HirLitKind::Float { .. } => None,
        },
        HirExprKind::Variant { tag, args } => {
            let tag: Box<str> = interner.resolve(tag.name).into();
            let mut lowered = Vec::new();
            for arg in module.resolved.module.store.args.get(args) {
                lowered.push(lower_attr_value(module, interner, arg.expr)?);
            }
            Some(AttrValue::Variant {
                tag,
                args: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Array { items } => {
            let mut lowered = Vec::new();
            for item in module.resolved.module.store.array_items.get(items) {
                if item.spread {
                    return None;
                }
                lowered.push(lower_attr_value(module, interner, item.expr)?);
            }
            Some(AttrValue::Array {
                items: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Record { items } => {
            let mut fields = Vec::new();
            for item in module.resolved.module.store.record_items.get(items) {
                if item.spread {
                    return None;
                }
                let name = item.name?;
                let value = lower_attr_value(module, interner, item.value)?;
                fields.push(AttrRecordField {
                    name: interner.resolve(name.name).into(),
                    value,
                });
            }
            Some(AttrValue::Record {
                fields: fields.into_boxed_slice(),
            })
        }
        _ => None,
    }
}

fn lower_attr(module: &ModuleState, interner: &Interner, attr: &HirAttr) -> Option<Attr> {
    let path = module
        .resolved
        .module
        .store
        .idents
        .get(attr.path)
        .iter()
        .map(|ident| interner.resolve(ident.name).into())
        .collect::<Vec<Box<str>>>()
        .into_boxed_slice();
    let mut args = Vec::<AttrArg>::new();
    for arg in module
        .resolved
        .module
        .store
        .attr_args
        .get(attr.args.clone())
    {
        let name = arg.name.map(|ident| interner.resolve(ident.name).into());
        let value = lower_attr_value(module, interner, arg.value)?;
        args.push(AttrArg { name, value });
    }
    Some(Attr {
        path,
        args: args.into_boxed_slice(),
    })
}

fn split_export_attrs(
    module: &ModuleState,
    interner: &Interner,
    attrs: &[HirAttr],
) -> (Box<[Attr]>, Box<[Attr]>) {
    let mut inert = Vec::new();
    let mut musi = Vec::new();
    for attr in attrs {
        let Some(lowered) = lower_attr(module, interner, attr) else {
            continue;
        };
        if attr_is_reserved(&lowered.path) {
            continue;
        }
        if attr_is_musi(&lowered.path) {
            musi.push(lowered);
        } else {
            inert.push(lowered);
        }
    }
    (inert.into_boxed_slice(), musi.into_boxed_slice())
}

fn lower_type_params(symbols: &[Symbol], interner: &Interner) -> Box<[Box<str>]> {
    symbols
        .iter()
        .map(|symbol| interner.resolve(*symbol).into())
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

impl ExportSurfaceCollector<'_, '_> {
    fn lower_constraints(&mut self, constraints: &[ConstraintFacts]) -> Box<[ConstraintSurface]> {
        constraints
            .iter()
            .map(|constraint| {
                let lowered = ConstraintSurface::new(
                    self.tys.interner.resolve(constraint.name),
                    constraint.kind,
                    self.tys.lower(constraint.value),
                );
                if let Some(shape_key) = constraint.shape_key.clone() {
                    lowered.with_shape_key(shape_key)
                } else {
                    lowered
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn collect_binding_exports<T, F>(&mut self, mut lower: F) -> Box<[T]>
    where
        F: FnMut(&mut Self, &ExportBinding, Box<[Attr]>, Box<[Attr]>) -> Option<T>,
    {
        self.exports
            .bindings
            .iter()
            .filter_map(|export| {
                let (inert_attrs, musi_attrs) =
                    split_export_attrs(self.module, self.tys.interner, &export.attrs);
                lower(self, export, inert_attrs, musi_attrs)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub(super) fn collect_exported_values(
        &mut self,
        typing: &TypingState,
        decls: &DeclState,
    ) -> Box<[ExportedValue]> {
        self.collect_binding_exports(|this, export, inert_attrs, musi_attrs| {
            if typing.is_gated_binding(export.binding) {
                return None;
            }
            this.lower_exported_value(typing, decls, export, inert_attrs, musi_attrs)
        })
    }

    fn lower_exported_value(
        &mut self,
        typing: &TypingState,
        decls: &DeclState,
        export: &ExportBinding,
        inert_attrs: Box<[Attr]>,
        musi_attrs: Box<[Attr]>,
    ) -> Option<ExportedValue> {
        let symbol = self.module.resolved.names.bindings.get(export.binding).name;
        let scheme = typing.binding_schemes().get(&export.binding);
        let ty = scheme.map_or_else(
            || typing.binding_types().get(&export.binding).copied(),
            |scheme| Some(scheme.ty),
        )?;
        let value = self.lower_exported_value_base(export, scheme, ty, inert_attrs, musi_attrs);
        Some(self.attach_exported_value_metadata(value, typing, decls, export, symbol))
    }

    fn lower_exported_value_base(
        &mut self,
        export: &ExportBinding,
        scheme: Option<&BindingScheme>,
        ty: HirTyId,
        inert_attrs: Box<[Attr]>,
        musi_attrs: Box<[Attr]>,
    ) -> ExportedValue {
        ExportedValue::new(export.name.clone(), self.tys.lower(ty))
            .with_type_params(scheme.map_or_else(Box::<[Box<str>]>::default, |scheme| {
                lower_type_params(&scheme.type_params, self.tys.interner)
            }))
            .with_type_param_kinds(self.lower_exported_type_param_kinds(scheme))
            .with_param_names(scheme.map_or_else(Box::<[Box<str>]>::default, |scheme| {
                lower_type_params(&scheme.param_names, self.tys.interner)
            }))
            .with_comptime_params(scheme.map_or_else(Box::<[bool]>::default, |scheme| {
                scheme.comptime_params.clone()
            }))
            .with_constraints(
                scheme.map_or_else(Box::<[ConstraintSurface]>::default, |scheme| {
                    self.lower_constraints(&scheme.constraints)
                }),
            )
            .with_effects(scheme.map_or_else(SurfaceEffectRow::default, |scheme| {
                lower_surface_effect_row(&mut self.tys, &scheme.effects)
            }))
            .with_opaque(export.opaque)
            .with_inert_attrs(inert_attrs)
            .with_musi_attrs(musi_attrs)
    }

    fn lower_exported_type_param_kinds(
        &mut self,
        scheme: Option<&BindingScheme>,
    ) -> Box<[SurfaceTyId]> {
        scheme.map_or_else(Box::<[SurfaceTyId]>::default, |scheme| {
            scheme
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| self.tys.lower(ty))
                .collect::<Vec<_>>()
                .into_boxed_slice()
        })
    }

    fn attach_exported_value_metadata(
        &self,
        mut value: ExportedValue,
        typing: &TypingState,
        decls: &DeclState,
        export: &ExportBinding,
        symbol: Symbol,
    ) -> ExportedValue {
        if let Some(import_record_target) =
            export_import_record_target(self.module, typing, export.binding)
        {
            value = value.with_import_record_target(import_record_target);
        }
        if let Some(shape_key) = decls
            .shape_facts_by_name()
            .get(&symbol)
            .map(|facts| facts.key.clone())
        {
            value = value.with_shape_key(shape_key);
        }
        if let Some(effect_key) = decls
            .effect_def(export.name.as_ref())
            .map(|effect| effect.key().clone())
        {
            value = value.with_effect_key(effect_key);
        }
        if let Some(data_key) = decls
            .data_def(export.name.as_ref())
            .map(|data| data.key().clone())
        {
            value = value.with_data_key(data_key);
        }
        if let Some(const_int) = typing.binding_const_ints().get(&export.binding).copied() {
            value = value.with_const_int(const_int);
        }
        if let Some(comptime_value) = typing.binding_comptime_values().get(&export.binding) {
            value = value.with_comptime_value(comptime_value.clone());
        }
        if self.module.resolved.names.bindings.get(export.binding).kind
            == NameBindingKind::AttachedMethod
        {
            value = value.with_attached_method();
        }
        value
    }
}

fn export_import_record_target(
    module: &ModuleState,
    typing: &TypingState,
    binding: NameBindingId,
) -> Option<ModuleKey> {
    typing
        .binding_import_record_targets()
        .get(&binding)
        .cloned()
        .or_else(|| {
            binding_value_expr(module, module.resolved.module.root, binding)
                .and_then(|expr| expr_import_record_target(module, typing, expr))
        })
}

fn binding_value_expr(
    module: &ModuleState,
    expr_id: HirExprId,
    binding: NameBindingId,
) -> Option<HirExprId> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } => module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied()
            .find_map(|expr| binding_value_expr(module, expr, binding)),
        HirExprKind::Let { pat, value, .. } => pat_binds(module, pat, binding)
            .then_some(value)
            .or_else(|| binding_value_expr(module, value, binding)),
        _ => None,
    }
}

fn pat_binds(module: &ModuleState, pat_id: HirPatId, binding: NameBindingId) -> bool {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Bind { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            module
                .binding_id_at_site(site)
                .is_some_and(|found| found == binding)
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => module
            .resolved
            .module
            .store
            .pat_ids
            .get(items)
            .iter()
            .copied()
            .any(|item| pat_binds(module, item, binding)),
        HirPatKind::Variant { args, .. } => module
            .resolved
            .module
            .store
            .variant_pat_args
            .get(args)
            .iter()
            .any(|item| pat_binds(module, item.pat, binding)),
        HirPatKind::Record { fields } => module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(fields)
            .iter()
            .any(|field| {
                field.value.map_or_else(
                    || {
                        let site = NameSite::new(module.resolved.module.source_id, field.name.span);
                        module
                            .binding_id_at_site(site)
                            .is_some_and(|found| found == binding)
                    },
                    |value| pat_binds(module, value, binding),
                )
            }),
        HirPatKind::Or { left, right } => {
            pat_binds(module, left, binding) || pat_binds(module, right, binding)
        }
        HirPatKind::As { pat, name } => {
            pat_binds(module, pat, binding) || {
                let site = NameSite::new(module.resolved.module.source_id, name.span);
                module
                    .binding_id_at_site(site)
                    .is_some_and(|found| found == binding)
            }
        }
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => false,
    }
}

fn expr_import_record_target(
    module: &ModuleState,
    typing: &TypingState,
    expr_id: HirExprId,
) -> Option<ModuleKey> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Import { .. } => {
            let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
            module
                .resolved
                .imports
                .iter()
                .find_map(|import| (import.span == span).then_some(import.to.clone()))
        }
        HirExprKind::Name { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            let binding = module.resolved.names.refs.get(&site).copied()?;
            typing
                .binding_import_record_targets()
                .get(&binding)
                .cloned()
        }
        _ => None,
    }
}

impl ExportSurfaceCollector<'_, '_> {
    pub(super) fn collect_exported_data(&mut self, decls: &DeclState) -> Box<[DataSurface]> {
        self.collect_binding_exports(|this, export, inert_attrs, musi_attrs| {
            let data = decls.data_def(export.name.as_ref())?;
            let variants = export
                .opaque
                .then(Box::<[DataVariantSurface]>::default)
                .unwrap_or_else(|| {
                    data.variants()
                        .map(|(name, variant)| {
                            let lowered = DataVariantSurface::new(
                                name,
                                variant
                                    .field_tys()
                                    .iter()
                                    .copied()
                                    .map(|ty| this.tys.lower(ty))
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                            );
                            let lowered = if let Some(payload) = variant.payload() {
                                lowered.with_payload(this.tys.lower(payload))
                            } else {
                                lowered
                            };
                            let lowered = if let Some(result) = variant.result() {
                                lowered.with_result(this.tys.lower(result))
                            } else {
                                lowered
                            };
                            lowered
                                .with_tag(variant.tag())
                                .with_field_names(variant.field_names().to_vec().into_boxed_slice())
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                });
            let mut surface = DataSurface::new(data.key().clone(), variants)
                .with_type_params(lower_type_params(data.type_params(), this.tys.interner))
                .with_type_param_kinds(
                    data.type_param_kinds()
                        .iter()
                        .copied()
                        .map(|ty| this.tys.lower(ty))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
                .with_inert_attrs(inert_attrs)
                .with_musi_attrs(musi_attrs);
            if let Some(repr_kind) = data.repr_kind() {
                surface = surface.with_repr_kind(repr_kind);
            }
            if let Some(layout_align) = data.layout_align() {
                surface = surface.with_layout_align(layout_align);
            }
            if let Some(layout_pack) = data.layout_pack() {
                surface = surface.with_layout_pack(layout_pack);
            }
            if data.frozen() {
                surface = surface.with_frozen(true);
            }
            if data.is_record_shape() {
                surface = surface.with_record_shape(true);
            }
            Some(surface)
        })
    }

    pub(super) fn collect_exported_shapes(&mut self, decls: &DeclState) -> Box<[ShapeSurface]> {
        self.collect_binding_exports(|this, export, inert_attrs, musi_attrs| {
            let symbol = this.module.resolved.names.bindings.get(export.binding).name;
            let facts = decls.shape_facts_by_name().get(&symbol)?;
            let members = facts
                .members
                .iter()
                .map(|member| {
                    ShapeMemberSurface::new(
                        this.tys.interner.resolve(member.name),
                        member
                            .params
                            .iter()
                            .copied()
                            .map(|ty| this.tys.lower(ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        this.tys.lower(member.result),
                    )
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let laws = facts
                .laws
                .iter()
                .map(|law| {
                    LawSurface::new(
                        this.tys.interner.resolve(law.name),
                        law.params
                            .iter()
                            .map(|param| {
                                LawParamSurface::new(
                                    this.tys.interner.resolve(param.name),
                                    this.tys.lower(param.ty),
                                )
                            })
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    )
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();
            Some(
                ShapeSurface::new(facts.key.clone(), members, laws)
                    .with_type_params(lower_type_params(&facts.type_params, this.tys.interner))
                    .with_type_param_kinds(
                        facts
                            .type_param_kinds
                            .iter()
                            .copied()
                            .map(|ty| this.tys.lower(ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    )
                    .with_constraints(this.lower_constraints(&facts.constraints))
                    .with_inert_attrs(inert_attrs)
                    .with_musi_attrs(musi_attrs),
            )
        })
    }

    pub(super) fn collect_exported_effects(&mut self, decls: &DeclState) -> Box<[EffectSurface]> {
        self.collect_binding_exports(|this, export, inert_attrs, musi_attrs| {
            let effect = decls.effect_def(export.name.as_ref())?;
            let ops = export
                .opaque
                .then(Box::<[EffectOpSurface]>::default)
                .unwrap_or_else(|| {
                    effect
                        .ops()
                        .map(|(name, op)| {
                            EffectOpSurface::new(
                                name,
                                op.params()
                                    .iter()
                                    .copied()
                                    .map(|ty| this.tys.lower(ty))
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                                lower_type_params(op.param_names(), this.tys.interner),
                                this.tys.lower(op.result()),
                            )
                            .with_comptime_safe(op.is_comptime_safe())
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                });
            let laws = export
                .opaque
                .then(Box::<[LawSurface]>::default)
                .unwrap_or_else(|| {
                    effect
                        .laws()
                        .iter()
                        .map(|law| {
                            LawSurface::new(
                                this.tys.interner.resolve(law.name),
                                law.params
                                    .iter()
                                    .map(|param| {
                                        LawParamSurface::new(
                                            this.tys.interner.resolve(param.name),
                                            this.tys.lower(param.ty),
                                        )
                                    })
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                            )
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                });
            Some(
                EffectSurface::new(effect.key().clone(), ops, laws)
                    .with_inert_attrs(inert_attrs)
                    .with_musi_attrs(musi_attrs),
            )
        })
    }

    pub(super) fn collect_exported_givens(&mut self, decls: &DeclState) -> Box<[GivenSurface]> {
        decls
            .given_facts()
            .values()
            .filter_map(|facts| {
                let export = self
                    .exports
                    .givens
                    .iter()
                    .find(|export| export.span == facts.origin.span)?;
                let (inert_attrs, musi_attrs) =
                    split_export_attrs(self.module, self.tys.interner, &export.attrs);
                Some(
                    GivenSurface::new(
                        facts.shape_key.clone(),
                        facts
                            .shape_args
                            .iter()
                            .copied()
                            .map(|ty| self.tys.lower(ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        lower_type_params(&facts.member_names, self.tys.interner),
                    )
                    .with_type_params(lower_type_params(&facts.type_params, self.tys.interner))
                    .with_type_param_kinds(
                        facts
                            .type_param_kinds
                            .iter()
                            .copied()
                            .map(|ty| self.tys.lower(ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    )
                    .with_constraints(self.lower_constraints(&facts.constraints))
                    .with_inert_attrs(inert_attrs)
                    .with_musi_attrs(musi_attrs),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

pub(super) fn collect_module_exports(module: &ModuleState, interner: &Interner) -> ModuleExports {
    let mut exports = ModuleExports::default();
    let mut attr_stack = Vec::<HirAttr>::new();
    collect_expr(
        module,
        interner,
        module.resolved.module.root,
        &mut exports,
        &mut attr_stack,
    );
    exports
}

fn collect_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    let expr = module.resolved.module.store.exprs.get(expr_id);
    let start = attr_stack.len();
    if !expr.mods.attrs.is_empty() {
        attr_stack.extend_from_slice(
            module
                .resolved
                .module
                .store
                .attrs
                .get(expr.mods.attrs.clone()),
        );
    }
    if let Some(export_mod) = &expr.mods.export {
        collect_direct_exports(
            module,
            interner,
            expr_id,
            export_mod.opaque,
            exports,
            attr_stack,
        );
    }

    collect_exports_from_kind(module, interner, &expr.kind, exports, attr_stack);
    attr_stack.truncate(start);
}

fn collect_exports_from_kind(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    if collect_aggregate_exports(module, interner, kind, exports, attr_stack)
        || collect_call_like_exports(module, interner, kind, exports, attr_stack)
        || collect_decl_or_control_exports(module, interner, kind, exports, attr_stack)
    {}
}

fn collect_aggregate_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            collect_expr_id_range(module, interner, *exprs, exports, attr_stack);
        }
        HirExprKind::Array { items } => {
            collect_array_exprs(module, interner, items, exports, attr_stack);
        }
        HirExprKind::Record { items } => {
            collect_recordish_exprs(module, interner, items, None, exports, attr_stack);
        }
        HirExprKind::RecordUpdate { base, items } => {
            collect_recordish_exprs(module, interner, items, Some(*base), exports, attr_stack);
        }
        HirExprKind::Template { parts } => {
            collect_template_exprs(module, interner, parts, exports, attr_stack);
        }
        _ => return false,
    }
    true
}

fn collect_call_like_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Pi { binder_ty, ret, .. } => {
            collect_pair_exprs(module, interner, *binder_ty, *ret, exports, attr_stack);
        }
        HirExprKind::Lambda { body, .. }
        | HirExprKind::Import { arg: body }
        | HirExprKind::Request { expr: body }
        | HirExprKind::Unsafe { body } => {
            collect_expr(module, interner, *body, exports, attr_stack);
        }
        HirExprKind::Pin { value, body, .. } => {
            collect_expr(module, interner, *value, exports, attr_stack);
            collect_expr(module, interner, *body, exports, attr_stack);
        }
        HirExprKind::Call { callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_arg_exprs(module, interner, args, exports, attr_stack);
        }
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_expr_id_range(module, interner, *args, exports, attr_stack);
        }
        HirExprKind::Field { base, .. }
        | HirExprKind::TypeTest { base, .. }
        | HirExprKind::TypeCast { base, .. }
        | HirExprKind::Prefix { expr: base, .. }
        | HirExprKind::PartialRange { expr: base, .. } => {
            collect_expr(module, interner, *base, exports, attr_stack);
        }
        HirExprKind::Binary { left, right, .. } => {
            collect_pair_exprs(module, interner, *left, *right, exports, attr_stack);
        }
        _ => return false,
    }
    true
}

fn collect_decl_or_control_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Let { value, .. } => {
            collect_expr(module, interner, *value, exports, attr_stack);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_match_exports(module, interner, *scrutinee, arms, exports, attr_stack);
        }
        HirExprKind::Data { variants, fields } => {
            collect_data_exports(module, interner, variants, fields, exports, attr_stack);
        }
        HirExprKind::Effect { members } | HirExprKind::Shape { members, .. } => {
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Given {
            capability,
            members,
            ..
        } => {
            collect_expr(module, interner, *capability, exports, attr_stack);
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::AnswerLit { clauses, .. } => {
            collect_handle_clause_exports(module, interner, clauses, exports, attr_stack);
        }
        HirExprKind::Handle { expr, handler } => {
            collect_expr(module, interner, *expr, exports, attr_stack);
            collect_expr(module, interner, *handler, exports, attr_stack);
        }
        HirExprKind::Resume { expr } => {
            collect_optional_expr(module, interner, *expr, exports, attr_stack);
        }
        HirExprKind::Quote { .. }
        | HirExprKind::Splice { .. }
        | HirExprKind::Error
        | HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::ArrayTy { .. }
        | HirExprKind::AnswerTy { .. }
        | HirExprKind::Variant { .. } => {}
        _ => return false,
    }
    true
}

fn collect_exprs<I>(
    module: &ModuleState,
    interner: &Interner,
    exprs: I,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) where
    I: IntoIterator<Item = HirExprId>,
{
    for expr_id in exprs {
        collect_expr(module, interner, expr_id, exports, attr_stack);
    }
}

fn collect_optional_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, expr_id, exports, attr_stack);
}

fn collect_expr_id_range(
    module: &ModuleState,
    interner: &Interner,
    exprs: SliceRange<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied(),
        exports,
        attr_stack,
    );
}

fn collect_pair_exprs(
    module: &ModuleState,
    interner: &Interner,
    left: HirExprId,
    right: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, [left, right], exports, attr_stack);
}

fn collect_array_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirArrayItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .array_items
            .get(items.clone())
            .iter()
            .map(|item| item.expr),
        exports,
        attr_stack,
    );
}

fn collect_arg_exprs(
    module: &ModuleState,
    interner: &Interner,
    args: &SliceRange<HirArg>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .args
            .get(args.clone())
            .iter()
            .map(|arg| arg.expr),
        exports,
        attr_stack,
    );
}

fn collect_template_exprs(
    module: &ModuleState,
    interner: &Interner,
    parts: &SliceRange<HirTemplatePart>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .template_parts
            .get(parts.clone())
            .iter()
            .filter_map(|part| match part {
                HirTemplatePart::Expr { expr } => Some(*expr),
                HirTemplatePart::Text { .. } => None,
            }),
        exports,
        attr_stack,
    );
}

fn collect_record_item_exports(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for item in module.resolved.module.store.record_items.get(items.clone()) {
        collect_expr(module, interner, item.value, exports, attr_stack);
    }
}

fn collect_recordish_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    base: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_record_item_exports(module, interner, items, exports, attr_stack);
    collect_optional_expr(module, interner, base, exports, attr_stack);
}

fn collect_match_exports(
    module: &ModuleState,
    interner: &Interner,
    scrutinee: HirExprId,
    arms: &SliceRange<HirMatchArm>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_expr(module, interner, scrutinee, exports, attr_stack);
    for arm in module.resolved.module.store.match_arms.get(arms.clone()) {
        collect_optional_expr(module, interner, arm.guard, exports, attr_stack);
        collect_expr(module, interner, arm.expr, exports, attr_stack);
    }
}

fn collect_data_exports(
    module: &ModuleState,
    interner: &Interner,
    variants: &SliceRange<HirVariantDef>,
    fields: &SliceRange<HirFieldDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for variant in module.resolved.module.store.variants.get(variants.clone()) {
        for field in module
            .resolved
            .module
            .store
            .variant_fields
            .get(variant.fields.clone())
        {
            collect_expr(module, interner, field.ty, exports, attr_stack);
        }
        collect_optional_expr(module, interner, variant.value, exports, attr_stack);
    }
    for field in module.resolved.module.store.fields.get(fields.clone()) {
        collect_expr(module, interner, field.ty, exports, attr_stack);
        collect_optional_expr(module, interner, field.value, exports, attr_stack);
    }
}

fn collect_param_exports(
    module: &ModuleState,
    interner: &Interner,
    params: &SliceRange<HirParam>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for param in module.resolved.module.store.params.get(params.clone()) {
        collect_optional_expr(module, interner, param.ty, exports, attr_stack);
        collect_optional_expr(module, interner, param.default, exports, attr_stack);
    }
}

fn collect_member_exports(
    module: &ModuleState,
    interner: &Interner,
    members: &SliceRange<HirMemberDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for member in module.resolved.module.store.members.get(members.clone()) {
        collect_param_exports(module, interner, &member.params, exports, attr_stack);
        collect_optional_expr(module, interner, member.sig, exports, attr_stack);
        collect_optional_expr(module, interner, member.value, exports, attr_stack);
    }
}

fn collect_handle_clause_exports(
    module: &ModuleState,
    interner: &Interner,
    clauses: &SliceRange<HirHandleClause>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for clause in module
        .resolved
        .module
        .store
        .handle_clauses
        .get(clauses.clone())
    {
        collect_expr(module, interner, clause.body, exports, attr_stack);
    }
}

fn collect_direct_exports(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match &module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Let { pat, value, .. } => {
            if matches!(
                module.resolved.module.store.exprs.get(*value).kind,
                HirExprKind::Given { .. }
            ) {
                collect_exported_given(module, *value, opaque, exports, attr_stack);
                return;
            }
            collect_export_bindings_from_pat(module, interner, *pat, opaque, exports, attr_stack);
        }
        HirExprKind::Given { .. } => {
            collect_exported_given(module, expr_id, opaque, exports, attr_stack);
        }
        _ => {}
    }
}

fn collect_exported_given(
    module: &ModuleState,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
    if opaque {
        return;
    }
    if exports.givens.iter().any(|export| export.span == span) {
        return;
    }
    exports.givens.push(ExportGiven {
        span,
        attrs: attr_stack.to_vec().into_boxed_slice(),
    });
}

fn collect_export_bindings_from_pat(
    module: &ModuleState,
    interner: &Interner,
    pat_id: HirPatId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => {}
        HirPatKind::Bind { name } => {
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
            collect_pat_id_range(module, interner, items, opaque, exports, attr_stack);
        }
        HirPatKind::Variant { args, .. } => {
            for item in module.resolved.module.store.variant_pat_args.get(args) {
                collect_export_bindings_from_pat(
                    module, interner, item.pat, opaque, exports, attr_stack,
                );
            }
        }
        HirPatKind::Record { fields } => {
            for field in module.resolved.module.store.record_pat_fields.get(fields) {
                if let Some(value) = field.value {
                    collect_export_bindings_from_pat(
                        module, interner, value, opaque, exports, attr_stack,
                    );
                } else {
                    push_export_binding(module, interner, field.name, opaque, exports, attr_stack);
                }
            }
        }
        HirPatKind::Or { left, right } => {
            collect_export_bindings_from_pat(module, interner, left, opaque, exports, attr_stack);
            collect_export_bindings_from_pat(module, interner, right, opaque, exports, attr_stack);
        }
        HirPatKind::As { pat, name } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports, attr_stack);
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
    }
}

fn collect_pat_id_range(
    module: &ModuleState,
    interner: &Interner,
    items: SliceRange<HirPatId>,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    for item in module
        .resolved
        .module
        .store
        .pat_ids
        .get(items)
        .iter()
        .copied()
    {
        collect_export_bindings_from_pat(module, interner, item, opaque, exports, attr_stack);
    }
}

fn push_export_binding(
    module: &ModuleState,
    interner: &Interner,
    name: Ident,
    opaque: bool,
    exports: &mut ModuleExports,
    attrs: &[HirAttr],
) {
    let site = NameSite::new(module.resolved.module.source_id, name.span);
    let Some(binding) = module.binding_id_at_site(site) else {
        return;
    };
    if exports
        .bindings
        .iter()
        .any(|export| export.binding == binding)
    {
        return;
    }
    exports.bindings.push(ExportBinding {
        binding,
        name: interner.resolve(name.name).into(),
        opaque,
        attrs: attrs.to_vec().into_boxed_slice(),
    });
}
