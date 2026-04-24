use std::collections::BTreeMap;

use music_base::diag::DiagContext;
use music_hir::{HirExprId, HirExprKind, HirPatId, HirPatKind, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{Ident, NameBindingId, Symbol};

use super::super::pats::{bind_pat, bound_name_from_pat};
use super::super::surface::import_surface_ty;
use super::super::{CheckPass, DataDef, DataVariantDef, DiagKind, EffectDef, EffectOpDef};
use crate::api::{
    ConstraintFacts, ConstraintSurface, ExportedValue, LawFacts, LawParamFacts, LawSurface,
    ModuleSurface, PatFacts, ShapeFacts, ShapeMemberFacts,
};
use crate::api::{DataSurface, EffectSurface, ExprFacts, ShapeSurface};

type ImportRecordTargetCtx<'a, 'ctx, 'interner, 'env> = &'a CheckPass<'ctx, 'interner, 'env>;
type ImportRecordExportCtx<'a, 'ctx, 'interner, 'env> = &'a CheckPass<'ctx, 'interner, 'env>;
type StructuralTargetCtx<'a, 'ctx, 'interner, 'env> = &'a CheckPass<'ctx, 'interner, 'env>;
type ImportRecordPatternCtx<'a, 'ctx, 'interner, 'env> = &'a mut CheckPass<'ctx, 'interner, 'env>;
type StructuralAliasCtx<'a, 'ctx, 'interner, 'env> = &'a mut CheckPass<'ctx, 'interner, 'env>;
type ImportBindingSeedCtx<'a, 'ctx, 'interner, 'env> = &'a mut CheckPass<'ctx, 'interner, 'env>;
type PreludeSeedCtx<'a, 'ctx, 'interner, 'env> = &'a mut CheckPass<'ctx, 'interner, 'env>;

impl CheckPass<'_, '_, '_> {
    pub(in super::super) fn check_import_expr(
        &mut self,
        expr_id: HirExprId,
        arg: HirExprId,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let arg_facts = super::super::exprs::check_expr(self, arg);
        let origin = self.expr(arg).origin;
        self.type_mismatch(origin, builtins.string_, arg_facts.ty);
        let ty = if let Some(target) = self.static_import_target(self.expr(expr_id).origin.span) {
            self.set_expr_import_record_target(expr_id, target.clone());
            self.import_record_ty_for_target(&target)
                .unwrap_or(builtins.unknown)
        } else {
            self.runtime_import_result_ty()
        };
        ExprFacts::new(ty, arg_facts.effects)
    }
}

pub(in super::super) fn import_record_target_for_expr(
    ctx: ImportRecordTargetCtx<'_, '_, '_, '_>,
    expr: HirExprId,
) -> Option<ModuleKey> {
    if let Some(target) = ctx.expr_import_record_target(expr) {
        return Some(target.clone());
    }
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_import_record_target(binding).cloned()),
        HirExprKind::Field { base, name, .. } => {
            let target = import_record_target_for_expr(ctx, base)?;
            let env = ctx.sema_env()?;
            let surface = env.module_surface(&target)?;
            surface
                .exported_value(ctx.resolve_symbol(name.name))
                .and_then(|export| export.import_record_target.clone())
        }
        _ => None,
    }
}

pub(in super::super) fn import_record_export_for_expr(
    ctx: ImportRecordExportCtx<'_, '_, '_, '_>,
    expr: HirExprId,
    name: Ident,
) -> Option<(ModuleSurface, ExportedValue)> {
    let target = import_record_target_for_expr(ctx, expr)?;
    let env = ctx.sema_env()?;
    let surface = env.module_surface(&target)?;
    let export = surface
        .exported_value(ctx.resolve_symbol(name.name))?
        .clone();
    Some((surface, export))
}

pub(in super::super) fn expr_has_structural_target(
    ctx: StructuralTargetCtx<'_, '_, '_, '_>,
    expr: HirExprId,
) -> bool {
    ctx.expr_has_structural_target_impl(expr)
}

pub(super) fn bind_import_record_pattern(
    ctx: ImportRecordPatternCtx<'_, '_, '_, '_>,
    pat: HirPatId,
    value: HirExprId,
) -> bool {
    ctx.bind_import_record_pattern_impl(pat, value)
}

pub(in super::super) fn bind_structural_alias(
    ctx: StructuralAliasCtx<'_, '_, '_, '_>,
    name: Ident,
    value: HirExprId,
) {
    ctx.bind_structural_alias_impl(name, value);
}

pub(in super::super) fn seed_prelude_bindings(
    ctx: PreludeSeedCtx<'_, '_, '_, '_>,
    surface: &ModuleSurface,
) {
    ctx.seed_prelude_bindings_impl(surface);
}

pub(in super::super) fn seed_import_bindings(ctx: ImportBindingSeedCtx<'_, '_, '_, '_>) {
    ctx.seed_import_bindings_impl();
}

impl CheckPass<'_, '_, '_> {
    fn import_record_ty_for_target(&mut self, target: &ModuleKey) -> Option<HirTyId> {
        let env = self.sema_env()?;
        let surface = env.module_surface(target)?;
        Some(self.import_record_ty(&surface))
    }

    fn import_record_ty(&mut self, surface: &ModuleSurface) -> HirTyId {
        let fields = surface
            .exported_values()
            .iter()
            .map(|export| {
                let name = self.intern(export.name.as_ref());
                let ty = import_surface_ty(self, surface, export.ty);
                HirTyField::new(name, ty)
            })
            .collect::<Vec<_>>();
        let fields = self.alloc_ty_fields(fields);
        self.alloc_ty(HirTyKind::Record { fields })
    }

    fn runtime_import_result_ty(&mut self) -> HirTyId {
        let result_symbol = self.intern("Result");
        let import_error = self.intern("ImportError");
        let empty_args = self.alloc_ty_list(Vec::<HirTyId>::new());
        let import_error = self.alloc_ty(HirTyKind::Named {
            name: import_error,
            args: empty_args,
        });
        let any_ty = self.builtins().any;
        let args = self.alloc_ty_list([any_ty, import_error]);
        self.alloc_ty(HirTyKind::Named {
            name: result_symbol,
            args,
        })
    }

    fn expr_has_structural_target_impl(&self, expr: HirExprId) -> bool {
        match self.expr(expr).kind {
            HirExprKind::Data { .. } | HirExprKind::Effect { .. } | HirExprKind::Shape { .. } => {
                true
            }
            HirExprKind::Let { value, .. } => self.expr_has_structural_target_impl(value),
            HirExprKind::Name { name } => {
                let text = self.resolve_symbol(name.name);
                self.data_def(text).is_some()
                    || self.effect_def(text).is_some()
                    || self.shape_facts_by_name(name.name).is_some()
            }
            HirExprKind::Field { base, name, .. } => {
                import_record_export_for_expr(self, base, name).is_some_and(|(_, export)| {
                    export.data_key.is_some()
                        || export.effect_key.is_some()
                        || export.shape_key.is_some()
                })
            }
            _ => false,
        }
    }

    fn seed_import_bindings_impl(&mut self) {
        let import_bindings = self.import_bindings();
        for binding in import_bindings {
            let Some(env) = self.sema_env() else {
                continue;
            };
            let Some(surface) = env.module_surface(&binding.from) else {
                continue;
            };
            let name = self.resolve_symbol(binding.name).to_owned();
            let Some(export) = surface.exported_value(&name).cloned() else {
                continue;
            };
            self.import_exported_value_binding_at(binding.binding, &surface, &export);
            if let Some(target) = export.import_record_target.clone() {
                self.insert_binding_import_record_target(binding.binding, target);
            }
        }
    }

    fn seed_prelude_bindings_impl(&mut self, surface: &ModuleSurface) {
        let prelude_bindings = self.prelude_bindings();
        for (binding, symbol) in prelude_bindings {
            let name = self.resolve_symbol(symbol);
            let Some(export) = surface.exported_value(name).cloned() else {
                continue;
            };
            self.import_exported_value_binding_at(binding, surface, &export);
            if let Some(target) = export.import_record_target.clone() {
                self.insert_binding_import_record_target(binding, target);
            }
            if let Some(shape_key) = export.shape_key.as_ref()
                && let Some(shape) = surface.exported_shape(shape_key)
            {
                self.import_shape_alias_as(symbol, surface, shape, export.opaque);
            }
            if let Some(effect_key) = export.effect_key.as_ref()
                && let Some(effect) = surface.exported_effect(effect_key)
            {
                self.import_effect_alias_as(symbol, surface, effect, export.opaque);
            }
            if let Some(data_key) = export.data_key.as_ref()
                && let Some(data) = surface.exported_data(data_key)
            {
                self.import_data_alias_as(symbol, surface, data, export.opaque);
            }
        }
    }

    fn bind_import_record_pattern_impl(&mut self, pat: HirPatId, value: HirExprId) -> bool {
        let Some(target) = import_record_target_for_expr(self, value) else {
            return false;
        };
        let Some(env) = self.sema_env() else {
            return false;
        };
        let Some(surface) = env.module_surface(&target) else {
            return false;
        };
        let HirPatKind::Record { fields } = self.pat(pat).kind else {
            return false;
        };
        let record_ty = self.import_record_ty(&surface);
        self.set_pat_facts(pat, PatFacts::new(record_ty));
        for field in self.record_pat_fields(fields) {
            let Some(export) = surface
                .exported_value(self.resolve_symbol(field.name.name))
                .cloned()
            else {
                let export_name = self.resolve_symbol(field.name.name).to_owned();
                self.diag_with(
                    field.name.span,
                    DiagKind::UnknownExport,
                    DiagContext::new().with("name", export_name),
                );
                continue;
            };
            let field_ty = import_surface_ty(self, &surface, export.ty);
            if let Some(value) = field.value {
                bind_pat(self, value, field_ty);
                if let Some(alias) = bound_name_from_pat(self, value) {
                    self.bind_imported_module_member(alias, &surface, &export);
                }
            } else if let Some(binding) = self.binding_id_for_decl(field.name) {
                self.insert_binding_type(binding, field_ty);
                self.bind_imported_module_member(field.name, &surface, &export);
            }
        }
        true
    }

    fn bind_structural_alias_impl(&mut self, alias: Ident, value: HirExprId) {
        match self.expr(value).kind {
            HirExprKind::Field {
                base, name: field, ..
            } => {
                let Some((surface, export)) = import_record_export_for_expr(self, base, field)
                else {
                    return;
                };
                self.bind_imported_module_member(alias, &surface, &export);
            }
            HirExprKind::Name { name } => {
                let alias_text: Box<str> = self.resolve_symbol(alias.name).into();
                let source_text: Box<str> = self.resolve_symbol(name.name).into();
                if let Some(data) = self.data_def(source_text.as_ref()).cloned() {
                    self.insert_data_def(alias_text.clone(), data);
                }
                if let Some(effect) = self.effect_def(source_text.as_ref()).cloned() {
                    self.insert_effect_def(alias_text, effect);
                }
                if let Some(facts) = self.shape_facts_by_name(name.name).cloned() {
                    self.insert_shape_facts_by_name(alias.name, facts);
                }
            }
            _ => {}
        }
    }

    fn bind_imported_module_member(
        &mut self,
        alias: Ident,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) {
        self.import_exported_value_binding(alias, surface, export);
        if let Some(binding) = self.binding_id_for_decl(alias)
            && let Some(target) = export.import_record_target.clone()
        {
            self.insert_binding_import_record_target(binding, target);
        }
        if let Some(shape_key) = export.shape_key.as_ref()
            && let Some(shape) = surface.exported_shape(shape_key)
        {
            self.import_shape_alias(alias, surface, shape, export.opaque);
        }
        if let Some(effect_key) = export.effect_key.as_ref()
            && let Some(effect) = surface.exported_effect(effect_key)
        {
            self.import_effect_alias(alias, surface, effect, export.opaque);
        }
        if let Some(data_key) = export.data_key.as_ref()
            && let Some(data) = surface.exported_data(data_key)
        {
            self.import_data_alias(alias, surface, data, export.opaque);
        }
    }

    fn import_shape_alias(
        &mut self,
        alias: Ident,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
        is_opaque: bool,
    ) {
        self.import_shape_alias_as(alias.name, module_surface, surface, is_opaque);
    }

    fn import_shape_alias_as(
        &mut self,
        alias_name: Symbol,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
        is_opaque: bool,
    ) {
        if is_opaque {
            self.mark_sealed_shape(surface.key.clone());
        }
        let members = self.import_shape_members(module_surface, surface);
        let laws = self.import_shape_laws(module_surface, surface);
        let constraints = self.import_shape_constraints(module_surface, surface);
        let facts = ShapeFacts::new(surface.key.clone(), alias_name, members, laws)
            .with_type_params(self.import_shape_type_params(surface))
            .with_type_param_kinds(self.import_shape_type_param_kinds(module_surface, surface))
            .with_constraints(constraints);
        self.insert_shape_facts_by_name(alias_name, facts);
    }

    fn import_shape_members(
        &mut self,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
    ) -> Box<[ShapeMemberFacts]> {
        surface
            .members
            .iter()
            .map(|member| {
                ShapeMemberFacts::new(
                    self.intern(&member.name),
                    member
                        .params
                        .iter()
                        .copied()
                        .map(|ty| import_surface_ty(self, module_surface, ty))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    import_surface_ty(self, module_surface, member.result),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn import_shape_laws(
        &mut self,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
    ) -> Box<[LawFacts]> {
        surface
            .laws
            .iter()
            .map(|law| self.import_shape_law(module_surface, law))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn import_shape_law(&mut self, module_surface: &ModuleSurface, law: &LawSurface) -> LawFacts {
        LawFacts::new(
            self.intern(&law.name),
            law.params
                .iter()
                .map(|param| {
                    LawParamFacts::new(
                        self.intern(&param.name),
                        import_surface_ty(self, module_surface, param.ty),
                    )
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
    }

    fn import_shape_constraints(
        &mut self,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
    ) -> Box<[ConstraintFacts]> {
        surface
            .constraints
            .iter()
            .map(|constraint| self.import_shape_constraint(module_surface, constraint))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn import_shape_constraint(
        &mut self,
        module_surface: &ModuleSurface,
        constraint: &ConstraintSurface,
    ) -> ConstraintFacts {
        let lowered = ConstraintFacts::new(
            self.intern(&constraint.name),
            constraint.kind,
            import_surface_ty(self, module_surface, constraint.value),
        );
        if let Some(shape_key) = constraint.shape_key.clone() {
            lowered.with_shape_key(shape_key)
        } else {
            lowered
        }
    }

    fn import_shape_type_params(&mut self, surface: &ShapeSurface) -> Box<[Symbol]> {
        surface
            .type_params
            .iter()
            .map(|param| self.intern(param))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn import_shape_type_param_kinds(
        &mut self,
        module_surface: &ModuleSurface,
        surface: &ShapeSurface,
    ) -> Box<[HirTyId]> {
        surface
            .type_param_kinds
            .iter()
            .copied()
            .map(|ty| import_surface_ty(self, module_surface, ty))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn import_effect_alias(
        &mut self,
        alias: Ident,
        module_surface: &ModuleSurface,
        surface: &EffectSurface,
        is_opaque: bool,
    ) {
        self.import_effect_alias_as(alias.name, module_surface, surface, is_opaque);
    }

    fn import_effect_alias_as(
        &mut self,
        alias_name: Symbol,
        module_surface: &ModuleSurface,
        surface: &EffectSurface,
        is_opaque: bool,
    ) {
        if is_opaque {
            return;
        }
        let ops = surface
            .ops
            .iter()
            .map(|op| {
                (
                    op.name.clone(),
                    EffectOpDef::new(
                        op.params
                            .iter()
                            .copied()
                            .map(|ty| import_surface_ty(self, module_surface, ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        op.param_names
                            .iter()
                            .map(|name| self.intern(name))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        import_surface_ty(self, module_surface, op.result),
                    )
                    .with_comptime_safe(op.is_comptime_safe),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let laws = surface
            .laws
            .iter()
            .map(|law| {
                LawFacts::new(
                    self.intern(&law.name),
                    law.params
                        .iter()
                        .map(|param| {
                            LawParamFacts::new(
                                self.intern(&param.name),
                                import_surface_ty(self, module_surface, param.ty),
                            )
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let alias_name: Box<str> = self.resolve_symbol(alias_name).into();
        self.insert_effect_def(alias_name, EffectDef::new(surface.key.clone(), ops, laws));
    }

    fn import_data_alias(
        &mut self,
        alias: Ident,
        module_surface: &ModuleSurface,
        surface: &DataSurface,
        is_opaque: bool,
    ) {
        self.import_data_alias_as(alias.name, module_surface, surface, is_opaque);
    }

    fn import_data_alias_as(
        &mut self,
        alias_name: Symbol,
        module_surface: &ModuleSurface,
        surface: &DataSurface,
        is_opaque: bool,
    ) {
        if is_opaque {
            return;
        }
        let variants = surface
            .variants
            .iter()
            .map(|variant| {
                (
                    variant.name.clone(),
                    DataVariantDef::new(
                        variant.tag,
                        variant
                            .payload
                            .map(|ty| import_surface_ty(self, module_surface, ty)),
                        variant
                            .result
                            .map(|ty| import_surface_ty(self, module_surface, ty)),
                        variant
                            .field_tys
                            .iter()
                            .copied()
                            .map(|ty| import_surface_ty(self, module_surface, ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        variant.field_names.clone(),
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let type_params = surface
            .type_params
            .iter()
            .map(|param| self.intern(param))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let type_param_kinds = surface
            .type_param_kinds
            .iter()
            .copied()
            .map(|ty| import_surface_ty(self, module_surface, ty))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let alias_name: Box<str> = self.resolve_symbol(alias_name).into();
        self.insert_data_def(
            alias_name,
            DataDef::new(
                surface.key.clone(),
                variants,
                surface.repr_kind.clone(),
                surface.layout_align,
                surface.layout_pack,
                surface.frozen,
            )
            .with_type_params(type_params, type_param_kinds)
            .with_record_shape(surface.is_record_shape),
        );
    }

    fn import_exported_value_binding(
        &mut self,
        alias: Ident,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) {
        let Some(binding) = self.binding_id_for_decl(alias) else {
            return;
        };
        self.import_exported_value_binding_at(binding, surface, export);
    }

    fn import_exported_value_binding_at(
        &mut self,
        binding: NameBindingId,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) {
        let scheme = self.scheme_from_export(surface, export);
        let instantiated = if scheme.type_params.is_empty() {
            Some(self.instantiate_monomorphic_scheme(&scheme))
        } else {
            None
        };
        let imported_ty = import_surface_ty(self, surface, export.ty);
        self.insert_binding_type(binding, imported_ty);
        self.insert_binding_effects(
            binding,
            instantiated.map_or_else(
                || scheme.effects.clone(),
                |instantiated| instantiated.effects,
            ),
        );
        let method_name = self.intern(export.name.as_ref());
        let evidence_keys = self
            .answer_scope_for_constraints(&scheme.constraints)
            .into_keys()
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.insert_binding_scheme(binding, scheme);
        if export.is_attached_method {
            self.insert_attached_method(method_name, binding);
        }
        if let Some(const_int) = export.const_int {
            self.insert_binding_const_int(binding, const_int);
        }
        if let Some(comptime_value) = export.comptime_value.clone() {
            self.insert_binding_comptime_value(binding, comptime_value);
        }
        self.set_binding_constraint_keys(binding, evidence_keys);
    }
}
