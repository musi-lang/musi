use std::collections::BTreeMap;

use music_hir::{HirExprId, HirExprKind, HirPatId, HirPatKind};
use music_module::ModuleKey;
use music_names::{Ident, NameBindingId, Symbol};

use super::super::patterns::{bind_pat, bound_name_from_pat};
use super::super::surface::import_surface_ty;
use super::super::{CheckPass, DataDef, DataVariantDef, DiagKind, EffectDef, EffectOpDef};
use crate::api::{
    ClassFacts, ClassMemberFacts, ConstraintFacts, ExportedValue, LawFacts, LawParamFacts,
    ModuleSurface, PatFacts,
};
use crate::api::{ClassSurface, DataSurface, EffectSurface, ExprFacts};

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
        if let Some(target) = self.static_import_target(self.expr(expr_id).origin.span) {
            self.set_expr_module_target(expr_id, target);
        }
        ExprFacts::new(builtins.module, arg_facts.effects)
    }
}

pub(in super::super) fn module_target_for_expr(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<ModuleKey> {
    if let Some(target) = ctx.expr_module_target(expr) {
        return Some(target.clone());
    }
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_module_target(binding).cloned()),
        HirExprKind::Field { base, name, .. } => {
            let target = module_target_for_expr(ctx, base)?;
            let env = ctx.sema_env()?;
            let surface = env.module_surface(&target)?;
            surface
                .exported_value(ctx.resolve_symbol(name.name))
                .and_then(|export| export.module_target.clone())
        }
        _ => None,
    }
}

pub(in super::super) fn module_export_for_expr(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
    name: Ident,
) -> Option<(ModuleSurface, ExportedValue)> {
    let target = module_target_for_expr(ctx, expr)?;
    let env = ctx.sema_env()?;
    let surface = env.module_surface(&target)?;
    let export = surface
        .exported_value(ctx.resolve_symbol(name.name))?
        .clone();
    Some((surface, export))
}

pub(super) fn bind_module_pattern(
    ctx: &mut CheckPass<'_, '_, '_>,
    pat: HirPatId,
    value: HirExprId,
) -> bool {
    ctx.bind_module_pattern_impl(pat, value)
}

pub(super) fn bind_imported_alias(ctx: &mut CheckPass<'_, '_, '_>, name: Ident, value: HirExprId) {
    ctx.bind_imported_alias_impl(name, value);
}

pub(in super::super) fn seed_prelude_bindings(
    ctx: &mut CheckPass<'_, '_, '_>,
    surface: &ModuleSurface,
) {
    ctx.seed_prelude_bindings_impl(surface);
}

impl CheckPass<'_, '_, '_> {
    fn seed_prelude_bindings_impl(&mut self, surface: &ModuleSurface) {
        let prelude_bindings = self.prelude_bindings();
        for (binding, symbol) in prelude_bindings {
            let name = self.resolve_symbol(symbol);
            let Some(export) = surface.exported_value(name).cloned() else {
                continue;
            };
            self.import_exported_value_binding_at(binding, surface, &export);
            if let Some(target) = export.module_target.clone() {
                self.insert_binding_module_target(binding, target);
            }
            if let Some(class_key) = export.class_key.as_ref()
                && let Some(class) = surface.exported_class(class_key)
            {
                self.import_class_alias_as(symbol, surface, class, export.opaque);
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

    fn bind_module_pattern_impl(&mut self, pat: HirPatId, value: HirExprId) -> bool {
        let Some(target) = module_target_for_expr(self, value) else {
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
        let module_ty = self.builtins().module;
        self.set_pat_facts(pat, PatFacts::new(module_ty));
        for field in self.record_pat_fields(fields) {
            let Some(export) = surface
                .exported_value(self.resolve_symbol(field.name.name))
                .cloned()
            else {
                self.diag(field.name.span, DiagKind::UnknownExport, "");
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

    fn bind_imported_alias_impl(&mut self, name: Ident, value: HirExprId) {
        let HirExprKind::Field {
            base, name: field, ..
        } = self.expr(value).kind
        else {
            return;
        };
        let Some((surface, export)) = module_export_for_expr(self, base, field) else {
            return;
        };
        self.bind_imported_module_member(name, &surface, &export);
    }

    fn bind_imported_module_member(
        &mut self,
        alias: Ident,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) {
        self.import_exported_value_binding(alias, surface, export);
        if let Some(binding) = self.binding_id_for_decl(alias)
            && let Some(target) = export.module_target.clone()
        {
            self.insert_binding_module_target(binding, target);
        }
        if let Some(class_key) = export.class_key.as_ref()
            && let Some(class) = surface.exported_class(class_key)
        {
            self.import_class_alias(alias, surface, class, export.opaque);
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

    fn import_class_alias(
        &mut self,
        alias: Ident,
        module_surface: &ModuleSurface,
        surface: &ClassSurface,
        is_opaque: bool,
    ) {
        self.import_class_alias_as(alias.name, module_surface, surface, is_opaque);
    }

    fn import_class_alias_as(
        &mut self,
        alias_name: Symbol,
        module_surface: &ModuleSurface,
        surface: &ClassSurface,
        is_opaque: bool,
    ) {
        if is_opaque {
            self.mark_sealed_class(surface.key.clone());
        }
        let members = surface
            .members
            .iter()
            .map(|member| {
                ClassMemberFacts::new(
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
            .into_boxed_slice();
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
        let constraints = surface
            .constraints
            .iter()
            .map(|constraint| {
                let lowered = ConstraintFacts::new(
                    self.intern(&constraint.name),
                    constraint.kind,
                    import_surface_ty(self, module_surface, constraint.value),
                );
                if let Some(class_key) = constraint.class_key.clone() {
                    lowered.with_class_key(class_key)
                } else {
                    lowered
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let facts = ClassFacts::new(surface.key.clone(), alias_name, members, laws)
            .with_type_params(
                surface
                    .type_params
                    .iter()
                    .map(|param| self.intern(param))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            )
            .with_constraints(constraints);
        self.insert_class_facts_by_name(alias_name, facts);
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
                        import_surface_ty(self, module_surface, op.result),
                    ),
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
                        variant
                            .payload
                            .map(|ty| import_surface_ty(self, module_surface, ty)),
                        variant
                            .field_tys
                            .iter()
                            .copied()
                            .map(|ty| import_surface_ty(self, module_surface, ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let alias_name: Box<str> = self.resolve_symbol(alias_name).into();
        self.insert_data_def(
            alias_name,
            DataDef::new(
                surface.key.clone(),
                variants,
                surface.repr_kind.clone(),
                surface.layout_align,
                surface.layout_pack,
            ),
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
        let evidence_keys = self
            .evidence_scope_for_constraints(&scheme.constraints)
            .into_keys()
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.insert_binding_scheme(binding, scheme);
        self.set_binding_evidence_keys(binding, evidence_keys);
    }
}
