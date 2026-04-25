use music_hir::HirTyId;
use music_names::{Interner, NameBindingKind, Symbol};

use super::super::attrs::split_export_attrs;
use super::super::types::{SurfaceTyBuilder, lower_surface_effect_row};
use super::import_record::export_import_record_target;
use super::model::{ExportBinding, ModuleExports};
use crate::BindingScheme;
use crate::api::{
    Attr, ConstraintFacts, ConstraintSurface, DataSurface, DataVariantSurface, EffectOpSurface,
    EffectSurface, ExportedValue, GivenSurface, LawParamSurface, LawSurface, ShapeMemberSurface,
    ShapeSurface, SurfaceEffectRow, SurfaceTy, SurfaceTyId,
};
use crate::checker::{DeclState, ModuleState, TypingState};

pub(in crate::checker::surface) struct ExportSurfaceCollector<'a, 'store> {
    module: &'a ModuleState,
    exports: &'a ModuleExports,
    tys: SurfaceTyBuilder<'store>,
}

impl<'a, 'store> ExportSurfaceCollector<'a, 'store> {
    pub(in crate::checker::surface) const fn new(
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

    pub(in crate::checker::surface) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.finish()
    }
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

    pub(in crate::checker::surface) fn collect_exported_values(
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
        let exported_value =
            self.lower_exported_value_base(export, scheme, ty, inert_attrs, musi_attrs);
        Some(self.attach_exported_value_metadata(exported_value, typing, decls, export, symbol))
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

impl ExportSurfaceCollector<'_, '_> {
    pub(in crate::checker::surface) fn collect_exported_data(
        &mut self,
        decls: &DeclState,
    ) -> Box<[DataSurface]> {
        self.collect_binding_exports(|this, export, inert_attrs, musi_attrs| {
            let data_def = decls.data_def(export.name.as_ref())?;
            let variants = export
                .opaque
                .then(Box::<[DataVariantSurface]>::default)
                .unwrap_or_else(|| {
                    data_def
                        .variants()
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
            let mut surface = DataSurface::new(data_def.key().clone(), variants)
                .with_type_params(lower_type_params(data_def.type_params(), this.tys.interner))
                .with_type_param_kinds(
                    data_def
                        .type_param_kinds()
                        .iter()
                        .copied()
                        .map(|ty| this.tys.lower(ty))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
                .with_inert_attrs(inert_attrs)
                .with_musi_attrs(musi_attrs);
            if let Some(repr_kind) = data_def.repr_kind() {
                surface = surface.with_repr_kind(repr_kind);
            }
            if let Some(layout_align) = data_def.layout_align() {
                surface = surface.with_layout_align(layout_align);
            }
            if let Some(layout_pack) = data_def.layout_pack() {
                surface = surface.with_layout_pack(layout_pack);
            }
            if data_def.frozen() {
                surface = surface.with_frozen(true);
            }
            if data_def.is_record_shape() {
                surface = surface.with_record_shape(true);
            }
            Some(surface)
        })
    }

    pub(in crate::checker::surface) fn collect_exported_shapes(
        &mut self,
        decls: &DeclState,
    ) -> Box<[ShapeSurface]> {
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

    pub(in crate::checker::surface) fn collect_exported_effects(
        &mut self,
        decls: &DeclState,
    ) -> Box<[EffectSurface]> {
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

    pub(in crate::checker::surface) fn collect_exported_givens(
        &mut self,
        decls: &DeclState,
    ) -> Box<[GivenSurface]> {
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
