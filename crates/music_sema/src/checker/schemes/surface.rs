use music_hir::HirOrigin;
use music_names::Symbol;

use crate::api::{
    ConstraintFacts, ConstraintSurface, ExportedValue, GivenFacts, GivenSurface, ModuleSurface,
    SurfaceEffectRow,
};
use crate::checker::PassBase;
use crate::checker::surface::import_surface_ty;
use crate::effects::{EffectKey, EffectRow};

use super::BindingScheme;

impl PassBase<'_, '_, '_> {
    pub fn scheme_from_export(
        &mut self,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) -> BindingScheme {
        let ctx = self;
        BindingScheme {
            type_params: export
                .type_params
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            type_param_kinds: export
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            param_names: export
                .param_names
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            comptime_params: export.comptime_params.clone(),
            constraints: export
                .constraints
                .iter()
                .map(|constraint| ctx.import_constraint_surface(surface, constraint))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            ty: import_surface_ty(ctx, surface, export.ty),
            effects: ctx.import_surface_effect_row(surface, &export.effects),
        }
    }

    pub fn given_facts_from_surface(
        &mut self,
        surface: &ModuleSurface,
        given: &GivenSurface,
    ) -> GivenFacts {
        let ctx = self;
        let shape_name =
            ctx.shape_name_from_surface(surface, given.shape_args.as_ref(), &given.shape_key);
        GivenFacts::new(
            HirOrigin::dummy(),
            given.shape_key.clone(),
            shape_name,
            given
                .shape_args
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            Vec::<Symbol>::new().into_boxed_slice(),
        )
        .with_type_params(
            given
                .type_params
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_type_param_kinds(
            given
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_constraints(
            given
                .constraints
                .iter()
                .map(|constraint| ctx.import_constraint_surface(surface, constraint))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
    }
}

impl PassBase<'_, '_, '_> {
    pub(super) fn import_constraint_surface(
        &mut self,
        surface: &ModuleSurface,
        constraint: &ConstraintSurface,
    ) -> ConstraintFacts {
        let ctx = self;
        let lowered = ConstraintFacts::new(
            ctx.intern(&constraint.name),
            constraint.kind,
            import_surface_ty(ctx, surface, constraint.value),
        );
        if let Some(shape_key) = constraint.shape_key.clone() {
            lowered.with_shape_key(shape_key)
        } else {
            lowered
        }
    }

    pub(super) fn import_surface_effect_row(
        &mut self,
        surface: &ModuleSurface,
        row: &SurfaceEffectRow,
    ) -> EffectRow {
        let ctx = self;
        let mut out = EffectRow::empty();
        for item in &row.items {
            out.add(EffectKey {
                name: item.name.clone(),
                arg: item.arg.map(|arg| import_surface_ty(ctx, surface, arg)),
            });
        }
        out.open = row
            .open
            .as_deref()
            .map(|name| ctx.fresh_open_row_name(name));
        out
    }
}
