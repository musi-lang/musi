use crate::api::{ConstraintFacts, ConstraintSurface, ExportedValue, ModuleSurface};
use crate::checker::PassBase;
use crate::checker::surface::import_surface_ty;

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
        }
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
}
