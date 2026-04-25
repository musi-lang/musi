use music_names::Symbol;

use crate::api::{DefinitionKey, GivenFacts, ModuleSurface, SurfaceTyId};
use crate::checker::{CheckPass, PassBase};

impl PassBase<'_, '_, '_> {
    pub(super) fn shape_name_from_surface(
        &mut self,
        surface: &ModuleSurface,
        shape_args: &[SurfaceTyId],
        shape_key: &DefinitionKey,
    ) -> Symbol {
        let ctx = self;
        for shape in surface.exported_shapes() {
            if &shape.key == shape_key {
                return ctx.intern(&shape.key.name);
            }
        }
        let _ = shape_args;
        ctx.intern(&shape_key.name)
    }
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn given_provider_name(&self, given: &GivenFacts) -> Box<str> {
        let args = given
            .shape_args
            .iter()
            .copied()
            .map(|arg| self.render_ty(arg))
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "__dict__::{}::{}[{args}]",
            given.shape_key.module.as_str(),
            given.shape_key.name
        )
        .into_boxed_str()
    }
}
