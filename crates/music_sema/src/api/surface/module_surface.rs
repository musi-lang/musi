use music_module::ModuleKey;

use super::{
    super::DefinitionKey, DataSurface, EffectSurface, ExportedValue, GivenSurface, ShapeSurface,
    SurfaceTy, SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSurface {
    module_key: ModuleKey,
    static_imports: Box<[ModuleKey]>,
    tys: Box<[SurfaceTy]>,
    exported_values: Box<[ExportedValue]>,
    exported_data: Box<[DataSurface]>,
    exported_shapes: Box<[ShapeSurface]>,
    exported_effects: Box<[EffectSurface]>,
    exported_givens: Box<[GivenSurface]>,
}

type ModuleSurfaceExports = (
    Box<[ExportedValue]>,
    Box<[DataSurface]>,
    Box<[ShapeSurface]>,
    Box<[EffectSurface]>,
    Box<[GivenSurface]>,
);

impl ModuleSurface {
    pub(crate) fn from_collected(
        module_key: ModuleKey,
        static_imports: Box<[ModuleKey]>,
        tys: Box<[SurfaceTy]>,
        exports: ModuleSurfaceExports,
    ) -> Self {
        Self {
            module_key,
            static_imports,
            tys,
            exported_values: exports.0,
            exported_data: exports.1,
            exported_shapes: exports.2,
            exported_effects: exports.3,
            exported_givens: exports.4,
        }
    }

    #[must_use]
    pub const fn module_key(&self) -> &ModuleKey {
        &self.module_key
    }

    #[must_use]
    pub fn static_imports(&self) -> &[ModuleKey] {
        &self.static_imports
    }

    #[must_use]
    pub fn types(&self) -> &[SurfaceTy] {
        &self.tys
    }

    #[must_use]
    pub fn exported_values(&self) -> &[ExportedValue] {
        &self.exported_values
    }

    #[must_use]
    pub fn exported_data_defs(&self) -> &[DataSurface] {
        &self.exported_data
    }

    #[must_use]
    pub fn exported_shapes(&self) -> &[ShapeSurface] {
        &self.exported_shapes
    }

    #[must_use]
    pub fn exported_effects(&self) -> &[EffectSurface] {
        &self.exported_effects
    }

    #[must_use]
    pub fn exported_givens(&self) -> &[GivenSurface] {
        &self.exported_givens
    }

    #[must_use]
    pub fn try_ty(&self, id: SurfaceTyId) -> Option<&SurfaceTy> {
        self.tys
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
    }

    #[must_use]
    pub fn exported_value(&self, name: &str) -> Option<&ExportedValue> {
        self.exported_values
            .iter()
            .find(|value| value.name.as_ref() == name)
    }

    #[must_use]
    pub fn exported_shape(&self, key: &DefinitionKey) -> Option<&ShapeSurface> {
        self.exported_shapes.iter().find(|shape| &shape.key == key)
    }

    #[must_use]
    pub fn exported_data(&self, key: &DefinitionKey) -> Option<&DataSurface> {
        self.exported_data.iter().find(|data| &data.key == key)
    }

    #[must_use]
    pub fn exported_effect(&self, key: &DefinitionKey) -> Option<&EffectSurface> {
        self.exported_effects
            .iter()
            .find(|effect| &effect.key == key)
    }
}
