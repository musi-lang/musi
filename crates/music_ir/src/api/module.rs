use music_module::ModuleKey;
use music_sema::{DefinitionKey, ExportedValue, SurfaceTy};

use super::{
    IrCallable, IrDataDef, IrEffectDef, IrForeignDef, IrGivenDef, IrGlobal, IrMetaRecord,
    IrModuleInitPart, IrShapeDef,
};

#[derive(Debug, Clone)]
pub struct IrModule {
    module_key: ModuleKey,
    static_imports: Box<[ModuleKey]>,
    types: Box<[SurfaceTy]>,
    exports: Box<[ExportedValue]>,
    callables: Box<[IrCallable]>,
    globals: Box<[IrGlobal]>,
    init_parts: Box<[IrModuleInitPart]>,
    data_defs: Box<[IrDataDef]>,
    foreigns: Box<[IrForeignDef]>,
    effects: Box<[IrEffectDef]>,
    shapes: Box<[IrShapeDef]>,
    givens: Box<[IrGivenDef]>,
    meta: Box<[IrMetaRecord]>,
}

#[derive(Debug, Clone)]
pub struct IrModuleParts {
    pub exports: Box<[ExportedValue]>,
    pub callables: Box<[IrCallable]>,
    pub globals: Box<[IrGlobal]>,
    pub init_parts: Box<[IrModuleInitPart]>,
    pub data_defs: Box<[IrDataDef]>,
    pub foreigns: Box<[IrForeignDef]>,
    pub effects: Box<[IrEffectDef]>,
    pub shapes: Box<[IrShapeDef]>,
    pub givens: Box<[IrGivenDef]>,
    pub meta: Box<[IrMetaRecord]>,
}

impl IrModule {
    #[must_use]
    pub fn new(
        module_key: ModuleKey,
        static_imports: Box<[ModuleKey]>,
        types: Box<[SurfaceTy]>,
        parts: IrModuleParts,
    ) -> Self {
        Self {
            module_key,
            static_imports,
            types,
            exports: parts.exports,
            callables: parts.callables,
            globals: parts.globals,
            init_parts: parts.init_parts,
            data_defs: parts.data_defs,
            foreigns: parts.foreigns,
            effects: parts.effects,
            shapes: parts.shapes,
            givens: parts.givens,
            meta: parts.meta,
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
        &self.types
    }

    #[must_use]
    pub fn exports(&self) -> &[ExportedValue] {
        &self.exports
    }

    #[must_use]
    pub fn callables(&self) -> &[IrCallable] {
        &self.callables
    }

    #[must_use]
    pub fn globals(&self) -> &[IrGlobal] {
        &self.globals
    }

    #[must_use]
    pub fn init_parts(&self) -> &[IrModuleInitPart] {
        &self.init_parts
    }

    #[must_use]
    pub fn data_defs(&self) -> &[IrDataDef] {
        &self.data_defs
    }

    #[must_use]
    pub fn foreigns(&self) -> &[IrForeignDef] {
        &self.foreigns
    }

    #[must_use]
    pub fn effects(&self) -> &[IrEffectDef] {
        &self.effects
    }

    #[must_use]
    pub fn shapes(&self) -> &[IrShapeDef] {
        &self.shapes
    }

    #[must_use]
    pub fn givens(&self) -> &[IrGivenDef] {
        &self.givens
    }

    #[must_use]
    pub fn meta(&self) -> &[IrMetaRecord] {
        &self.meta
    }

    #[must_use]
    pub fn exported_value(&self, name: &str) -> Option<&ExportedValue> {
        self.exports
            .iter()
            .find(|value| value.name.as_ref() == name)
    }

    #[must_use]
    pub fn effect(&self, key: &DefinitionKey) -> Option<&IrEffectDef> {
        self.effects.iter().find(|effect| &effect.key == key)
    }

    #[must_use]
    pub fn shape(&self, key: &DefinitionKey) -> Option<&IrShapeDef> {
        self.shapes.iter().find(|shape| &shape.key == key)
    }
}
