use music_il::format::{ClassDescriptor, EffectDescriptor, ForeignDescriptor, TypeDescriptor};

use crate::module::Module;

pub struct Program {
    module: Module,
}

impl Program {
    pub(crate) const fn new(module: Module) -> Self {
        Self { module }
    }

    pub(crate) const fn module(&self) -> &Module {
        &self.module
    }

    #[must_use]
    pub fn strings(&self) -> &[String] {
        &self.module.strings
    }

    #[must_use]
    pub fn types(&self) -> &[TypeDescriptor] {
        &self.module.types
    }

    #[must_use]
    pub fn effects(&self) -> &[EffectDescriptor] {
        &self.module.effects
    }

    #[must_use]
    pub fn classes(&self) -> &[ClassDescriptor] {
        &self.module.classes
    }

    #[must_use]
    pub fn foreigns(&self) -> &[ForeignDescriptor] {
        &self.module.foreigns
    }

    #[must_use]
    pub fn effect_id(&self, module_name: &str, effect_name: &str) -> Option<u16> {
        self.module
            .effects
            .iter()
            .find(|effect| effect.module_name == module_name && effect.name == effect_name)
            .map(|effect| effect.id)
    }

    #[must_use]
    pub fn effect_op_id(&self, effect_id: u16, op_name: &str) -> Option<u16> {
        self.module
            .effects
            .iter()
            .find(|effect| effect.id == effect_id)
            .and_then(|effect| {
                effect
                    .operations
                    .iter()
                    .find(|op| op.name == op_name)
                    .map(|op| op.id)
            })
    }

    #[doc(hidden)]
    #[must_use]
    pub fn from_internal(module: Module) -> Self {
        Self::new(module)
    }
}

impl From<Module> for Program {
    fn from(module: Module) -> Self {
        Self::new(module)
    }
}
