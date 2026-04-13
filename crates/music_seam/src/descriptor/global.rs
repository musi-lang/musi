use music_arena::Idx;

use crate::artifact::StringId;
use crate::descriptor::MethodDescriptor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDescriptor {
    pub name: StringId,
    pub export: bool,
    pub initializer: Option<Idx<MethodDescriptor>>,
}

impl GlobalDescriptor {
    #[must_use]
    pub const fn new(name: StringId) -> Self {
        Self {
            name,
            export: false,
            initializer: None,
        }
    }

    #[must_use]
    pub const fn with_export(mut self, export: bool) -> Self {
        self.export = export;
        self
    }

    #[must_use]
    pub const fn with_initializer(mut self, initializer: Idx<MethodDescriptor>) -> Self {
        self.initializer = Some(initializer);
        self
    }
}
