use crate::artifact::StringId;
use crate::instruction::CodeEntry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub name: StringId,
    pub params: u16,
    pub locals: u16,
    pub export: bool,
    pub labels: Box<[StringId]>,
    pub code: Box<[CodeEntry]>,
}

impl MethodDescriptor {
    #[must_use]
    pub fn new(name: StringId, params: u16, locals: u16, code: Box<[CodeEntry]>) -> Self {
        Self {
            name,
            params,
            locals,
            export: false,
            labels: Box::new([]),
            code,
        }
    }

    #[must_use]
    pub const fn with_export(mut self, export: bool) -> Self {
        self.export = export;
        self
    }

    #[must_use]
    pub fn with_labels(mut self, labels: Box<[StringId]>) -> Self {
        self.labels = labels;
        self
    }
}
