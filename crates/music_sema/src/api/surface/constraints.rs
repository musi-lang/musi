use super::{
    super::{ConstraintKind, DefinitionKey},
    SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintSurface {
    pub name: Box<str>,
    pub kind: ConstraintKind,
    pub value: SurfaceTyId,
    pub shape_key: Option<DefinitionKey>,
}

impl ConstraintSurface {
    #[must_use]
    pub fn new<Name>(name: Name, kind: ConstraintKind, value: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            kind,
            value,
            shape_key: None,
        }
    }

    #[must_use]
    pub fn with_shape_key(mut self, shape_key: DefinitionKey) -> Self {
        self.shape_key = Some(shape_key);
        self
    }
}
