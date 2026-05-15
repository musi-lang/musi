use crate::artifact::{StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeDescriptor {
    pub name: StringId,
    pub payload_ty: Option<TypeId>,
    pub witness: Option<StringId>,
    pub dispatch_table: Option<StringId>,
    pub layout_identity: Option<TypeId>,
    pub root_visible: bool,
}

impl ShapeDescriptor {
    #[must_use]
    pub const fn new(name: StringId) -> Self {
        Self {
            name,
            payload_ty: None,
            witness: None,
            dispatch_table: None,
            layout_identity: None,
            root_visible: false,
        }
    }

    #[must_use]
    pub const fn with_payload_ty(mut self, payload_ty: TypeId) -> Self {
        self.payload_ty = Some(payload_ty);
        self
    }

    #[must_use]
    pub const fn with_witness(mut self, witness: StringId) -> Self {
        self.witness = Some(witness);
        self
    }

    #[must_use]
    pub const fn with_dispatch_table(mut self, dispatch_table: StringId) -> Self {
        self.dispatch_table = Some(dispatch_table);
        self
    }

    #[must_use]
    pub const fn with_layout_identity(mut self, layout_identity: TypeId) -> Self {
        self.layout_identity = Some(layout_identity);
        self
    }

    #[must_use]
    pub const fn with_root_visible(mut self, root_visible: bool) -> Self {
        self.root_visible = root_visible;
        self
    }
}
