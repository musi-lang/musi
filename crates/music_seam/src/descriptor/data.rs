use crate::artifact::StringId;
use crate::artifact::TypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariantDescriptor {
    pub name: StringId,
    pub field_tys: Box<[TypeId]>,
}

impl DataVariantDescriptor {
    #[must_use]
    pub const fn new(name: StringId, field_tys: Box<[TypeId]>) -> Self {
        Self { name, field_tys }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataDescriptor {
    pub name: StringId,
    pub variant_count: u32,
    pub field_count: u32,
    pub variants: Box<[DataVariantDescriptor]>,
    pub repr_kind: Option<StringId>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
    pub frozen: bool,
}

impl DataDescriptor {
    /// # Panics
    ///
    /// Panics if the variant count or total field count does not fit in `u32`.
    #[must_use]
    pub fn new(name: StringId, variants: Box<[DataVariantDescriptor]>) -> Self {
        let variant_count =
            u32::try_from(variants.len()).expect("data variant count should fit in u32");
        let field_count = variants
            .iter()
            .map(|variant| variant.field_tys.len())
            .sum::<usize>();
        let field_count = u32::try_from(field_count).expect("data field count should fit in u32");
        Self {
            name,
            variant_count,
            field_count,
            variants,
            repr_kind: None,
            layout_align: None,
            layout_pack: None,
            frozen: false,
        }
    }

    #[must_use]
    pub const fn with_repr_kind(mut self, repr_kind: StringId) -> Self {
        self.repr_kind = Some(repr_kind);
        self
    }

    #[must_use]
    pub const fn with_layout_align(mut self, layout_align: u32) -> Self {
        self.layout_align = Some(layout_align);
        self
    }

    #[must_use]
    pub const fn with_layout_pack(mut self, layout_pack: u32) -> Self {
        self.layout_pack = Some(layout_pack);
        self
    }

    #[must_use]
    pub const fn with_frozen(mut self, frozen: bool) -> Self {
        self.frozen = frozen;
        self
    }
}
