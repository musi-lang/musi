use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestDescriptor {
    pub package: StringId,
    pub version: StringId,
    pub entry: Option<StringId>,
    pub profile: StringId,
}

impl ManifestDescriptor {
    #[must_use]
    pub const fn new(package: StringId, version: StringId, profile: StringId) -> Self {
        Self {
            package,
            version,
            entry: None,
            profile,
        }
    }

    #[must_use]
    pub const fn with_entry(mut self, entry: StringId) -> Self {
        self.entry = Some(entry);
        self
    }
}
