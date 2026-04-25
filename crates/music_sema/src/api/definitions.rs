use music_module::ModuleKey;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub module: ModuleKey,
    pub name: Box<str>,
}

impl DefinitionKey {
    #[must_use]
    pub fn new<Name>(module: ModuleKey, name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            module,
            name: name.into(),
        }
    }
}
