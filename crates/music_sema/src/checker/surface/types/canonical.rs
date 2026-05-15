use music_module::ModuleKey;
use music_names::{Interner, Symbol};

use crate::api::DefinitionKey;

pub fn surface_key(module_key: &ModuleKey, interner: &Interner, name: Symbol) -> DefinitionKey {
    DefinitionKey::new(module_key.clone(), interner.resolve(name))
}
