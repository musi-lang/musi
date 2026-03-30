use std::collections::{HashMap, HashSet};

use music_basic::SourceId;
use music_resolve::ImportEnv;

#[derive(Debug, Clone)]
pub struct SessionImportModule {
    exports: Box<[String]>,
    opaque_exports: HashSet<String>,
}

impl SessionImportModule {
    #[must_use]
    pub fn new(exports: impl IntoIterator<Item = String>) -> Self {
        Self {
            exports: exports.into_iter().collect(),
            opaque_exports: HashSet::new(),
        }
    }

    #[must_use]
    pub fn with_opaque_exports(
        exports: impl IntoIterator<Item = String>,
        opaque_exports: impl IntoIterator<Item = String>,
    ) -> Self {
        Self {
            exports: exports.into_iter().collect(),
            opaque_exports: opaque_exports.into_iter().collect(),
        }
    }

    #[must_use]
    pub fn exports(&self) -> &[String] {
        &self.exports
    }

    #[must_use]
    pub fn is_export_opaque(&self, name: &str) -> bool {
        self.opaque_exports.contains(name)
    }
}

/// Simple import environment for tooling and tests.
///
/// This is intentionally string-based to avoid borrowing an interner while
/// `music_resolve` holds a mutable borrow of the compilation interner.
#[derive(Debug, Default)]
pub struct SessionImportEnv {
    modules: HashMap<String, SessionImportModule>,
}

impl SessionImportEnv {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, path: impl Into<String>, module: SessionImportModule) {
        let _prev = self.modules.insert(path.into(), module);
    }

    #[must_use]
    pub fn get(&self, path: &str) -> Option<&SessionImportModule> {
        self.modules.get(path)
    }
}

impl ImportEnv for SessionImportEnv {
    fn has_module(&self, _from: SourceId, path: &str) -> bool {
        self.modules.contains_key(path)
    }

    fn for_each_export(&self, _from: SourceId, path: &str, f: &mut dyn FnMut(&str)) {
        let Some(module) = self.modules.get(path) else {
            return;
        };
        for name in module.exports() {
            f(name.as_str());
        }
    }

    fn is_export_opaque(&self, _from: SourceId, path: &str, name: &str) -> bool {
        self.modules
            .get(path)
            .is_some_and(|m| m.is_export_opaque(name))
    }
}
