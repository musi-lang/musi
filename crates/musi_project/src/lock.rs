use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Lockfile {
    pub version: u32,
    pub packages: Vec<LockedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedPackage {
    pub name: String,
    pub version: String,
    pub source: LockedPackageSource,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum LockedPackageSource {
    Workspace,
    Registry {
        registry: String,
    },
    Git {
        url: String,
        reference: String,
        commit: String,
    },
}

impl Lockfile {
    #[must_use]
    pub const fn new(version: u32, packages: Vec<LockedPackage>) -> Self {
        Self { version, packages }
    }

    #[must_use]
    pub const fn empty(version: u32) -> Self {
        Self::new(version, Vec::new())
    }

    #[must_use]
    pub fn normalized(mut self) -> Self {
        self.packages.sort_by(|left, right| {
            left.name
                .cmp(&right.name)
                .then_with(|| left.version.cmp(&right.version))
        });
        self
    }
}

impl LockedPackage {
    #[must_use]
    pub const fn new(name: String, version: String, source: LockedPackageSource) -> Self {
        Self {
            name,
            version,
            source,
        }
    }
}

impl LockedPackageSource {
    #[must_use]
    pub const fn workspace() -> Self {
        Self::Workspace
    }

    #[must_use]
    pub fn registry(registry: impl Into<String>) -> Self {
        Self::Registry {
            registry: registry.into(),
        }
    }

    #[must_use]
    pub fn git(
        url: impl Into<String>,
        reference: impl Into<String>,
        commit: impl Into<String>,
    ) -> Self {
        Self::Git {
            url: url.into(),
            reference: reference.into(),
            commit: commit.into(),
        }
    }
}
