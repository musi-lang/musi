use std::path::PathBuf;

use music_module::ModuleKey;

use crate::{PackageId, Project};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageTestModule {
    pub package: PackageId,
    pub module_key: ModuleKey,
    pub path: PathBuf,
}

impl Project {
    #[must_use]
    pub fn package_test_modules(&self) -> Vec<PackageTestModule> {
        let mut modules = self
            .workspace()
            .packages
            .iter()
            .flat_map(|(package_id, package)| {
                package.module_keys.iter().filter_map(|(module_key, path)| {
                    path.file_name()
                        .and_then(|name| name.to_str())
                        .filter(|name| name.ends_with(".test.ms"))
                        .map(|_| PackageTestModule {
                            package: package_id.clone(),
                            module_key: module_key.clone(),
                            path: path.clone(),
                        })
                })
            })
            .collect::<Vec<_>>();
        modules.sort_by(|left, right| left.module_key.cmp(&right.module_key));
        modules
    }
}
