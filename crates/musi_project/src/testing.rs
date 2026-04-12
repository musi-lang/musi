use std::path::PathBuf;

use music_module::ModuleKey;
use music_session::LawSuiteModule;

use crate::{PackageId, Project, ProjectResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProjectTestTargetKind {
    Module,
    SyntheticLawSuite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectTestTargetSource {
    ModulePath,
    SyntheticModule,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectTestTarget {
    pub package: PackageId,
    pub module_key: ModuleKey,
    pub source_module_key: ModuleKey,
    pub export_name: Box<str>,
    pub path: PathBuf,
    pub kind: ProjectTestTargetKind,
    pub source: ProjectTestTargetSource,
}

impl Project {
    #[must_use]
    fn file_test_targets(&self) -> Vec<ProjectTestTarget> {
        let mut modules = self
            .workspace()
            .packages
            .iter()
            .flat_map(|(package_id, package)| {
                package.module_keys.iter().filter_map(|(module_key, path)| {
                    path.file_name()
                        .and_then(|name| name.to_str())
                        .filter(|name| name.ends_with(".test.ms"))
                        .map(|_| ProjectTestTarget {
                            package: package_id.clone(),
                            module_key: module_key.clone(),
                            source_module_key: module_key.clone(),
                            export_name: "test".into(),
                            path: path.clone(),
                            kind: ProjectTestTargetKind::Module,
                            source: ProjectTestTargetSource::ModulePath,
                        })
                })
            })
            .collect::<Vec<_>>();
        modules.sort_by(|left, right| left.module_key.cmp(&right.module_key));
        modules
    }

    /// # Errors
    ///
    /// Returns [`crate::ProjectError`] when law-suite synthesis needs a session phase that fails.
    pub fn test_targets(&self) -> ProjectResult<Vec<ProjectTestTarget>> {
        let mut targets = self.file_test_targets();
        let mut session = self.build_session()?;
        let suites = session.law_suite_modules()?;
        targets.extend(
            suites
                .iter()
                .filter_map(|suite| self.project_test_target_for_law_suite(suite)),
        );
        targets.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then_with(|| left.module_key.cmp(&right.module_key))
        });
        Ok(targets)
    }

    fn project_test_target_for_law_suite(
        &self,
        suite: &LawSuiteModule,
    ) -> Option<ProjectTestTarget> {
        let package = self
            .package_id_for_module(&suite.source_module_key)?
            .clone();
        let path = self.module_path_for_key(&suite.source_module_key)?.clone();
        Some(ProjectTestTarget {
            package,
            module_key: suite.suite_module_key.clone(),
            source_module_key: suite.source_module_key.clone(),
            export_name: suite.export_name.clone(),
            path,
            kind: ProjectTestTargetKind::SyntheticLawSuite,
            source: ProjectTestTargetSource::SyntheticModule,
        })
    }

    fn package_id_for_module(&self, module_key: &ModuleKey) -> Option<&PackageId> {
        self.workspace()
            .packages
            .iter()
            .find_map(|(package_id, package)| {
                package
                    .module_keys
                    .contains_key(module_key)
                    .then_some(package_id)
            })
    }

    fn module_path_for_key(&self, module_key: &ModuleKey) -> Option<&PathBuf> {
        self.workspace()
            .packages
            .values()
            .find_map(|package| package.module_keys.get(module_key))
    }
}
