use crate::ProjectResult;
use crate::errors::ProjectError;
use crate::manifest::TaskConfig;
use crate::project::model::{Project, TaskSpec, VisitedPackageNames};

impl Project {
    #[must_use]
    pub fn task(&self, name: &str) -> Option<TaskSpec> {
        let TaskConfig {
            description,
            command,
            dependencies,
        } = self.manifest.task_config(name)?;
        let task = if let Some(description) = description {
            TaskSpec::new(command).with_description(description)
        } else {
            TaskSpec::new(command)
        };
        Some(task.with_dependencies(dependencies))
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named task does not exist or the task graph contains a
    /// dependency cycle.
    pub fn task_plan(&self, name: &str) -> ProjectResult<Vec<TaskSpec>> {
        let mut order = Vec::new();
        let mut seen = VisitedPackageNames::new();
        let mut active = VisitedPackageNames::new();
        self.collect_task_plan(name, &mut seen, &mut active, &mut order)?;
        Ok(order)
    }

    fn collect_task_plan(
        &self,
        name: &str,
        seen: &mut VisitedPackageNames,
        active: &mut VisitedPackageNames,
        out: &mut Vec<TaskSpec>,
    ) -> ProjectResult {
        if !seen.insert(name.into()) {
            return Ok(());
        }
        if !active.insert(name.into()) {
            return Err(ProjectError::TaskDependencyCycle { name: name.into() });
        }
        let task = self
            .task(name)
            .ok_or_else(|| ProjectError::UnknownTask { name: name.into() })?;
        for dependency in &task.dependencies {
            self.collect_task_plan(dependency, seen, active, out)?;
        }
        let _ = active.remove(name);
        out.push(task);
        Ok(())
    }
}
