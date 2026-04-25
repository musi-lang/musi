use std::path::Path;
use std::process::Command as ProcessCommand;

use crate::error::{MusiError, MusiResult};
use musi_project::{Project, ProjectOptions, TaskSpec, load_project_ancestor};
use musi_tooling::ToolingError;

use super::project_target::project_anchor;

pub(super) fn run_task(name: &str, target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let plan = project.task_plan(name)?;
    for task in plan {
        run_task_command(&project, &task)?;
    }
    Ok(())
}
fn run_task_command(project: &Project, task: &TaskSpec) -> MusiResult {
    println!("{}", task.command);
    #[cfg(windows)]
    let status = ProcessCommand::new("cmd")
        .args([windows_shell_run_arg(), task.command.as_str()])
        .current_dir(project.root_dir())
        .status();
    #[cfg(not(windows))]
    let status = ProcessCommand::new("sh")
        .args(["-lc", task.command.as_str()])
        .current_dir(project.root_dir())
        .status();
    let status = status.map_err(|source| ToolingError::ToolingIoFailed {
        path: project.root_dir().to_path_buf(),
        source,
    })?;
    if status.success() {
        Ok(())
    } else {
        Err(MusiError::TaskFailed {
            name: task.command.clone(),
        })
    }
}

#[cfg(windows)]
const fn windows_shell_run_arg() -> &'static str {
    concat!("/", "C")
}
