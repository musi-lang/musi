mod cli;

use std::env::current_dir;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command as ProcessCommand, ExitCode};

use clap::Parser;
use cli::{Cli, Command, DiagnosticsFormatArg};
use musi_native::NativeHost;
use musi_project::{
    Project, ProjectEntry, ProjectError, ProjectOptions, TaskSpec, load_project_ancestor,
};
use musi_rt::{Runtime, RuntimeOptions};
use musi_vm::{Value, ValueView};
use music_sema::TargetInfo;
use music_session::{Session, SessionError};
use music_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, project_error_report, render_project_error,
    render_session_error, session_error_report, write_artifact_bytes,
};
use thiserror::Error;

type MusiResult<T = ()> = Result<T, MusiError>;

#[derive(Debug, Error)]
enum MusiError {
    #[error(transparent)]
    Project(#[from] ProjectError),
    #[error(transparent)]
    Session(#[from] SessionError),
    #[error(transparent)]
    Runtime(#[from] musi_rt::RuntimeError),
    #[error(transparent)]
    Tooling(#[from] music_tooling::ToolingError),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error("current directory unavailable")]
    MissingCurrentDirectory,
    #[error("task `{name}` failed")]
    TaskFailed { name: String },
    #[error("run arguments unsupported")]
    RunArgsUnsupported,
    #[error("package `{name}` already exists")]
    PackageExists { name: String },
    #[error("target `{target}` not found in project")]
    UnknownTarget { target: PathBuf },
    #[error("check failed")]
    CheckFailed,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(MusiError::CheckFailed) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(1)
        }
    }
}

fn run() -> MusiResult {
    let cli = Cli::parse();
    match cli.command {
        Command::New { name } => new_package(&name),
        Command::Check {
            target,
            diagnostics_format,
        } => check(target.as_deref(), diagnostics_format.into()),
        Command::Build {
            target,
            out,
            target_name,
        } => build(target.as_deref(), out.as_deref(), target_name.as_deref()),
        Command::Run { target, args } => run_project(target.as_deref(), &args),
        Command::Test { target } => test_project(target.as_deref()),
        Command::Task { name, target } => run_task(&name, target.as_deref()),
    }
}

fn new_package(name: &str) -> MusiResult {
    let root = current_dir()
        .map_err(|_| MusiError::MissingCurrentDirectory)?
        .join(name);
    if root.exists() {
        return Err(MusiError::PackageExists {
            name: name.to_owned(),
        });
    }
    fs::create_dir_all(&root).map_err(|source| music_tooling::ToolingError::Io {
        path: root.clone(),
        source,
    })?;
    fs::write(
        root.join("musi.json"),
        format!(
            "{{\n  \"name\": \"{}\",\n  \"version\": \"0.1.0\",\n  \"main\": \"index.ms\"\n}}\n",
            name
        ),
    )
    .map_err(|source| music_tooling::ToolingError::Io {
        path: root.join("musi.json"),
        source,
    })?;
    fs::write(root.join("index.ms"), "export let main () : Int := 42;\n").map_err(|source| {
        music_tooling::ToolingError::Io {
            path: root.join("index.ms"),
            source,
        }
    })?;
    fs::write(root.join(".gitignore"), "target/\n").map_err(|source| {
        music_tooling::ToolingError::Io {
            path: root.join(".gitignore"),
            source,
        }
    })?;
    println!("{}", root.display());
    Ok(())
}

fn check(target: Option<&Path>, diagnostics_format: DiagnosticsFormat) -> MusiResult {
    match check_project(target) {
        Ok((project, _entry)) => {
            if diagnostics_format == DiagnosticsFormat::Json {
                let report = ok_report(
                    "musi",
                    "check",
                    Some(project.root_dir()),
                    Some(project.root_manifest_path()),
                );
                println!("{}", serde_json::to_string_pretty(&report)?);
            }
            Ok(())
        }
        Err(CheckProjectError::Project {
            package_root,
            manifest,
            error,
        }) => {
            match diagnostics_format {
                DiagnosticsFormat::Text => eprint!("{}", render_project_error(&error)),
                DiagnosticsFormat::Json => {
                    let report = project_error_report(
                        "musi",
                        "check",
                        package_root.as_deref(),
                        manifest.as_deref(),
                        &error,
                    );
                    println!("{}", serde_json::to_string_pretty(&report)?);
                }
            }
            Err(MusiError::CheckFailed)
        }
        Err(CheckProjectError::Session {
            project,
            session,
            error,
        }) => {
            match diagnostics_format {
                DiagnosticsFormat::Text => eprint!("{}", render_session_error(&session, &error)?),
                DiagnosticsFormat::Json => {
                    let report = session_error_report(
                        "musi",
                        "check",
                        Some(project.root_dir()),
                        Some(project.root_manifest_path()),
                        &session,
                        &error,
                    );
                    println!("{}", serde_json::to_string_pretty(&report)?);
                }
            }
            Err(MusiError::CheckFailed)
        }
    }
}

fn build(target: Option<&Path>, out: Option<&Path>, target_name: Option<&str>) -> MusiResult {
    let mut options = ProjectOptions::default();
    if let Some(target_name) = target_name {
        options.target = Some(target_info(target_name));
    }
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, options)?;
    let entry = resolve_project_entry(&project, target)?;
    let mut session = project.build_session()?;
    let output = session.compile_entry(&entry.module_key)?;
    let out_path = out
        .map(Path::to_path_buf)
        .or_else(|| manifest_output_path(&project, &entry))
        .unwrap_or_else(|| entry.path.with_extension("seam"));
    write_artifact_bytes(&out_path, &output.bytes)?;
    println!("{}", out_path.display());
    Ok(())
}

fn run_project(target: Option<&Path>, args: &[String]) -> MusiResult {
    if !args.is_empty() {
        return Err(MusiError::RunArgsUnsupported);
    }
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, target)?;
    let mut runtime = project_runtime(&project);
    runtime.load_root(entry.module_key.as_str())?;
    let value = runtime.call_export("main", &[])?;
    print_runtime_value(&runtime, &value);
    Ok(())
}

fn test_project(target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let tests = resolve_test_targets(&project, target)?;
    let mut runtime = project_runtime(&project);
    let mut failed = 0usize;
    for test in tests {
        let report = runtime.run_test_module(test.module_key.as_str())?;
        for case in &report.cases {
            let status = if case.passed { "pass" } else { "fail" };
            println!(
                "{status} {} :: {} :: {}",
                report.module, case.suite, case.name
            );
            if !case.passed {
                failed += 1;
            }
        }
    }
    if failed > 0 {
        return Err(MusiError::TaskFailed {
            name: format!("{failed} test case(s) failed"),
        });
    }
    Ok(())
}

fn run_task(name: &str, target: Option<&Path>) -> MusiResult {
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
        .args(["/C", task.command.as_str()])
        .current_dir(project.root_dir())
        .status();
    #[cfg(not(windows))]
    let status = ProcessCommand::new("sh")
        .args(["-lc", task.command.as_str()])
        .current_dir(project.root_dir())
        .status();
    let status = status.map_err(|source| music_tooling::ToolingError::Io {
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

fn project_anchor(target: Option<&Path>) -> MusiResult<PathBuf> {
    match target {
        Some(path) => Ok(path.to_path_buf()),
        None => current_dir().map_err(|_| MusiError::MissingCurrentDirectory),
    }
}

fn project_runtime(project: &Project) -> Runtime {
    let mut options = RuntimeOptions::default();
    options.session.import_map = project.import_map().clone();
    let mut runtime = Runtime::new(NativeHost::new(), options);
    for (key, text) in project.module_texts() {
        runtime
            .register_module_text(key.as_str(), text)
            .expect("project module texts should register into runtime");
    }
    runtime
}

fn resolve_project_entry(project: &Project, target: Option<&Path>) -> MusiResult<ProjectEntry> {
    let Some(target) = target else {
        return project.root_entry().cloned().map_err(Into::into);
    };
    if target.file_name().is_some_and(|name| name == "musi.json") || target.is_dir() {
        return project.root_entry().cloned().map_err(Into::into);
    }
    if target.extension().is_some_and(|ext| ext == "ms") || target.components().count() > 1 {
        return resolve_project_path_entry(project, target);
    }
    let package_name = target.to_string_lossy();
    if let Ok(entry) = project.package_entry(package_name.as_ref()) {
        return Ok(entry.clone());
    }
    resolve_project_path_entry(project, target)
}

fn resolve_project_path_entry(project: &Project, target: &Path) -> MusiResult<ProjectEntry> {
    let resolved = if target.is_absolute() {
        target.to_path_buf()
    } else {
        project.root_dir().join(target)
    };
    let resolved = resolved
        .canonicalize()
        .map_err(|_| MusiError::UnknownTarget {
            target: resolved.clone(),
        })?;
    for package in project.workspace().packages.values() {
        for (module_key, path) in &package.module_keys {
            if path == &resolved {
                return Ok(ProjectEntry {
                    package: package.id.clone(),
                    module_key: module_key.clone(),
                    path: path.clone(),
                });
            }
        }
    }
    Err(MusiError::UnknownTarget { target: resolved })
}

fn resolve_test_targets(project: &Project, target: Option<&Path>) -> MusiResult<Vec<ProjectEntry>> {
    let Some(target) = target else {
        return Ok(project
            .package_test_modules()
            .into_iter()
            .map(|test| ProjectEntry {
                package: test.package,
                module_key: test.module_key,
                path: test.path,
            })
            .collect());
    };
    if target.extension().is_some_and(|ext| ext == "ms") || target.components().count() > 1 {
        return Ok(vec![resolve_project_path_entry(project, target)?]);
    }
    let package_name = target.to_string_lossy();
    let Some(package) = project.package(package_name.as_ref()) else {
        return Ok(vec![resolve_project_path_entry(project, target)?]);
    };
    Ok(project
        .package_test_modules()
        .into_iter()
        .filter(|test| test.package == package.id)
        .map(|test| ProjectEntry {
            package: test.package,
            module_key: test.module_key,
            path: test.path,
        })
        .collect())
}

fn manifest_output_path(project: &Project, entry: &ProjectEntry) -> Option<PathBuf> {
    project
        .workspace()
        .packages
        .get(&entry.package)
        .and_then(|package| package.manifest.compile.as_ref())
        .and_then(|compile| compile.output.as_deref())
        .map(|output| project.root_dir().join(output))
}

fn target_info(target_name: &str) -> TargetInfo {
    TargetInfo {
        os: Some(target_name.into()),
        ..TargetInfo::default()
    }
}

fn print_runtime_value(runtime: &Runtime, value: &Value) {
    let rendered = match runtime
        .inspect(value)
        .expect("runtime should inspect loaded value")
    {
        ValueView::Unit => None,
        ValueView::Int(value) => Some(value.to_string()),
        ValueView::Float(value) => Some(value.to_string()),
        ValueView::Bool(value) => Some(value.to_string()),
        ValueView::String(text) => Some(text.as_str().to_owned()),
        ValueView::Syntax(term) => Some(term.term().text().to_owned()),
        ValueView::Seq(seq) => Some(format!("<seq:{}>", seq.len())),
        ValueView::Record(record) => Some(format!("<record:{}>", record.len())),
        ValueView::Data(record) => Some(format!("<data:{}:{}>", record.tag(), record.len())),
        ValueView::Closure => Some("<closure>".to_owned()),
        ValueView::Continuation => Some("<continuation>".to_owned()),
        ValueView::Type(ty) => Some(format!("<type:{}>", ty.raw())),
        ValueView::Module(spec) => Some(format!("<module:{spec}>")),
        ValueView::Foreign(foreign) => Some(format!("<foreign:{}>", foreign.raw())),
        ValueView::Effect(effect) => Some(format!("<effect:{}>", effect.raw())),
        ValueView::Class(class) => Some(format!("<class:{}>", class.raw())),
    };
    if let Some(rendered) = rendered {
        println!("{rendered}");
    }
}

fn ok_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport {
        schema: "musi.diagnostics.v1",
        tool: tool.to_owned(),
        command: command.to_owned(),
        status: "ok",
        package_root: package_root.map(|path| path.display().to_string()),
        manifest: manifest.map(|path| path.display().to_string()),
        diagnostics: Vec::new(),
    }
}

enum CheckProjectError {
    Project {
        package_root: Option<PathBuf>,
        manifest: Option<PathBuf>,
        error: ProjectError,
    },
    Session {
        project: Project,
        session: Session,
        error: SessionError,
    },
}

fn check_project(target: Option<&Path>) -> Result<(Project, ProjectEntry), CheckProjectError> {
    let anchor = project_anchor(target).map_err(|error| CheckProjectError::Project {
        package_root: None,
        manifest: None,
        error: match error {
            MusiError::Project(project) => project,
            other => ProjectError::Validation {
                message: other.to_string(),
            },
        },
    })?;
    let project = load_project_ancestor(&anchor, ProjectOptions::default()).map_err(|error| {
        CheckProjectError::Project {
            package_root: anchor
                .is_dir()
                .then(|| anchor.clone())
                .or_else(|| anchor.parent().map(Path::to_path_buf)),
            manifest: (anchor.file_name().is_some_and(|name| name == "musi.json"))
                .then(|| anchor.clone()),
            error,
        }
    })?;
    let entry =
        resolve_project_entry(&project, target).map_err(|error| CheckProjectError::Project {
            package_root: Some(project.root_dir().to_path_buf()),
            manifest: Some(project.root_manifest_path().to_path_buf()),
            error: ProjectError::Validation {
                message: error.to_string(),
            },
        })?;
    let mut session = project
        .build_session()
        .map_err(|error| CheckProjectError::Project {
            package_root: Some(project.root_dir().to_path_buf()),
            manifest: Some(project.root_manifest_path().to_path_buf()),
            error,
        })?;
    match session.check_module(&entry.module_key) {
        Ok(_) => Ok((project, entry)),
        Err(error) => Err(CheckProjectError::Session {
            project,
            session,
            error,
        }),
    }
}

impl From<DiagnosticsFormatArg> for DiagnosticsFormat {
    fn from(value: DiagnosticsFormatArg) -> Self {
        match value {
            DiagnosticsFormatArg::Text => Self::Text,
            DiagnosticsFormatArg::Json => Self::Json,
        }
    }
}
