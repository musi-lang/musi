mod cli;
mod diag;
mod workspace;

use std::env::current_dir;
use std::error::Error;
use std::ffi::OsStr;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Command as ProcessCommand, ExitCode};
use std::thread;
use std::time::{Duration, SystemTime};

use clap::Parser;
use cli::{Cli, Command, DiagnosticsFormatArg, FmtArgs};
use diag::{CliDiagKind, cli_error_kind};
use musi_fmt::{
    FormatError, FormatInputKind, FormatOptions, format_markdown, format_paths, format_source,
};
use musi_lsp::run_stdio_server;
use musi_native::{NativeHost, NativeTestReport};
use musi_project::{
    PackageId, Project, ProjectEntry, ProjectError, ProjectOptions, ProjectTestTarget,
    ProjectTestTargetSource, TaskSpec, load_project, load_project_ancestor,
};
use musi_rt::{Runtime, RuntimeError, RuntimeOptions, RuntimeOutputMode};
use musi_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, ToolingError, project_error_report,
    render_project_error, render_session_error, session_error_report, write_artifact_bytes,
};
use music_base::diag::{CatalogDiagnostic, DiagContext, display_catalog_or_source};
use music_sema::TargetInfo;
use music_session::{Session, SessionError};
use workspace::{selected_package_entries, selected_package_roots, uses_workspace_scope};

type MusiResult<T = ()> = Result<T, MusiError>;

const STARTER_INDEX: &str =
    "let io := import \"@std/io\";\n\nlet message := \"Hello, world!\";\nio.writeLine(message);\n";
const STARTER_TEST: &str = r#"let Testing := import "@std/testing";

let add (left : Int, right : Int) : Int := left + right;

export let test () :=
  (
    Testing.describe("add");
    Testing.it("adds values", Testing.toBe(add(2, 3), 5));
    Testing.endDescribe()
  );
"#;

#[derive(Debug)]
enum MusiError {
    ProjectModelFailed(ProjectError),
    SessionCompilationFailed(SessionError),
    RuntimeExecutionFailed(RuntimeError),
    ToolingIoFailed(ToolingError),
    FormattingFailed(FormatError),
    JsonSerializationFailed(serde_json::Error),
    MissingCurrentDirectory,
    TaskFailed {
        name: String,
    },
    UnsupportedRunArgs {
        argument: String,
    },
    PackageAlreadyInitialized {
        path: PathBuf,
    },
    MissingPackageName {
        path: PathBuf,
    },
    UnknownTarget {
        target: PathBuf,
    },
    CheckCommandFailed,
    CommandUnavailable {
        command: &'static str,
        feature: &'static str,
    },
    LspServerFailed {
        message: String,
    },
    IncompatibleCommandArgs {
        left: String,
        right: String,
    },
}

impl MusiError {
    fn diagnostic(&self) -> Option<CatalogDiagnostic<CliDiagKind>> {
        Some(CatalogDiagnostic::new(
            self.diag_kind()?,
            self.diag_context(),
        ))
    }

    const fn diag_kind(&self) -> Option<CliDiagKind> {
        cli_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::TaskFailed { name } => DiagContext::new().with("name", name),
            Self::UnsupportedRunArgs { argument } => DiagContext::new().with("argument", argument),
            Self::PackageAlreadyInitialized { path } | Self::MissingPackageName { path } => {
                DiagContext::new().with("path", path.display())
            }
            Self::UnknownTarget { target } => DiagContext::new().with("target", target.display()),
            Self::CommandUnavailable { command, feature } => DiagContext::new()
                .with("command", command)
                .with("feature", feature),
            Self::LspServerFailed { message } => DiagContext::new().with("message", message),
            Self::IncompatibleCommandArgs { left, right } => {
                DiagContext::new().with("left", left).with("right", right)
            }
            _ => DiagContext::new(),
        }
    }
}

impl From<ProjectError> for MusiError {
    fn from(source: ProjectError) -> Self {
        Self::ProjectModelFailed(source)
    }
}

impl From<SessionError> for MusiError {
    fn from(source: SessionError) -> Self {
        Self::SessionCompilationFailed(source)
    }
}

impl From<RuntimeError> for MusiError {
    fn from(source: RuntimeError) -> Self {
        Self::RuntimeExecutionFailed(source)
    }
}

impl From<ToolingError> for MusiError {
    fn from(source: ToolingError) -> Self {
        Self::ToolingIoFailed(source)
    }
}

impl From<FormatError> for MusiError {
    fn from(source: FormatError) -> Self {
        Self::FormattingFailed(source)
    }
}

impl From<serde_json::Error> for MusiError {
    fn from(source: serde_json::Error) -> Self {
        Self::JsonSerializationFailed(source)
    }
}

impl Display for MusiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_catalog_or_source(
            self.diagnostic(),
            Error::source(self),
            "CLI diagnostic missing",
            f,
        )
    }
}

impl Error for MusiError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ProjectModelFailed(source) => Some(source),
            Self::SessionCompilationFailed(source) => Some(source),
            Self::RuntimeExecutionFailed(source) => Some(source),
            Self::ToolingIoFailed(source) => Some(source),
            Self::FormattingFailed(source) => Some(source),
            Self::JsonSerializationFailed(source) => Some(source),
            _ => None,
        }
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(MusiError::CheckCommandFailed) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(1)
        }
    }
}

fn run() -> MusiResult {
    let cli = Cli::parse();
    let command = cli.command;
    match command {
        Command::Init { path } => init_package(path.as_deref()),
        Command::Check {
            target,
            workspace,
            diagnostics_format,
        } => check(target.as_deref(), workspace, diagnostics_format.into()),
        Command::Build {
            target,
            workspace,
            out,
            target_name,
        } => build(
            target.as_deref(),
            workspace,
            out.as_deref(),
            target_name.as_deref(),
        ),
        Command::Run { target, args } => run_project(target.as_deref(), &args),
        Command::Test { target, workspace } => test_project(target.as_deref(), workspace),
        Command::Task { name, target } => run_task(&name, target.as_deref()),
        Command::Info { target } => print_project_metadata(target.as_deref()),
        Command::Install { target } => install_project(target.as_deref()),
        Command::Lsp => run_stdio_server().map_err(|source| MusiError::LspServerFailed {
            message: source.to_string(),
        }),
        Command::Fmt(args) => fmt_project(&args),
        Command::Compile(_)
        | Command::Lint(_)
        | Command::Bench(_)
        | Command::Doc(_)
        | Command::Coverage(_)
        | Command::Serve(_)
        | Command::Repl(_)
        | Command::Eval(_)
        | Command::Add(_)
        | Command::Remove(_)
        | Command::Update(_)
        | Command::Outdated(_)
        | Command::Audit(_)
        | Command::Publish(_)
        | Command::Clean(_) => reserved_command_for(command),
    }
}

fn init_package(target: Option<&Path>) -> MusiResult {
    let root = match target {
        Some(path) => path.to_path_buf(),
        None => current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?,
    };
    let name = package_name_for_path(&root)?;
    if package_marker_exists(&root) {
        return Err(MusiError::PackageAlreadyInitialized { path: root });
    }
    fs::create_dir_all(&root).map_err(|source| ToolingError::ToolingIoFailed {
        path: root.clone(),
        source,
    })?;
    fs::write(
        root.join("musi.json"),
        format!(
            "{{\n  \"name\": \"{name}\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}}\n"
        ),
    )
    .map_err(|source| ToolingError::ToolingIoFailed {
        path: root.join("musi.json"),
        source,
    })?;
    fs::write(root.join("index.ms"), STARTER_INDEX).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: root.join("index.ms"),
            source,
        }
    })?;
    fs::write(root.join("add.test.ms"), STARTER_TEST).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: root.join("add.test.ms"),
            source,
        }
    })?;
    if !root.join(".gitignore").exists() {
        fs::write(root.join(".gitignore"), "musi_modules/\n").map_err(|source| {
            ToolingError::ToolingIoFailed {
                path: root.join(".gitignore"),
                source,
            }
        })?;
    }
    println!("{}", root.display());
    Ok(())
}

fn package_name_for_path(root: &Path) -> MusiResult<String> {
    let canonical_root = root.canonicalize().ok();
    let name_source = if root.file_name().is_some() {
        root
    } else {
        canonical_root.as_deref().unwrap_or(root)
    };
    name_source
        .file_name()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| OsStr::new(""))
        .to_str()
        .filter(|name| !name.is_empty())
        .map(str::to_owned)
        .ok_or_else(|| MusiError::MissingPackageName {
            path: root.to_path_buf(),
        })
}

fn package_marker_exists(root: &Path) -> bool {
    root.join("musi.json").exists()
        || root.join("index.ms").exists()
        || root.join("add.test.ms").exists()
}

fn check(
    target: Option<&Path>,
    workspace: u8,
    diagnostics_format: DiagnosticsFormat,
) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    match check_project(target, workspace) {
        Ok((project, _entries)) => {
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
        Err(CheckProjectFailure::ProjectModelFailure {
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
            Err(MusiError::CheckCommandFailed)
        }
        Err(CheckProjectFailure::SessionCompilationFailure {
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
            Err(MusiError::CheckCommandFailed)
        }
    }
}

fn build(
    target: Option<&Path>,
    workspace: u8,
    out: Option<&Path>,
    target_name: Option<&str>,
) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    if workspace > 0 && out.is_some() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--workspace".to_owned(),
            right: "--out".to_owned(),
        });
    }
    let mut options = ProjectOptions::default();
    if let Some(target_name) = target_name {
        options.target = Some(target_info(target_name));
    }
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, options)?;
    let mut session = project.build_session()?;
    for entry in resolve_project_entries(&project, target, workspace)? {
        let output = session.compile_entry(&entry.module_key)?;
        let out_path = out
            .map(Path::to_path_buf)
            .or_else(|| manifest_output_path(&project, &entry))
            .unwrap_or_else(|| entry.path.with_extension("seam"));
        write_artifact_bytes(&out_path, &output.bytes)?;
        println!("{}", out_path.display());
    }
    Ok(())
}

fn run_project(target: Option<&Path>, args: &[String]) -> MusiResult {
    if let Some(argument) = args.first() {
        return Err(MusiError::UnsupportedRunArgs {
            argument: argument.clone(),
        });
    }
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, target)?;
    let mut runtime = project_runtime(&project);
    runtime.load_root(entry.module_key.as_str())?;
    Ok(())
}

fn test_project(target: Option<&Path>, workspace: u8) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let tests = resolve_test_targets(&project, target, workspace)?;
    let mut runtime = project_runtime_with_output(&project, RuntimeOutputMode::Capture);
    register_synthetic_test_modules(&project, &tests, &mut runtime)?;
    let mut reports = Vec::new();
    for test in &tests {
        let report = runtime.run_test_export(test.module_key.as_str(), &test.export_name)?;
        let label = test_target_label(&project, test);
        reports.push(TestModuleReport { label, report });
    }
    let failed = print_test_reports(&reports);
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

fn install_project(target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    if project.lockfile_needs_write() {
        project.write_lockfile()?;
    }
    match project.modules_dir() {
        Some(path) => println!("musiModules: {}", path.display()),
        None => println!("musiModules: disabled"),
    }
    println!("globalCache: {}", project.global_cache_dir().display());
    println!("lockfile: {}", project.lockfile_path().display());
    Ok(())
}

fn print_project_metadata(target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, target)?;
    let workspace = project.workspace();
    println!("package: {}", entry.package.name);
    println!("manifest: {}", project.root_manifest_path().display());
    println!("entry: {}", entry.path.display());
    println!("module: {}", entry.module_key.as_str());
    println!("workspacePackages: {}", workspace.packages.len());
    println!("workspaceMembers: {}", workspace.members.len());
    println!("modules: {}", project.module_texts().count());
    match project.modules_dir() {
        Some(path) => println!("musiModules: {}", path.display()),
        None => println!("musiModules: disabled"),
    }
    println!("globalCache: {}", project.global_cache_dir().display());
    println!("lockfile: {}", project.lockfile_path().display());
    Ok(())
}

fn fmt_project(args: &FmtArgs) -> MusiResult {
    if args.all > 0 && !args.paths.is_empty() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--all".to_owned(),
            right: "PATH".to_owned(),
        });
    }
    if args.paths.iter().any(|path| path == Path::new("-")) {
        return fmt_stdin(args);
    }

    if args.watch > 0 {
        return watch_fmt_project(args);
    }

    let session = fmt_session(args)?;
    run_fmt_once(args, &session)
}

fn run_fmt_once(args: &FmtArgs, session: &FmtSession) -> MusiResult {
    let mut options = session.options.clone();
    options.exclude.extend(args.watch_exclude.iter().cloned());

    let roots = if args.all > 0 {
        fmt_all_roots(session)
    } else if args.paths.is_empty() {
        vec![session.base_dir.clone()]
    } else {
        args.paths
            .iter()
            .map(|path| {
                if path.is_absolute() {
                    path.clone()
                } else {
                    session.invocation_dir.join(path)
                }
            })
            .collect()
    };
    let summary = format_paths(&roots, &session.base_dir, &options, args.check > 0)?;
    if summary.is_empty() && args.permit_no_files == 0 {
        return Err(FormatError::NoFiles.into());
    }
    let changed = summary.changed_paths();
    for path in &changed {
        println!("{}", path.display());
    }
    if args.check > 0 && !changed.is_empty() {
        return Err(MusiError::CheckCommandFailed);
    }
    Ok(())
}

fn fmt_all_roots(session: &FmtSession) -> Vec<PathBuf> {
    let project = load_project_ancestor(&session.base_dir, ProjectOptions::default()).ok();
    let Some(project) = project else {
        return vec![session.base_dir.clone()];
    };
    let roots = selected_package_roots(&project);
    if roots.is_empty() {
        vec![project.root_dir().to_path_buf()]
    } else {
        roots
    }
}

fn fmt_stdin(args: &FmtArgs) -> MusiResult {
    let mut source = String::new();
    let _bytes_read = io::stdin().read_to_string(&mut source).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: PathBuf::from("-"),
            source,
        }
    })?;
    let options = fmt_session(args)?.options;
    let kind = stdin_format_kind(args)?;
    let formatted = match kind {
        FormatInputKind::Musi => format_source(&source, &options)?,
        FormatInputKind::Markdown => format_markdown(&source, &options)?,
    };
    if args.check > 0 && formatted.changed {
        return Err(MusiError::CheckCommandFailed);
    }
    if args.check == 0 {
        print!("{}", formatted.text);
    }
    Ok(())
}

fn apply_fmt_args(options: &mut FormatOptions, args: &FmtArgs) {
    if let Some(line_width) = args.line_width {
        options.line_width = line_width;
    }
    if let Some(indent_width) = args.indent_width {
        options.indent_width = indent_width;
    }
    if args.use_tabs > 0 {
        options.use_tabs = true;
    }
    if let Some(ext) = &args.ext {
        options.assume_extension = FormatInputKind::from_extension(ext);
    }
    options.exclude.extend(args.ignore.iter().cloned());
}

#[derive(Debug, Clone)]
struct FmtSession {
    base_dir: PathBuf,
    invocation_dir: PathBuf,
    options: FormatOptions,
}

fn fmt_session(args: &FmtArgs) -> MusiResult<FmtSession> {
    validate_fmt_extension(args)?;
    let current = current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?;
    if args.no_config > 0 {
        let mut options = FormatOptions::default();
        apply_fmt_args(&mut options, args);
        return Ok(FmtSession {
            base_dir: current.clone(),
            invocation_dir: current,
            options,
        });
    }
    if let Some(config) = &args.config {
        let project = load_project(config, ProjectOptions::default())?;
        let mut options = FormatOptions::from_manifest(project.manifest().fmt.as_ref());
        apply_fmt_args(&mut options, args);
        return Ok(FmtSession {
            base_dir: project.root_dir().to_path_buf(),
            invocation_dir: current,
            options,
        });
    }
    let anchor = args
        .paths
        .first()
        .cloned()
        .unwrap_or_else(|| current.clone());
    let project = load_project_ancestor(&anchor, ProjectOptions::default()).ok();
    let base_dir = project.as_ref().map_or_else(
        || current.clone(),
        |project| project.root_dir().to_path_buf(),
    );
    let mut options = project
        .as_ref()
        .map_or_else(FormatOptions::default, |project| {
            FormatOptions::from_manifest(project.manifest().fmt.as_ref())
        });
    apply_fmt_args(&mut options, args);
    Ok(FmtSession {
        base_dir,
        invocation_dir: current,
        options,
    })
}

fn stdin_format_kind(args: &FmtArgs) -> MusiResult<FormatInputKind> {
    args.ext
        .as_deref()
        .map_or(Ok(FormatInputKind::Musi), |ext| {
            FormatInputKind::from_extension(ext).ok_or_else(|| {
                FormatError::UnsupportedExtension {
                    extension: ext.to_owned(),
                }
                .into()
            })
        })
}

fn validate_fmt_extension(args: &FmtArgs) -> MusiResult {
    if let Some(ext) = &args.ext
        && FormatInputKind::from_extension(ext).is_none()
    {
        return Err(FormatError::UnsupportedExtension {
            extension: ext.to_owned(),
        }
        .into());
    }
    Ok(())
}

fn watch_fmt_project(args: &FmtArgs) -> MusiResult {
    let mut session = fmt_session(args)?;
    session
        .options
        .exclude
        .extend(args.watch_exclude.iter().cloned());
    let mut last = watch_snapshot(&session.base_dir, &session.options)?;
    run_fmt_once(args, &session)?;
    loop {
        thread::sleep(Duration::from_millis(500));
        let next = watch_snapshot(&session.base_dir, &session.options)?;
        if next != last {
            if args.no_clear_screen == 0 {
                print!("\x1b[2J\x1b[H");
            }
            run_fmt_once(args, &session)?;
            last = watch_snapshot(&session.base_dir, &session.options)?;
        }
    }
}

fn watch_snapshot(
    root: &Path,
    options: &FormatOptions,
) -> MusiResult<Vec<(PathBuf, Option<SystemTime>)>> {
    let summary = format_paths(&[root.to_path_buf()], root, options, true)?;
    let mut snapshot = Vec::new();
    for file in summary.files {
        let modified = fs::metadata(&file.path)
            .ok()
            .and_then(|metadata| metadata.modified().ok());
        snapshot.push((file.path, modified));
    }
    snapshot.sort_by(|left, right| left.0.cmp(&right.0));
    Ok(snapshot)
}

fn reserved_command_for(command: Command) -> MusiResult {
    let (command, feature, args) = match command {
        Command::Compile(args) => ("compile", "native executable output", args.args),
        Command::Lint(args) => ("lint", "linter", args.args),
        Command::Bench(args) => ("bench", "benchmark runner", args.args),
        Command::Doc(args) => ("doc", "documentation generator", args.args),
        Command::Coverage(args) => ("coverage", "coverage reporter", args.args),
        Command::Serve(args) => ("serve", "HTTP server runtime", args.args),
        Command::Repl(args) => ("repl", "interactive runtime", args.args),
        Command::Eval(args) => ("eval", "inline evaluator", args.args),
        Command::Add(args) => ("add", "package dependency writer", args.args),
        Command::Remove(args) => ("remove", "package dependency remover", args.args),
        Command::Update(args) => ("update", "package updater", args.args),
        Command::Outdated(args) => ("outdated", "package version reporter", args.args),
        Command::Audit(args) => ("audit", "package audit", args.args),
        Command::Publish(args) => ("publish", "registry publisher", args.args),
        Command::Clean(args) => ("clean", "artifact cleaner", args.args),
        _ => return Ok(()),
    };
    let _argument_count = args.len();
    Err(MusiError::CommandUnavailable { command, feature })
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

fn project_anchor(target: Option<&Path>) -> MusiResult<PathBuf> {
    let current = current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?;
    Ok(target.map_or_else(
        || current.clone(),
        |path| {
            if path.is_absolute() {
                path.to_path_buf()
            } else {
                current.join(path)
            }
        },
    ))
}

fn project_runtime(project: &Project) -> Runtime {
    project_runtime_with_output(project, RuntimeOutputMode::Inherit)
}

fn project_runtime_with_output(project: &Project, output: RuntimeOutputMode) -> Runtime {
    let mut options = RuntimeOptions::default();
    options.session.import_map = project.import_map().clone();
    options.output = output;
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

fn resolve_project_entries(
    project: &Project,
    target: Option<&Path>,
    workspace: u8,
) -> MusiResult<Vec<ProjectEntry>> {
    reject_workspace_target(workspace, target)?;
    if uses_workspace_scope(project, target, workspace) {
        return Ok(selected_package_entries(project));
    }
    Ok(vec![resolve_project_entry(project, target)?])
}

fn reject_workspace_target(workspace: u8, target: Option<&Path>) -> MusiResult {
    if workspace > 0 && target.is_some() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--workspace".to_owned(),
            right: "target".to_owned(),
        });
    }
    Ok(())
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
                return Ok(ProjectEntry::new(
                    package.id.clone(),
                    module_key.clone(),
                    path.clone(),
                ));
            }
        }
    }
    Err(MusiError::UnknownTarget { target: resolved })
}

fn resolve_test_targets(
    project: &Project,
    target: Option<&Path>,
    workspace: u8,
) -> MusiResult<Vec<ProjectTestTarget>> {
    reject_workspace_target(workspace, target)?;
    if uses_workspace_scope(project, target, workspace) {
        return Ok(project.test_targets()?);
    }
    let Some(target) = target else {
        let package = project.root_package()?;
        return package_test_targets(project, &package.id);
    };
    if target.extension().is_some_and(|ext| ext == "ms") || target.components().count() > 1 {
        let entry = resolve_project_path_entry(project, target)?;
        return Ok(vec![ProjectTestTarget::module(
            entry.package,
            entry.module_key,
            entry.path,
        )]);
    }
    let package_name = target.to_string_lossy();
    let Some(package) = project.package(package_name.as_ref()) else {
        let entry = resolve_project_path_entry(project, target)?;
        return Ok(vec![ProjectTestTarget::module(
            entry.package,
            entry.module_key,
            entry.path,
        )]);
    };
    Ok(project
        .test_targets()?
        .into_iter()
        .filter(|test| test.package == package.id)
        .collect())
}

fn package_test_targets(
    project: &Project,
    package: &PackageId,
) -> MusiResult<Vec<ProjectTestTarget>> {
    Ok(project
        .test_targets()?
        .into_iter()
        .filter(|test| &test.package == package)
        .collect())
}

fn register_synthetic_test_modules(
    project: &Project,
    tests: &[ProjectTestTarget],
    runtime: &mut Runtime,
) -> MusiResult {
    if !tests
        .iter()
        .any(|target| matches!(target.source, ProjectTestTargetSource::SyntheticModule))
    {
        return Ok(());
    }
    let mut session = project.build_session()?;
    let _ = session.law_suite_modules()?;
    for target in tests {
        if !matches!(target.source, ProjectTestTargetSource::SyntheticModule) {
            continue;
        }
        let Some(source) = session.module_text(&target.module_key) else {
            return Err(MusiError::SessionCompilationFailed(
                SessionError::ModuleNotRegistered {
                    key: target.module_key.clone(),
                },
            ));
        };
        runtime.register_module_text(target.module_key.as_str(), source.to_owned())?;
    }
    Ok(())
}

fn manifest_output_path(project: &Project, entry: &ProjectEntry) -> Option<PathBuf> {
    project
        .workspace()
        .packages
        .get(&entry.package)
        .and_then(|package| {
            package
                .manifest
                .compile
                .as_ref()
                .and_then(|compile| compile.output.as_deref())
                .map(|output| package.root_dir.join(output))
        })
}

fn target_info(target_name: &str) -> TargetInfo {
    TargetInfo::new().with_os(target_name)
}

struct TestModuleReport {
    label: String,
    report: NativeTestReport,
}

fn print_test_reports(reports: &[TestModuleReport]) -> usize {
    let mut passed_files = 0usize;
    let mut failed_files = 0usize;
    let mut passed_cases = 0usize;
    let mut failed_cases = 0usize;
    for module in reports {
        let module_failed = module
            .report
            .cases
            .iter()
            .any(|test_case| !test_case.passed);
        if module_failed {
            failed_files += 1;
        } else {
            passed_files += 1;
        }
        let status = if module_failed { "×" } else { "✓" };
        println!(" {status} {} ({})", module.label, module.report.cases.len());
        for test_case in &module.report.cases {
            let status = if test_case.passed {
                passed_cases += 1;
                "✓"
            } else {
                failed_cases += 1;
                "×"
            };
            println!(
                "   {status} {} > {}",
                test_case.suite.as_ref(),
                test_case.name.as_ref()
            );
        }
        if module_failed {
            print_captured_output(&module.label, "stdout", module.report.stdout.as_ref());
            print_captured_output(&module.label, "stderr", module.report.stderr.as_ref());
        }
    }
    println!();
    println!(
        " Test Files  {} passed{} ({})",
        passed_files,
        failed_summary_suffix(failed_files),
        reports.len()
    );
    println!(
        "      Tests  {} passed{} ({})",
        passed_cases,
        failed_summary_suffix(failed_cases),
        passed_cases + failed_cases
    );
    failed_cases
}

fn print_captured_output(module: &str, stream: &str, output: &str) {
    if output.is_empty() {
        return;
    }
    println!("   --- {module} {stream} ---");
    for line in output.lines() {
        println!("   {line}");
    }
}

fn failed_summary_suffix(failed: usize) -> String {
    if failed == 0 {
        String::new()
    } else {
        format!(", {failed} failed")
    }
}

fn test_target_label(project: &Project, test: &ProjectTestTarget) -> String {
    test.path.strip_prefix(project.root_dir()).map_or_else(
        |_| test.module_key.as_str().to_owned(),
        |path| path.display().to_string(),
    )
}

fn ok_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport::ok(tool, command, package_root, manifest)
}

enum CheckProjectFailure {
    ProjectModelFailure {
        package_root: Option<PathBuf>,
        manifest: Option<PathBuf>,
        error: Box<ProjectError>,
    },
    SessionCompilationFailure {
        project: Box<Project>,
        session: Box<Session>,
        error: Box<SessionError>,
    },
}

fn check_project(
    target: Option<&Path>,
    workspace: u8,
) -> Result<(Project, Vec<ProjectEntry>), CheckProjectFailure> {
    let anchor =
        project_anchor(target).map_err(|error| CheckProjectFailure::ProjectModelFailure {
            package_root: None,
            manifest: None,
            error: match error {
                MusiError::ProjectModelFailed(project) => Box::new(project),
                other => ProjectError::ManifestValidationFailed {
                    message: other.to_string(),
                }
                .into(),
            },
        })?;
    let project = load_project_ancestor(&anchor, ProjectOptions::default()).map_err(|error| {
        CheckProjectFailure::ProjectModelFailure {
            package_root: anchor
                .is_dir()
                .then(|| anchor.clone())
                .or_else(|| anchor.parent().map(Path::to_path_buf)),
            manifest: (anchor.file_name().is_some_and(|name| name == "musi.json"))
                .then(|| anchor.clone()),
            error: Box::new(error),
        }
    })?;
    let entries = resolve_project_entries(&project, target, workspace).map_err(|error| {
        CheckProjectFailure::ProjectModelFailure {
            package_root: Some(project.root_dir().to_path_buf()),
            manifest: Some(project.root_manifest_path().to_path_buf()),
            error: match error {
                MusiError::ProjectModelFailed(project) => Box::new(project),
                other => ProjectError::ManifestValidationFailed {
                    message: other.to_string(),
                }
                .into(),
            },
        }
    })?;
    let mut session =
        project
            .build_session()
            .map_err(|error| CheckProjectFailure::ProjectModelFailure {
                package_root: Some(project.root_dir().to_path_buf()),
                manifest: Some(project.root_manifest_path().to_path_buf()),
                error: Box::new(error),
            })?;
    for entry in &entries {
        if let Err(error) = session.check_module(&entry.module_key) {
            return Err(CheckProjectFailure::SessionCompilationFailure {
                project: Box::new(project),
                session: Box::new(session),
                error: Box::new(error),
            });
        }
    }
    Ok((project, entries))
}

impl From<DiagnosticsFormatArg> for DiagnosticsFormat {
    fn from(value: DiagnosticsFormatArg) -> Self {
        match value {
            DiagnosticsFormatArg::Text => Self::Text,
            DiagnosticsFormatArg::Json => Self::Json,
        }
    }
}
