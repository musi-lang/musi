mod cli;

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use cli::{Cli, Command, DiagnosticsFormatArg};
use musi_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, ToolingError, load_direct_graph, read_artifact_bytes,
    render_session_error, render_tooling_error, session_error_report, tooling_error_report,
    write_artifact_bytes,
};
use musi_vm::{Program, Value, Vm, VmError, VmOptions, render_value_view};
use music_seam::descriptor::ExportTarget;
use music_seam::{AssemblyError, BINARY_VERSION, decode_binary, format_text};
use music_session::{Session, SessionError, SessionOptions};
use thiserror::Error;

type MusicResult<T = ()> = Result<T, MusicError>;

#[derive(Debug, Error)]
enum MusicError {
    #[error(transparent)]
    DirectToolingFailed(#[from] Box<ToolingError>),
    #[error(transparent)]
    SessionCompilationFailed(#[from] Box<SessionError>),
    #[error(transparent)]
    ArtifactTransportFailed(#[from] Box<AssemblyError>),
    #[error(transparent)]
    VmExecutionFailed(#[from] Box<VmError>),
    #[error(transparent)]
    JsonSerializationFailed(#[from] serde_json::Error),
    #[error("run arguments unsupported")]
    RunArgsUnsupported,
    #[error("check command failed")]
    CheckCommandFailed,
}

impl From<ToolingError> for MusicError {
    fn from(value: ToolingError) -> Self {
        Self::DirectToolingFailed(Box::new(value))
    }
}

impl From<SessionError> for MusicError {
    fn from(value: SessionError) -> Self {
        Self::SessionCompilationFailed(Box::new(value))
    }
}

impl From<AssemblyError> for MusicError {
    fn from(value: AssemblyError) -> Self {
        Self::ArtifactTransportFailed(Box::new(value))
    }
}

impl From<VmError> for MusicError {
    fn from(value: VmError) -> Self {
        Self::VmExecutionFailed(Box::new(value))
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(MusicError::CheckCommandFailed) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(1)
        }
    }
}

fn run() -> MusicResult {
    let cli = Cli::parse();
    match cli.command {
        Command::Check {
            path,
            diagnostics_format,
        } => check(&path, diagnostics_format.into()),
        Command::Build { path, out } => build(&path, out.as_deref()),
        Command::Run { path, args } => run_target(&path, &args),
        Command::Inspect { path } => inspect(&path),
        Command::Disasm { path } => disasm(&path),
    }
}

fn check(path: &Path, diagnostics_format: DiagnosticsFormat) -> MusicResult {
    match check_session(path) {
        Ok(()) => {
            if diagnostics_format == DiagnosticsFormat::Json {
                let report = ok_report("music", "check");
                println!("{}", serde_json::to_string_pretty(&report)?);
            }
            Ok(())
        }
        Err(CheckCommandFailure::DirectToolingFailure(error)) => {
            emit_tooling_check_error("music", "check", diagnostics_format, &error)
        }
        Err(CheckCommandFailure::SessionCompilationFailure { session, error }) => {
            emit_session_check_error("music", "check", diagnostics_format, &session, &error)
        }
    }
}

fn build(path: &Path, out: Option<&Path>) -> MusicResult {
    let graph = load_direct_graph(path)?;
    let mut session = graph.build_session(SessionOptions::default())?;
    let output = session.compile_entry(graph.entry_key())?;
    let out_path = out.map_or_else(|| default_seam_path(path), Path::to_path_buf);
    write_artifact_bytes(&out_path, &output.bytes)?;
    println!("{}", out_path.display());
    Ok(())
}

fn run_target(path: &Path, args: &[String]) -> MusicResult {
    if !args.is_empty() {
        return Err(MusicError::RunArgsUnsupported);
    }
    if path.extension().is_some_and(|ext| ext == "seam") {
        let bytes = read_artifact_bytes(path)?;
        let program = Program::from_bytes(&bytes)?;
        return run_program(program);
    }
    let graph = load_direct_graph(path)?;
    let mut session = graph.build_session(SessionOptions::default())?;
    let output = session.compile_entry(graph.entry_key())?;
    let program = Program::from_bytes(&output.bytes)?;
    run_program(program)
}

fn run_program(program: Program) -> MusicResult {
    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize()?;
    let value = vm.call_export("main", &[])?;
    print_vm_value(&vm, &value);
    Ok(())
}

fn inspect(path: &Path) -> MusicResult {
    let bytes = read_artifact_bytes(path)?;
    let artifact = decode_binary(&bytes)?;

    println!("binaryVersion: {BINARY_VERSION}");
    println!("strings: {}", artifact.strings.len());
    println!("types: {}", artifact.types.len());
    println!("globals: {}", artifact.globals.len());
    println!("methods: {}", artifact.methods.len());
    println!("effects: {}", artifact.effects.len());
    println!("classes: {}", artifact.classes.len());
    println!("foreigns: {}", artifact.foreigns.len());
    println!("exports: {}", artifact.exports.len());
    println!("data: {}", artifact.data.len());
    println!("meta: {}", artifact.meta.len());
    for (_, export) in artifact.exports.iter() {
        println!(
            "export {} {}{}",
            artifact.string_text(export.name),
            export_kind_name(export.target),
            if export.opaque { " opaque" } else { "" }
        );
    }
    Ok(())
}

fn disasm(path: &Path) -> MusicResult {
    let bytes = read_artifact_bytes(path)?;
    let artifact = decode_binary(&bytes)?;
    print!("{}", format_text(&artifact));
    Ok(())
}

fn print_vm_value(vm: &Vm, value: &Value) {
    let rendered = render_value_view(vm.inspect(value));
    if let Some(rendered) = rendered {
        println!("{rendered}");
    }
}

fn default_seam_path(path: &Path) -> PathBuf {
    path.with_extension("seam")
}

const fn export_kind_name(target: ExportTarget) -> &'static str {
    match target {
        ExportTarget::Method(_) => "method",
        ExportTarget::Global(_) => "global",
        ExportTarget::Foreign(_) => "foreign",
        ExportTarget::Type(_) => "type",
        ExportTarget::Effect(_) => "effect",
        ExportTarget::Class(_) => "class",
    }
}

fn ok_report(tool: &str, command: &str) -> CliDiagnosticsReport {
    CliDiagnosticsReport::ok(tool, command, None, None)
}

fn emit_tooling_check_error(
    tool: &str,
    command: &str,
    diagnostics_format: DiagnosticsFormat,
    error: &ToolingError,
) -> MusicResult {
    match diagnostics_format {
        DiagnosticsFormat::Text => {
            eprint!("{}", render_tooling_error(error));
        }
        DiagnosticsFormat::Json => {
            let report = tooling_error_report(tool, command, None, None, error);
            println!("{}", serde_json::to_string_pretty(&report)?);
        }
    }
    Err(MusicError::CheckCommandFailed)
}

fn emit_session_check_error(
    tool: &str,
    command: &str,
    diagnostics_format: DiagnosticsFormat,
    session: &Session,
    error: &SessionError,
) -> MusicResult {
    match diagnostics_format {
        DiagnosticsFormat::Text => {
            eprint!("{}", render_session_error(session, error)?);
        }
        DiagnosticsFormat::Json => {
            let report = session_error_report(tool, command, None, None, session, error);
            println!("{}", serde_json::to_string_pretty(&report)?);
        }
    }
    Err(MusicError::CheckCommandFailed)
}

enum CheckCommandFailure {
    DirectToolingFailure(Box<ToolingError>),
    SessionCompilationFailure {
        session: Box<Session>,
        error: Box<SessionError>,
    },
}

fn check_session(path: &Path) -> Result<(), CheckCommandFailure> {
    let graph = load_direct_graph(path)
        .map_err(Box::new)
        .map_err(CheckCommandFailure::DirectToolingFailure)?;
    let mut session = graph
        .build_session(SessionOptions::default())
        .map_err(Box::new)
        .map_err(CheckCommandFailure::DirectToolingFailure)?;
    session
        .check_module(graph.entry_key())
        .map(|_| ())
        .map_err(|error| CheckCommandFailure::SessionCompilationFailure {
            session: Box::new(session),
            error: Box::new(error),
        })
}

impl From<DiagnosticsFormatArg> for DiagnosticsFormat {
    fn from(value: DiagnosticsFormatArg) -> Self {
        match value {
            DiagnosticsFormatArg::Text => Self::Text,
            DiagnosticsFormatArg::Json => Self::Json,
        }
    }
}
