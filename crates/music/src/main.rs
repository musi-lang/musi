mod cli;
mod diag;

use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use cli::{Cli, Command, DiagnosticsFormatArg, DisasmLevelArg};
use diag::{MusicCliDiagKind, music_cli_error_kind};
use musi_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, ToolingError, load_direct_graph, read_artifact_bytes,
    render_session_error, render_tooling_error, session_error_report, tooling_error_report,
    write_artifact_bytes,
};
use musi_vm::{Program, Value, Vm, VmError, VmOptions, render_value_view};
use music_base::diag::{CatalogDiagnostic, DiagContext, display_catalog_or_source};
use music_seam::descriptor::ExportTarget;
use music_seam::{
    AssemblyError, BINARY_VERSION, decode_binary, format_hil_projection, format_text,
};
use music_session::{Session, SessionError, SessionOptions};

type MusicResult<T = ()> = Result<T, MusicError>;

#[derive(Debug)]
enum MusicError {
    DirectToolingFailed(Box<ToolingError>),
    SessionCompilationFailed(Box<SessionError>),
    ArtifactTransportFailed(Box<AssemblyError>),
    VmExecutionFailed(Box<VmError>),
    JsonSerializationFailed(serde_json::Error),
    UnsupportedRunArgs { argument: String },
    CheckCommandFailed,
}

impl MusicError {
    fn diagnostic(&self) -> Option<CatalogDiagnostic<MusicCliDiagKind>> {
        Some(CatalogDiagnostic::new(
            self.diag_kind()?,
            self.diag_context(),
        ))
    }

    const fn diag_kind(&self) -> Option<MusicCliDiagKind> {
        music_cli_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::UnsupportedRunArgs { argument } => DiagContext::new().with("argument", argument),
            _ => DiagContext::new(),
        }
    }
}

impl Display for MusicError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_catalog_or_source(
            self.diagnostic(),
            Error::source(self),
            "music CLI diagnostic missing",
            f,
        )
    }
}

impl Error for MusicError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if let Self::DirectToolingFailed(source) = self {
            return Some(source);
        }
        if let Self::SessionCompilationFailed(source) = self {
            return Some(source);
        }
        if let Self::ArtifactTransportFailed(source) = self {
            return Some(source);
        }
        if let Self::VmExecutionFailed(source) = self {
            return Some(source);
        }
        if let Self::JsonSerializationFailed(source) = self {
            return Some(source);
        }
        None
    }
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

impl From<serde_json::Error> for MusicError {
    fn from(value: serde_json::Error) -> Self {
        Self::JsonSerializationFailed(value)
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
        Command::Info { path } => print_artifact_metadata(&path),
        Command::Disasm { path, level } => disasm(&path, level),
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
    if let Some(argument) = args.first() {
        return Err(MusicError::UnsupportedRunArgs {
            argument: argument.clone(),
        });
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

fn print_artifact_metadata(path: &Path) -> MusicResult {
    let bytes = artifact_bytes_for(path)?;
    let artifact = decode_binary(&bytes)?;

    println!("binaryVersion: {BINARY_VERSION}");
    println!("strings: {}", artifact.strings.len());
    println!("types: {}", artifact.types.len());
    println!("globals: {}", artifact.globals.len());
    println!("procedures: {}", artifact.procedures.len());
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

fn disasm(path: &Path, level: DisasmLevelArg) -> MusicResult {
    let bytes = artifact_bytes_for(path)?;
    let artifact = decode_binary(&bytes)?;
    match level {
        DisasmLevelArg::Hil => print!("{}", format_hil_projection(&artifact)),
        DisasmLevelArg::Seam => print!("{}", format_text(&artifact)),
    }
    Ok(())
}

fn artifact_bytes_for(path: &Path) -> MusicResult<Vec<u8>> {
    if path.extension().is_some_and(|ext| ext == "ms") {
        return compile_source_bytes(path);
    }
    if path.extension().is_some_and(|ext| ext == "seam") {
        return Ok(read_artifact_bytes(path)?);
    }
    if path.extension().is_none() {
        let artifact_path = path.with_extension("seam");
        if artifact_path.exists() {
            return Ok(read_artifact_bytes(&artifact_path)?);
        }
        let source_path = path.with_extension("ms");
        if source_path.exists() {
            return compile_source_bytes(path);
        }
    }
    if path.exists() {
        return Ok(read_artifact_bytes(path)?);
    }
    compile_source_bytes(path)
}

fn compile_source_bytes(path: &Path) -> MusicResult<Vec<u8>> {
    let graph = load_direct_graph(path)?;
    let mut session = graph.build_session(SessionOptions::default())?;
    Ok(session.compile_entry(graph.entry_key())?.bytes)
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
        ExportTarget::Procedure(_) => "procedure",
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
