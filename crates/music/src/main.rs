mod cli;

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use cli::{Cli, Command, DiagnosticsFormatArg};
use musi_vm::{Program, Value, ValueView, Vm, VmError, VmOptions};
use music_assembly::{decode_binary, format_text};
use music_bc::descriptor::ExportTarget;
use music_session::{Session, SessionError, SessionOptions};
use music_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, ToolingError, load_direct_graph, read_artifact_bytes,
    render_session_error, render_tooling_error, session_error_report, tooling_error_report,
    write_artifact_bytes,
};
use thiserror::Error;

type MusicResult<T = ()> = Result<T, MusicError>;

#[derive(Debug, Error)]
enum MusicError {
    #[error(transparent)]
    Tooling(#[from] ToolingError),
    #[error(transparent)]
    Session(#[from] SessionError),
    #[error(transparent)]
    Assembly(#[from] music_assembly::AssemblyError),
    #[error(transparent)]
    Vm(#[from] VmError),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error("run arguments unsupported")]
    RunArgsUnsupported,
    #[error("check failed")]
    CheckFailed,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(MusicError::CheckFailed) => ExitCode::from(1),
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
        Err(CheckError::Tooling(error)) => {
            emit_tooling_check_error("music", "check", diagnostics_format, &error)
        }
        Err(CheckError::Session { session, error }) => {
            emit_session_check_error("music", "check", diagnostics_format, &session, &error)
        }
    }
}

fn build(path: &Path, out: Option<&Path>) -> MusicResult {
    let graph = load_direct_graph(path)?;
    let mut session = graph.build_session(SessionOptions::default())?;
    let output = session.compile_entry(graph.entry_key())?;
    let out_path = out
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_seam_path(path));
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
    let mut vm = Vm::with_rejecting_host(program, VmOptions::default());
    vm.initialize()?;
    let value = vm.call_export("main", &[])?;
    print_vm_value(&vm, &value);
    Ok(())
}

fn inspect(path: &Path) -> MusicResult {
    let bytes = read_artifact_bytes(path)?;
    let artifact = decode_binary(&bytes)?;

    println!("binaryVersion: {}", music_bc::BINARY_VERSION);
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
    let rendered = match vm.inspect(value) {
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

fn default_seam_path(path: &Path) -> PathBuf {
    path.with_extension("seam")
}

fn export_kind_name(target: ExportTarget) -> &'static str {
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
    CliDiagnosticsReport {
        schema: "musi.diagnostics.v1",
        tool: tool.to_owned(),
        command: command.to_owned(),
        status: "ok",
        package_root: None,
        manifest: None,
        diagnostics: Vec::new(),
    }
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
    Err(MusicError::CheckFailed)
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
    Err(MusicError::CheckFailed)
}

enum CheckError {
    Tooling(ToolingError),
    Session {
        session: Session,
        error: SessionError,
    },
}

fn check_session(path: &Path) -> Result<(), CheckError> {
    let graph = load_direct_graph(path).map_err(CheckError::Tooling)?;
    let mut session = graph
        .build_session(SessionOptions::default())
        .map_err(CheckError::Tooling)?;
    session
        .check_module(graph.entry_key())
        .map(|_| ())
        .map_err(|error| CheckError::Session { session, error })
}

impl From<DiagnosticsFormatArg> for DiagnosticsFormat {
    fn from(value: DiagnosticsFormatArg) -> Self {
        match value {
            DiagnosticsFormatArg::Text => Self::Text,
            DiagnosticsFormatArg::Json => Self::Json,
        }
    }
}
