//! Frontend pipeline: lex → parse → type-check.

use std::fs;
use std::io::{IsTerminal, stderr};
use std::path::Path;

use music_ast::ParsedModule;
use music_ir::lower::lower as lower_ir;
use music_sema::SemaResult;
use music_shared::{DiagnosticBag, Interner, SourceDb};

/// Output of a successful frontend run.
pub struct FrontendOutput {
    /// The semantic analysis result.
    pub sema: SemaResult,
    /// The parsed module (needed for IR lowering).
    pub parsed: ParsedModule,
    /// The symbol interner (needed for IR lowering and emit).
    pub interner: Interner,
}

/// Runs lex → parse → sema on `path`.
///
/// Prints diagnostics to stderr. Returns `Err(())` if any errors occurred.
pub fn run_frontend(path: &Path) -> Result<FrontendOutput, ()> {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: {}: {e}", path.display());
            return Err(());
        }
    };

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let file_id = source_db.add(path.display().to_string(), source.as_str());
    let lexed = music_lex::lex(&source, file_id, &mut interner, &mut diags);
    let parsed = music_parse::parse(&lexed.tokens, file_id, &mut diags, &interner);
    let sema = music_sema::analyze(&parsed, &mut interner, file_id, &mut diags);

    render_diagnostics(&diags, &source_db);

    if diags.has_errors() {
        Err(())
    } else {
        Ok(FrontendOutput {
            sema,
            parsed,
            interner,
        })
    }
}

/// Runs IR lowering and bytecode emission.
///
/// Returns the raw `.msbc` bytes on success, or `Err(())` after printing
/// the error to stderr.
pub fn run_backend(out: &FrontendOutput) -> Result<Vec<u8>, ()> {
    let ir = match lower_ir(&out.parsed, &out.sema, &out.interner) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: {e}");
            return Err(());
        }
    };
    match music_emit::emit(&ir, &out.interner) {
        Ok(emit_out) => Ok(emit_out.bytes),
        Err(e) => {
            eprintln!("error: {e}");
            Err(())
        }
    }
}

fn render_diagnostics(diags: &DiagnosticBag, source_db: &SourceDb) {
    let use_color = stderr().is_terminal();
    for diag in diags.iter() {
        eprintln!("{}", diag.render_rich(source_db, use_color));
    }
}
