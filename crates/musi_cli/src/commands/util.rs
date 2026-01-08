use std::fs;
use std::io::{self, Write as _};
use std::path::Path;

use musi_core::{DiagnosticBag, Interner, SourceFile};

use crate::cli::EmitKind;
use crate::types::OptEmitKind;

pub fn check_file(path: &Path, emit: OptEmitKind) -> Result<(), String> {
    let contents =
        fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {e}", path.display()))?;

    let mut interner = Interner::new();
    let file = SourceFile::new(path.display().to_string(), contents, 0);

    let errs = run_phases(&file, &mut interner, emit);

    if errs > 0 {
        Err(format!("{errs} error(s) in {}", path.display()))
    } else {
        Ok(())
    }
}

pub fn run_phases(file: &SourceFile, interner: &mut Interner, emit: OptEmitKind) -> usize {
    let (tokens, lex_bag) = musi_lex::tokenize(file, interner, true);

    if emit == Some(EmitKind::Tokens) {
        for token in &tokens {
            eprintln!("{}", token.kind.display(interner));
        }
    }

    let parse_result = musi_parse::parse(&tokens, interner);
    let mut total_errors = lex_bag.errors + parse_result.diagnostics.errors;

    if emit == Some(EmitKind::Ast) {
        eprintln!("{:#?}", parse_result.prog);
    }

    if total_errors == 0 {
        let (_, _, sema_bag) = musi_sema::bind(&parse_result.arena, interner, &parse_result.prog);
        total_errors += sema_bag.errors;
        emit_diagnostics(file, &sema_bag);
    }

    emit_diagnostics(file, &lex_bag);
    emit_diagnostics(file, &parse_result.diagnostics);
    total_errors
}

fn emit_diagnostics(file: &SourceFile, bag: &DiagnosticBag) {
    for diag in &bag.diagnostics {
        let (line, col) = file.location_at(diag.span.lo);
        eprintln!(
            "\x1b[1;31merror\x1b[0m[{}:{}:{}]: {}",
            &file.name, line, col, diag.message
        );
    }
}

pub fn error(msg: &str) {
    let mut stderr = io::stderr();
    writeln!(stderr, "\x1b[1;31merror:\x1b[0m {msg}").unwrap_or(());
}
