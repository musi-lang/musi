use std::panic;
use std::sync::{Arc, Mutex};

use async_lsp::lsp_types::Diagnostic;
use musi_basic::{interner::Interner, source::SourceFile};
use musi_lex::lexer::tokenize;
use musi_parse::parse;

use crate::diagnostics::convert_diagnostics;

pub type AnalysisResult = (Vec<Diagnostic>, Option<String>);

pub fn compute_diagnostics(
    source_file: &SourceFile,
    interner: &Arc<Mutex<Interner>>,
) -> AnalysisResult {
    // NB: release mutex BEFORE parsing to prevent deadlock
    let (tokens, lex_errors) = {
        let mut interner_guard = match interner.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        tokenize(source_file, &mut interner_guard)
    };

    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let mut lex_errors = lex_errors;
        let (_program, parse_errors) = parse(&tokens);
        lex_errors.merge(parse_errors);
        convert_diagnostics(source_file, &lex_errors.diagnostics)
    }));

    match result {
        Ok(diags) => (diags, None),
        Err(_) => (
            vec![],
            Some(format!(
                "internal error: panic in parser for {}",
                source_file.name
            )),
        ),
    }
}
