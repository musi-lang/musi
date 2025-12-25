use std::sync::{Arc, Mutex};

use async_lsp::lsp_types::Diagnostic;
use musi_basic::{interner::Interner, source::SourceFile};
use musi_lex::lexer::tokenize;
use musi_parse::parse;

use crate::diagnostics::convert_diagnostics;

pub fn compute_diagnostics(
    source_file: &SourceFile,
    interner: &Arc<Mutex<Interner>>,
) -> Vec<Diagnostic> {
    let mut interner = interner.lock().unwrap();

    let (tokens, mut lex_errors) = tokenize(source_file, &mut interner);
    let (_program, parse_errors) = parse(&tokens);

    lex_errors.merge(parse_errors);

    convert_diagnostics(source_file, &lex_errors.diagnostics)
}
