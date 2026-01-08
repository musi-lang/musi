use musi_core::{Diagnostic as MusiDiagnostic, Interner, Level, MusiError, SourceFile, Span};
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range};

pub fn run_diagnostics(uri_path: &str, text: &str) -> Vec<Diagnostic> {
    let mut interner = Interner::new();
    let source = SourceFile::new(uri_path.into(), text.into(), 0);
    let (tokens, lex_errors) = musi_lex::tokenize(&source, &mut interner, true);

    let mut diagnostics: Vec<Diagnostic> = lex_errors
        .diagnostics
        .iter()
        .map(|err| musi_diag_to_lsp(err, text))
        .collect();

    let parse_result = musi_parse::parse(&tokens, &interner);
    for diag in &parse_result.diagnostics.diagnostics {
        diagnostics.push(musi_diag_to_lsp(diag, text));
    }

    if parse_result.diagnostics.errors == 0 {
        let sema_result = musi_sema::analyze(&parse_result.arena, &interner, &parse_result.prog);
        if let Err(err) = sema_result {
            diagnostics.push(musi_error_to_lsp(&err, text));
        }
    }

    diagnostics
}

fn musi_diag_to_lsp(diag: &MusiDiagnostic, text: &str) -> Diagnostic {
    let range = span_to_range(diag.span, text);
    let severity = match diag.level {
        Level::Error => DiagnosticSeverity::ERROR,
        Level::Warning => DiagnosticSeverity::WARNING,
        Level::Note => DiagnosticSeverity::INFORMATION,
        _ => DiagnosticSeverity::HINT,
    };
    Diagnostic {
        range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("musi".to_owned()),
        message: diag.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn musi_error_to_lsp(err: &MusiError, text: &str) -> Diagnostic {
    let range = span_to_range(err.span, text);
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("musi".to_owned()),
        message: err.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn span_to_range(span: Span, text: &str) -> Range {
    let lo: usize = span.lo.try_into().expect("span.lo overflow");
    let hi: usize = span.hi.try_into().expect("span.hi overflow");
    let start = offset_to_position(lo, text);
    let end = offset_to_position(hi, text);
    Range { start, end }
}

pub fn offset_to_position(offset: usize, text: &str) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;

    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Position::new(line, col)
}
