use music_syntax::{Lexer, parse};

use crate::{
    FormatError, FormatOptions, FormatResultOf, imports::organize_imports_protecting,
    protected::protected_line_ranges,
};

mod cst;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatResult {
    pub text: String,
    pub changed: bool,
}

/// Formats one Musi source string.
///
/// # Errors
///
/// Returns [`FormatError::SyntaxErrors`] when lexing or parsing fails.
pub fn format_source(source: &str, options: &FormatOptions) -> FormatResultOf {
    let original_source = source;
    if has_ignore_file(source) {
        return Ok(FormatResult {
            text: ensure_final_newline(source),
            changed: source != ensure_final_newline(source),
        });
    }
    let lexed = Lexer::new(source).lex();
    if !lexed.errors().is_empty() {
        return Err(FormatError::SyntaxErrors);
    }
    let parsed = parse(lexed);
    if !parsed.errors().is_empty() {
        return Err(FormatError::SyntaxErrors);
    }
    let tree = parsed.tree();
    let protected_ranges = protected_line_ranges(source, tree);
    let organized = organize_imports_protecting(source, &protected_ranges);
    let formatted_text = if let Some(organized) = organized.as_deref() {
        let lexed = Lexer::new(organized).lex();
        if !lexed.errors().is_empty() {
            return Err(FormatError::SyntaxErrors);
        }
        let parsed = parse(lexed);
        if !parsed.errors().is_empty() {
            return Err(FormatError::SyntaxErrors);
        }
        let tree = parsed.tree();
        let protected_ranges = protected_line_ranges(organized, tree);
        cst::format_cst_source(organized, tree, options, protected_ranges)
    } else {
        cst::format_cst_source(source, tree, options, protected_ranges)
    };
    Ok(FormatResult {
        changed: formatted_text != original_source,
        text: formatted_text,
    })
}

fn has_ignore_file(source: &str) -> bool {
    source
        .lines()
        .take(5)
        .any(|line| line.contains("musi-fmt-ignore-file"))
}

fn ensure_final_newline(source: &str) -> String {
    let mut text = source.trim_end_matches(['\r', '\n']).to_owned();
    text.push('\n');
    text
}
