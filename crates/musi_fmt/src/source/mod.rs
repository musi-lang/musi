use music_syntax::{Lexer, parse};

use crate::{
    FormatError, FormatOptions, FormatResultOf, imports::organize_imports,
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
    let organized = organize_imports(source);
    let source = organized.as_deref().unwrap_or(source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(lexed.clone());
    if !lexed.errors().is_empty() || !parsed.errors().is_empty() {
        return Err(FormatError::SyntaxErrors);
    }

    let tree = parsed.tree();
    let protected_ranges = protected_line_ranges(source, tree);
    let formatted_text = cst::format_cst_source(source, tree, options, protected_ranges);
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
