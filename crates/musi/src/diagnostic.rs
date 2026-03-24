use music_found::{SourceId, SourceMap, Span};

pub enum Severity {
    Error,
    Warning,
}

pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub source_id: SourceId,
}

/// Render a diagnostic to a human-readable string.
///
/// Format:
/// ```text
/// error: <message>
///  --> path:line:col
///   |
/// L | source text
///   | ^
/// ```
#[must_use]
pub fn render(diag: &Diagnostic, source_map: &SourceMap) -> String {
    let label = match diag.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };

    let Some(source) = source_map.get(diag.source_id) else {
        return format!("{label}: {}", diag.message);
    };

    let (line0, col0) = source.line_col(diag.span.start);
    let line1 = line0.saturating_add(1);
    let col1 = col0.saturating_add(1);
    let path = source.path().display();

    let line_text = source.line_text(line0).unwrap_or("");
    let line_num_str = line1.to_string();
    let padding = " ".repeat(line_num_str.len());

    let span_len = usize::try_from(diag.span.end.saturating_sub(diag.span.start)).unwrap_or(0);
    let caret_len = if span_len == 0 { 1 } else { span_len };
    let caret_offset = " ".repeat(col0);
    let carets = "^".repeat(caret_len);

    format!(
        "{label}: {msg}\n --> {path}:{line1}:{col1}\n{padding}  |\n{line_num_str} | {line_text}\n{padding}  | {caret_offset}{carets}",
        msg = diag.message,
    )
}
