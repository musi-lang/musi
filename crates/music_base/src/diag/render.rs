use std::io::{self, IsTerminal, Write};

use crate::SourceMap;

use super::*;

/// Terminal colors used for diagnostic rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagColor {
    Red,
    Yellow,
    Cyan,
    Purple,
    Bold,
    Reset,
}

impl DiagColor {
    /// ANSI escape sequence for this color.
    #[must_use]
    pub const fn ansi_code(self) -> &'static str {
        match self {
            Self::Red => "\x1b[1;31m",
            Self::Yellow => "\x1b[1;33m",
            Self::Cyan => "\x1b[1;36m",
            Self::Purple => "\x1b[1;35m",
            Self::Bold => "\x1b[1m",
            Self::Reset => "\x1b[0m",
        }
    }
}

impl DiagLevel {
    /// ANSI color associated with this severity.
    #[must_use]
    pub const fn color(self) -> DiagColor {
        match self {
            Self::Fatal => DiagColor::Purple,
            Self::Error => DiagColor::Red,
            Self::Warning => DiagColor::Yellow,
            Self::Note => DiagColor::Cyan,
        }
    }
}

/// Render a diagnostic to the given writer.
///
/// # Errors
///
/// Returns `io::Error` if writing to `writer` fails.
pub fn emit<W: Write>(
    writer: &mut W,
    diag: &Diag,
    sources: &SourceMap,
    use_color: bool,
) -> io::Result<()> {
    for label in diag.labels() {
        emit_label(writer, diag, sources, label, use_color)?;
    }

    if diag.labels().is_empty() {
        emit_headline_without_labels(writer, diag, use_color)?;
    }

    if let Some(hint) = diag.hint() {
        writeln!(
            writer,
            "{}: {}",
            paint(use_color, DiagColor::Cyan, "help"),
            paint(use_color, DiagColor::Bold, hint),
        )?;
    }

    for note_msg in diag.notes() {
        writeln!(
            writer,
            "{}: {}",
            paint(use_color, DiagColor::Cyan, "note"),
            paint(use_color, DiagColor::Bold, note_msg.as_str()),
        )?;
    }

    for fix in diag.fixes() {
        if let Some(source) = sources.get(fix.source_id()) {
            let (line, col) = source.line_col(fix.span().start);
            writeln!(
                writer,
                "{}: replace at {}:{} with `{}`",
                paint(use_color, DiagColor::Cyan, "fix-it"),
                line,
                col,
                fix.replacement(),
            )?;
        }
    }

    Ok(())
}

fn emit_label<W: Write>(
    writer: &mut W,
    diag: &Diag,
    sources: &SourceMap,
    label: &DiagLabel,
    use_color: bool,
) -> io::Result<()> {
    let Some(source) = sources.get(label.source_id()) else {
        return Ok(());
    };
    let span = label.span();
    let (line, col) = source.line_col(span.start);
    let path_display = source.path().display();
    let loc = format!("{path_display}:{line}:{col}:");
    writeln!(
        writer,
        "{} {}: {}",
        paint(use_color, DiagColor::Bold, loc.as_str()),
        paint(use_color, diag.level().color(), level_label(diag).as_str()),
        paint(use_color, DiagColor::Bold, diag.message()),
    )?;
    if let Some(line_text) = source.line_text(line) {
        emit_label_line(writer, diag, label, line, col, line_text, use_color)?;
    }
    Ok(())
}

fn emit_label_line<W: Write>(
    writer: &mut W,
    diag: &Diag,
    label: &DiagLabel,
    line: usize,
    col: usize,
    line_text: &str,
    use_color: bool,
) -> io::Result<()> {
    let line_num = format!("{line}");
    let padding = " ".repeat(line_num.len());
    writeln!(writer, "{padding} |")?;
    writeln!(writer, "{line_num} | {line_text}")?;
    let caret_text = label_caret_text(label, col, line_text);
    let caret_offset = caret_offset(col, line_text);
    let caret_padding = " ".repeat(caret_offset);
    writeln!(
        writer,
        "{padding} | {caret_padding}{}",
        paint(use_color, diag.level().color(), caret_text.as_str()),
    )
}

fn emit_headline_without_labels<W: Write>(
    writer: &mut W,
    diag: &Diag,
    use_color: bool,
) -> io::Result<()> {
    writeln!(
        writer,
        "{}: {}",
        paint(use_color, diag.level().color(), level_label(diag).as_str()),
        paint(use_color, DiagColor::Bold, diag.message()),
    )
}

fn label_caret_text(label: &DiagLabel, col: usize, line_text: &str) -> String {
    let remaining = line_text
        .chars()
        .count()
        .saturating_sub(caret_offset(col, line_text));
    let span_len = usize::try_from(label.span().len()).unwrap_or(1).max(1);
    let caret_count = span_len.min(remaining.max(1)).max(1);
    let marker = match label.kind() {
        DiagLabelKind::Primary => '^',
        DiagLabelKind::Secondary => '~',
    };
    let carets = marker.to_string().repeat(caret_count);
    if label.message().is_empty() {
        carets
    } else {
        format!("{carets} {}", label.message())
    }
}

fn caret_offset(col: usize, line_text: &str) -> usize {
    let byte_col = col.saturating_sub(1).min(line_text.len());
    line_text
        .char_indices()
        .take_while(|&(i, _)| i < byte_col)
        .count()
}

fn level_label(diag: &Diag) -> String {
    diag.code().map_or_else(
        || String::from(diag.level().label()),
        |code| format!("{}[{code}]", diag.level().label()),
    )
}

fn paint(use_color: bool, color: DiagColor, text: &str) -> String {
    if use_color {
        format!(
            "{}{text}{}",
            color.ansi_code(),
            DiagColor::Reset.ansi_code()
        )
    } else {
        String::from(text)
    }
}

/// Emit a diagnostic to stderr, auto-detecting color support.
///
/// # Errors
///
/// Returns `io::Error` if writing to stderr fails.
pub fn emit_to_stderr(diag: &Diag, sources: &SourceMap) -> io::Result<()> {
    let use_color = supports_color();
    let mut stderr = io::stderr();
    emit(&mut stderr, diag, sources, use_color)
}

/// Whether stderr is a terminal that likely supports ANSI colors.
#[must_use]
pub fn supports_color() -> bool {
    io::stderr().is_terminal()
}
