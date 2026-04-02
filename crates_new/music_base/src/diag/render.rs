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
    let paint = |color: DiagColor, text: &str| -> String {
        if use_color {
            format!(
                "{}{text}{}",
                color.ansi_code(),
                DiagColor::Reset.ansi_code()
            )
        } else {
            String::from(text)
        }
    };

    for label in diag.labels() {
        if let Some(source) = sources.get(label.source_id()) {
            let span = label.span();
            let (line, col) = source.line_col(span.start);
            let path_display = source.path().display();

            let loc = format!("{path_display}:{line}:{col}:");
            let level = diag.code().map_or_else(
                || String::from(diag.level().label()),
                |code| format!("{}[{code}]", diag.level().label()),
            );
            let message = diag.hint().map_or_else(
                || String::from(diag.message()),
                |hint| format!("{}; {hint}", diag.message()),
            );
            writeln!(
                writer,
                "{} {}: {}",
                paint(DiagColor::Bold, loc.as_str()),
                paint(diag.level().color(), level.as_str()),
                paint(DiagColor::Bold, message.as_str()),
            )?;

            if let Some(line_text) = source.line_text(line) {
                let line_num = format!("{line}");
                let padding = " ".repeat(line_num.len());

                writeln!(writer, "{padding} |")?;
                writeln!(writer, "{line_num} | {line_text}")?;

                let byte_col = col.saturating_sub(1).min(line_text.len());
                let caret_offset = line_text
                    .char_indices()
                    .take_while(|&(i, _)| i < byte_col)
                    .count();
                let total_chars = line_text.chars().count();
                let remaining = total_chars.saturating_sub(caret_offset);
                let span_len = usize::try_from(span.len()).unwrap_or(1).max(1);
                let caret_count = span_len.min(remaining.max(1)).max(1);
                let caret_padding = " ".repeat(caret_offset);
                let carets = "^".repeat(caret_count);

                let caret_text = if label.message().is_empty() {
                    carets
                } else {
                    format!("{carets} {}", label.message())
                };

                writeln!(
                    writer,
                    "{padding} | {caret_padding}{}",
                    paint(diag.level().color(), caret_text.as_str()),
                )?;
            }
        }
    }

    if diag.labels().is_empty() {
        let level = diag.code().map_or_else(
            || String::from(diag.level().label()),
            |code| format!("{}[{code}]", diag.level().label()),
        );
        let message = diag.hint().map_or_else(
            || String::from(diag.message()),
            |hint| format!("{}; {hint}", diag.message()),
        );
        writeln!(
            writer,
            "{}: {}",
            paint(diag.level().color(), level.as_str()),
            paint(DiagColor::Bold, message.as_str()),
        )?;
    }

    for note_msg in diag.notes() {
        writeln!(
            writer,
            "{}: {}",
            paint(DiagColor::Cyan, "note"),
            paint(DiagColor::Bold, note_msg.as_str()),
        )?;
    }

    Ok(())
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
