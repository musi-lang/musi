use crate::basic::{
    errors::{self, Level},
    source::SourceMap,
    span::Span,
};
use std::io::{self, IsTerminal, Write};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
}

#[derive(Default, Debug, Clone)]
pub struct DiagnosticBag {
    pub diagnostics: Vec<Diagnostic>,
    pub errors: usize,
    pub warnings: usize,
}

impl DiagnosticBag {
    pub fn add(&mut self, diag: Diagnostic) {
        match diag.level {
            Level::Error => self.errors += 1,
            Level::Warning => self.warnings += 1,
            _ => {}
        }
        self.diagnostics.push(diag);
    }

    pub fn merge(&mut self, other: DiagnosticBag) {
        self.errors += other.errors;
        self.warnings += other.warnings;
        self.diagnostics.extend(other.diagnostics);
    }

    pub const fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

pub fn emit_all(bag: &DiagnosticBag, source_map: &SourceMap) {
    for diag in &bag.diagnostics {
        emit(diag, source_map);
    }
}

pub fn report(err: errors::Error, span: Span) -> Diagnostic {
    let message = err.to_string();
    let hint = err.hint();
    let level = err.level();

    let mut diag = Diagnostic {
        level,
        message,
        span,
        notes: vec![],
    };
    if let Some(hint_text) = hint {
        diag.notes.push((hint_text.to_string(), span));
    }
    diag
}

pub fn emit(diag: &Diagnostic, source_map: &SourceMap) {
    let mut writer = io::stderr();
    let use_color = writer.is_terminal();
    let _ = render(&mut writer, diag, source_map, use_color);
}

fn render(
    writer: &mut impl Write,
    diag: &Diagnostic,
    source_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (style, header) = match diag.level {
        Level::Error => ("\x1b[31m", "error"),
        Level::Warning => ("\x1b[33m", "warning"),
        Level::Note => ("\x1b[36m", "note"),
    };

    let src_file = source_map.lookup_source(diag.span.lo);
    if let Some(src) = src_file {
        let (ln, col) = src.location_at(diag.span.lo);
        if colour {
            write!(writer, "\x1b[1m")?;
        }
        write!(writer, "{}:{ln}:{col}: ", src.name)?;
        if colour {
            write!(writer, "\x1b[0m")?;
        }
    }

    if colour {
        write!(writer, "\x1b[1m{style}{header}:\x1b[0m {}\n", diag.message)?;
    } else {
        writeln!(writer, "{header}: {}", diag.message)?;
    }

    if let Some(src) = src_file {
        let (line_index, col) = src.location_at(diag.span.lo);
        if let Some(line) = src.line_at(line_index - 1) {
            let line_index_str = line_index.to_string();
            if colour {
                write!(writer, " \x1b[1m{line_index_str} |\x1b[0m {line}\n")?;
            } else {
                writeln!(writer, " {line_index_str} | {line}")?;
            }

            let (_end_line_index, end_col) = src.location_at(diag.span.hi);
            let highlight_len = if src.line_index(diag.span.lo) == src.line_index(diag.span.hi) {
                end_col.saturating_sub(col).max(1)
            } else {
                line.len().saturating_sub(col).saturating_add(1)
            };

            let padding = " ".repeat(line_index_str.len());
            let indent = " ".repeat(col.saturating_sub(1));
            let carets = "^".repeat(highlight_len);

            if colour {
                writeln!(writer, " {padding} | {indent}{style}{carets}\x1b[0m")?;
            } else {
                writeln!(writer, " {padding} | {indent}{carets}")?;
            }
        }
    }

    for (msg, span) in &diag.notes {
        let (note_style, note_header) = ("\x1b[36m", "note");
        let src_file = source_map.lookup_source(span.lo);
        if let Some(src) = src_file {
            let (ln_idx, col_idx) = src.location_at(span.lo);
            if colour {
                write!(writer, "\x1b[1m")?;
            }
            write!(writer, "{}:{ln_idx}:{col_idx}: ", src.name)?;
            if colour {
                write!(writer, "\x1b[0m")?;
            }
        }
        if colour {
            write!(writer, "\x1b[1m{note_style}{note_header}:\x1b[0m {msg}\n")?;
        } else {
            writeln!(writer, "{note_header}: {msg}")?;
        }
    }
    Ok(())
}
