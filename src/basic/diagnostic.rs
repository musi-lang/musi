use crate::basic::{
    errors::{self, Level},
    source::Source,
    span::Span,
};
use std::{
    collections::HashMap,
    io::{self, IsTerminal, Write},
};

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

pub fn emit_all(bag: &DiagnosticBag, files: &HashMap<u32, Source>) {
    for diag in &bag.diagnostics {
        emit(diag, files);
    }
}

pub fn report(err: errors::Error, span: Span) -> Diagnostic {
    let message = err.to_string();
    let hint = err.hint_opt();
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

pub fn emit(diag: &Diagnostic, files: &HashMap<u32, Source>) {
    let mut writer = io::stderr();
    let use_color = writer.is_terminal();
    let _ = render(&mut writer, diag, files, use_color);
}

fn render(
    writer: &mut impl Write,
    diag: &Diagnostic,
    files: &HashMap<u32, Source>,
    colour: bool,
) -> io::Result<()> {
    let (style, header) = match diag.level {
        Level::Error => ("\x1b[31m", "error"),
        Level::Warning => ("\x1b[33m", "warning"),
        Level::Note => ("\x1b[36m", "note"),
    };

    let src = files.get(&diag.span.file_id);
    if let Some(src) = src {
        let (ln, col) = src.location_at(diag.span.start);
        if colour {
            write!(writer, "\x1b[1m")?;
        }
        write!(writer, "{}:{ln}:{col}: ", src.filename)?;
        if colour {
            write!(writer, "\x1b[0m")?;
        }
    }

    if colour {
        write!(writer, "\x1b[1m{style}{header}:\x1b[0m {}\n", diag.message)?;
    } else {
        writeln!(writer, "{header}: {}", diag.message)?;
    }

    if let Some(src) = src {
        let (line_index, col) = src.location_at(diag.span.start);
        if let Some(line) = src.line_at_opt(line_index) {
            let line_index_str = line_index.to_string();
            if colour {
                write!(writer, " \x1b[1m{line_index_str} |\x1b[0m {line}\n")?;
            } else {
                writeln!(writer, " {line_index_str} | {line}")?;
            }

            let (end_line_index, end_col) = src.location_at(diag.span.end);
            let highlight_len = if line_index == end_line_index {
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
        let src = files.get(&span.file_id);
        if let Some(src) = src {
            let (ln_idx, col_idx) = src.location_at(span.start);
            if colour {
                write!(writer, "\x1b[1m")?;
            }
            write!(writer, "{}:{ln_idx}:{col_idx}: ", src.filename)?;
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
