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

impl From<errors::Error> for Diagnostic {
    fn from(err: errors::Error) -> Self {
        let message = err.to_string();
        let hint = err.hint();
        let level = err.level();
        let span = err.span;

        let mut diag = Self {
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
}

pub fn report(err: errors::Error) -> Diagnostic {
    Diagnostic::from(err)
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
        render_location(writer, src, diag.span.lo, colour)?;
    }

    render_message(writer, header, &diag.message, style, colour)?;

    if let Some(src) = src_file {
        render_snippet(writer, src, diag.span, style, colour)?;
    }

    for (msg, span) in &diag.notes {
        render_note(writer, msg, *span, source_map, colour)?;
    }

    Ok(())
}

fn render_location(
    writer: &mut impl Write,
    src: &crate::basic::source::SourceFile,
    offset: u32,
    colour: bool,
) -> io::Result<()> {
    let (ln, col) = src.location_at(offset);
    if colour {
        write!(writer, "\x1b[1m")?;
    }
    write!(writer, "{}:{ln}:{col}: ", src.name)?;
    if colour {
        write!(writer, "\x1b[0m")?;
    }
    Ok(())
}

fn render_message(
    writer: &mut impl Write,
    header: &str,
    message: &str,
    style: &str,
    colour: bool,
) -> io::Result<()> {
    if colour {
        write!(writer, "\x1b[1m{style}{header}:\x1b[0m {message}\n")
    } else {
        writeln!(writer, "{header}: {message}")
    }
}

fn render_snippet(
    writer: &mut impl Write,
    src: &crate::basic::source::SourceFile,
    span: Span,
    style: &str,
    colour: bool,
) -> io::Result<()> {
    let (line_index, col) = src.location_at(span.lo);
    if let Some(line) = src.line_at(line_index - 1) {
        let line_index_str = line_index.to_string();
        if colour {
            write!(writer, " \x1b[1m{line_index_str} |\x1b[0m {line}\n")?;
        } else {
            writeln!(writer, " {line_index_str} | {line}")?;
        }

        let (_end_line_index, end_col) = src.location_at(span.hi);
        let highlight_len = if src.line_index(span.lo) == src.line_index(span.hi) {
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
    Ok(())
}

fn render_note(
    writer: &mut impl Write,
    msg: &str,
    span: Span,
    source_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (note_style, note_header) = ("\x1b[36m", "note");
    let src_file = source_map.lookup_source(span.lo);
    if let Some(src) = src_file {
        render_location(writer, src, span.lo, colour)?;
    }
    if colour {
        write!(writer, "\x1b[1m{note_style}{note_header}:\x1b[0m {msg}\n")
    } else {
        writeln!(writer, "{note_header}: {msg}")
    }
}
