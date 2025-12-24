use crate::{
    errors::{Level, MusiError},
    source::{SourceFile, SourceMap},
    span::Span,
};
use std::io::{self, IsTerminal as _, Write};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub notes: Vec<(String, Span)>,
    pub span: Span,
}

#[derive(Default, Debug, Clone)]
#[non_exhaustive]
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
            Level::Note => {}
        }
        self.diagnostics.push(diag);
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn merge(&mut self, other: Self) {
        self.errors += other.errors;
        self.warnings += other.warnings;
        self.diagnostics.extend(other.diagnostics);
    }
}

impl From<MusiError> for Diagnostic {
    fn from(err: MusiError) -> Self {
        let mut diag = Self {
            level: err.level,
            message: err.message,
            notes: vec![],
            span: err.span,
        };
        if let Some(hint_text) = err.hint {
            diag.notes.push((hint_text.to_owned(), err.span));
        }
        diag
    }
}

#[must_use]
pub fn report(err: MusiError) -> Diagnostic {
    Diagnostic::from(err)
}

pub fn emit_all(bag: &DiagnosticBag, src_map: &SourceMap) {
    for diag in &bag.diagnostics {
        emit(diag, src_map);
    }
}

pub fn emit(diag: &Diagnostic, src_map: &SourceMap) {
    let mut writer = io::stderr();
    let use_color = writer.is_terminal();
    let _: io::Result<()> = render(&mut writer, diag, src_map, use_color);
}

fn render(
    writer: &mut impl Write,
    diag: &Diagnostic,
    src_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (style, header) = match diag.level {
        Level::Error => ("\x1b[31m", "error"),
        Level::Warning => ("\x1b[33m", "warning"),
        Level::Note => ("\x1b[36m", "note"),
    };

    let src_file = src_map.lookup_source(diag.span.lo);
    if let Some(src) = src_file {
        render_location(writer, src, diag.span.lo, colour)?;
    }

    render_message(writer, header, &diag.message, style, colour)?;

    if let Some(src) = src_file {
        render_snippet(writer, src, diag.span, style, colour)?;
    }

    for (msg, span) in &diag.notes {
        render_note(writer, msg, *span, src_map, colour)?;
    }

    Ok(())
}

fn render_location(
    writer: &mut impl Write,
    src_file: &SourceFile,
    offset: u32,
    colour: bool,
) -> io::Result<()> {
    let (ln, col) = src_file.location_at(offset);
    if colour {
        write!(writer, "\x1b[1m")?;
    }
    write!(writer, "{}:{ln}:{col}: ", src_file.name)?;
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
        writeln!(writer, "\x1b[1m{style}{header}:\x1b[0m {message}")
    } else {
        writeln!(writer, "{header}: {message}")
    }
}

fn render_snippet(
    writer: &mut impl Write,
    src_file: &SourceFile,
    span: Span,
    style: &str,
    colour: bool,
) -> io::Result<()> {
    let (ln_idx, col) = src_file.location_at(span.lo);
    if let Some(line) = src_file.line_at(ln_idx - 1) {
        let ln_idx_str = ln_idx.to_string();
        if colour {
            writeln!(writer, " \x1b[1m{ln_idx_str} |\x1b[0m {line}")?;
        } else {
            writeln!(writer, " {ln_idx_str} | {line}")?;
        }

        let (_end_line_idx, end_col) = src_file.location_at(span.hi);
        let highlight_len = if src_file.line_index(span.lo) == src_file.line_index(span.hi) {
            (end_col - col).max(1)
        } else {
            line.len() - col + 1
        };

        let padding = " ".repeat(ln_idx_str.len());
        let indent = " ".repeat(col - 1);
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
    src_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (note_style, note_header) = ("\x1b[36m", "note");
    let src_file = src_map.lookup_source(span.lo);
    if let Some(src) = src_file {
        render_location(writer, src, span.lo, colour)?;
    }
    if colour {
        writeln!(writer, "\x1b[1m{note_style}{note_header}:\x1b[0m {msg}")
    } else {
        writeln!(writer, "{note_header}: {msg}")
    }
}
