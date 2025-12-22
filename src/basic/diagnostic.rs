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
pub struct Bag {
    pub diags: Vec<Diagnostic>,
    pub errors: usize,
    pub warnings: usize,
}

impl Bag {
    pub fn add(&mut self, diag: Diagnostic) {
        match diag.level {
            Level::Error => self.errors += 1,
            Level::Warning => self.warnings += 1,
            _ => {}
        }
        self.diags.push(diag);
    }

    pub fn merge(&mut self, other: Bag) {
        self.errors += other.errors;
        self.warnings += other.warnings;
        self.diags.extend(other.diags);
    }

    pub const fn is_empty(&self) -> bool {
        self.diags.is_empty()
    }
}

pub fn emit_all(bag: &Bag, files: &HashMap<u32, Source>) {
    for diag in &bag.diags {
        emit(diag, files);
    }
}

pub fn report(err: errors::Error, span: Span) -> Diagnostic {
    let msg = err.to_string();
    let hint = err.hint();
    let level = err.level();

    let mut diag = Diagnostic {
        level,
        message: msg,
        span,
        notes: Vec::new(),
    };
    if let Some(h) = hint {
        diag.notes.push((h.to_string(), span));
    }
    diag
}

pub fn emit(diag: &Diagnostic, files: &HashMap<u32, Source>) {
    let mut w = io::stderr();
    let color = w.is_terminal();
    let _ = emit_inner(&mut w, diag, files, color);
}

fn emit_inner(
    w: &mut impl Write,
    diag: &Diagnostic,
    files: &HashMap<u32, Source>,
    color: bool,
) -> io::Result<()> {
    let (lc, hdr) = match diag.level {
        Level::Error => ("\x1b[31m", "error"),
        Level::Warning => ("\x1b[33m", "warning"),
        Level::Note => ("\x1b[36m", "note"),
    };

    let src = files.get(&diag.span.file_id);
    if let Some(src) = src {
        let (l, c) = src.line_col(diag.span.start);
        if color {
            write!(w, "\x1b[1m")?;
        }
        write!(w, "{}:{l}:{c}: ", src.filename)?;
        if color {
            write!(w, "\x1b[0m")?;
        }
    }

    if color {
        write!(w, "\x1b[1m{lc}{}:\x1b[0m {}\n", hdr, diag.message)?;
    } else {
        writeln!(w, "{hdr}: {}", diag.message)?;
    }

    if let Some(src) = src {
        let (l, c) = src.line_col(diag.span.start);
        if let Some(ln_) = src.line_text(l) {
            let ln = l.to_string();
            if color {
                write!(w, " \x1b[1m{ln} |\x1b[0m {ln_}\n")?;
            } else {
                writeln!(w, " {ln} | {ln_}")?;
            }

            let (el, ec) = src.line_col(diag.span.end);
            let len = if l == el {
                ec.saturating_sub(c).max(1)
            } else {
                ln_.len().saturating_sub(c) + 1
            };

            let pad = " ".repeat(ln.len());
            let sp = " ".repeat(c.saturating_sub(1));
            let car = "^".repeat(len);

            if color {
                writeln!(w, " {pad} | {sp}{lc}{car}\x1b[0m")?;
            } else {
                writeln!(w, " {pad} | {sp}{car}")?;
            }
        }
    }

    for (msg, span) in &diag.notes {
        let (lc, hdr) = ("\x1b[36m", "note");
        let src = files.get(&span.file_id);
        if let Some(src) = src {
            let (l, c) = src.line_col(span.start);
            if color {
                write!(w, "\x1b[1m")?;
            }
            write!(w, "{}:{l}:{c}: ", src.filename)?;
            if color {
                write!(w, "\x1b[0m")?;
            }
        }
        if color {
            write!(w, "\x1b[1m{lc}{hdr}:\x1b[0m {msg}\n")?;
        } else {
            writeln!(w, "{hdr}: {msg}")?;
        }
    }
    Ok(())
}
