#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Doc {
    Nil,
    Text(String),
    Line,
    SoftLine,
    HardLine,
    Concat(Vec<Self>),
    Indent(Box<Self>),
    Group(Box<Self>),
    IfBreak { broken: Box<Self>, flat: Box<Self> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Flat,
    Break,
}

#[derive(Debug, Clone, Copy)]
struct Command<'a> {
    indent: usize,
    mode: Mode,
    doc: &'a Doc,
}

#[derive(Debug, Clone)]
pub struct PrettyOptions {
    pub line_width: usize,
    pub indent: String,
}

impl Doc {
    #[must_use]
    pub const fn nil() -> Self {
        Self::Nil
    }

    #[must_use]
    pub fn text(text: impl Into<String>) -> Self {
        Self::Text(text.into())
    }

    #[must_use]
    pub const fn line() -> Self {
        Self::Line
    }

    #[must_use]
    pub const fn soft_line() -> Self {
        Self::SoftLine
    }

    #[must_use]
    pub const fn hard_line() -> Self {
        Self::HardLine
    }

    #[must_use]
    pub const fn hardline() -> Self {
        Self::hard_line()
    }

    #[must_use]
    pub fn concat(docs: impl Into<Vec<Self>>) -> Self {
        Self::Concat(docs.into())
    }

    #[must_use]
    pub fn join(docs: impl IntoIterator<Item = Self>, separator: &Self) -> Self {
        let mut docs = docs.into_iter();
        let Some(first) = docs.next() else {
            return Self::Nil;
        };
        let mut joined = vec![first];
        for doc in docs {
            joined.push(separator.clone());
            joined.push(doc);
        }
        Self::Concat(joined)
    }

    #[must_use]
    pub fn join_hardline(docs: impl IntoIterator<Item = Self>) -> Self {
        Self::join(docs, &Self::hard_line())
    }

    #[must_use]
    pub fn indent(doc: Self) -> Self {
        Self::Indent(Box::new(doc))
    }

    #[must_use]
    pub fn group(doc: Self) -> Self {
        Self::Group(Box::new(doc))
    }

    #[must_use]
    pub fn if_break(broken: Self, flat: Self) -> Self {
        Self::IfBreak {
            broken: Box::new(broken),
            flat: Box::new(flat),
        }
    }
}

#[must_use]
pub fn render(doc: &Doc, options: &PrettyOptions) -> String {
    let mut out = String::new();
    let mut column = 0usize;
    let mut stack = vec![Command {
        indent: 0,
        mode: Mode::Break,
        doc,
    }];
    while let Some(command) = stack.pop() {
        match command.doc {
            Doc::Nil => {}
            Doc::Text(text) => {
                column = column.saturating_add(text.len());
                out.push_str(text);
            }
            Doc::Line => match command.mode {
                Mode::Flat => {
                    column = column.saturating_add(1);
                    out.push(' ');
                }
                Mode::Break => write_newline(&mut out, &mut column, command.indent, options),
            },
            Doc::SoftLine => {
                if command.mode == Mode::Break {
                    write_newline(&mut out, &mut column, command.indent, options);
                }
            }
            Doc::HardLine => write_newline(&mut out, &mut column, command.indent, options),
            Doc::Concat(docs) => {
                for doc in docs.iter().rev() {
                    stack.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc,
                    });
                }
            }
            Doc::Indent(doc) => stack.push(Command {
                indent: command.indent.saturating_add(1),
                mode: command.mode,
                doc,
            }),
            Doc::Group(doc) => {
                let mode = if fits(
                    options.line_width.saturating_sub(column),
                    Command {
                        indent: command.indent,
                        mode: Mode::Flat,
                        doc,
                    },
                    &stack,
                ) {
                    Mode::Flat
                } else {
                    Mode::Break
                };
                stack.push(Command {
                    indent: command.indent,
                    mode,
                    doc,
                });
            }
            Doc::IfBreak { broken, flat } => stack.push(Command {
                indent: command.indent,
                mode: command.mode,
                doc: if command.mode == Mode::Break {
                    broken
                } else {
                    flat
                },
            }),
        }
    }
    out
}

#[must_use]
pub fn render_with_final_newline(doc: &Doc, options: &PrettyOptions) -> String {
    let mut out = render(doc, options);
    if out.ends_with('\n') {
        return out;
    }
    trim_trailing_spaces(&mut out);
    out.push('\n');
    out
}

fn write_newline(out: &mut String, column: &mut usize, indent: usize, options: &PrettyOptions) {
    trim_trailing_spaces(out);
    out.push('\n');
    for _ in 0..indent {
        out.push_str(&options.indent);
    }
    *column = indent.saturating_mul(options.indent.len());
}

fn trim_trailing_spaces(out: &mut String) {
    while out.ends_with(' ') || out.ends_with('\t') {
        let _ = out.pop();
    }
}

fn fits<'a>(mut remaining: usize, first: Command<'a>, rest: &[Command<'a>]) -> bool {
    let mut stack = rest.to_vec();
    stack.push(first);
    while let Some(command) = stack.pop() {
        match command.doc {
            Doc::Nil => {}
            Doc::Text(text) => {
                let len = text.len();
                if len > remaining {
                    return false;
                }
                remaining = remaining.saturating_sub(len);
            }
            Doc::Line => {
                if command.mode == Mode::Break {
                    return true;
                }
                if remaining == 0 {
                    return false;
                }
                remaining = remaining.saturating_sub(1);
            }
            Doc::SoftLine => {
                if command.mode == Mode::Break {
                    return true;
                }
            }
            Doc::HardLine => return true,
            Doc::Concat(docs) => {
                for doc in docs.iter().rev() {
                    stack.push(Command {
                        indent: command.indent,
                        mode: command.mode,
                        doc,
                    });
                }
            }
            Doc::Indent(doc) => stack.push(Command {
                indent: command.indent.saturating_add(1),
                mode: command.mode,
                doc,
            }),
            Doc::Group(doc) => stack.push(Command {
                indent: command.indent,
                mode: Mode::Flat,
                doc,
            }),
            Doc::IfBreak { broken, flat } => stack.push(Command {
                indent: command.indent,
                mode: command.mode,
                doc: if command.mode == Mode::Break {
                    broken
                } else {
                    flat
                },
            }),
        }
    }
    true
}

#[cfg(test)]
mod tests;
