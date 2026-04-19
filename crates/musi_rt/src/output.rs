use std::cell::RefCell;
use std::io::{self, Write};
use std::mem::take;
use std::rc::Rc;

use crate::RuntimeOutputMode;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RuntimeOutput {
    pub stdout: Box<str>,
    pub stderr: Box<str>,
}

impl RuntimeOutput {
    #[must_use]
    pub const fn new(stdout: Box<str>, stderr: Box<str>) -> Self {
        Self { stdout, stderr }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.stdout.is_empty() && self.stderr.is_empty()
    }
}

#[derive(Debug)]
pub struct RuntimeOutputSink {
    mode: RuntimeOutputMode,
    stdout: String,
    stderr: String,
}

pub type RuntimeOutputSinkCell = Rc<RefCell<RuntimeOutputSink>>;

impl RuntimeOutputSink {
    #[must_use]
    pub(crate) fn shared(mode: RuntimeOutputMode) -> RuntimeOutputSinkCell {
        Rc::new(RefCell::new(Self {
            mode,
            stdout: String::new(),
            stderr: String::new(),
        }))
    }

    pub(crate) fn clear(&mut self) {
        self.stdout.clear();
        self.stderr.clear();
    }

    #[must_use]
    pub(crate) fn take(&mut self) -> RuntimeOutput {
        RuntimeOutput::new(
            take(&mut self.stdout).into_boxed_str(),
            take(&mut self.stderr).into_boxed_str(),
        )
    }

    pub(crate) fn write_stdout(&mut self, text: &str, line: bool) -> io::Result<()> {
        match self.mode {
            RuntimeOutputMode::Inherit => write_inherited_stdout(text, line),
            RuntimeOutputMode::Capture => {
                push_output(&mut self.stdout, text, line);
                Ok(())
            }
            RuntimeOutputMode::Suppress => Ok(()),
        }
    }

    pub(crate) fn write_stderr(&mut self, text: &str, line: bool) -> io::Result<()> {
        match self.mode {
            RuntimeOutputMode::Inherit => write_inherited_stderr(text, line),
            RuntimeOutputMode::Capture => {
                push_output(&mut self.stderr, text, line);
                Ok(())
            }
            RuntimeOutputMode::Suppress => Ok(()),
        }
    }
}

fn push_output(target: &mut String, text: &str, line: bool) {
    target.push_str(text);
    if line {
        target.push('\n');
    }
}

fn write_inherited_stdout(text: &str, line: bool) -> io::Result<()> {
    let mut stdout = io::stdout().lock();
    stdout.write_all(text.as_bytes())?;
    if line {
        stdout.write_all(b"\n")?;
    }
    stdout.flush()
}

fn write_inherited_stderr(text: &str, line: bool) -> io::Result<()> {
    let mut stderr = io::stderr().lock();
    stderr.write_all(text.as_bytes())?;
    if line {
        stderr.write_all(b"\n")?;
    }
    stderr.flush()
}
