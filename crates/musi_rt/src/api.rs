use musi_vm::VmOptions;
use music_session::SessionOptions;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum RuntimeOutputMode {
    #[default]
    Inherit,
    Capture,
    Suppress,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RuntimeOptions {
    pub session: SessionOptions,
    pub vm: VmOptions,
    pub output: RuntimeOutputMode,
}

impl RuntimeOptions {
    #[must_use]
    pub const fn new(session: SessionOptions, vm: VmOptions, output: RuntimeOutputMode) -> Self {
        Self {
            session,
            vm,
            output,
        }
    }

    #[must_use]
    pub fn with_session(mut self, session: SessionOptions) -> Self {
        self.session = session;
        self
    }

    #[must_use]
    pub const fn with_vm(mut self, vm: VmOptions) -> Self {
        self.vm = vm;
        self
    }

    #[must_use]
    pub const fn with_output(mut self, output: RuntimeOutputMode) -> Self {
        self.output = output;
        self
    }
}
