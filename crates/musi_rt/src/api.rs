use musi_vm::VmOptions;
use music_session::SessionOptions;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RuntimeOptions {
    pub session: SessionOptions,
    pub vm: VmOptions,
}

impl RuntimeOptions {
    #[must_use]
    pub const fn new(session: SessionOptions, vm: VmOptions) -> Self {
        Self { session, vm }
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
}
