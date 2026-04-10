use musi_vm::VmOptions;
use music_session::SessionOptions;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RuntimeOptions {
    pub session: SessionOptions,
    pub vm: VmOptions,
}
