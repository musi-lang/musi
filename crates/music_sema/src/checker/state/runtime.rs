use music_names::{Interner, KnownSymbols};

use crate::api::{SemaEnv, TargetInfo};

use super::Builtins;

pub struct RuntimeEnv<'interner, 'env> {
    pub interner: &'interner mut Interner,
    pub known: KnownSymbols,
    pub builtins: Builtins,
    pub target: Option<TargetInfo>,
    pub env: Option<&'env dyn SemaEnv>,
}

impl<'interner, 'env> RuntimeEnv<'interner, 'env> {
    pub fn new(interner: &'interner mut Interner, known: KnownSymbols, builtins: Builtins) -> Self {
        Self {
            interner,
            known,
            builtins,
            target: None,
            env: None,
        }
    }

    pub fn with_target(mut self, target: TargetInfo) -> Self {
        self.target = Some(target);
        self
    }

    pub const fn with_env(mut self, env: Option<&'env dyn SemaEnv>) -> Self {
        self.env = env;
        self
    }
}

pub fn host_target_info() -> TargetInfo {
    use std::env::consts::{ARCH, OS};

    let pointer_width = u16::try_from(usize::BITS).unwrap_or(64);
    let endian = if cfg!(target_endian = "big") {
        "big"
    } else {
        "little"
    };
    let mut target = TargetInfo::new()
        .with_os(OS)
        .with_arch(ARCH)
        .with_pointer_width(pointer_width)
        .with_endian(endian);
    if cfg!(unix) {
        target = target.with_family("unix").with_family("posix");
    }
    if cfg!(windows) {
        target = target.with_family("windows");
    }
    if cfg!(target_vendor = "apple") {
        target = target.with_family("darwin");
    }
    if cfg!(target_os = "linux") {
        target = target.with_family("linux");
    }
    if cfg!(target_family = "wasm") || ARCH.starts_with("wasm") {
        target = target.with_family("webassembly");
    }
    target
}

impl RuntimeEnv<'_, '_> {
    pub const fn interner(&self) -> &Interner {
        self.interner
    }
}
