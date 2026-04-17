use std::env::consts::{ARCH, OS};

pub(super) fn target_os() -> &'static str {
    match OS {
        "macos" => "macos",
        "ios" => "ios",
        "tvos" => "tvos",
        "watchos" => "watchos",
        "visionos" => "visionos",
        "linux" => "linux",
        "android" => "android",
        "windows" => "windows",
        "freebsd" => "freebsd",
        "openbsd" => "openbsd",
        "netbsd" => "netbsd",
        "wasi" => "wasi",
        "emscripten" => "emscripten",
        other => other,
    }
}

pub(super) fn normalize_target_text(text: &str) -> String {
    text.trim().to_ascii_lowercase().replace('_', "-")
}

pub(super) fn normalize_arch_text(text: &str) -> String {
    match normalize_target_text(text).as_str() {
        "x86-64" => "x86-64".into(),
        "aarch64" => "aarch64".into(),
        "arm" => "aarch32".into(),
        other => other.into(),
    }
}

pub(super) fn target_arch() -> &'static str {
    match ARCH {
        "x86" | "i386" | "i586" | "i686" => "x86",
        "x86_64" => "x86-64",
        "arm" => "aarch32",
        "aarch64" => "aarch64",
        "riscv32" => "rv32",
        "riscv64" => "rv64",
        "wasm32" => "wasm32",
        "wasm64" => "wasm64",
        other => other,
    }
}

pub(super) fn target_arch_family() -> &'static str {
    match target_arch() {
        "x86" | "x86-64" => "x86",
        "aarch32" | "aarch64" => "arm",
        "rv32" | "rv64" => "risc-v",
        "wasm32" | "wasm64" => "webassembly",
        "powerpc" | "powerpc64" => "powerpc",
        "mips" | "mips64" => "mips",
        "loongarch32" | "loongarch64" => "loongarch",
        "s390x" => "ibm-z",
        _ => "other",
    }
}

pub(super) fn target_family() -> &'static str {
    if cfg!(target_family = "wasm") || ARCH.starts_with("wasm") {
        "webassembly"
    } else if cfg!(windows) {
        "windows"
    } else if cfg!(target_vendor = "apple") {
        "darwin"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else if cfg!(unix) {
        "unix"
    } else {
        "embedded"
    }
}

pub(super) const fn target_endian() -> &'static str {
    if cfg!(target_endian = "big") {
        "big"
    } else {
        "little"
    }
}

pub(super) fn jit_supported() -> bool {
    matches!(target_arch(), "x86-64" | "aarch64" | "rv64" | "s390x")
}

pub(super) fn jit_backend() -> &'static str {
    if jit_supported() { "cranelift" } else { "" }
}

pub(super) fn jit_isa() -> &'static str {
    match target_arch() {
        "x86-64" => "x64",
        "aarch64" => "aarch64",
        "rv64" => "riscv64",
        "s390x" => "s390x",
        _ => "",
    }
}
