use std::env::consts::{ARCH, FAMILY, OS};

use musi_native::NativeHost;
use musi_vm::Value;

use super::values::foreign_string_arg;

const SYS_PREFIX: &str = "@@std@0.1.0/_sys.ms::";

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__targetOs"),
        |ctx, _foreign, _args| ctx.alloc_string(target_os()),
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__targetArch"),
        |ctx, _foreign, _args| ctx.alloc_string(target_arch()),
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__targetArchFamily"),
        |ctx, _foreign, _args| ctx.alloc_string(target_arch_family()),
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__targetFamily"),
        |ctx, _foreign, _args| ctx.alloc_string(target_family()),
    );
    host.register_foreign_handler(
        format!("{SYS_PREFIX}Musi__targetPointerWidth"),
        |_foreign, _args| Ok(Value::Int(i64::from(usize::BITS))),
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__targetEndian"),
        |ctx, _foreign, _args| ctx.alloc_string(target_endian()),
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__matchesOs"),
        |ctx, foreign, args| {
            let value = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(matches_target(
                value,
                target_os(),
                normalize_target_text,
            ))))
        },
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__matchesArch"),
        |ctx, foreign, args| {
            let value = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(matches_target(
                value,
                target_arch(),
                normalize_arch_text,
            ))))
        },
    );
    host.register_foreign_handler_with_context(
        format!("{SYS_PREFIX}Musi__matchesFamily"),
        |ctx, foreign, args| {
            let value = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(matches_target(
                value,
                target_family(),
                normalize_target_text,
            ))))
        },
    );
}

fn matches_target(value: &str, target: &str, normalize: fn(&str) -> String) -> bool {
    normalize(value) == normalize(target)
}

const fn target_os() -> &'static str {
    OS
}

fn target_arch() -> &'static str {
    match ARCH {
        "x86_64" => "x86-64",
        "arm" => "aarch32",
        other => other,
    }
}

fn target_arch_family() -> &'static str {
    match ARCH {
        "x86" | "x86_64" => "x86",
        "arm" | "aarch64" => "arm",
        "wasm32" | "wasm64" => "wasm",
        other => other,
    }
}

const fn target_family() -> &'static str {
    FAMILY
}

const fn target_endian() -> &'static str {
    if cfg!(target_endian = "big") {
        "big"
    } else {
        "little"
    }
}

fn normalize_target_text(text: &str) -> String {
    text.trim().to_ascii_lowercase().replace('_', "-")
}

fn normalize_arch_text(text: &str) -> String {
    match normalize_target_text(text).as_str() {
        "x86-64" => "x86-64".into(),
        "aarch64" => "aarch64".into(),
        "arm" => "aarch32".into(),
        other => other.into(),
    }
}
