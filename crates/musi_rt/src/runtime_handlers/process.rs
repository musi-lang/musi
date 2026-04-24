use std::env::{args_os, current_dir};
use std::process::Command;

use musi_foundation::process as foundation_process;
use musi_native::NativeHost;
use musi_vm::{EffectCall, Value, VmError};

use super::errors::{invalid_runtime_args, runtime_effect_failed, runtime_effect_unsupported};
use super::values::{saturating_usize_to_i64, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_process::EFFECT,
        foundation_process::ARG_COUNT_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            Ok(Value::Int(saturating_usize_to_i64(args_os().count())))
        },
    );

    host.register_effect_handler_with_context(
        foundation_process::EFFECT,
        foundation_process::ARG_AT_OP,
        |ctx, effect, args| {
            let [Value::Int(index)] = args else {
                return Err(invalid_runtime_args(effect, "integer index", args.len()));
            };
            let value = usize::try_from(*index).map_or_else(
                |_| String::new(),
                |index| {
                    args_os()
                        .nth(index)
                        .map(|arg| arg.to_string_lossy().into_owned())
                        .unwrap_or_default()
                },
            );
            ctx.alloc_string(value)
        },
    );

    host.register_effect_handler_with_context(
        foundation_process::EFFECT,
        foundation_process::CWD_OP,
        |ctx, effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            let cwd = current_dir().map_err(|error| runtime_effect_failed(effect, error))?;
            ctx.alloc_string(cwd.to_string_lossy().into_owned())
        },
    );

    host.register_effect_handler_with_context(
        foundation_process::EFFECT,
        foundation_process::RUN_OP,
        |ctx, effect, args| {
            let command = string_arg(ctx, effect, args, "processRun")?;
            Ok(Value::Int(run_shell_command(command, effect)?))
        },
    );

    host.register_effect_handler(
        foundation_process::EFFECT,
        foundation_process::EXIT_OP,
        |effect, args| {
            let [Value::Int(_code)] = args else {
                return Err(invalid_runtime_args(effect, "integer code", args.len()));
            };
            Err(runtime_effect_unsupported(effect))
        },
    );
}

fn run_shell_command(command: &str, effect: &EffectCall) -> Result<i64, VmError> {
    let status = if cfg!(windows) {
        Command::new("cmd")
            .args([windows_shell_flag().as_str(), command])
            .status()
    } else {
        Command::new("sh").args(["-c", command]).status()
    }
    .map_err(|error| runtime_effect_failed(effect, error))?;
    Ok(i64::from(status.code().unwrap_or(-1)))
}

fn windows_shell_flag() -> String {
    ['/', 'C'].into_iter().collect()
}
