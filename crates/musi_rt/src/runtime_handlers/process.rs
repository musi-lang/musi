use std::env::{args_os, current_dir};

use musi_foundation::runtime as foundation_runtime;
use musi_native::NativeHost;
use musi_vm::Value;

use super::{invalid_runtime_effect, run_shell_command, saturating_usize_to_i64};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_ARG_COUNT_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(
                    effect,
                    "invalid processArgCount args",
                ));
            }
            Ok(Value::Int(saturating_usize_to_i64(args_os().count())))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_ARG_AT_OP,
        |effect, args| {
            let [Value::Int(index)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid processArgAt args"));
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
            Ok(Value::string(value))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_CWD_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid processCwd args"));
            }
            let cwd = current_dir().map_err(|error| {
                invalid_runtime_effect(effect, format!("processCwd failed (`{error}`)"))
            })?;
            Ok(Value::string(cwd.to_string_lossy().into_owned()))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_RUN_OP,
        |effect, args| {
            let [Value::String(command)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid processRun args"));
            };
            Ok(Value::Int(run_shell_command(command.as_ref(), effect)?))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_EXIT_OP,
        |effect, args| {
            let [Value::Int(_code)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid processExit args"));
            };
            Err(invalid_runtime_effect(
                effect,
                "processExit unsupported in current runtime",
            ))
        },
    );
}
