use std::env::{args_os, current_dir};

use musi_foundation::runtime as foundation_runtime;
use musi_native::NativeHost;
use musi_vm::{EffectCall, Value, VmError, VmHostContext};

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

    host.register_effect_handler_with_context(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_ARG_AT_OP,
        |ctx, effect, args| {
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
            ctx.alloc_string(value)
        },
    );

    host.register_effect_handler_with_context(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_CWD_OP,
        |ctx, effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid processCwd args"));
            }
            let cwd = current_dir().map_err(|error| {
                invalid_runtime_effect(effect, format!("processCwd failed (`{error}`)"))
            })?;
            ctx.alloc_string(cwd.to_string_lossy().into_owned())
        },
    );

    host.register_effect_handler_with_context(
        foundation_runtime::EFFECT,
        foundation_runtime::PROCESS_RUN_OP,
        |ctx, effect, args| {
            let command = string_arg(ctx, effect, args, "processRun")?;
            Ok(Value::Int(run_shell_command(command, effect)?))
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

fn string_arg<'a>(
    ctx: &'a VmHostContext<'_>,
    effect: &EffectCall,
    args: &'a [Value],
    op_name: &str,
) -> Result<&'a str, VmError> {
    let [value] = args else {
        return Err(invalid_runtime_effect(
            effect,
            format!("invalid {op_name} args"),
        ));
    };
    ctx.string(value)
        .map(|text| text.as_str())
        .ok_or_else(|| invalid_runtime_effect(effect, format!("invalid {op_name} args")))
}
