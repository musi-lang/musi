use std::env::{remove_var, set_var, var, var_os};

use musi_foundation::env as foundation_env;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::invalid_runtime_args;
use super::values::string_arg;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_env::EFFECT,
        foundation_env::GET_OP,
        |ctx, effect, args| {
            let name = string_arg(ctx, effect, args, "envGet")?;
            ctx.alloc_string(var(name).unwrap_or_default())
        },
    );

    host.register_effect_handler_with_context(
        foundation_env::EFFECT,
        foundation_env::HAS_OP,
        |ctx, effect, args| {
            let name = string_arg(ctx, effect, args, "envHas")?;
            Ok(Value::Int(i64::from(var_os(name).is_some())))
        },
    );

    host.register_effect_handler_with_context(
        foundation_env::EFFECT,
        foundation_env::SET_OP,
        |ctx, effect, args| {
            let [name, value] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "name and value strings",
                    args.len(),
                ));
            };
            let name = ctx
                .string(name)
                .ok_or_else(|| invalid_runtime_args(effect, "name string", name.kind()))?;
            let value = ctx
                .string(value)
                .ok_or_else(|| invalid_runtime_args(effect, "value string", value.kind()))?;
            let name = name.as_str();
            let value = value.as_str();
            if !valid_env_key(name) || value.contains('\0') {
                return Ok(Value::Int(0));
            }
            // SAFETY: Musi's runtime executes on one VM thread; no concurrent Rust env access exists here.
            #[allow(unsafe_code)]
            unsafe {
                set_var(name, value);
            }
            Ok(Value::Int(1))
        },
    );

    host.register_effect_handler_with_context(
        foundation_env::EFFECT,
        foundation_env::REMOVE_OP,
        |ctx, effect, args| {
            let name = string_arg(ctx, effect, args, "envRemove")?;
            if !valid_env_key(name) {
                return Ok(Value::Int(0));
            }
            // SAFETY: Musi's runtime executes on one VM thread; no concurrent Rust env access exists here.
            #[allow(unsafe_code)]
            unsafe {
                remove_var(name);
            }
            Ok(Value::Int(1))
        },
    );
}

fn valid_env_key(name: &str) -> bool {
    !name.is_empty() && !name.contains('=') && !name.contains('\0')
}
