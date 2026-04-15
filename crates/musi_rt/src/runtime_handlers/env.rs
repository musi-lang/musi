use std::env::{remove_var, set_var, var, var_os};

use musi_foundation::runtime as foundation_runtime;
use musi_native::NativeHost;
use musi_vm::Value;

use super::invalid_runtime_effect;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::ENV_GET_OP,
        |effect, args| {
            let [Value::String(name)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envGet args"));
            };
            Ok(Value::string(var(name.as_ref()).unwrap_or_default()))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::ENV_HAS_OP,
        |effect, args| {
            let [Value::String(name)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envHas args"));
            };
            Ok(Value::Int(i64::from(var_os(name.as_ref()).is_some())))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::ENV_SET_OP,
        |effect, args| {
            let [Value::String(name), Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envSet args"));
            };
            if !valid_env_key(name) || value.contains('\0') {
                return Ok(Value::Int(0));
            }
            // SAFETY: Musi's runtime executes on one VM thread; no concurrent Rust env access exists here.
            #[allow(unsafe_code)]
            unsafe {
                set_var(name.as_ref(), value.as_ref());
            }
            Ok(Value::Int(1))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::ENV_REMOVE_OP,
        |effect, args| {
            let [Value::String(name)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envRemove args"));
            };
            if !valid_env_key(name) {
                return Ok(Value::Int(0));
            }
            // SAFETY: Musi's runtime executes on one VM thread; no concurrent Rust env access exists here.
            #[allow(unsafe_code)]
            unsafe {
                remove_var(name.as_ref());
            }
            Ok(Value::Int(1))
        },
    );
}

fn valid_env_key(name: &str) -> bool {
    !name.is_empty() && !name.contains('=') && !name.contains('\0')
}
