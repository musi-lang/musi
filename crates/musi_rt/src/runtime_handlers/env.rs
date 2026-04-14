use std::env::{var, var_os};

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
            let [Value::String(_name), Value::String(_value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envSet args"));
            };
            Err(invalid_runtime_effect(
                effect,
                "envSet unsupported in current runtime",
            ))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::ENV_REMOVE_OP,
        |effect, args| {
            let [Value::String(_name)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid envRemove args"));
            };
            Err(invalid_runtime_effect(
                effect,
                "envRemove unsupported in current runtime",
            ))
        },
    );
}
