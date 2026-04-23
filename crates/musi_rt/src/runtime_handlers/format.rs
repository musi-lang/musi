use musi_foundation::fmt as foundation_fmt;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::invalid_runtime_args;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_fmt::EFFECT,
        foundation_fmt::INT_OP,
        |ctx, effect, args| {
            let [Value::Int(value)] = args else {
                return Err(invalid_runtime_args(effect, "integer value", args.len()));
            };
            ctx.alloc_string(value.to_string())
        },
    );
    host.register_effect_handler_with_context(
        foundation_fmt::EFFECT,
        foundation_fmt::FLOAT_OP,
        |ctx, effect, args| {
            let [Value::Float(value)] = args else {
                return Err(invalid_runtime_args(effect, "float value", args.len()));
            };
            ctx.alloc_string(value.to_string())
        },
    );
}
