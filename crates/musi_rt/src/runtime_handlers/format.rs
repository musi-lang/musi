use musi_foundation::fmt as foundation_fmt;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::{foreign_rejected, invalid_runtime_args};

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context("musi:fmt::Musi__int", |ctx, foreign, args| {
        let [Value::Int(value)] = args else {
            return Err(foreign_rejected(foreign));
        };
        ctx.alloc_string(value.to_string())
    });
    host.register_foreign_handler_with_context("musi:fmt::Musi__float", |ctx, foreign, args| {
        let [Value::Float(value)] = args else {
            return Err(foreign_rejected(foreign));
        };
        ctx.alloc_string(value.to_string())
    });
    host.register_foundation_handler_with_context(
        foundation_fmt::EFFECT,
        foundation_fmt::INT_OP,
        |ctx, foreign, args| {
            let [Value::Int(value)] = args else {
                return Err(invalid_runtime_args(foreign, "integer value", args.len()));
            };
            ctx.alloc_string(value.to_string())
        },
    );
    host.register_foundation_handler_with_context(
        foundation_fmt::EFFECT,
        foundation_fmt::FLOAT_OP,
        |ctx, foreign, args| {
            let [Value::Float(value)] = args else {
                return Err(invalid_runtime_args(foreign, "float value", args.len()));
            };
            ctx.alloc_string(value.to_string())
        },
    );
}
