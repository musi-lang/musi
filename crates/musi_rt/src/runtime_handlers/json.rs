use musi_foundation::json_host as foundation_json;
use musi_native::NativeHost;
use musi_vm::Value;

use super::values::{normalize_json, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_json::EFFECT,
        foundation_json::IS_VALID_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "jsonIsValid")?;
            Ok(Value::Int(i64::from(
                serde_json::from_str::<serde_json::Value>(source).is_ok(),
            )))
        },
    );

    host.register_effect_handler_with_context(
        foundation_json::EFFECT,
        foundation_json::NORMALIZE_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "jsonNormalize")?.to_owned();
            ctx.alloc_string(normalize_json(&source, effect)?)
        },
    );
}
