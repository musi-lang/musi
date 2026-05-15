use musi_foundation::json as foundation_json;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::foreign_rejected;
use super::values::{foreign_string_arg, normalize_json, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context("musi:json::Musi__isValid", |ctx, foreign, args| {
        let source = foreign_string_arg(ctx, foreign, args)?;
        Ok(Value::Int(i64::from(
            serde_json::from_str::<serde_json::Value>(source).is_ok(),
        )))
    });
    host.register_foreign_handler_with_context(
        "musi:json::Musi__normalize",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            let parsed: serde_json::Value =
                serde_json::from_str(source).map_err(|_| foreign_rejected(foreign))?;
            let normalized =
                serde_json::to_string(&parsed).map_err(|_| foreign_rejected(foreign))?;
            ctx.alloc_string(normalized)
        },
    );
    host.register_foundation_handler_with_context(
        foundation_json::EFFECT,
        foundation_json::IS_VALID_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "jsonIsValid")?;
            Ok(Value::Int(i64::from(
                serde_json::from_str::<serde_json::Value>(source).is_ok(),
            )))
        },
    );

    host.register_foundation_handler_with_context(
        foundation_json::EFFECT,
        foundation_json::NORMALIZE_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "jsonNormalize")?.to_owned();
            ctx.alloc_string(normalize_json(&source, foreign)?)
        },
    );
}
