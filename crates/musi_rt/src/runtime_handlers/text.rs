use musi_foundation::text as foundation_text;
use musi_native::NativeHost;
use musi_vm::{ForeignCall, Value, VmError, VmHostContext};

use super::errors::{foreign_rejected, invalid_runtime_args};
use super::values::{foreign_string_arg, saturating_usize_to_i64, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    register_foreign_handlers(host);
    register_effect_handlers(host);
}

fn register_foreign_handlers(host: &mut NativeHost) {
    host.register_foreign_handler_with_context("musi:text::Musi__length", |ctx, foreign, args| {
        let text_arg = foreign_string_arg(ctx, foreign, args)?;
        Ok(Value::Int(saturating_usize_to_i64(
            text_arg.chars().count(),
        )))
    });
    host.register_foreign_handler_with_context("musi:text::Musi__concat", |ctx, foreign, args| {
        let (left, right) = foreign_two_strings(ctx, foreign, args)?;
        let mut text = String::with_capacity(left.len().saturating_add(right.len()));
        text.push_str(left);
        text.push_str(right);
        ctx.alloc_string(text)
    });
    host.register_foreign_handler_with_context("musi:text::Musi__slice", |ctx, foreign, args| {
        let [text_value, Value::Int(start), Value::Int(end)] = args else {
            return Err(foreign_rejected(foreign));
        };
        let text_value = ctx
            .string(text_value)
            .ok_or_else(|| foreign_rejected(foreign))?;
        ctx.alloc_string(text_slice(text_value.as_str(), *start, *end))
    });
    host.register_foreign_handler_with_context("musi:text::Musi__byteAt", |ctx, foreign, args| {
        let [text_value, Value::Int(index)] = args else {
            return Err(foreign_rejected(foreign));
        };
        let text_value = ctx
            .string(text_value)
            .ok_or_else(|| foreign_rejected(foreign))?;
        let byte = usize::try_from(*index)
            .ok()
            .and_then(|index| text_value.as_str().as_bytes().get(index).copied())
            .map_or(-1, i64::from);
        Ok(Value::Int(byte))
    });
    host.register_foreign_handler_with_context(
        "musi:text::Musi__fromByte",
        |ctx, foreign, args| {
            let [Value::Int(byte_code)] = args else {
                return Err(foreign_rejected(foreign));
            };
            let byte = u8::try_from((*byte_code).clamp(0, 127)).unwrap_or(0);
            ctx.alloc_string(char::from(byte).to_string())
        },
    );
}

fn register_effect_handlers(host: &mut NativeHost) {
    host.register_foundation_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::LENGTH_OP,
        |ctx, foreign, args| {
            let text_arg = string_arg(ctx, foreign, args, "textLength")?;
            Ok(Value::Int(saturating_usize_to_i64(
                text_arg.chars().count(),
            )))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::CONCAT_OP,
        |ctx, foreign, args| {
            let [left, right] = args else {
                return Err(invalid_runtime_args(
                    foreign,
                    "left and right strings",
                    args.len(),
                ));
            };
            let left = ctx
                .string(left)
                .ok_or_else(|| invalid_runtime_args(foreign, "left string", left.kind()))?;
            let right = ctx
                .string(right)
                .ok_or_else(|| invalid_runtime_args(foreign, "right string", right.kind()))?;
            let mut text =
                String::with_capacity(left.as_str().len().saturating_add(right.as_str().len()));
            text.push_str(left.as_str());
            text.push_str(right.as_str());
            ctx.alloc_string(text)
        },
    );
    host.register_foundation_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::SLICE_OP,
        |ctx, foreign, args| {
            let [text_value, Value::Int(start), Value::Int(end)] = args else {
                return Err(invalid_runtime_args(
                    foreign,
                    "value string and integer bounds",
                    args.len(),
                ));
            };
            let text_value = ctx
                .string(text_value)
                .ok_or_else(|| invalid_runtime_args(foreign, "value string", text_value.kind()))?;
            ctx.alloc_string(text_slice(text_value.as_str(), *start, *end))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::BYTE_AT_OP,
        |ctx, foreign, args| {
            let [text_value, Value::Int(index)] = args else {
                return Err(invalid_runtime_args(
                    foreign,
                    "value string and integer index",
                    args.len(),
                ));
            };
            let text_value = ctx
                .string(text_value)
                .ok_or_else(|| invalid_runtime_args(foreign, "value string", text_value.kind()))?;
            let byte = usize::try_from(*index)
                .ok()
                .and_then(|index| text_value.as_str().as_bytes().get(index).copied())
                .map_or(-1, i64::from);
            Ok(Value::Int(byte))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::FROM_BYTE_OP,
        |ctx, foreign, args| {
            let [Value::Int(byte_code)] = args else {
                return Err(invalid_runtime_args(foreign, "integer byte", args.len()));
            };
            let byte = u8::try_from((*byte_code).clamp(0, 127)).unwrap_or(0);
            ctx.alloc_string(char::from(byte).to_string())
        },
    );
}

fn text_slice(value: &str, start: i64, end: i64) -> String {
    let len = value.len();
    let start = usize::try_from(start.max(0)).unwrap_or(0).min(len);
    let end = usize::try_from(end.max(0)).unwrap_or(0).min(len);
    let start = floor_char_boundary(value, start);
    let end = floor_char_boundary(value, end);
    if end < start {
        return String::new();
    }
    value.get(start..end).unwrap_or("").to_owned()
}

const fn floor_char_boundary(value: &str, mut index: usize) -> usize {
    while index > 0 && !value.is_char_boundary(index) {
        index -= 1;
    }
    index
}

fn foreign_two_strings<'a>(
    ctx: &'a VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &'a [Value],
) -> Result<(&'a str, &'a str), VmError> {
    let [left, right] = args else {
        return Err(foreign_rejected(foreign));
    };
    let left = ctx.string(left).ok_or_else(|| foreign_rejected(foreign))?;
    let right = ctx.string(right).ok_or_else(|| foreign_rejected(foreign))?;
    Ok((left.as_str(), right.as_str()))
}
