use musi_foundation::text as foundation_text;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::invalid_runtime_args;
use super::values::{saturating_usize_to_i64, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::LENGTH_OP,
        |ctx, effect, args| {
            let value = string_arg(ctx, effect, args, "textLength")?;
            Ok(Value::Int(saturating_usize_to_i64(value.chars().count())))
        },
    );
    host.register_effect_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::CONCAT_OP,
        |ctx, effect, args| {
            let [left, right] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "left and right strings",
                    args.len(),
                ));
            };
            let left = ctx
                .string(left)
                .ok_or_else(|| invalid_runtime_args(effect, "left string", left.kind()))?;
            let right = ctx
                .string(right)
                .ok_or_else(|| invalid_runtime_args(effect, "right string", right.kind()))?;
            let mut text =
                String::with_capacity(left.as_str().len().saturating_add(right.as_str().len()));
            text.push_str(left.as_str());
            text.push_str(right.as_str());
            ctx.alloc_string(text)
        },
    );
    host.register_effect_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::SLICE_OP,
        |ctx, effect, args| {
            let [value, Value::Int(start), Value::Int(end)] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "value string and integer bounds",
                    args.len(),
                ));
            };
            let value = ctx
                .string(value)
                .ok_or_else(|| invalid_runtime_args(effect, "value string", value.kind()))?;
            ctx.alloc_string(text_slice(value.as_str(), *start, *end))
        },
    );
    host.register_effect_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::BYTE_AT_OP,
        |ctx, effect, args| {
            let [value, Value::Int(index)] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "value string and integer index",
                    args.len(),
                ));
            };
            let value = ctx
                .string(value)
                .ok_or_else(|| invalid_runtime_args(effect, "value string", value.kind()))?;
            let byte = usize::try_from(*index)
                .ok()
                .and_then(|index| value.as_str().as_bytes().get(index).copied())
                .map_or(-1, i64::from);
            Ok(Value::Int(byte))
        },
    );
    host.register_effect_handler_with_context(
        foundation_text::EFFECT,
        foundation_text::FROM_BYTE_OP,
        |ctx, effect, args| {
            let [Value::Int(value)] = args else {
                return Err(invalid_runtime_args(effect, "integer byte", args.len()));
            };
            let byte = u8::try_from((*value).clamp(0, 127)).unwrap_or(0);
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
