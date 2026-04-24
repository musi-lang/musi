use std::str::from_utf8;

use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use musi_foundation::encoding_host as foundation_encoding;
use musi_native::NativeHost;
use musi_vm::Value;

use super::values::{decode_utf8_encoded, string_arg, transform_string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_ENCODE_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "base64Encode")?.to_owned();
            ctx.alloc_string(BASE64_STANDARD.encode(source.as_bytes()))
        },
    );
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_DECODE_OP,
        |ctx, effect, args| {
            decode_utf8_encoded(ctx, effect, args, "base64Decode", |source| {
                BASE64_STANDARD
                    .decode(source)
                    .map_err(|error| format!("base64Decode failed (`{error}`)").into())
            })
        },
    );
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_IS_VALID_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "base64IsValid")?;
            Ok(Value::Int(i64::from(
                BASE64_STANDARD.decode(source).is_ok(),
            )))
        },
    );
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_ENCODE_OP,
        |ctx, effect, args| transform_string_arg(ctx, effect, args, "utf8Encode", str::to_owned),
    );
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_DECODE_OP,
        |ctx, effect, args| transform_string_arg(ctx, effect, args, "utf8Decode", str::to_owned),
    );
    host.register_effect_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_IS_VALID_OP,
        |ctx, effect, args| {
            let bytes = string_arg(ctx, effect, args, "utf8IsValid")?;
            Ok(Value::Int(i64::from(from_utf8(bytes.as_bytes()).is_ok())))
        },
    );
}

pub(super) fn hex_encode(bytes: &[u8]) -> String {
    const HEX_DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        out.push(char::from(HEX_DIGITS[usize::from(byte >> 4)]));
        out.push(char::from(HEX_DIGITS[usize::from(byte & 0x0f)]));
    }
    out
}
