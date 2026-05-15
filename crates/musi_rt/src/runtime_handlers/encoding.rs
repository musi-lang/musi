use std::str::from_utf8;

use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use musi_foundation::encoding as foundation_encoding;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::foreign_rejected;
use super::values::{decode_utf8_encoded, foreign_string_arg, string_arg, transform_string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__base64Encode",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            ctx.alloc_string(BASE64_STANDARD.encode(source.as_bytes()))
        },
    );
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__base64Decode",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            let bytes = BASE64_STANDARD
                .decode(source)
                .map_err(|_| foreign_rejected(foreign))?;
            let text = String::from_utf8(bytes).map_err(|_| foreign_rejected(foreign))?;
            ctx.alloc_string(text)
        },
    );
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__base64IsValid",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(
                BASE64_STANDARD.decode(source).is_ok(),
            )))
        },
    );
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__utf8Encode",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            ctx.alloc_string(source.to_owned())
        },
    );
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__utf8Decode",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            ctx.alloc_string(source.to_owned())
        },
    );
    host.register_foreign_handler_with_context(
        "musi:encoding::Musi__utf8IsValid",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(from_utf8(source.as_bytes()).is_ok())))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_ENCODE_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "base64Encode")?.to_owned();
            ctx.alloc_string(BASE64_STANDARD.encode(source.as_bytes()))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_DECODE_OP,
        |ctx, foreign, args| {
            decode_utf8_encoded(ctx, foreign, args, "base64Decode", |source| {
                BASE64_STANDARD
                    .decode(source)
                    .map_err(|error| format!("base64Decode failed (`{error}`)").into())
            })
        },
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::BASE64_IS_VALID_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "base64IsValid")?;
            Ok(Value::Int(i64::from(
                BASE64_STANDARD.decode(source).is_ok(),
            )))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_ENCODE_OP,
        |ctx, foreign, args| transform_string_arg(ctx, foreign, args, "utf8Encode", str::to_owned),
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_DECODE_OP,
        |ctx, foreign, args| transform_string_arg(ctx, foreign, args, "utf8Decode", str::to_owned),
    );
    host.register_foundation_handler_with_context(
        foundation_encoding::EFFECT,
        foundation_encoding::UTF8_IS_VALID_OP,
        |ctx, foreign, args| {
            let bytes = string_arg(ctx, foreign, args, "utf8IsValid")?;
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
