use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use musi_foundation::{crypto as foundation_crypto, random as foundation_random};
use musi_native::NativeHost;
use musi_vm::Value;
use sha2::{Digest, Sha256};

use super::encoding::hex_encode;
use super::errors::{invalid_runtime_args, runtime_foreign_failed};
use super::values::{foreign_string_arg, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context(
        "musi:crypto::Musi__sha256Hex",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(hex_encode(&digest))
        },
    );
    host.register_foreign_handler_with_context(
        "musi:crypto::Musi__sha256Base64",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(BASE64_STANDARD.encode(digest))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_random::EFFECT,
        foundation_random::ENTROPY_HEX_OP,
        |ctx, foreign, args| {
            let [Value::Int(count)] = args else {
                return Err(invalid_runtime_args(foreign, "byte count", args.len()));
            };
            let count = usize::try_from((*count).max(0)).unwrap_or(0);
            let mut bytes = vec![0u8; count];
            getrandom::fill(&mut bytes).map_err(|error| runtime_foreign_failed(foreign, error))?;
            ctx.alloc_string(hex_encode(&bytes))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_crypto::EFFECT,
        foundation_crypto::SHA256_HEX_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "sha256Hex")?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(hex_encode(&digest))
        },
    );
    host.register_foundation_handler_with_context(
        foundation_crypto::EFFECT,
        foundation_crypto::SHA256_BASE64_OP,
        |ctx, foreign, args| {
            let source = string_arg(ctx, foreign, args, "sha256Base64")?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(BASE64_STANDARD.encode(digest))
        },
    );
}
