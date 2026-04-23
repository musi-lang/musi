use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use musi_foundation::{crypto_host as foundation_crypto, random as foundation_random};
use musi_native::NativeHost;
use musi_vm::Value;
use sha2::{Digest, Sha256};

use super::encoding::hex_encode;
use super::errors::{invalid_runtime_args, runtime_effect_failed};
use super::values::string_arg;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_random::EFFECT,
        foundation_random::ENTROPY_HEX_OP,
        |ctx, effect, args| {
            let [Value::Int(count)] = args else {
                return Err(invalid_runtime_args(effect, "byte count", args.len()));
            };
            let count = usize::try_from((*count).max(0)).unwrap_or(0);
            let mut bytes = vec![0u8; count];
            getrandom::fill(&mut bytes).map_err(|error| runtime_effect_failed(effect, error))?;
            ctx.alloc_string(hex_encode(&bytes))
        },
    );
    host.register_effect_handler_with_context(
        foundation_crypto::EFFECT,
        foundation_crypto::SHA256_HEX_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "sha256Hex")?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(hex_encode(&digest))
        },
    );
    host.register_effect_handler_with_context(
        foundation_crypto::EFFECT,
        foundation_crypto::SHA256_BASE64_OP,
        |ctx, effect, args| {
            let source = string_arg(ctx, effect, args, "sha256Base64")?;
            let digest = Sha256::digest(source.as_bytes());
            ctx.alloc_string(BASE64_STANDARD.encode(digest))
        },
    );
}
