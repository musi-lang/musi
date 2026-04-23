use super::errors::invalid_runtime_args;
use musi_foundation::uuid_host as foundation_uuid;
use musi_native::NativeHost;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_uuid::EFFECT,
        foundation_uuid::V4_OP,
        |ctx, effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            ctx.alloc_string(uuid::Uuid::new_v4().to_string())
        },
    );
}
