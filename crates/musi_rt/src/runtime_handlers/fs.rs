use std::fs;
use std::io::Write;
use std::path::Path;

use musi_foundation::fs as foundation_fs;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::{invalid_runtime_args, runtime_effect_failed};
use super::values::string_arg;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::READ_TEXT_OP,
        |ctx, effect, args| {
            let path = string_arg(ctx, effect, args, "fsReadText")?.to_owned();
            let text =
                fs::read_to_string(path).map_err(|error| runtime_effect_failed(effect, error))?;
            ctx.alloc_string(text)
        },
    );

    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::WRITE_TEXT_OP,
        |ctx, effect, args| {
            let [path, text] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "path and text strings",
                    args.len(),
                ));
            };
            let path = ctx
                .string(path)
                .ok_or_else(|| invalid_runtime_args(effect, "path string", path.kind()))?;
            let text = ctx
                .string(text)
                .ok_or_else(|| invalid_runtime_args(effect, "text string", text.kind()))?;
            fs::write(path.as_str(), text.as_str())
                .map_err(|error| runtime_effect_failed(effect, error))?;
            Ok(Value::Unit)
        },
    );

    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::EXISTS_OP,
        |ctx, effect, args| {
            let path = string_arg(ctx, effect, args, "fsExists")?;
            Ok(Value::Int(i64::from(Path::new(path).exists())))
        },
    );

    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::APPEND_TEXT_OP,
        |ctx, effect, args| {
            let [path, text] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "path and text strings",
                    args.len(),
                ));
            };
            let path = ctx
                .string(path)
                .ok_or_else(|| invalid_runtime_args(effect, "path string", path.kind()))?;
            let text = ctx
                .string(text)
                .ok_or_else(|| invalid_runtime_args(effect, "text string", text.kind()))?;
            let result = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path.as_str())
                .and_then(|mut file| file.write_all(text.as_str().as_bytes()));
            Ok(Value::Int(i64::from(result.is_ok())))
        },
    );

    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::REMOVE_OP,
        |ctx, effect, args| {
            let path = string_arg(ctx, effect, args, "fsRemove")?;
            let path = Path::new(path);
            let removed = if path.is_dir() {
                fs::remove_dir_all(path)
            } else {
                fs::remove_file(path)
            }
            .is_ok();
            Ok(Value::Int(i64::from(removed)))
        },
    );

    host.register_effect_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::CREATE_DIR_ALL_OP,
        |ctx, effect, args| {
            let path = string_arg(ctx, effect, args, "fsCreateDirAll")?;
            Ok(Value::Int(i64::from(fs::create_dir_all(path).is_ok())))
        },
    );
}
