use std::fs;
use std::io::Write;
use std::path::Path;

use musi_foundation::fs as foundation_fs;
use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::{foreign_rejected, invalid_runtime_args, runtime_foreign_failed};
use super::values::{foreign_string_arg, string_arg};

pub(super) fn register(host: &mut NativeHost) {
    register_foreign_handlers(host);
    register_effect_handlers(host);
}

fn register_foreign_handlers(host: &mut NativeHost) {
    host.register_foreign_handler_with_context("musi:fs::Musi__readText", |ctx, foreign, args| {
        let path = foreign_string_arg(ctx, foreign, args)?.to_owned();
        let text = fs::read_to_string(path).map_err(|_| foreign_rejected(foreign))?;
        ctx.alloc_string(text)
    });
    host.register_foreign_handler_with_context("musi:fs::Musi__writeText", |ctx, foreign, args| {
        let [path, text] = args else {
            return Err(foreign_rejected(foreign));
        };
        let path = ctx.string(path).ok_or_else(|| foreign_rejected(foreign))?;
        let text = ctx.string(text).ok_or_else(|| foreign_rejected(foreign))?;
        fs::write(path.as_str(), text.as_str()).map_err(|_| foreign_rejected(foreign))?;
        Ok(Value::Unit)
    });
    host.register_foreign_handler_with_context("musi:fs::Musi__exists", |ctx, foreign, args| {
        let path = foreign_string_arg(ctx, foreign, args)?;
        Ok(Value::Int(i64::from(Path::new(path).exists())))
    });
    host.register_foreign_handler_with_context(
        "musi:fs::Musi__appendText",
        |ctx, foreign, args| {
            let [path, text] = args else {
                return Err(foreign_rejected(foreign));
            };
            let path = ctx.string(path).ok_or_else(|| foreign_rejected(foreign))?;
            let text = ctx.string(text).ok_or_else(|| foreign_rejected(foreign))?;
            let append_result = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path.as_str())
                .and_then(|mut file| file.write_all(text.as_str().as_bytes()));
            Ok(Value::Int(i64::from(append_result.is_ok())))
        },
    );
    host.register_foreign_handler_with_context("musi:fs::Musi__remove", |ctx, foreign, args| {
        let path = foreign_string_arg(ctx, foreign, args)?;
        let path = Path::new(path);
        let removed = if path.is_dir() {
            fs::remove_dir_all(path)
        } else {
            fs::remove_file(path)
        }
        .is_ok();
        Ok(Value::Int(i64::from(removed)))
    });
    host.register_foreign_handler_with_context(
        "musi:fs::Musi__createDirAll",
        |ctx, foreign, args| {
            let path = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Int(i64::from(fs::create_dir_all(path).is_ok())))
        },
    );
}

fn register_effect_handlers(host: &mut NativeHost) {
    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::READ_TEXT_OP,
        |ctx, foreign, args| {
            let path = string_arg(ctx, foreign, args, "fsReadText")?.to_owned();
            let text =
                fs::read_to_string(path).map_err(|error| runtime_foreign_failed(foreign, error))?;
            ctx.alloc_string(text)
        },
    );

    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::WRITE_TEXT_OP,
        |ctx, foreign, args| {
            let [path, text] = args else {
                return Err(invalid_runtime_args(
                    foreign,
                    "path and text strings",
                    args.len(),
                ));
            };
            let path = ctx
                .string(path)
                .ok_or_else(|| invalid_runtime_args(foreign, "path string", path.kind()))?;
            let text = ctx
                .string(text)
                .ok_or_else(|| invalid_runtime_args(foreign, "text string", text.kind()))?;
            fs::write(path.as_str(), text.as_str())
                .map_err(|error| runtime_foreign_failed(foreign, error))?;
            Ok(Value::Unit)
        },
    );

    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::EXISTS_OP,
        |ctx, foreign, args| {
            let path = string_arg(ctx, foreign, args, "fsExists")?;
            Ok(Value::Int(i64::from(Path::new(path).exists())))
        },
    );

    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::APPEND_TEXT_OP,
        |ctx, foreign, args| {
            let [path, text] = args else {
                return Err(invalid_runtime_args(
                    foreign,
                    "path and text strings",
                    args.len(),
                ));
            };
            let path = ctx
                .string(path)
                .ok_or_else(|| invalid_runtime_args(foreign, "path string", path.kind()))?;
            let text = ctx
                .string(text)
                .ok_or_else(|| invalid_runtime_args(foreign, "text string", text.kind()))?;
            let append_result = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path.as_str())
                .and_then(|mut file| file.write_all(text.as_str().as_bytes()));
            Ok(Value::Int(i64::from(append_result.is_ok())))
        },
    );

    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::REMOVE_OP,
        |ctx, foreign, args| {
            let path = string_arg(ctx, foreign, args, "fsRemove")?;
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

    host.register_foundation_handler_with_context(
        foundation_fs::EFFECT,
        foundation_fs::CREATE_DIR_ALL_OP,
        |ctx, foreign, args| {
            let path = string_arg(ctx, foreign, args, "fsCreateDirAll")?;
            Ok(Value::Int(i64::from(fs::create_dir_all(path).is_ok())))
        },
    );
}
