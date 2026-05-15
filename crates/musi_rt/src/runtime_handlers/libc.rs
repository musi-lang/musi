use std::cmp::Ordering;
use std::io::Error;

use musi_native::NativeHost;
use musi_vm::Value;

use super::errors::foreign_rejected;
use super::values::foreign_string_arg;

pub(super) fn register(host: &mut NativeHost) {
    host.register_foreign_handler_with_context(
        "@@std@0.1.0/libc.ms::C__strcmp",
        |ctx, foreign, args| {
            let [left, right] = args else {
                return Err(foreign_rejected(foreign));
            };
            let left = ctx.string(left).ok_or_else(|| foreign_rejected(foreign))?;
            let right = ctx.string(right).ok_or_else(|| foreign_rejected(foreign))?;
            let value = match left.as_str().cmp(right.as_str()) {
                Ordering::Less => -1,
                Ordering::Equal => 0,
                Ordering::Greater => 1,
            };
            Ok(Value::Int(value))
        },
    );
    host.register_foreign_handler_with_context(
        "@@std@0.1.0/libc.ms::C__strerror",
        |ctx, foreign, args| {
            let [Value::Int(code)] = args else {
                return Err(foreign_rejected(foreign));
            };
            let code = i32::try_from(*code).map_err(|_| foreign_rejected(foreign))?;
            ctx.alloc_string(Error::from_raw_os_error(code).to_string())
        },
    );
    host.register_foreign_handler_with_context(
        "@@std@0.1.0/libc.ms::C__strlen",
        |ctx, foreign, args| {
            let source = foreign_string_arg(ctx, foreign, args)?;
            Ok(Value::Nat(u64::try_from(source.len()).unwrap_or(u64::MAX)))
        },
    );
}
