use musi_native::NativeHost;
use musi_vm::{BitsValue, ForeignCall, Value, VmResult};

use super::errors::foreign_rejected;

const BITS_PREFIX: &str = "@@std@0.1.0/bits.ms::";

pub(super) fn register(host: &mut NativeHost) {
    for width in [1, 8, 16, 32, 64] {
        register_width(host, width);
    }
}

fn register_width(host: &mut NativeHost, width: u32) {
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__zero{width}"),
        move |foreign, args| {
            reject_args(foreign, args, 0)?;
            Ok(Value::Bits(BitsValue::from_u64(width, 0)))
        },
    );
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__ones{width}"),
        move |foreign, args| {
            reject_args(foreign, args, 0)?;
            let value = if width == 64 {
                u64::MAX
            } else {
                (1_u64 << width) - 1
            };
            Ok(Value::Bits(BitsValue::from_u64(width, value)))
        },
    );
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__fromNat{width}"),
        move |foreign, args| {
            let value = nat_arg(foreign, args)?;
            Ok(Value::Bits(BitsValue::from_u64(width, value)))
        },
    );
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__fromInt{width}"),
        move |foreign, args| {
            let value = int_arg(foreign, args)?;
            Ok(Value::Bits(BitsValue::from_u64(width, value)))
        },
    );
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__toNat{width}"),
        move |foreign, args| Ok(Value::Nat(bits_arg(foreign, args)?.to_u64().unwrap_or(0))),
    );
    host.register_foreign_handler(
        format!("{BITS_PREFIX}Musi__toInt{width}"),
        move |foreign, args| {
            let value = bits_arg(foreign, args)?.to_u64().unwrap_or(0);
            i64::try_from(value)
                .map(Value::Int)
                .map_err(|_| foreign_rejected(foreign))
        },
    );
}

fn reject_args(foreign: &ForeignCall, args: &[Value], expected: usize) -> VmResult {
    if args.len() == expected {
        Ok(())
    } else {
        Err(foreign_rejected(foreign))
    }
}

fn nat_arg(foreign: &ForeignCall, args: &[Value]) -> VmResult<u64> {
    let [value] = args else {
        return Err(foreign_rejected(foreign));
    };
    match value {
        Value::Nat(value) => Ok(*value),
        Value::Int(value) => u64::try_from(*value).map_err(|_| foreign_rejected(foreign)),
        _ => Err(foreign_rejected(foreign)),
    }
}

fn int_arg(foreign: &ForeignCall, args: &[Value]) -> VmResult<u64> {
    let [value] = args else {
        return Err(foreign_rejected(foreign));
    };
    match value {
        Value::Int(value) => Ok(u64::from_ne_bytes(value.to_ne_bytes())),
        Value::Nat(value) => Ok(*value),
        _ => Err(foreign_rejected(foreign)),
    }
}

fn bits_arg<'a>(foreign: &ForeignCall, args: &'a [Value]) -> VmResult<&'a BitsValue> {
    let [Value::Bits(value)] = args else {
        return Err(foreign_rejected(foreign));
    };
    Ok(value)
}
