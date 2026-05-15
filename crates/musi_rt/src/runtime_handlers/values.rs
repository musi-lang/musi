use musi_vm::{ForeignCall, Value, VmError, VmHostCallContext, VmHostContext};

use super::errors::{foreign_rejected, invalid_runtime_args, runtime_foreign_failed};

pub(super) fn transform_string_arg(
    ctx: VmHostCallContext<'_, '_>,
    foreign: &ForeignCall,
    args: &[Value],
    op_name: &str,
    f: impl FnOnce(&str) -> String,
) -> Result<Value, VmError> {
    let source_text = string_arg(ctx, foreign, args, op_name)?;
    ctx.alloc_string(f(source_text))
}

pub(super) fn normalize_json(source: &str, foreign: &ForeignCall) -> Result<String, VmError> {
    let parsed: serde_json::Value =
        serde_json::from_str(source).map_err(|error| runtime_foreign_failed(foreign, error))?;
    serde_json::to_string(&parsed).map_err(|error| runtime_foreign_failed(foreign, error))
}

pub(super) fn decode_utf8_encoded(
    ctx: VmHostCallContext<'_, '_>,
    foreign: &ForeignCall,
    args: &[Value],
    op_name: &str,
    decoder: impl FnOnce(&str) -> Result<Vec<u8>, Box<str>>,
) -> Result<Value, VmError> {
    let source = string_arg(ctx, foreign, args, op_name)?;
    let bytes = decoder(source).map_err(|reason| runtime_foreign_failed(foreign, reason))?;
    let text = String::from_utf8(bytes).map_err(|error| runtime_foreign_failed(foreign, error))?;
    ctx.alloc_string(text)
}

pub(super) fn string_arg<'a>(
    ctx: &'a VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &'a [Value],
    _op_name: &str,
) -> Result<&'a str, VmError> {
    let [value] = args else {
        return Err(invalid_runtime_args(foreign, "one string", args.len()));
    };
    ctx.string(value)
        .map(|text| text.as_str())
        .ok_or_else(|| invalid_runtime_args(foreign, "string argument", value.kind()))
}

pub(super) fn foreign_string_arg<'a>(
    ctx: &'a VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &'a [Value],
) -> Result<&'a str, VmError> {
    let [value] = args else {
        return Err(foreign_rejected(foreign));
    };
    ctx.string(value)
        .map(|text| text.as_str())
        .ok_or_else(|| foreign_rejected(foreign))
}

pub(super) fn saturating_usize_to_i64(value: usize) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}
