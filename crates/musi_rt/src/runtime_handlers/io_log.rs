use std::io;
use std::sync::Arc;

use musi_foundation::io as foundation_io;
use musi_native::NativeHost;
use musi_vm::{ForeignCall, Value, VmError, VmHostContext};

use crate::output::RuntimeOutputSinkCell;

use super::errors::{
    foreign_rejected, invalid_runtime_args, runtime_foreign_failed, runtime_host_unavailable,
};

pub(super) fn register(host: &mut NativeHost, output: &RuntimeOutputSinkCell) {
    register_foreign_io_handlers(host, output);
    register_io_effect_handlers(host, output);
}

fn register_foreign_io_handlers(host: &mut NativeHost, output: &RuntimeOutputSinkCell) {
    let foreign_stdout_output = Arc::clone(output);
    host.register_foreign_handler_with_context(
        "musi:io::Musi__write",
        move |ctx, foreign, args| {
            write_foreign_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stdout,
                false,
                &foreign_stdout_output,
            )
        },
    );
    let foreign_stdout_line_output = Arc::clone(output);
    host.register_foreign_handler_with_context(
        "musi:io::Musi__writeLn",
        move |ctx, foreign, args| {
            write_foreign_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stdout,
                true,
                &foreign_stdout_line_output,
            )
        },
    );
    let foreign_stderr_output = Arc::clone(output);
    host.register_foreign_handler_with_context(
        "musi:io::Musi__writeErr",
        move |ctx, foreign, args| {
            write_foreign_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stderr,
                false,
                &foreign_stderr_output,
            )
        },
    );
    let foreign_stderr_line_output = Arc::clone(output);
    host.register_foreign_handler_with_context(
        "musi:io::Musi__writeErrLn",
        move |ctx, foreign, args| {
            write_foreign_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stderr,
                true,
                &foreign_stderr_line_output,
            )
        },
    );
    host.register_foreign_handler_with_context("musi:io::Musi__readLine", |ctx, foreign, args| {
        if !args.is_empty() {
            return Err(foreign_rejected(foreign));
        }
        ctx.alloc_string(read_line_foreign(foreign)?)
    });
}

fn register_io_effect_handlers(host: &mut NativeHost, output: &RuntimeOutputSinkCell) {
    let stdout_output = Arc::clone(output);
    host.register_foundation_handler_with_context(
        foundation_io::EFFECT,
        foundation_io::WRITE_OP,
        move |ctx, foreign, args| {
            write_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stdout,
                false,
                &stdout_output,
            )
        },
    );
    let stdout_line_output = Arc::clone(output);
    host.register_foundation_handler_with_context(
        foundation_io::EFFECT,
        foundation_io::WRITE_LN_OP,
        move |ctx, foreign, args| {
            write_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stdout,
                true,
                &stdout_line_output,
            )
        },
    );
    let stderr_output = Arc::clone(output);
    host.register_foundation_handler_with_context(
        foundation_io::EFFECT,
        foundation_io::WRITE_ERR_OP,
        move |ctx, foreign, args| {
            write_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stderr,
                false,
                &stderr_output,
            )
        },
    );
    let stderr_line_output = Arc::clone(output);
    host.register_foundation_handler_with_context(
        foundation_io::EFFECT,
        foundation_io::WRITE_ERR_LN_OP,
        move |ctx, foreign, args| {
            write_stream(
                ctx,
                foreign,
                args,
                StreamKind::Stderr,
                true,
                &stderr_line_output,
            )
        },
    );
    host.register_foundation_handler_with_context(
        foundation_io::EFFECT,
        foundation_io::READ_LINE_OP,
        |ctx, foreign, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(foreign, "no arguments", args.len()));
            }
            ctx.alloc_string(read_line(foreign)?)
        },
    );
}

#[derive(Clone, Copy)]
enum StreamKind {
    Stdout,
    Stderr,
}

fn write_stream(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
    stream: StreamKind,
    line: bool,
    output: &RuntimeOutputSinkCell,
) -> Result<Value, VmError> {
    let [text] = args else {
        return Err(invalid_runtime_args(foreign, "one string", args.len()));
    };
    let Some(text) = ctx.string(text) else {
        return Err(invalid_runtime_args(
            foreign,
            "string argument",
            text.kind(),
        ));
    };
    let write_result = match stream {
        StreamKind::Stdout => output
            .lock()
            .map_err(|_| runtime_host_unavailable(foreign, "runtime output lock"))?
            .write_stdout(text.as_str(), line),
        StreamKind::Stderr => output
            .lock()
            .map_err(|_| runtime_host_unavailable(foreign, "runtime output lock"))?
            .write_stderr(text.as_str(), line),
    };
    write_result.map_err(|error| runtime_foreign_failed(foreign, error))?;
    Ok(Value::Unit)
}

fn read_line(foreign: &ForeignCall) -> Result<String, VmError> {
    let mut line = String::new();
    let _bytes = io::stdin()
        .read_line(&mut line)
        .map_err(|error| runtime_foreign_failed(foreign, error))?;
    if line.ends_with('\n') {
        let _ = line.pop();
        if line.ends_with('\r') {
            let _ = line.pop();
        }
    }
    Ok(line)
}

fn foreign_string_arg<'a>(
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

fn write_foreign_stream(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
    stream: StreamKind,
    line: bool,
    output: &RuntimeOutputSinkCell,
) -> Result<Value, VmError> {
    let text = foreign_string_arg(ctx, foreign, args)?;
    let write_result = match stream {
        StreamKind::Stdout => output
            .lock()
            .map_err(|_| foreign_rejected(foreign))?
            .write_stdout(text, line),
        StreamKind::Stderr => output
            .lock()
            .map_err(|_| foreign_rejected(foreign))?
            .write_stderr(text, line),
    };
    write_result.map_err(|_| foreign_rejected(foreign))?;
    Ok(Value::Unit)
}

fn read_line_foreign(foreign: &ForeignCall) -> Result<String, VmError> {
    let mut line = String::new();
    let _bytes = io::stdin()
        .read_line(&mut line)
        .map_err(|_| foreign_rejected(foreign))?;
    if line.ends_with('\n') {
        let _ = line.pop();
        if line.ends_with('\r') {
            let _ = line.pop();
        }
    }
    Ok(line)
}
