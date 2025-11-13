use crate::errors::{MusiError, MusiResult};
use crate::value::Value;

fn assert_with_checks<F>(
    args: &[Value],
    expected_len: usize,
    err_msg: &str,
    pred: F,
) -> MusiResult<Value>
where
    F: Fn(&[Value]) -> MusiResult<bool>,
{
    if args.len() != expected_len {
        return Err(MusiError::InvalidVmState(format!(
            "'{err_msg}' expects {expected_len} argument{}",
            if expected_len == 1 { "" } else { "s" }
        )));
    }
    match pred(args) {
        Ok(true) => Ok(Value::Unit),
        Ok(false) => Err(MusiError::UnhandledException(format!(
            "assertion failed: {err_msg}",
        ))),
        Err(why) => Err(why),
    }
}

pub fn assert(args: &[Value]) -> MusiResult<Value> {
    assert_with_checks(args, 1, "assert", |args| Ok(args[0].is_truthy()))
}

pub fn assert_eq(args: &[Value]) -> MusiResult<Value> {
    assert_with_checks(
        args,
        2,
        &format!(
            "{} =/= {}",
            args.get(0).unwrap_or(&Value::Unit),
            args.get(1).unwrap_or(&Value::Unit)
        ),
        |args| args[0].equals(&args[1]),
    )
}

pub fn assert_neq(args: &[Value]) -> MusiResult<Value> {
    assert_with_checks(
        args,
        2,
        &format!(
            "{} = {}",
            args.get(0).unwrap_or(&Value::Unit),
            args.get(1).unwrap_or(&Value::Unit)
        ),
        |args| args[0].equals(&args[1]).map(|eq| !eq),
    )
}
