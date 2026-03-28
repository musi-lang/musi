use musi_vm::{ArrayValue, Value, ValueView, Vm};

use super::model::{HookKind, Outcome, TestEvent};

pub(super) fn parse_event(vm: &Vm, value: Value) -> Result<TestEvent, String> {
    let ArrayValue { elements, .. } =
        decode_array(vm, value).ok_or_else(|| "test event is not an array".to_owned())?;
    if elements.is_empty() {
        return Err("test event must have at least a kind field".into());
    }
    let kind = decode_string(vm, elements[0])
        .ok_or_else(|| "test event kind must be a string".to_owned())?;

    match kind.as_str() {
        "suite_start" => {
            if elements.len() != 2 {
                return Err("suite_start event must have 2 fields".into());
            }
            let name = decode_string(vm, elements[1])
                .ok_or_else(|| "suite_start name must be a string".to_owned())?;
            Ok(TestEvent::SuiteStart(name))
        }
        "suite_end" => {
            if elements.len() != 1 {
                return Err("suite_end event must have 1 field".into());
            }
            Ok(TestEvent::SuiteEnd)
        }
        "case" => {
            if elements.len() != 3 {
                return Err("case event must have 3 fields".into());
            }
            let name = decode_string(vm, elements[1])
                .ok_or_else(|| "case name must be a string".to_owned())?;
            Ok(TestEvent::Case(name, elements[2]))
        }
        "before_all" | "after_all" | "before_each" | "after_each" => {
            if elements.len() != 2 {
                return Err(format!("{kind} event must have 2 fields"));
            }
            let hook_kind = match kind.as_str() {
                "before_all" => HookKind::BeforeAll,
                "after_all" => HookKind::AfterAll,
                "before_each" => HookKind::BeforeEach,
                "after_each" => HookKind::AfterEach,
                _ => unreachable!(),
            };
            Ok(TestEvent::Hook(hook_kind, elements[1]))
        }
        other => Err(format!("unknown test event kind `{other}`")),
    }
}

pub(super) fn interpret_outcome(vm: &Vm, value: Value) -> Result<Outcome, String> {
    let ArrayValue { elements, .. } = decode_array(vm, value)
        .ok_or_else(|| "test result must be an outcome record".to_owned())?;
    if elements.len() != 2 {
        return Err("outcome record must have 2 fields".into());
    }

    let (passed, message) = if let Some(passed) = decode_bool(vm, elements[0]) {
        let message = decode_string(vm, elements[1])
            .ok_or_else(|| "outcome `message` field must be String".to_owned())?;
        (passed, message)
    } else if let Some(passed) = decode_bool(vm, elements[1]) {
        let message = decode_string(vm, elements[0])
            .ok_or_else(|| "outcome `message` field must be String".to_owned())?;
        (passed, message)
    } else {
        return Err("outcome `passed` field must be Bool".to_owned());
    };
    if passed {
        Ok(Outcome::Pass)
    } else {
        Ok(Outcome::Fail(message))
    }
}

pub(super) fn decode_bool(vm: &Vm, value: Value) -> Option<bool> {
    if value.is_bool() {
        return Some(value.as_bool());
    }

    let ArrayValue {
        tag,
        elements: fields,
    } = decode_array(vm, value)?;
    if !fields.is_empty() {
        return None;
    }

    match decode_string(vm, tag)?.as_str() {
        "True" => Some(true),
        "False" => Some(false),
        _ => None,
    }
}

pub(super) fn decode_array(vm: &Vm, value: Value) -> Option<ArrayValue> {
    match vm.inspect(value)? {
        ValueView::Array(array) => Some(array),
        _ => None,
    }
}

pub(super) fn decode_string(vm: &Vm, value: Value) -> Option<String> {
    match vm.inspect(value)? {
        ValueView::String(s) => Some(s),
        _ => None,
    }
}
