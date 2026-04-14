use std::cell::RefCell;
use std::rc::Rc;
use std::thread::sleep;
use std::time::Duration;

use musi_foundation::runtime as foundation_runtime;
use musi_native::NativeHost;
use musi_vm::Value;

use super::{
    RandomStateCell, current_unix_millis, invalid_runtime_effect, monotonic_origin,
    next_random_int, random_float01, random_int_in_range, random_seed,
};

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TIME_NOW_UNIX_MS_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid timeNowUnixMs args"));
            }
            Ok(Value::Int(current_unix_millis(effect)?))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TIME_MONOTONIC_MS_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(
                    effect,
                    "invalid timeMonotonicMs args",
                ));
            }
            let millis =
                i64::try_from(monotonic_origin().elapsed().as_millis()).unwrap_or(i64::MAX);
            Ok(Value::Int(millis))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TIME_SLEEP_MS_OP,
        |effect, args| {
            let [Value::Int(ms)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid timeSleepMs args"));
            };
            sleep(Duration::from_millis(u64::try_from(*ms).unwrap_or(0)));
            Ok(Value::Unit)
        },
    );

    let random_state = Rc::new(RefCell::new(random_seed()));
    register_random(host, &random_state);
}

fn register_random(host: &mut NativeHost, random_state: &RandomStateCell) {
    let int_random_state = Rc::clone(random_state);
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::RANDOM_INT_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid randomInt args"));
            }
            Ok(Value::Int(next_random_int(&int_random_state)))
        },
    );

    let ranged_random_state = Rc::clone(random_state);
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::RANDOM_INT_IN_RANGE_OP,
        move |effect, args| {
            let [Value::Int(lower_bound), Value::Int(upper_bound)] = args else {
                return Err(invalid_runtime_effect(
                    effect,
                    "invalid randomIntInRange args",
                ));
            };
            Ok(Value::Int(random_int_in_range(
                &ranged_random_state,
                *lower_bound,
                *upper_bound,
            )))
        },
    );

    let bool_random_state = Rc::clone(random_state);
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::RANDOM_BOOL_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid randomBool args"));
            }
            Ok(Value::Int(next_random_int(&bool_random_state) & 1))
        },
    );

    let float_random_state = Rc::clone(random_state);
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::RANDOM_FLOAT_01_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid randomFloat01 args"));
            }
            Ok(Value::Float(random_float01(&float_random_state)))
        },
    );
}
