use std::sync::{Arc, Mutex, OnceLock};
use std::thread::sleep;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use musi_foundation::{random as foundation_random, time as foundation_time};
use musi_native::NativeHost;
use musi_vm::{EffectCall, Value, VmError};

use super::errors::{invalid_runtime_args, runtime_effect_failed};

type RandomStateCell = Arc<Mutex<u64>>;

pub(super) fn register(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_time::EFFECT,
        foundation_time::NOW_UNIX_MS_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            Ok(Value::Int(current_unix_millis(effect)?))
        },
    );

    host.register_effect_handler(
        foundation_time::EFFECT,
        foundation_time::MONOTONIC_MS_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            let millis =
                i64::try_from(monotonic_origin().elapsed().as_millis()).unwrap_or(i64::MAX);
            Ok(Value::Int(millis))
        },
    );

    host.register_effect_handler(
        foundation_time::EFFECT,
        foundation_time::SLEEP_MS_OP,
        |effect, args| {
            let [Value::Int(ms)] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "integer milliseconds",
                    args.len(),
                ));
            };
            sleep(Duration::from_millis(u64::try_from(*ms).unwrap_or(0)));
            Ok(Value::Unit)
        },
    );

    let random_state = Arc::new(Mutex::new(random_seed()));
    register_random(host, &random_state);
}

fn register_random(host: &mut NativeHost, random_state: &RandomStateCell) {
    let int_random_state = Arc::clone(random_state);
    host.register_effect_handler(
        foundation_random::EFFECT,
        foundation_random::INT_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            Ok(Value::Int(next_random_int(&int_random_state)))
        },
    );

    let ranged_random_state = Arc::clone(random_state);
    host.register_effect_handler(
        foundation_random::EFFECT,
        foundation_random::INT_IN_RANGE_OP,
        move |effect, args| {
            let [Value::Int(lower_bound), Value::Int(upper_bound)] = args else {
                return Err(invalid_runtime_args(
                    effect,
                    "lower and upper integer bounds",
                    args.len(),
                ));
            };
            Ok(Value::Int(random_int_in_range(
                &ranged_random_state,
                *lower_bound,
                *upper_bound,
            )))
        },
    );

    let bool_random_state = Arc::clone(random_state);
    host.register_effect_handler(
        foundation_random::EFFECT,
        foundation_random::BOOL_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            Ok(Value::Int(next_random_int(&bool_random_state) & 1))
        },
    );

    let float_random_state = Arc::clone(random_state);
    host.register_effect_handler(
        foundation_random::EFFECT,
        foundation_random::FLOAT_01_OP,
        move |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_args(effect, "no arguments", args.len()));
            }
            Ok(Value::Float(random_float01(&float_random_state)))
        },
    );
}

fn current_unix_millis(effect: &EffectCall) -> Result<i64, VmError> {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| runtime_effect_failed(effect, error))?;
    Ok(i64::try_from(now.as_millis()).unwrap_or(i64::MAX))
}

fn random_seed() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| {
            let nanos = duration.as_nanos();
            u64::try_from(nanos).unwrap_or(u64::MAX)
        })
}

fn next_random_int(state: &RandomStateCell) -> i64 {
    let mut state_word = state.lock().expect("random state should lock");
    *state_word = state_word
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1);
    i64::from_ne_bytes(state_word.to_ne_bytes()) & i64::MAX
}

fn random_int_in_range(state: &RandomStateCell, lower_bound: i64, upper_bound: i64) -> i64 {
    if upper_bound <= lower_bound {
        return lower_bound;
    }
    let span = u64::try_from(upper_bound - lower_bound).unwrap_or(0);
    if span == 0 {
        return lower_bound;
    }
    let raw = u64::try_from(next_random_int(state)).unwrap_or(0);
    let offset = i64::try_from(raw % span).unwrap_or(0);
    lower_bound + offset
}

fn random_float01(state: &RandomStateCell) -> f64 {
    let raw_bits = u32::try_from(next_random_int(state) & i64::from(u32::MAX)).unwrap_or(u32::MAX);
    f64::from(raw_bits) / f64::from(u32::MAX)
}

fn monotonic_origin() -> &'static Instant {
    static ORIGIN: OnceLock<Instant> = OnceLock::new();
    ORIGIN.get_or_init(Instant::now)
}
