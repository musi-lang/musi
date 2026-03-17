//! `musi:core` builtins - string, array, numeric, and rune operations.

use std::cmp::Ordering;
use std::f64::consts;

use musi_vm::{Heap, Value, VmError};

type BuiltinFn = fn(&[Value], &mut Heap) -> Result<Value, VmError>;

pub fn lookup(name: &str) -> Option<BuiltinFn> {
    match name {
        // String
        "str_len" => Some(str_len),
        "str_byte_len" => Some(str_byte_len),
        "str_at" => Some(str_at),
        "str_slice" => Some(str_slice),
        "str_contains" => Some(str_contains),
        "str_starts_with" => Some(str_starts_with),
        "str_ends_with" => Some(str_ends_with),
        "str_index_of" => Some(str_index_of),
        "str_to_upper" => Some(str_to_upper),
        "str_to_lower" => Some(str_to_lower),
        "str_trim" => Some(str_trim),
        "str_trim_start" => Some(str_trim_start),
        "str_trim_end" => Some(str_trim_end),
        "str_split" => Some(str_split),
        "str_replace" => Some(str_replace),
        "str_repeat" => Some(str_repeat),
        "str_chars" => Some(str_chars),
        "str_from_chars" => Some(str_from_chars),
        "str_parse_int" => Some(str_parse_int),
        "str_parse_float" => Some(str_parse_float),
        "str_join" => Some(str_join),
        // Array
        "arr_len" => Some(arr_len),
        "arr_push" => Some(arr_push),
        "arr_pop" => Some(arr_pop),
        "arr_slice" => Some(arr_slice),
        "arr_concat" => Some(arr_concat),
        "arr_reverse" => Some(arr_reverse),
        "arr_contains" => Some(arr_contains),
        "arr_index_of" => Some(arr_index_of),
        "arr_sort" => Some(arr_sort),
        // Numeric - int
        "int_abs" => Some(int_abs),
        "int_min" => Some(int_min),
        "int_max" => Some(int_max),
        "int_clamp" => Some(int_clamp),
        "int_pow" => Some(int_pow),
        // Numeric - float
        "float_abs" => Some(float_abs),
        "float_floor" => Some(float_floor),
        "float_ceil" => Some(float_ceil),
        "float_round" => Some(float_round),
        "float_sqrt" => Some(float_sqrt),
        "float_sin" => Some(float_sin),
        "float_cos" => Some(float_cos),
        "float_tan" => Some(float_tan),
        "float_log" => Some(float_log),
        "float_exp" => Some(float_exp),
        "float_is_nan" => Some(float_is_nan),
        "float_is_infinite" => Some(float_is_infinite),
        // Numeric - constants
        "float_pi" => Some(float_pi),
        "float_e" => Some(float_e),
        "float_infinity" => Some(float_infinity),
        "float_nan" => Some(float_nan),
        "int_min_val" => Some(int_min_val),
        "int_max_val" => Some(int_max_val),
        // Rune
        "rune_is_alpha" => Some(rune_is_alpha),
        "rune_is_digit" => Some(rune_is_digit),
        "rune_is_whitespace" => Some(rune_is_whitespace),
        "rune_to_upper" => Some(rune_to_upper),
        "rune_to_lower" => Some(rune_to_lower),
        "rune_to_int" => Some(rune_to_int),
        "rune_from_int" => Some(rune_from_int),
        _ => None,
    }
}

fn get_str(val: Value, heap: &Heap) -> Result<&str, VmError> {
    let ptr = val.as_ref()?;
    let node = heap.get(ptr)?;
    node.string.as_deref().ok_or(VmError::TypeError {
        expected: "string",
        found: "non-string ref",
    })
}

fn get_rune(val: Value) -> Result<char, VmError> {
    val.as_rune()
}

fn arg(args: &[Value], idx: usize) -> Result<Value, VmError> {
    args.get(idx).copied().ok_or_else(|| VmError::Malformed {
        desc: format!("expected argument at index {idx}").into_boxed_str(),
    })
}

fn i64_to_usize(v: i64) -> Result<usize, VmError> {
    usize::try_from(v).map_err(|_| VmError::OutOfBounds { index: 0, len: 0 })
}

fn usize_to_i64(v: usize) -> Result<i64, VmError> {
    i64::try_from(v).map_err(|_| VmError::Malformed {
        desc: "integer overflow".into(),
    })
}

fn str_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    Ok(Value::from_int(usize_to_i64(s.chars().count())?))
}

fn str_byte_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    Ok(Value::from_int(usize_to_i64(s.len())?))
}

fn str_at(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let idx = i64_to_usize(arg(args, 1)?.as_int()?)?;
    let c = s.chars().nth(idx).ok_or_else(|| VmError::OutOfBounds {
        index: idx,
        len: s.chars().count(),
    })?;
    Ok(Value::from_rune(c))
}

fn str_slice(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let start = i64_to_usize(arg(args, 1)?.as_int()?)?;
    let end = i64_to_usize(arg(args, 2)?.as_int()?)?;
    let sliced: String = s
        .chars()
        .skip(start)
        .take(end.saturating_sub(start))
        .collect();
    let ptr = heap.alloc_string(0, sliced.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_contains(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let haystack = get_str(arg(args, 0)?, heap)?;
    let needle = get_str(arg(args, 1)?, heap)?;
    Ok(Value::from_bool(haystack.contains(needle)))
}

fn str_starts_with(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let prefix = get_str(arg(args, 1)?, heap)?;
    Ok(Value::from_bool(s.starts_with(prefix)))
}

fn str_ends_with(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let suffix = get_str(arg(args, 1)?, heap)?;
    Ok(Value::from_bool(s.ends_with(suffix)))
}

fn str_index_of(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let haystack = get_str(arg(args, 0)?, heap)?;
    let needle = get_str(arg(args, 1)?, heap)?;
    let idx = haystack.find(needle).map_or(Ok(-1i64), |byte_idx| {
        usize_to_i64(haystack.get(..byte_idx).unwrap_or("").chars().count())
    })?;
    Ok(Value::from_int(idx))
}

fn str_to_upper(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let upper = s.to_uppercase();
    let ptr = heap.alloc_string(0, upper.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_to_lower(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let lower = s.to_lowercase();
    let ptr = heap.alloc_string(0, lower.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_trim(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let trimmed = s.trim();
    let ptr = heap.alloc_string(0, Box::from(trimmed));
    Ok(Value::from_ref(ptr))
}

fn str_trim_start(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let trimmed = s.trim_start();
    let ptr = heap.alloc_string(0, Box::from(trimmed));
    Ok(Value::from_ref(ptr))
}

fn str_trim_end(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let trimmed = s.trim_end();
    let ptr = heap.alloc_string(0, Box::from(trimmed));
    Ok(Value::from_ref(ptr))
}

#[allow(clippy::needless_collect)] // need to collect into owned strings to release shared borrow on `heap`
fn str_split(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let delim = get_str(arg(args, 1)?, heap)?;
    let owned: Vec<Box<str>> = s.split(delim).map(Box::from).collect();
    let parts = owned
        .into_iter()
        .map(|part| Value::from_ref(heap.alloc_string(0, part)))
        .collect();
    let arr_ptr = heap.alloc_array(0, parts);
    Ok(Value::from_ref(arr_ptr))
}

fn str_replace(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let from = get_str(arg(args, 1)?, heap)?;
    let to = get_str(arg(args, 2)?, heap)?;
    let replaced = s.replace(from, to);
    let ptr = heap.alloc_string(0, replaced.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_repeat(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let n = i64_to_usize(arg(args, 1)?.as_int()?)?;
    let repeated = s.repeat(n);
    let ptr = heap.alloc_string(0, repeated.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_chars(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let runes: Vec<Value> = s.chars().map(Value::from_rune).collect();
    let arr_ptr = heap.alloc_array(0, runes);
    Ok(Value::from_ref(arr_ptr))
}

fn str_from_chars(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let arr_ref = arg(args, 0)?.as_ref()?;
    let arr = heap.get(arr_ref)?;
    let elems = arr.elems.clone();
    let mut s = String::with_capacity(elems.len());
    for v in &elems {
        s.push(get_rune(*v)?);
    }
    let ptr = heap.alloc_string(0, s.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_parse_int(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let n = s.parse::<i64>().unwrap_or(0);
    Ok(Value::from_int(n))
}

fn str_parse_float(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    Ok(s.parse::<f64>().map_or(Value::NAN, Value::from_float))
}

#[allow(clippy::needless_collect)]
fn str_join(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let arr_ptr = arg(args, 0)?.as_ref()?;
    let sep = get_str(arg(args, 1)?, heap)?.to_owned();
    let elems = heap.get(arr_ptr)?.elems.clone();
    let parts: Vec<String> = elems
        .iter()
        .map(|v| {
            let ptr = v.as_ref().unwrap_or_default();
            heap.get(ptr)
                .ok()
                .and_then(|node| node.string.as_deref().map(String::from))
                .unwrap_or_default()
        })
        .collect();
    let result = parts.join(&sep);
    let ptr = heap.alloc_string(0, result.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn arr_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let arr = heap.get(ptr)?;
    Ok(Value::from_int(usize_to_i64(arr.elems.len())?))
}

fn arr_push(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let val = arg(args, 1)?;
    let arr = heap.get_mut(ptr)?;
    arr.elems.push(val);
    Ok(Value::UNIT)
}

fn arr_pop(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let arr = heap.get_mut(ptr)?;
    let val = arr
        .elems
        .pop()
        .ok_or(VmError::OutOfBounds { index: 0, len: 0 })?;
    Ok(val)
}

fn arr_slice(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let start = i64_to_usize(arg(args, 1)?.as_int()?)?;
    let end = i64_to_usize(arg(args, 2)?.as_int()?)?;
    let arr = heap.get(ptr)?;
    let len = arr.elems.len();
    let s = start.min(len);
    let e = end.min(len);
    let sliced = arr.elems[s..e].to_vec();
    let new_ptr = heap.alloc_array(0, sliced);
    Ok(Value::from_ref(new_ptr))
}

fn arr_concat(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr_a = arg(args, 0)?.as_ref()?;
    let ptr_b = arg(args, 1)?.as_ref()?;
    let a = heap.get(ptr_a)?.elems.clone();
    let b = heap.get(ptr_b)?.elems.clone();
    let mut combined = a;
    combined.extend(b);
    let new_ptr = heap.alloc_array(0, combined);
    Ok(Value::from_ref(new_ptr))
}

fn arr_reverse(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let arr = heap.get_mut(ptr)?;
    arr.elems.reverse();
    Ok(Value::UNIT)
}

fn arr_contains(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let needle = arg(args, 1)?;
    let arr = heap.get(ptr)?;
    let found = arr.elems.contains(&needle);
    Ok(Value::from_bool(found))
}

fn arr_index_of(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let needle = arg(args, 1)?;
    let arr = heap.get(ptr)?;
    let idx = arr
        .elems
        .iter()
        .position(|v| *v == needle)
        .map_or(-1i64, |i| i64::try_from(i).unwrap_or(-1));
    Ok(Value::from_int(idx))
}

fn arr_sort(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let arr = heap.get_mut(ptr)?;
    arr.elems.sort_by(|a, b| {
        // Numeric comparison: try int first, then float.
        if let (Ok(ai), Ok(bi)) = (a.as_int(), b.as_int()) {
            return ai.cmp(&bi);
        }
        if let (Ok(af), Ok(bf)) = (a.as_float(), b.as_float()) {
            return af.partial_cmp(&bf).unwrap_or(Ordering::Equal);
        }
        Ordering::Equal
    });
    Ok(Value::UNIT)
}

fn int_abs(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let n = arg(args, 0)?.as_int()?;
    Ok(Value::from_int(n.wrapping_abs()))
}

fn int_min(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let a = arg(args, 0)?.as_int()?;
    let b = arg(args, 1)?.as_int()?;
    Ok(Value::from_int(a.min(b)))
}

fn int_max(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let a = arg(args, 0)?.as_int()?;
    let b = arg(args, 1)?.as_int()?;
    Ok(Value::from_int(a.max(b)))
}

fn int_clamp(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let val = arg(args, 0)?.as_int()?;
    let lo = arg(args, 1)?.as_int()?;
    let hi = arg(args, 2)?.as_int()?;
    Ok(Value::from_int(val.clamp(lo, hi)))
}

fn int_pow(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let base = arg(args, 0)?.as_int_wide(heap)?;
    let exp = arg(args, 1)?.as_int_wide(heap)?;
    let exp_u32 = u32::try_from(exp).map_err(|_| VmError::Malformed {
        desc: "int_pow: exponent must be non-negative and fit in u32".into(),
    })?;
    Ok(Value::from_int_wide(base.wrapping_pow(exp_u32), heap))
}

fn float_abs(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.abs()))
}

fn float_floor(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.floor()))
}

fn float_ceil(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.ceil()))
}

fn float_round(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.round()))
}

fn float_sqrt(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.sqrt()))
}

fn float_sin(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.sin()))
}

fn float_cos(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.cos()))
}

fn float_tan(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.tan()))
}

fn float_log(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.ln()))
}

fn float_exp(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_float(f.exp()))
}

fn float_is_nan(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_bool(f.is_nan()))
}

fn float_is_infinite(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = arg(args, 0)?.as_float()?;
    Ok(Value::from_bool(f.is_infinite()))
}

#[allow(clippy::unnecessary_wraps)]
const fn float_pi(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(consts::PI))
}

#[allow(clippy::unnecessary_wraps)]
const fn float_e(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(consts::E))
}

#[allow(clippy::unnecessary_wraps)]
const fn float_infinity(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(f64::INFINITY))
}

#[allow(clippy::unnecessary_wraps)]
const fn float_nan(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::NAN)
}

#[allow(clippy::unnecessary_wraps)]
fn int_min_val(_args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_int_wide(i64::MIN, heap))
}

#[allow(clippy::unnecessary_wraps)]
fn int_max_val(_args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_int_wide(i64::MAX, heap))
}

fn rune_is_alpha(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    Ok(Value::from_bool(c.is_alphabetic()))
}

fn rune_is_digit(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    Ok(Value::from_bool(c.is_ascii_digit()))
}

fn rune_is_whitespace(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    Ok(Value::from_bool(c.is_whitespace()))
}

fn rune_to_upper(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    let upper = c.to_uppercase().next().unwrap_or(c);
    Ok(Value::from_rune(upper))
}

fn rune_to_lower(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    let lower = c.to_lowercase().next().unwrap_or(c);
    Ok(Value::from_rune(lower))
}

fn rune_to_int(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let c = get_rune(arg(args, 0)?)?;
    Ok(Value::from_int(i64::from(u32::from(c))))
}

fn rune_from_int(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let n = arg(args, 0)?.as_int()?;
    let scalar = u32::try_from(n)
        .ok()
        .and_then(char::from_u32)
        .unwrap_or('\0');
    Ok(Value::from_rune(scalar))
}

#[cfg(test)]
mod tests;
