//! `musi:core` builtins — string, array, numeric, and rune operations.

use musi_vm::{Heap, Value, VmError};

type BuiltinFn = fn(&[Value], &mut Heap) -> Result<Value, VmError>;

// ── Dispatch ─────────────────────────────────────────────────────────

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
        // Numeric — int
        "int_abs" => Some(int_abs),
        "int_min" => Some(int_min),
        "int_max" => Some(int_max),
        "int_clamp" => Some(int_clamp),
        "int_pow" => Some(int_pow),
        // Numeric — float
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
        // Numeric — constants
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

// ── Helpers ──────────────────────────────────────────────────────────

fn get_str<'a>(val: Value, heap: &'a Heap) -> Result<&'a str, VmError> {
    let ptr = val.as_ref()?;
    let obj = heap.get(ptr)?;
    obj.string.as_deref().ok_or(VmError::TypeError {
        expected: "string",
        found: "non-string ref",
    })
}

fn get_rune(val: Value) -> Result<char, VmError> {
    // Rune tag = 0x7FF4, payload is a u32 Unicode scalar.
    const TAG_RUNE: u16 = 0x7FF4;
    if val.tag() != TAG_RUNE {
        return Err(VmError::TypeError {
            expected: "rune",
            found: "non-rune",
        });
    }
    let payload = val.0 & 0x0000_FFFF_FFFF_FFFF;
    let scalar = u32::try_from(payload).map_err(|_| VmError::TypeError {
        expected: "rune",
        found: "invalid rune payload",
    })?;
    char::from_u32(scalar).ok_or(VmError::TypeError {
        expected: "rune",
        found: "invalid Unicode scalar",
    })
}

fn arg(args: &[Value], idx: usize) -> Result<Value, VmError> {
    args.get(idx).copied().ok_or_else(|| VmError::Malformed {
        desc: format!("expected argument at index {idx}").into_boxed_str(),
    })
}

// ── String builtins ──────────────────────────────────────────────────

fn str_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    Ok(Value::from_int(s.chars().count() as i64))
}

fn str_byte_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    Ok(Value::from_int(s.len() as i64))
}

fn str_at(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let idx = arg(args, 1)?.as_int()? as usize;
    let c = s.chars().nth(idx).ok_or(VmError::OutOfBounds {
        index: idx,
        len: s.chars().count(),
    })?;
    Ok(Value::from_rune(c))
}

fn str_slice(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let start = arg(args, 1)?.as_int()? as usize;
    let end = arg(args, 2)?.as_int()? as usize;
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
    let idx = haystack.find(needle).map_or(-1i64, |byte_idx| {
        haystack[..byte_idx].chars().count() as i64
    });
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

fn str_split(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let delim = get_str(arg(args, 1)?, heap)?;
    let owned_parts: Vec<Box<str>> = s.split(delim).map(Box::from).collect();
    let parts: Vec<Value> = owned_parts
        .into_iter()
        .map(|part| {
            let ptr = heap.alloc_string(0, part);
            Value::from_ref(ptr)
        })
        .collect();
    let arr_ptr = heap.alloc_array(0, parts);
    Ok(Value::from_ref(arr_ptr))
}

fn str_replace(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let from = get_str(arg(args, 1)?, heap)?;
    let to = get_str(arg(args, 2)?, heap)?;
    let result = s.replace(from, to);
    let ptr = heap.alloc_string(0, result.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn str_repeat(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let s = get_str(arg(args, 0)?, heap)?;
    let n = arg(args, 1)?.as_int()? as usize;
    let result = s.repeat(n);
    let ptr = heap.alloc_string(0, result.into_boxed_str());
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
    let obj = heap.get(arr_ref)?;
    let elems = obj.elems.clone();
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
    match s.parse::<f64>() {
        Ok(f) => Ok(Value::from_float(f)),
        Err(_) => Ok(Value::NAN),
    }
}

// ── Array builtins ───────────────────────────────────────────────────

fn arr_len(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let obj = heap.get(ptr)?;
    Ok(Value::from_int(obj.elems.len() as i64))
}

fn arr_push(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let val = arg(args, 1)?;
    let obj = heap.get_mut(ptr)?;
    obj.elems.push(val);
    Ok(Value::UNIT)
}

fn arr_pop(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let obj = heap.get_mut(ptr)?;
    let val = obj
        .elems
        .pop()
        .ok_or(VmError::OutOfBounds { index: 0, len: 0 })?;
    Ok(val)
}

fn arr_slice(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let start = arg(args, 1)?.as_int()? as usize;
    let end = arg(args, 2)?.as_int()? as usize;
    let obj = heap.get(ptr)?;
    let len = obj.elems.len();
    let s = start.min(len);
    let e = end.min(len);
    let sliced = obj.elems[s..e].to_vec();
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
    let obj = heap.get_mut(ptr)?;
    obj.elems.reverse();
    Ok(Value::UNIT)
}

fn arr_contains(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let needle = arg(args, 1)?;
    let obj = heap.get(ptr)?;
    let found = obj.elems.iter().any(|v| v.0 == needle.0);
    Ok(Value::from_bool(found))
}

fn arr_index_of(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let needle = arg(args, 1)?;
    let obj = heap.get(ptr)?;
    let idx = obj
        .elems
        .iter()
        .position(|v| v.0 == needle.0)
        .map_or(-1i64, |i| i as i64);
    Ok(Value::from_int(idx))
}

fn arr_sort(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let ptr = arg(args, 0)?.as_ref()?;
    let obj = heap.get_mut(ptr)?;
    obj.elems.sort_by(|a, b| {
        // Numeric comparison: try int first, then float.
        if let (Ok(ai), Ok(bi)) = (a.as_int(), b.as_int()) {
            return ai.cmp(&bi);
        }
        if let (Ok(af), Ok(bf)) = (a.as_float(), b.as_float()) {
            return af.partial_cmp(&bf).unwrap_or(std::cmp::Ordering::Equal);
        }
        // Fallback: raw bit comparison for stable ordering.
        a.0.cmp(&b.0)
    });
    Ok(Value::UNIT)
}

// ── Numeric builtins — int ───────────────────────────────────────────

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

fn int_pow(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let base = arg(args, 0)?.as_int()?;
    let exp = arg(args, 1)?.as_int()?;
    let exp_u32 = u32::try_from(exp).map_err(|_| VmError::Malformed {
        desc: "int_pow: exponent must be non-negative and fit in u32".into(),
    })?;
    Ok(Value::from_int(base.wrapping_pow(exp_u32)))
}

// ── Numeric builtins — float ─────────────────────────────────────────

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

// ── Numeric constants ────────────────────────────────────────────────

fn float_pi(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(std::f64::consts::PI))
}

fn float_e(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(std::f64::consts::E))
}

fn float_infinity(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_float(f64::INFINITY))
}

fn float_nan(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::NAN)
}

fn int_min_val(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_int(-(1i64 << 47)))
}

fn int_max_val(_args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    Ok(Value::from_int((1i64 << 47) - 1))
}

// ── Rune builtins ────────────────────────────────────────────────────

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
    Ok(Value::from_int(c as i64))
}

fn rune_from_int(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let n = arg(args, 0)?.as_int()?;
    let scalar = u32::try_from(n)
        .ok()
        .and_then(char::from_u32)
        .unwrap_or('\0');
    Ok(Value::from_rune(scalar))
}

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_string(heap: &mut Heap, s: &str) -> Value {
        let ptr = heap.alloc_string(0, Box::from(s));
        Value::from_ref(ptr)
    }

    fn make_array(heap: &mut Heap, elems: Vec<Value>) -> Value {
        let ptr = heap.alloc_array(0, elems);
        Value::from_ref(ptr)
    }

    fn extract_str<'a>(val: Value, heap: &'a Heap) -> &'a str {
        let ptr = val.as_ref().unwrap();
        heap.get(ptr).unwrap().string.as_deref().unwrap()
    }

    fn extract_elems(val: Value, heap: &Heap) -> Vec<Value> {
        let ptr = val.as_ref().unwrap();
        heap.get(ptr).unwrap().elems.clone()
    }

    // ── String tests ─────────────────────────────────────────────────

    #[test]
    fn test_str_len() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello");
        let result = str_len(&[s], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 5);
    }

    #[test]
    fn test_str_len_unicode() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "héllo");
        let result = str_len(&[s], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 5);
    }

    #[test]
    fn test_str_byte_len() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "héllo");
        let result = str_byte_len(&[s], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 6); // é is 2 bytes
    }

    #[test]
    fn test_str_at() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "abc");
        let idx = Value::from_int(1);
        let result = str_at(&[s, idx], &mut heap).unwrap();
        assert_eq!(get_rune(result).unwrap(), 'b');
    }

    #[test]
    fn test_str_slice() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let result = str_slice(&[s, Value::from_int(0), Value::from_int(5)], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hello");
    }

    #[test]
    fn test_str_contains() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let needle = make_string(&mut heap, "world");
        let result = str_contains(&[s, needle], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }

    #[test]
    fn test_str_contains_false() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello");
        let needle = make_string(&mut heap, "xyz");
        let result = str_contains(&[s, needle], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), false);
    }

    #[test]
    fn test_str_starts_with() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let prefix = make_string(&mut heap, "hello");
        let result = str_starts_with(&[s, prefix], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }

    #[test]
    fn test_str_ends_with() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let suffix = make_string(&mut heap, "world");
        let result = str_ends_with(&[s, suffix], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }

    #[test]
    fn test_str_index_of() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let needle = make_string(&mut heap, "world");
        let result = str_index_of(&[s, needle], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 6);
    }

    #[test]
    fn test_str_index_of_not_found() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello");
        let needle = make_string(&mut heap, "xyz");
        let result = str_index_of(&[s, needle], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), -1);
    }

    #[test]
    fn test_str_to_upper() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello");
        let result = str_to_upper(&[s], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "HELLO");
    }

    #[test]
    fn test_str_to_lower() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "HELLO");
        let result = str_to_lower(&[s], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hello");
    }

    #[test]
    fn test_str_trim() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "  hello  ");
        let result = str_trim(&[s], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hello");
    }

    #[test]
    fn test_str_trim_start() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "  hello  ");
        let result = str_trim_start(&[s], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hello  ");
    }

    #[test]
    fn test_str_trim_end() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "  hello  ");
        let result = str_trim_end(&[s], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "  hello");
    }

    #[test]
    fn test_str_split() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "a,b,c");
        let delim = make_string(&mut heap, ",");
        let result = str_split(&[s, delim], &mut heap).unwrap();
        let elems = extract_elems(result, &heap);
        assert_eq!(elems.len(), 3);
        assert_eq!(extract_str(elems[0], &heap), "a");
        assert_eq!(extract_str(elems[1], &heap), "b");
        assert_eq!(extract_str(elems[2], &heap), "c");
    }

    #[test]
    fn test_str_replace() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "hello world");
        let from = make_string(&mut heap, "world");
        let to = make_string(&mut heap, "rust");
        let result = str_replace(&[s, from, to], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hello rust");
    }

    #[test]
    fn test_str_repeat() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "ab");
        let result = str_repeat(&[s, Value::from_int(3)], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "ababab");
    }

    #[test]
    fn test_str_chars() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "abc");
        let result = str_chars(&[s], &mut heap).unwrap();
        let elems = extract_elems(result, &heap);
        assert_eq!(elems.len(), 3);
        assert_eq!(get_rune(elems[0]).unwrap(), 'a');
        assert_eq!(get_rune(elems[1]).unwrap(), 'b');
        assert_eq!(get_rune(elems[2]).unwrap(), 'c');
    }

    #[test]
    fn test_str_from_chars() {
        let mut heap = Heap::new();
        let runes = vec![Value::from_rune('h'), Value::from_rune('i')];
        let arr = make_array(&mut heap, runes);
        let result = str_from_chars(&[arr], &mut heap).unwrap();
        assert_eq!(extract_str(result, &heap), "hi");
    }

    #[test]
    fn test_str_parse_int() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "42");
        let result = str_parse_int(&[s], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 42);
    }

    #[test]
    fn test_str_parse_int_invalid() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "abc");
        let result = str_parse_int(&[s], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 0);
    }

    #[test]
    fn test_str_parse_float() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "3.14");
        let result = str_parse_float(&[s], &mut heap).unwrap();
        let f = result.as_float().unwrap();
        assert!((f - 3.14).abs() < 1e-10);
    }

    #[test]
    fn test_str_parse_float_invalid() {
        let mut heap = Heap::new();
        let s = make_string(&mut heap, "abc");
        let result = str_parse_float(&[s], &mut heap).unwrap();
        assert!(result.as_float().unwrap().is_nan());
    }

    // ── Array tests ──────────────────────────────────────────────────

    #[test]
    fn test_arr_len() {
        let mut heap = Heap::new();
        let arr = make_array(&mut heap, vec![Value::from_int(1), Value::from_int(2)]);
        let result = arr_len(&[arr], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 2);
    }

    #[test]
    fn test_arr_push_pop() {
        let mut heap = Heap::new();
        let arr = make_array(&mut heap, vec![Value::from_int(10)]);
        let _ = arr_push(&[arr, Value::from_int(20)], &mut heap).unwrap();
        let len = arr_len(&[arr], &mut heap).unwrap();
        assert_eq!(len.as_int().unwrap(), 2);

        let popped = arr_pop(&[arr], &mut heap).unwrap();
        assert_eq!(popped.as_int().unwrap(), 20);
        let len = arr_len(&[arr], &mut heap).unwrap();
        assert_eq!(len.as_int().unwrap(), 1);
    }

    #[test]
    fn test_arr_slice() {
        let mut heap = Heap::new();
        let arr = make_array(
            &mut heap,
            vec![
                Value::from_int(1),
                Value::from_int(2),
                Value::from_int(3),
                Value::from_int(4),
            ],
        );
        let result = arr_slice(&[arr, Value::from_int(1), Value::from_int(3)], &mut heap).unwrap();
        let elems = extract_elems(result, &heap);
        assert_eq!(elems.len(), 2);
        assert_eq!(elems[0].as_int().unwrap(), 2);
        assert_eq!(elems[1].as_int().unwrap(), 3);
    }

    #[test]
    fn test_arr_concat() {
        let mut heap = Heap::new();
        let a = make_array(&mut heap, vec![Value::from_int(1), Value::from_int(2)]);
        let b = make_array(&mut heap, vec![Value::from_int(3)]);
        let result = arr_concat(&[a, b], &mut heap).unwrap();
        let elems = extract_elems(result, &heap);
        assert_eq!(elems.len(), 3);
        assert_eq!(elems[2].as_int().unwrap(), 3);
    }

    #[test]
    fn test_arr_reverse() {
        let mut heap = Heap::new();
        let arr = make_array(
            &mut heap,
            vec![Value::from_int(1), Value::from_int(2), Value::from_int(3)],
        );
        let _ = arr_reverse(&[arr], &mut heap).unwrap();
        let elems = extract_elems(arr, &heap);
        assert_eq!(elems[0].as_int().unwrap(), 3);
        assert_eq!(elems[1].as_int().unwrap(), 2);
        assert_eq!(elems[2].as_int().unwrap(), 1);
    }

    #[test]
    fn test_arr_contains() {
        let mut heap = Heap::new();
        let arr = make_array(&mut heap, vec![Value::from_int(10), Value::from_int(20)]);
        let found = arr_contains(&[arr, Value::from_int(10)], &mut heap).unwrap();
        assert_eq!(found.as_bool().unwrap(), true);
        let not_found = arr_contains(&[arr, Value::from_int(99)], &mut heap).unwrap();
        assert_eq!(not_found.as_bool().unwrap(), false);
    }

    #[test]
    fn test_arr_index_of() {
        let mut heap = Heap::new();
        let arr = make_array(
            &mut heap,
            vec![
                Value::from_int(10),
                Value::from_int(20),
                Value::from_int(30),
            ],
        );
        let idx = arr_index_of(&[arr, Value::from_int(20)], &mut heap).unwrap();
        assert_eq!(idx.as_int().unwrap(), 1);
        let not_found = arr_index_of(&[arr, Value::from_int(99)], &mut heap).unwrap();
        assert_eq!(not_found.as_int().unwrap(), -1);
    }

    #[test]
    fn test_arr_sort() {
        let mut heap = Heap::new();
        let arr = make_array(
            &mut heap,
            vec![Value::from_int(3), Value::from_int(1), Value::from_int(2)],
        );
        let _ = arr_sort(&[arr], &mut heap).unwrap();
        let elems = extract_elems(arr, &heap);
        assert_eq!(elems[0].as_int().unwrap(), 1);
        assert_eq!(elems[1].as_int().unwrap(), 2);
        assert_eq!(elems[2].as_int().unwrap(), 3);
    }

    // ── Numeric tests ────────────────────────────────────────────────

    #[test]
    fn test_int_abs() {
        let mut heap = Heap::new();
        let result = int_abs(&[Value::from_int(-42)], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 42);
    }

    #[test]
    fn test_int_min_max() {
        let mut heap = Heap::new();
        let min = int_min(&[Value::from_int(3), Value::from_int(7)], &mut heap).unwrap();
        assert_eq!(min.as_int().unwrap(), 3);
        let max = int_max(&[Value::from_int(3), Value::from_int(7)], &mut heap).unwrap();
        assert_eq!(max.as_int().unwrap(), 7);
    }

    #[test]
    fn test_int_clamp() {
        let mut heap = Heap::new();
        let result = int_clamp(
            &[Value::from_int(15), Value::from_int(0), Value::from_int(10)],
            &mut heap,
        )
        .unwrap();
        assert_eq!(result.as_int().unwrap(), 10);
    }

    #[test]
    fn test_int_pow() {
        let mut heap = Heap::new();
        let result = int_pow(&[Value::from_int(2), Value::from_int(10)], &mut heap).unwrap();
        assert_eq!(result.as_int().unwrap(), 1024);
    }

    #[test]
    fn test_float_sqrt() {
        let mut heap = Heap::new();
        let result = float_sqrt(&[Value::from_float(4.0)], &mut heap).unwrap();
        assert!((result.as_float().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_float_trig() {
        let mut heap = Heap::new();
        let result = float_sin(&[Value::from_float(0.0)], &mut heap).unwrap();
        assert!((result.as_float().unwrap()).abs() < 1e-10);

        let result = float_cos(&[Value::from_float(0.0)], &mut heap).unwrap();
        assert!((result.as_float().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_float_is_nan() {
        let mut heap = Heap::new();
        let result = float_is_nan(&[Value::from_float(f64::NAN)], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
        let result = float_is_nan(&[Value::from_float(1.0)], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), false);
    }

    #[test]
    fn test_float_is_infinite() {
        let mut heap = Heap::new();
        let result = float_is_infinite(&[Value::from_float(f64::INFINITY)], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }

    #[test]
    fn test_float_constants() {
        let mut heap = Heap::new();
        let pi = float_pi(&[], &mut heap).unwrap().as_float().unwrap();
        assert!((pi - std::f64::consts::PI).abs() < 1e-15);
        let e = float_e(&[], &mut heap).unwrap().as_float().unwrap();
        assert!((e - std::f64::consts::E).abs() < 1e-15);
    }

    #[test]
    fn test_int_bounds() {
        let mut heap = Heap::new();
        let min = int_min_val(&[], &mut heap).unwrap().as_int().unwrap();
        let max = int_max_val(&[], &mut heap).unwrap().as_int().unwrap();
        assert_eq!(min, -(1i64 << 47));
        assert_eq!(max, (1i64 << 47) - 1);
    }

    // ── Rune tests ───────────────────────────────────────────────────

    #[test]
    fn test_rune_is_alpha() {
        let mut heap = Heap::new();
        let result = rune_is_alpha(&[Value::from_rune('A')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
        let result = rune_is_alpha(&[Value::from_rune('3')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), false);
    }

    #[test]
    fn test_rune_is_digit() {
        let mut heap = Heap::new();
        let result = rune_is_digit(&[Value::from_rune('9')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
        let result = rune_is_digit(&[Value::from_rune('a')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), false);
    }

    #[test]
    fn test_rune_is_whitespace() {
        let mut heap = Heap::new();
        let result = rune_is_whitespace(&[Value::from_rune(' ')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
        let result = rune_is_whitespace(&[Value::from_rune('a')], &mut heap).unwrap();
        assert_eq!(result.as_bool().unwrap(), false);
    }

    #[test]
    fn test_rune_case_conversion() {
        let mut heap = Heap::new();
        let upper = rune_to_upper(&[Value::from_rune('a')], &mut heap).unwrap();
        assert_eq!(get_rune(upper).unwrap(), 'A');
        let lower = rune_to_lower(&[Value::from_rune('A')], &mut heap).unwrap();
        assert_eq!(get_rune(lower).unwrap(), 'a');
    }

    #[test]
    fn test_rune_int_roundtrip() {
        let mut heap = Heap::new();
        let n = rune_to_int(&[Value::from_rune('A')], &mut heap).unwrap();
        assert_eq!(n.as_int().unwrap(), 65);
        let c = rune_from_int(&[Value::from_int(65)], &mut heap).unwrap();
        assert_eq!(get_rune(c).unwrap(), 'A');
    }

    #[test]
    fn test_rune_from_int_invalid() {
        let mut heap = Heap::new();
        let c = rune_from_int(&[Value::from_int(0xD800)], &mut heap).unwrap(); // surrogate
        assert_eq!(get_rune(c).unwrap(), '\0');
    }
}
