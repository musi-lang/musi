use musi_macros::musi_module;

#[musi_module]
pub mod array {
    use std::cell::RefCell;

    #[musi_src("['T](arr: []'T): Int")]
    pub fn array_length(arr: Value) -> i64 {
        match arr {
            Value::Array(a) => i64::try_from(a.borrow().len()).expect("array length fits i64"),
            _ => 0,
        }
    }

    #[musi_src("['T](arr: []'T, val: 'T): Unit")]
    pub fn array_push(arr: Value, val: Value) -> () {
        if let Value::Array(a) = arr {
            a.borrow_mut().push(val);
        }
    }

    #[musi_src("['T](arr: []'T): Option['T]")]
    pub fn array_pop(arr: Value) -> Option<Value> {
        match arr {
            Value::Array(a) => a.borrow_mut().pop(),
            _ => None,
        }
    }

    #[musi_src("['T](arr: []'T, idx: Int): 'T")]
    pub fn array_get(arr: Value, idx: i64) -> Value {
        match arr {
            Value::Array(a) => {
                let borrowed = a.borrow();
                let usize_idx = usize::try_from(idx).ok();
                match usize_idx.and_then(|i| borrowed.get(i)) {
                    Some(v) => v.clone(),
                    None    => Value::Unit,
                }
            }
            _ => Value::Unit,
        }
    }

    #[musi_src("['T](arr: []'T, idx: Int, val: 'T): Unit")]
    pub fn array_set(arr: Value, idx: i64, val: Value) -> () {
        if let Value::Array(a) = arr {
            if let Ok(i) = usize::try_from(idx) {
                let mut borrowed = a.borrow_mut();
                if let Some(slot) = borrowed.get_mut(i) {
                    *slot = val;
                }
            }
        }
    }

    #[musi_src("['T](arr: []'T, start: Int, end: Int): []'T")]
    pub fn array_slice(arr: Value, start: i64, end: i64) -> Value {
        match arr {
            Value::Array(a) => {
                let borrowed = a.borrow();
                let len = i64::try_from(borrowed.len()).expect("array length fits i64");
                let (lo, hi) = crate::registry::slice_range(start, end, len);
                let sliced: Vec<Value> = borrowed[lo..hi].to_vec();
                Value::Array(Rc::new(RefCell::new(sliced)))
            }
            _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
        }
    }
}
