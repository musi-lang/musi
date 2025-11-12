use crate::exec::value_is_int64;
use crate::stack::{stack_pop, stack_push};
use crate::types::{Builtin, BuiltinFn, Value, VM};

pub unsafe fn cstr_eq(a: *const i8, b: *const i8) -> bool {
    let mut i = 0;
    loop {
        let a_char = *a.add(i);
        let b_char = *b.add(i);
        if a_char != b_char {
            return false;
        }
        if a_char == 0 {
            return true;
        }
        i += 1;
    }
}

pub const MAX_BUILTINS: usize = 32;

pub unsafe fn builtin_write(vm: *mut VM) {
    let fd = stack_pop((*vm).stack);
    let string_value = stack_pop((*vm).stack);

    if value_is_int64(fd) {
        let fd_val = match fd {
            Value::Int64(v) => v,
            _ => panic!("type error in 'write'"),
        };

        if let Value::Str(string_ptr) = string_value {
            let mut len = 0;
            while *string_ptr.add(len) != 0 {
                len += 1;
            }
            if fd_val == 1 {
                libc::write(1, string_ptr as *const libc::c_void, len);
            }
            stack_push((*vm).stack, Value::Int64(len as i64));
        } else {
            panic!("type error in 'write'");
        }
    } else {
        panic!("type error in 'write'");
    }
}

pub unsafe fn builtin_writeln(vm: *mut VM) {
    let string_value = stack_pop((*vm).stack);

    if let Value::Str(string_ptr) = string_value {
        let mut len = 0;
        while *string_ptr.add(len) != 0 {
            len += 1;
        }
        libc::write(1, string_ptr as *const libc::c_void, len);
        libc::write(1, "\n\0".as_ptr() as *const libc::c_void, 1);
        stack_push((*vm).stack, Value::Int64(len as i64));
    } else {
        panic!("type error in 'writeln'");
    }
}

pub unsafe fn builtin_string_len(vm: *mut VM) {
    let string_value = stack_pop((*vm).stack);

    if let Value::Str(string_ptr) = string_value {
        let mut len = 0;
        while *string_ptr.add(len) != 0 {
            len += 1;
        }
        stack_push((*vm).stack, Value::Int64(len as i64));
    } else {
        panic!("type error in 'string_len'");
    }
}

pub unsafe fn builtin_string_get_char(vm: *mut VM) {
    let index = stack_pop((*vm).stack);
    let string_value = stack_pop((*vm).stack);

    if let (Value::Str(string_ptr), Value::Int64(idx)) = (string_value, index) {
        let mut len = 0;
        while *string_ptr.add(len) != 0 {
            len += 1;
        }

        if idx >= 0 && idx < len as i64 {
            let char_code = *string_ptr.add(idx as usize) as i64;
            stack_push((*vm).stack, Value::Int64(char_code));
        } else {
            stack_push((*vm).stack, Value::Int64(0)); // null character for out of bounds
        }
    } else {
        panic!("type error in 'string_get_char'");
    }
}

pub unsafe fn builtin_string_sub(vm: *mut VM) {
    let length = stack_pop((*vm).stack);
    let start = stack_pop((*vm).stack);
    let string_value = stack_pop((*vm).stack);

    if let (Value::Str(string_ptr), Value::Int64(start_idx), Value::Int64(len_val)) = (string_value, start, length) {
        let mut total_len = 0;
        while *string_ptr.add(total_len) != 0 {
            total_len += 1;
        }

        if start_idx >= 0 && start_idx < total_len as i64 {
            let actual_len = if len_val < 0 { total_len as i64 - start_idx }
                            else { len_val.min(total_len as i64 - start_idx) };

            if actual_len > 0 {
                // Allocate new string and copy substring
                let new_string_ptr = libc::malloc(actual_len as usize + 1) as *mut i8;
                for i in 0..actual_len {
                    *new_string_ptr.add(i as usize) = *string_ptr.add((start_idx + i) as usize);
                }
                *new_string_ptr.add(actual_len as usize) = 0;
                stack_push((*vm).stack, Value::Str(new_string_ptr));
            } else {
                stack_push((*vm).stack, Value::Str("\0".as_ptr() as *mut i8));
            }
        } else {
            stack_push((*vm).stack, Value::Str("\0".as_ptr() as *mut i8));
        }
    } else {
        panic!("type error in 'string_sub'");
    }
}

pub unsafe fn builtin_string_eq(vm: *mut VM) {
    let b = stack_pop((*vm).stack);
    let a = stack_pop((*vm).stack);

    if let (Value::Str(a_ptr), Value::Str(b_ptr)) = (a, b) {
        let result = cstr_eq(a_ptr, b_ptr);
        stack_push((*vm).stack, Value::Bool(result));
    } else {
        panic!("type error in 'string_eq'");
    }
}

pub unsafe fn builtin_string_hash(vm: *mut VM) {
    let string_value = stack_pop((*vm).stack);

    if let Value::Str(string_ptr) = string_value {
        let mut hash = 5381u32;
        let mut i = 0;
        loop {
            let c = *string_ptr.add(i);
            if c == 0 {
                break;
            }
            hash = ((hash << 5).wrapping_add(hash)) ^ c as u32;
            i += 1;
        }
        stack_push((*vm).stack, Value::Int64(hash as i64));
    } else {
        panic!("type error in 'string_hash'");
    }
}

// Simple dynamic array implementation using malloc/realloc
pub unsafe fn builtin_array_new(vm: *mut VM) {
    let initial_capacity = stack_pop((*vm).stack);

    if let Value::Int64(cap) = initial_capacity {
        if cap <= 0 {
            stack_push((*vm).stack, Value::Unit);
        } else {
            // For now, we'll store array length in the first element
            // This is a simplified approach - a real implementation would need proper metadata
            let array_ptr = libc::malloc((cap as usize + 1) * 8) as *mut i64;
            *array_ptr = 0; // Store current length
            stack_push((*vm).stack, Value::Int64(array_ptr as i64));
        }
    } else {
        panic!("type error in 'array_new'");
    }
}

pub unsafe fn builtin_array_push(vm: *mut VM) {
    let value = stack_pop((*vm).stack);
    let array_ptr_value = stack_pop((*vm).stack);

    if let (Value::Int64(value_int), Value::Int64(array_ptr_int)) = (value, array_ptr_value) {
        let array_ptr = array_ptr_int as *mut i64;
        let current_len = *array_ptr as usize;

        // Store the value at the next position
        *array_ptr.add(current_len + 1) = value_int;
        // Update length
        *array_ptr = (current_len + 1) as i64;

        stack_push((*vm).stack, Value::Unit);
    } else {
        panic!("type error in 'array_push'");
    }
}

pub unsafe fn builtin_array_get(vm: *mut VM) {
    let index = stack_pop((*vm).stack);
    let array_ptr_value = stack_pop((*vm).stack);

    if let (Value::Int64(idx), Value::Int64(array_ptr_int)) = (index, array_ptr_value) {
        let array_ptr = array_ptr_int as *mut i64;
        let current_len = *array_ptr as usize;

        if idx >= 0 && idx < current_len as i64 {
            let value = *array_ptr.add((idx + 1) as usize);
            stack_push((*vm).stack, Value::Int64(value));
        } else {
            stack_push((*vm).stack, Value::Int64(0));
        }
    } else {
        panic!("type error in 'array_get'");
    }
}

pub unsafe fn builtin_array_set(vm: *mut VM) {
    let value = stack_pop((*vm).stack);
    let index = stack_pop((*vm).stack);
    let array_ptr_value = stack_pop((*vm).stack);

    if let (Value::Int64(val), Value::Int64(idx), Value::Int64(array_ptr_int)) = (value, index, array_ptr_value) {
        let array_ptr = array_ptr_int as *mut i64;
        let current_len = *array_ptr as usize;

        if idx >= 0 && idx < current_len as i64 {
            *array_ptr.add((idx + 1) as usize) = val;
            stack_push((*vm).stack, Value::Unit);
        } else {
            panic!("array index out of bounds");
        }
    } else {
        panic!("type error in 'array_set'");
    }
}

pub unsafe fn builtin_array_len(vm: *mut VM) {
    let array_ptr_value = stack_pop((*vm).stack);

    if let Value::Int64(array_ptr_int) = array_ptr_value {
        let array_ptr = array_ptr_int as *mut i64;
        let len = *array_ptr;
        stack_push((*vm).stack, Value::Int64(len));
    } else {
        panic!("type error in 'array_len'");
    }
}

pub static mut BUILTINS: [Builtin; MAX_BUILTINS] = [Builtin {
    name: core::ptr::null(),
    func: None,
    param_count: 0,
}; MAX_BUILTINS];
pub static mut BUILTINS_READY: bool = false;

pub unsafe fn init_builtins() {
    if BUILTINS_READY {
        return;
    }

    BUILTINS[0] = Builtin {
        name: "__builtin_write\0".as_ptr() as *const i8,
        func: Some(builtin_write),
        param_count: 2,
    };
    BUILTINS[1] = Builtin {
        name: "writeln\0".as_ptr() as *const i8,
        func: Some(builtin_writeln),
        param_count: 1,
    };
    BUILTINS[2] = Builtin {
        name: "__builtin_string_len\0".as_ptr() as *const i8,
        func: Some(builtin_string_len),
        param_count: 1,
    };
    BUILTINS[3] = Builtin {
        name: "__builtin_string_get_char\0".as_ptr() as *const i8,
        func: Some(builtin_string_get_char),
        param_count: 2,
    };
    BUILTINS[4] = Builtin {
        name: "__builtin_string_sub\0".as_ptr() as *const i8,
        func: Some(builtin_string_sub),
        param_count: 3,
    };
    BUILTINS[5] = Builtin {
        name: "__builtin_string_eq\0".as_ptr() as *const i8,
        func: Some(builtin_string_eq),
        param_count: 2,
    };
    BUILTINS[6] = Builtin {
        name: "__builtin_string_hash\0".as_ptr() as *const i8,
        func: Some(builtin_string_hash),
        param_count: 1,
    };
    BUILTINS[7] = Builtin {
        name: "__builtin_array_new\0".as_ptr() as *const i8,
        func: Some(builtin_array_new),
        param_count: 1,
    };
    BUILTINS[8] = Builtin {
        name: "__builtin_array_push\0".as_ptr() as *const i8,
        func: Some(builtin_array_push),
        param_count: 2,
    };
    BUILTINS[9] = Builtin {
        name: "__builtin_array_get\0".as_ptr() as *const i8,
        func: Some(builtin_array_get),
        param_count: 2,
    };
    BUILTINS[10] = Builtin {
        name: "__builtin_array_set\0".as_ptr() as *const i8,
        func: Some(builtin_array_set),
        param_count: 3,
    };
    BUILTINS[11] = Builtin {
        name: "__builtin_array_len\0".as_ptr() as *const i8,
        func: Some(builtin_array_len),
        param_count: 1,
    };
    BUILTINS_READY = true;
}

pub unsafe fn builtin_find(name: *const i8) -> Option<BuiltinFn> {
    init_builtins();
    let builtins_ptr = core::ptr::addr_of!(BUILTINS);
    for i in 0..MAX_BUILTINS {
        let builtin = *(*builtins_ptr).as_ptr().add(i);
        if let Some(func) = builtin.func {
            if cstr_eq(builtin.name, name) {
                return Some(func);
            }
        }
    }
    None
}

pub unsafe fn builtin_register(name: *const i8, func: BuiltinFn, param_count: u8) -> bool {
    init_builtins();
    let builtins_ptr = core::ptr::addr_of_mut!(BUILTINS);
    for i in 0..MAX_BUILTINS {
        let builtin = (*builtins_ptr).as_mut_ptr().add(i);
        if (*builtin).func.is_none() {
            (*builtin).name = name;
            (*builtin).func = Some(func);
            (*builtin).param_count = param_count;
            return true;
        }
    }
    false
}
