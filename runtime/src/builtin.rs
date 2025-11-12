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

pub const MAX_BUILTINS: usize = 16;

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
