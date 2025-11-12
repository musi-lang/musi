use crate::builtin::builtin_register;
use crate::stack::{stack_pop, stack_push};
use crate::stdlib::iface::get_default_interface;
use crate::string::string_new;
use crate::types::{BuiltinFn, Value, VM};

unsafe extern "C" {
    pub unsafe fn strlen(s: *const i8) -> usize;
}

pub unsafe fn stdlib_strlen(vm: *mut VM) {
    let string_value = stack_pop((*vm).stack);
    if let Value::Str(string_ptr) = string_value {
        let len = strlen(string_ptr);
        stack_push((*vm).stack, Value::Int64(len as i64));
    } else {
        panic!("type error in 'strlen'");
    }
}

pub unsafe fn stdlib_puts(vm: *mut VM) {
    let string_value = stack_pop((*vm).stack);
    if let Value::Str(string_ptr) = string_value {
        libc::printf(b"%s\0".as_ptr() as *const i8, string_ptr);
        stack_push((*vm).stack, Value::Int64(0));
    } else {
        panic!("type error in 'puts'");
    }
}

pub unsafe fn boot_stdlib() -> bool {
    // TODO: do i have to use this? i think i did before, but now...?
    let _iface = get_default_interface();

    builtin_register(string_new("strlen"), stdlib_strlen as BuiltinFn, 1);
    builtin_register(string_new("puts"), stdlib_puts as BuiltinFn, 1);

    true
}

pub unsafe fn init_vm_with_stdlib(_vm: *mut VM) -> bool {
    matches!(boot_stdlib(), true)
}
