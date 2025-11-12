use crate::{memory::mem_alloc, memory::mem_free, types::Value};

pub type AllocFn = unsafe extern "C" fn(usize) -> *mut u8;
pub type FreeFn = unsafe extern "C" fn(*mut u8);
pub type CallProcFn = unsafe extern "C" fn(*const i8, *mut Value, u32) -> Value;
pub type PanicFn = unsafe extern "C" fn(*const i8);

#[derive(Clone, Copy)]
#[repr(C)]
pub struct MemoryInterface {
    pub alloc: AllocFn,
    pub free: FreeFn,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct RuntimeInterface {
    pub call_proc: CallProcFn,
    pub panic: PanicFn,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct StdlibInterface {
    pub memory: MemoryInterface,
    pub runtime: RuntimeInterface,
}

unsafe extern "C" {
    pub unsafe fn stdlib_bootstrap(iface: StdlibInterface) -> i32;
}

pub unsafe extern "C" fn default_alloc(size: usize) -> *mut u8 {
    mem_alloc(size)
}

pub unsafe extern "C" fn default_free(ptr: *mut u8) {
    mem_free(ptr);
}

pub unsafe extern "C" fn default_call_proc(
    _name: *const i8,
    _args: *mut Value,
    _arg_count: u32,
) -> Value {
    panic!("STL 'call_proc' not implemented");
}

pub unsafe extern "C" fn default_panic(msg: *const i8) {
    libc::printf(b"STL panic: %s\n\0".as_ptr() as *const i8, msg as *const i8);
    libc::exit(1);
}

pub unsafe fn get_default_interface() -> StdlibInterface {
    StdlibInterface {
        memory: MemoryInterface {
            alloc: default_alloc,
            free: default_free,
        },
        runtime: RuntimeInterface {
            call_proc: default_call_proc,
            panic: default_panic,
        },
    }
}
