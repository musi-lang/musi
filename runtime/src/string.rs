use crate::memory::mem_alloc;

pub unsafe fn string_new(s: &str) -> *const i8 {
    let bytes = s.as_bytes();
    let ptr = mem_alloc(bytes.len() + 1);
    let dest = core::slice::from_raw_parts_mut(ptr, bytes.len());
    dest.copy_from_slice(bytes);
    *ptr.add(bytes.len()) = 0;
    ptr as *const i8
}

pub unsafe fn string_from_bytes(bytes: &[u8]) -> *const i8 {
    let ptr = mem_alloc(bytes.len() + 1);
    let dest = core::slice::from_raw_parts_mut(ptr, bytes.len());
    dest.copy_from_slice(bytes);
    *ptr.add(bytes.len()) = 0;
    ptr as *const i8
}
