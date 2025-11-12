pub unsafe fn mem_alloc(size: usize) -> *mut u8 {
    libc::malloc(size) as *mut u8
}

pub unsafe fn mem_free(ptr: *mut u8) {
    libc::free(ptr as *mut libc::c_void);
}
