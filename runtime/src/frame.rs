use crate::types::{Frame, Frames};

pub const BASE_FRAME_CAPACITY: usize = 256;

pub unsafe fn frames_new() -> Frames {
    let data = Box::into_raw(Box::new(Vec::with_capacity(BASE_FRAME_CAPACITY)));
    let locals = Box::into_raw(Box::new(Vec::with_capacity(BASE_FRAME_CAPACITY)));
    Frames { data, locals }
}

pub unsafe fn frames_drop(frames: Frames) {
    drop(Box::from_raw(frames.data));
    drop(Box::from_raw(frames.locals));
}

pub unsafe fn frames_pop(frames: Frames) -> Frame {
    let locals_ptr = (*frames.locals).pop().unwrap();
    drop(Box::from_raw(locals_ptr));

    match (*frames.data).pop() {
        Some(frame) => frame,
        None => panic!("frame stack underflow"),
    }
}

pub unsafe fn frames_is_empty(frames: Frames) -> bool {
    (*frames.data).is_empty()
}
