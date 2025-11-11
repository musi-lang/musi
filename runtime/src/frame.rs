use crate::value::ValueList;
use alloc::vec::Vec;

#[derive(Clone, Copy)]
pub struct Frame {
    pub ret_addr: u32,
    pub stack_base: usize,
}

pub type FrameList = Vec<Frame>;

pub struct Frames {
    pub data: FrameList,
    pub locals: Vec<ValueList>,
}

const BASE_FRAME_CAPACITY: usize = 256;

impl Frames {
    pub unsafe fn new() -> Self {
        Self {
            data: Vec::with_capacity(BASE_FRAME_CAPACITY),
            locals: Vec::with_capacity(BASE_FRAME_CAPACITY),
        }
    }

    pub unsafe fn push(&mut self, frame: Frame, local_count: usize) {
        self.data.push(frame);
        self.locals.push(Vec::with_capacity(local_count));
    }

    pub unsafe fn pop(&mut self) -> Frame {
        self.locals.pop();
        self.data.pop().unwrap()
    }

    pub unsafe fn current_locals(&mut self) -> &mut ValueList {
        self.locals.last_mut().unwrap()
    }
}
