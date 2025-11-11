use crate::frame::Frames;
use crate::loader::Bytecode;
use crate::stack::Stack;

pub struct VM {
    pub stack: Stack,
    pub frames: Frames,
    pub ip: usize,
    pub bc: Bytecode,
}

impl VM {
    pub unsafe fn new(bc: Bytecode) -> Self {
        Self {
            stack: Stack::new(),
            frames: Frames::new(),
            ip: 0,
            bc,
        }
    }

    pub unsafe fn exec(&mut self) {
        // TODO: main exec loop
        // TODO: decode and dispatch instrs
        // TODO: handle ctrl flow
        // TODO: handle calls/rets
    }
}
