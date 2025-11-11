use crate::value::ValueList;
use alloc::vec::Vec;

#[derive(Clone, Copy)]
pub struct ProcInfo {
    pub code_offset: u32,
    pub param_count: u16,
    pub local_count: u16,
}

pub struct Bytecode {
    pub consts: ValueList,
    pub procs: Vec<ProcInfo>,
    pub code: Vec<u8>,
}

pub unsafe fn load(path: &str) -> Bytecode {
    // TODO: read .msc file
    // TODO: parse hdr
    // TODO: load const pool
    // TODO: load proc table
    // TODO: load bc
    Bytecode {
        consts: Vec::new(),
        procs: Vec::new(),
        code: Vec::new(),
    }
}
