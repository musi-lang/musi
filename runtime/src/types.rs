#[derive(Clone, Copy)]
#[repr(C)]
pub enum Value {
    Int64(i64),
    Bin64(f64),
    Bool(bool),
    Str(*const i8),
    Unit,
}

pub type ValueList = *mut Vec<Value>;

#[derive(Clone, Copy)]
pub struct Stack {
    pub data: ValueList,
}

#[derive(Clone, Copy)]
pub struct Frame {
    pub ret_addr: u32,
    pub stack_base: usize,
}

pub type FrameList = *mut Vec<Frame>;

#[derive(Clone, Copy)]
pub struct Frames {
    pub data: FrameList,
    pub locals: *mut Vec<ValueList>,
}

#[derive(Clone, Copy)]
pub struct ProcInfo {
    pub name: *const i8,
    pub code_offset: u32,
    pub param_count: u16,
    pub local_count: u16,
}

pub type ProcTable = *mut Vec<ProcInfo>;

#[derive(Clone, Copy)]
pub struct Bytecode {
    pub consts: ValueList,
    pub procs: ProcTable,
    pub code: *mut Vec<u8>,
}

#[derive(Clone, Copy)]
pub struct VM {
    pub stack: Stack,
    pub frames: Frames,
    pub ip: usize,
    pub bc: *const Bytecode,
}

pub type BuiltinFn = unsafe fn(*mut VM);

#[derive(Clone, Copy)]
pub struct Builtin {
    pub name: *const i8,
    pub func: Option<BuiltinFn>,
    pub param_count: u8,
}
