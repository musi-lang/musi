use crate::builtin::builtin_find;
use crate::frame::{frames_drop, frames_is_empty, frames_new, frames_pop};
use crate::stack::{stack_drop, stack_dup, stack_len, stack_new, stack_pop, stack_push};
use crate::types::{Bytecode, Value, VM};

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Nop,
    Pop,
    Dup,
    LdC(u32),
    LdCI4(i32),
    LdCStr(u32),
    LdLoc(u32),
    StLoc(u32),
    LdArg(u32),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    CmpEq,
    CmpNe,
    CmpLt,
    CmpGt,
    CmpLe,
    CmpGe,
    Br(i32),
    BrTrue(i32),
    BrFalse(i32),
    Call(*const i8),
    Ret,
}

pub unsafe fn value_is_int64(v: Value) -> bool {
    matches!(v, Value::Int64(_))
}

pub unsafe fn value_as_int64(v: Value) -> i64 {
    match v {
        Value::Int64(i) => i,
        _ => panic!("expected 'Int64'"),
    }
}

pub unsafe fn vm_new(bc: *const Bytecode) -> VM {
    VM {
        stack: stack_new(),
        frames: frames_new(),
        ip: 0,
        bc,
    }
}

pub unsafe fn vm_drop(vm: VM) {
    stack_drop(vm.stack);
    frames_drop(vm.frames);
}

pub unsafe fn vm_exec(vm: *mut VM) {
    loop {
        let bytecode = (*vm).bc;
        let code_ptr = (*(*bytecode).code).as_ptr();
        let instr = decode_instr(code_ptr, &mut (*vm).ip);
        match instr {
            Instr::Nop => {}
            Instr::Pop => {
                stack_pop((*vm).stack);
            }
            Instr::Dup => stack_dup((*vm).stack),
            Instr::LdC(idx) => vm_exec_ldc(vm, idx),
            Instr::LdCI4(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::Add => vm_exec_binop(vm, |a, b| a + b),
            Instr::Sub => vm_exec_binop(vm, |a, b| a - b),
            Instr::Mul => vm_exec_binop(vm, |a, b| a * b),
            Instr::Div => vm_exec_binop(vm, |a, b| {
                if b == 0 {
                    panic!("division by zero");
                }
                a / b
            }),
            Instr::Call(name) => vm_exec_call(vm, name),
            Instr::Ret => vm_exec_ret(vm),
            _ => panic!("unimplemented instruction '{instr:?}'"),
        }
    }
}

pub unsafe fn vm_exec_ldc(vm: *mut VM, idx: u32) {
    let bytecode = (*vm).bc;
    let value = (&(*(*bytecode).consts))[idx as usize];
    stack_push((*vm).stack, value);
}

pub unsafe fn vm_exec_binop<F>(vm: *mut VM, op: F)
where
    F: Fn(i64, i64) -> i64,
{
    let right = stack_pop((*vm).stack);
    let left = stack_pop((*vm).stack);

    if value_is_int64(left) && value_is_int64(right) {
        let result = op(value_as_int64(left), value_as_int64(right));
        stack_push((*vm).stack, Value::Int64(result));
    } else {
        panic!("type error in binary operation");
    }
}

pub unsafe fn vm_exec_call(vm: *mut VM, name: *const i8) {
    if let Some(builtin_func) = builtin_find(name) {
        builtin_func(vm);
    } else {
        panic!("procedure '{name:?}' not found");
    }
}

pub unsafe fn vm_exec_ret(vm: *mut VM) {
    if frames_is_empty((*vm).frames) {
        libc::exit(0);
    }

    let result = stack_pop((*vm).stack);
    let frame = frames_pop((*vm).frames);

    while stack_len((*vm).stack) > frame.stack_base {
        stack_pop((*vm).stack);
    }

    (*vm).ip = frame.ret_addr as usize;
    stack_push((*vm).stack, result);
}

pub unsafe fn decode_instr(code: *const u8, ip: &mut usize) -> Instr {
    let op = *code.add(*ip);
    *ip += 1;

    match op {
        0x00 => Instr::Nop,
        0x01 => Instr::Pop,
        0x02 => Instr::Dup,
        0x30 => Instr::Add,
        0x31 => Instr::Sub,
        0x32 => Instr::Mul,
        0x33 => Instr::Div,
        0x72 => Instr::Ret,
        _ => panic!("unknown opcode '0x{op:02x}'"),
    }
}
