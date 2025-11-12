use crate::builtin::builtin_find;
use crate::frame::{frames_drop, frames_is_empty, frames_new, frames_pop};
use crate::stack::{stack_drop, stack_dup, stack_len, stack_new, stack_pop, stack_push};
use crate::types::{Bytecode, Value, VM};
use crate::parse::read_i64_le;

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    // Base Instructions
    Nop,
    Br(i32),
    BrTrue(i32),
    BrFalse(i32),
    Beq(i32),
    Bge(i32),
    Bgt(i32),
    Ble(i32),
    Blt(i32),
    Bne(i32),
    Leave(i32),
    Ret,
    Throw,
    Rethrow,
    Try,
    Defer,

    // Stack Operations
    Dup,
    Pop,

    // Load Constants
    LdNull,
    LdCI4(i32),
    LdCI8(i64),
    LdCN8(u8),
    LdCN16(u16),
    LdCN32(u32),
    LdCN64(u64),
    LdCB32(f32),
    LdCB64(f64),
    LdCD32(f32),
    LdCD64(f64),
    LdCStr(u32),

    // Load/Store Variables
    LdLoc(u32),
    StLoc(u32),
    LdLocA(u32),
    LdArg(u32),
    StArg(u32),
    LdArgA(u32),

    // Object Operations
    NewObj(u32),
    Call(*const i8),
    CallVirt(*const i8),
    CallI,
    LdFld(u32),
    StFld(u32),
    LdFldA(u32),
    LdSFld(u32),
    StSFld(u32),
    LdSFldA(u32),
    NewArr(u32),
    LdElem,
    StElem,
    LdElemA,
    LdLen,

    // Type Operations
    LdType(u32),
    IsInst(u32),
    CastClass(u32),
    Box(u32),
    UnboxAny(u32),
    ConvI8,
    ConvI16,
    ConvI32,
    ConvI64,
    ConvI128,
    ConvN8,
    ConvN16,
    ConvN32,
    ConvN64,
    ConvN128,
    ConvB32,
    ConvB64,
    ConvB128,
    ConvD32,
    ConvD64,
    ConvD128,

    // Arithmetic Operations
    Add,
    AddOvf,
    AddOvfUn,
    Sub,
    SubOvf,
    SubOvfUn,
    Mul,
    MulOvf,
    MulOvfUn,
    Div,
    DivUn,
    Rem,
    RemUn,
    Mod,
    Neg,

    // Logical/Bitwise Operations
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    ShrUn,

    // Comparison Operations
    Ceq,
    Cgt,
    CgtUn,
    Clt,
    CltUn,

    // Memory Management
    Pin,
    Unpin,

    // Dynamic Operations
    LdFldDyn(*const i8),
    StFldDyn(*const i8),
    LdFldADyn(*const i8),
    CallDyn(*const i8),
    CallVirtDyn(*const i8),
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
            Instr::LdCI4(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::LdCI8(n) => {
                stack_push((*vm).stack, Value::Int64(n));
            }
            Instr::LdCN8(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::LdCN16(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::LdCN32(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::LdCN64(n) => {
                stack_push((*vm).stack, Value::Int64(n as i64));
            }
            Instr::LdCB32(f) => {
                stack_push((*vm).stack, Value::Bin64(f as f64));
            }
            Instr::LdCB64(f) => {
                stack_push((*vm).stack, Value::Bin64(f));
            }
            Instr::LdCD32(f) => {
                stack_push((*vm).stack, Value::Bin64(f as f64));
            }
            Instr::LdCD64(f) => {
                stack_push((*vm).stack, Value::Bin64(f));
            }
            Instr::LdCStr(idx) => vm_exec_ldcstr(vm, idx),
            Instr::LdNull => {
                // Use a special integer value to represent null
                stack_push((*vm).stack, Value::Int64(0x7FFFFFFFFFFFFFFF));
            }
            Instr::LdLoc(idx) => vm_exec_ldloc(vm, idx),
            Instr::StLoc(idx) => vm_exec_stloc(vm, idx),
            Instr::LdArg(idx) => vm_exec_ldarg(vm, idx),
            Instr::Add => vm_exec_binop(vm, |a, b| a + b),
            Instr::Sub => vm_exec_binop(vm, |a, b| a - b),
            Instr::Mul => vm_exec_binop(vm, |a, b| a * b),
            Instr::Div => vm_exec_binop(vm, |a, b| {
                if b == 0 {
                    panic!("division by zero");
                }
                a / b
            }),
            Instr::Mod => vm_exec_binop(vm, |a, b| a % b),
            Instr::Neg => {
                let val = stack_pop((*vm).stack);
                if value_is_int64(val) {
                    stack_push((*vm).stack, Value::Int64(-value_as_int64(val)));
                } else {
                    panic!("type error in negation");
                }
            }
            Instr::And => vm_exec_binop(vm, |a, b| a & b),
            Instr::Or => vm_exec_binop(vm, |a, b| a | b),
            Instr::Xor => vm_exec_binop(vm, |a, b| a ^ b),
            Instr::Not => {
                let val = stack_pop((*vm).stack);
                if value_is_int64(val) {
                    stack_push((*vm).stack, Value::Int64(!value_as_int64(val)));
                } else {
                    panic!("type error in bitwise not");
                }
            }
            Instr::Shl => vm_exec_binop(vm, |a, b| a << b),
            Instr::Shr => vm_exec_binop(vm, |a, b| a >> b),
            Instr::Ceq => vm_exec_binop_bool(vm, |a, b| a == b),
            Instr::Cgt => vm_exec_binop_bool(vm, |a, b| a > b),
            Instr::CgtUn => vm_exec_binop_bool(vm, |a, b| a > b), // For simplicity, treat as signed
            Instr::Clt => vm_exec_binop_bool(vm, |a, b| a < b),
            Instr::CltUn => vm_exec_binop_bool(vm, |a, b| a < b), // For simplicity, treat as signed
            Instr::NewObj(type_id) => vm_exec_newobj(vm, type_id),
            Instr::LdFld(idx) => vm_exec_ldfld(vm, idx),
            Instr::StFld(idx) => vm_exec_stfld(vm, idx),
            Instr::LdElem => vm_exec_ldelem(vm),
            Instr::StElem => vm_exec_stelem(vm),
            Instr::LdLen => vm_exec_ldlen(vm),
            Instr::IsInst(type_id) => vm_exec_isinst(vm, type_id),
            Instr::CastClass(type_id) => vm_exec_castclass(vm, type_id),
            Instr::Box(type_id) => vm_exec_box(vm, type_id),
            Instr::UnboxAny(type_id) => vm_exec_unbox_any(vm, type_id),
            Instr::Call(name) => vm_exec_call(vm, name),
            Instr::CallVirt(name) => vm_exec_callvirt(vm, name),
            Instr::CallI => vm_exec_calli(vm),
            Instr::Beq(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if value_equals(a, b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Bge(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if value_as_int64(a) >= value_as_int64(b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Bgt(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if value_as_int64(a) > value_as_int64(b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Ble(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if value_as_int64(a) <= value_as_int64(b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Blt(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if value_as_int64(a) < value_as_int64(b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Bne(offset) => {
                let b = stack_pop((*vm).stack);
                let a = stack_pop((*vm).stack);
                if !value_equals(a, b) {
                    (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                }
            }
            Instr::Leave(offset) => {
                // TODO: Implement proper stack cleanup
                (*vm).ip = ((*vm).ip as i32 + offset) as usize;
            }
            Instr::Throw => {
                panic!("Throw not implemented");
            }
            Instr::Rethrow => {
                panic!("Rethrow not implemented");
            }
            Instr::Try => {
                // TODO: Implement exception handling
                panic!("Try not implemented");
            }
            Instr::Defer => {
                // TODO: Implement defer blocks
                panic!("Defer not implemented");
            }
            Instr::StArg(idx) => vm_exec_starg(vm, idx),
            Instr::LdArgA(idx) => vm_exec_ldarga(vm, idx),
            Instr::LdLocA(idx) => vm_exec_ldloca(vm, idx),
            Instr::LdSFld(idx) => vm_exec_ldsfld(vm, idx),
            Instr::StSFld(idx) => vm_exec_stsfld(vm, idx),
            Instr::LdSFldA(idx) => vm_exec_ldsflda(vm, idx),
            Instr::NewArr(type_id) => vm_exec_newarr(vm, type_id),
            Instr::LdFldA(idx) => vm_exec_ldflda(vm, idx),
            Instr::LdElemA => vm_exec_ldelema(vm),
            Instr::LdLen => vm_exec_ldlen(vm),
            Instr::StElem => vm_exec_stelem(vm),
            Instr::LdElem => vm_exec_ldelem(vm),
            Instr::LdType(type_id) => vm_exec_ldtype(vm, type_id),
            Instr::Rem => vm_exec_binop(vm, |a, b| a % b),
            Instr::ConvI8 => vm_exec_conv_i8(vm),
            Instr::ConvI16 => vm_exec_conv_i16(vm),
            Instr::ConvI32 => vm_exec_conv_i32(vm),
            Instr::ConvI64 => vm_exec_conv_i64(vm),
            Instr::ConvI128 => vm_exec_conv_i128(vm),
            Instr::ConvN8 => vm_exec_conv_n8(vm),
            Instr::ConvN16 => vm_exec_conv_n16(vm),
            Instr::ConvN32 => vm_exec_conv_n32(vm),
            Instr::ConvN64 => vm_exec_conv_n64(vm),
            Instr::ConvN128 => vm_exec_conv_n128(vm),
            Instr::ConvB32 => vm_exec_conv_b32(vm),
            Instr::ConvB64 => vm_exec_conv_b64(vm),
            Instr::ConvB128 => vm_exec_conv_b128(vm),
            Instr::ConvD32 => vm_exec_conv_d32(vm),
            Instr::ConvD64 => vm_exec_conv_d64(vm),
            Instr::ConvD128 => vm_exec_conv_d128(vm),
            Instr::AddOvf => vm_exec_add_ovf(vm),
            Instr::AddOvfUn => vm_exec_add_ovf_un(vm),
            Instr::SubOvf => vm_exec_sub_ovf(vm),
            Instr::SubOvfUn => vm_exec_sub_ovf_un(vm),
            Instr::MulOvf => vm_exec_mul_ovf(vm),
            Instr::MulOvfUn => vm_exec_mul_ovf_un(vm),
            Instr::DivUn => vm_exec_div_un(vm),
            Instr::RemUn => vm_exec_rem_un(vm),
            Instr::ShrUn => vm_exec_shr_un(vm),
            Instr::Pin => vm_exec_pin(vm),
            Instr::Unpin => vm_exec_unpin(vm),
            Instr::LdFldDyn(name) => vm_exec_ldfld_dyn(vm, name),
            Instr::StFldDyn(name) => vm_exec_stfld_dyn(vm, name),
            Instr::LdFldADyn(name) => vm_exec_ldflda_dyn(vm, name),
            Instr::CallDyn(name) => vm_exec_call_dyn(vm, name),
            Instr::CallVirtDyn(name) => vm_exec_callvirt_dyn(vm, name),
            Instr::Ret => vm_exec_ret(vm),
            Instr::Br(offset) => {
                (*vm).ip = ((*vm).ip as i32 + offset) as usize;
            }
            Instr::BrTrue(offset) => {
                let cond = stack_pop((*vm).stack);
                match cond {
                    Value::Bool(b) => {
                        if b {
                            (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                        }
                    }
                    _ => panic!("expected Bool in BrTrue"),
                }
            }
            Instr::BrFalse(offset) => {
                let cond = stack_pop((*vm).stack);
                match cond {
                    Value::Bool(b) => {
                        if !b {
                            (*vm).ip = ((*vm).ip as i32 + offset) as usize;
                        }
                    }
                    _ => panic!("expected Bool in BrFalse"),
                }
            }
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

pub unsafe fn vm_exec_ldcstr(vm: *mut VM, idx: u32) {
    let bytecode = (*vm).bc;
    let value = (&(*(*bytecode).consts))[idx as usize];
    stack_push((*vm).stack, value);
}

pub unsafe fn vm_exec_ldloc(vm: *mut VM, idx: u32) {
    let frames = (*vm).frames;
    if (*frames.locals).is_empty() {
        panic!("no active frame for local variable access");
    }

    let current_locals = (*frames.locals).last().unwrap();
    let value = (&(**current_locals))[idx as usize];
    stack_push((*vm).stack, value);
}

pub unsafe fn vm_exec_stloc(vm: *mut VM, idx: u32) {
    let frames = (*vm).frames;
    if (*frames.locals).is_empty() {
        panic!("no active frame for local variable access");
    }

    let value = stack_pop((*vm).stack);
    let current_locals = (*frames.locals).last().unwrap();
    (&mut (**current_locals))[idx as usize] = value;
}

pub unsafe fn vm_exec_ldarg(vm: *mut VM, idx: u32) {
    if (*(*vm).frames.data).is_empty() {
        panic!("no active frame for argument access");
    }

    let frame = (*(*vm).frames.data).last().unwrap();
    let arg_pos = frame.stack_base + idx as usize;
    let stack = (*vm).stack;
    let value = (&(*stack.data))[arg_pos];
    stack_push(stack, value);
}

pub unsafe fn vm_exec_binop_bool<F>(vm: *mut VM, op: F)
where
    F: Fn(i64, i64) -> bool,
{
    let right = stack_pop((*vm).stack);
    let left = stack_pop((*vm).stack);

    if value_is_int64(left) && value_is_int64(right) {
        let result = op(value_as_int64(left), value_as_int64(right));
        stack_push((*vm).stack, Value::Bool(result));
    } else {
        panic!("type error in boolean comparison operation");
    }
}

pub unsafe fn vm_exec_newobj(vm: *mut VM, _type_id: u32) {
    stack_push((*vm).stack, Value::Unit);
}

pub unsafe fn vm_exec_ldfld(_vm: *mut VM, _idx: u32) {
    panic!("LdFld not implemented");
}

pub unsafe fn vm_exec_stfld(_vm: *mut VM, _idx: u32) {
    panic!("StFld not implemented");
}

pub unsafe fn vm_exec_ldelem(_vm: *mut VM) {
    panic!("LdElem not implemented");
}

pub unsafe fn vm_exec_stelem(_vm: *mut VM) {
    panic!("StElem not implemented");
}

pub unsafe fn vm_exec_ldlen(_vm: *mut VM) {
    panic!("LdLen not implemented");
}

pub unsafe fn vm_exec_isinst(_vm: *mut VM, _type_id: u32) {
    panic!("IsInst not implemented");
}

pub unsafe fn vm_exec_asinst(_vm: *mut VM, _type_id: u32) {
    panic!("AsInst not implemented");
}

pub unsafe fn vm_exec_refinc(_vm: *mut VM) {}

pub unsafe fn vm_exec_refdec(_vm: *mut VM) {}

pub unsafe fn vm_exec_calltail(vm: *mut VM, name: *const i8) {
    vm_exec_call(vm, name);
}

pub unsafe fn vm_exec_call(vm: *mut VM, name: *const i8) {
    let name_str = {
        let mut len = 0;
        while *name.add(len) != 0 {
            len += 1;
        }
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(name as *const u8, len))
    };

    if let Some(builtin_func) = builtin_find(name) {
        builtin_func(vm);
    } else {
        panic!("procedure '{}' not found", name_str);
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

    // Debug: print first few opcodes
    static mut DEBUG_COUNT: usize = 0;
    if DEBUG_COUNT < 3 {
        libc::printf(
            b"opcode: 0x%02x at ip: %d\n\0".as_ptr() as *const i8,
            op as libc::c_uint,
            *ip - 1,
        );
        DEBUG_COUNT += 1;
    }

    // Match MSIL opcodes from encoder.ml
    match op {
        // Base Instructions - CIL compatible
        0x00 => Instr::Nop,
        0x38 => {
            let offset = read_i32_le(code, ip);
            Instr::Br(offset)
        }
        0x39 => {
            let offset = read_i32_le(code, ip);
            Instr::BrTrue(offset)
        }
        0x3A => {
            let offset = read_i32_le(code, ip);
            Instr::BrFalse(offset)
        }
        0x3B => {
            let offset = read_i32_le(code, ip);
            Instr::Beq(offset)
        }
        0x3C => {
            let offset = read_i32_le(code, ip);
            Instr::Bge(offset)
        }
        0x3D => {
            let offset = read_i32_le(code, ip);
            Instr::Bgt(offset)
        }
        0x3E => {
            let offset = read_i32_le(code, ip);
            Instr::Ble(offset)
        }
        0x3F => {
            let offset = read_i32_le(code, ip);
            Instr::Blt(offset)
        }
        0x40 => {
            let offset = read_i32_le(code, ip);
            Instr::Bne(offset)
        }
        0xC7 => {
            let offset = read_i32_le(code, ip);
            Instr::Leave(offset)
        }
        0x2A => Instr::Ret,
        0x7A => Instr::Throw,
        0x7B => Instr::Rethrow,
        0xC8 => Instr::Try,
        0xC9 => Instr::Defer,

        // Stack Operations
        0x25 => Instr::Dup,
        0x26 => Instr::Pop,

        // Load Constants
        0x14 => Instr::LdNull,
        0x20 => {
            let n = read_i32_le(code, ip);
            Instr::LdCI4(n)
        }
        0x21 => {
            let n = read_i64_le(code, ip);
            Instr::LdCI8(n)
        }
        0x22 => {
            let n = read_u32_le(code, ip);
            Instr::LdCN8(n as u8)
        }
        0x23 => {
            let n = read_u32_le(code, ip);
            Instr::LdCN16(n as u16)
        }
        0x24 => {
            let n = read_u32_le(code, ip);
            Instr::LdCN32(n)
        }
        0x2F => {
            let n = read_u32_le(code, ip);
            Instr::LdCN64(n as u64)
        }
        0x30 => {
            let f = read_f32_le(code, ip);
            Instr::LdCB32(f)
        }
        0x31 => {
            let f = read_f64_le(code, ip);
            Instr::LdCB64(f)
        }
        0x32 => {
            let f = read_f32_le(code, ip);
            Instr::LdCD32(f)
        }
        0x33 => {
            let f = read_f64_le(code, ip);
            Instr::LdCD64(f)
        }
        0x72 => {
            let idx = read_u32_le(code, ip);
            Instr::LdCStr(idx)
        }

        // Load/Store Variables
        0x0E => {
            let idx = read_u32_le(code, ip);
            Instr::LdLoc(idx)
        }
        0x0F => {
            let idx = read_u32_le(code, ip);
            Instr::StLoc(idx)
        }
        0x10 => {
            let idx = read_u32_le(code, ip);
            Instr::LdLocA(idx)
        }
        0x02 => {
            let idx = read_u32_le(code, ip);
            Instr::LdArg(idx)
        }
        0x03 => {
            let idx = read_u32_le(code, ip);
            Instr::StArg(idx)
        }
        0x04 => {
            let idx = read_u32_le(code, ip);
            Instr::LdArgA(idx)
        }

        // Object Operations
        0x73 => {
            let ctor = read_u32_le(code, ip);
            Instr::NewObj(ctor)
        }
        0x28 => {
            let name = read_cstr(code, ip);
            Instr::Call(name)
        }
        0x6F => {
            let name = read_cstr(code, ip);
            Instr::CallVirt(name)
        }
        0x29 => Instr::CallI,
        0x7B => {
            let idx = read_u32_le(code, ip);
            Instr::LdFld(idx)
        }
        0x7D => {
            let idx = read_u32_le(code, ip);
            Instr::StFld(idx)
        }
        0x7C => {
            let idx = read_u32_le(code, ip);
            Instr::LdFldA(idx)
        }
        0x80 => {
            let idx = read_u32_le(code, ip);
            Instr::LdSFld(idx)
        }
        0x81 => {
            let idx = read_u32_le(code, ip);
            Instr::StSFld(idx)
        }
        0x82 => {
            let idx = read_u32_le(code, ip);
            Instr::LdSFldA(idx)
        }
        0x8D => {
            let type_id = read_u32_le(code, ip);
            Instr::NewArr(type_id)
        }
        0xA3 => Instr::LdElem,
        0xA4 => Instr::StElem,
        0x8F => Instr::LdElemA,
        0x8E => Instr::LdLen,

        // Type Operations
        0xD1 => {
            let type_id = read_u32_le(code, ip);
            Instr::LdType(type_id)
        }
        0x75 => {
            let type_id = read_u32_le(code, ip);
            Instr::IsInst(type_id)
        }
        0x74 => {
            let type_id = read_u32_le(code, ip);
            Instr::CastClass(type_id)
        }
        0x8C => {
            let type_id = read_u32_le(code, ip);
            Instr::Box(type_id)
        }
        0xA5 => {
            let type_id = read_u32_le(code, ip);
            Instr::UnboxAny(type_id)
        }
        0x67 => Instr::ConvI8,
        0x68 => Instr::ConvI16,
        0x69 => Instr::ConvI32,
        0x6A => Instr::ConvI64,
        0xD0 => Instr::ConvI128,
        0xD2 => Instr::ConvN8,
        0xD3 => Instr::ConvN16,
        0xD4 => Instr::ConvN32,
        0xD5 => Instr::ConvN64,
        0xD6 => Instr::ConvN128,
        0xB6 => Instr::ConvB32,
        0xB7 => Instr::ConvB64,
        0xD7 => Instr::ConvB128,
        0xD8 => Instr::ConvD32,
        0xD9 => Instr::ConvD64,
        0xDA => Instr::ConvD128,

        // Arithmetic Operations
        0x58 => Instr::Add,
        0xD6 => Instr::AddOvf,
        0xD7 => Instr::AddOvfUn,
        0x59 => Instr::Sub,
        0xD8 => Instr::SubOvf,
        0xD9 => Instr::SubOvfUn,
        0x5A => Instr::Mul,
        0xDA => Instr::MulOvf,
        0xDB => Instr::MulOvfUn,
        0x5B => Instr::Div,
        0x5C => Instr::DivUn,
        0x5D => Instr::Rem,
        0x5E => Instr::RemUn,
        0xDC => Instr::Mod,
        0x65 => Instr::Neg,

        // Logical/Bitwise Operations
        0x5F => Instr::And,
        0x60 => Instr::Or,
        0x61 => Instr::Xor,
        0x66 => Instr::Not,
        0x62 => Instr::Shl,
        0x63 => Instr::Shr,
        0x64 => Instr::ShrUn,

        // Comparison Operations
        0xFE => Instr::Ceq,
        0xC2 => Instr::Cgt,
        0xC3 => Instr::CgtUn,
        0xC4 => Instr::Clt,
        0xC5 => Instr::CltUn,

        // Memory Management
        0xDF => Instr::Pin,
        0xE0 => Instr::Unpin,

        // Dynamic Operations
        0xE1 => {
            let name = read_cstr(code, ip);
            Instr::LdFldDyn(name)
        }
        0xE2 => {
            let name = read_cstr(code, ip);
            Instr::StFldDyn(name)
        }
        0xE3 => {
            let name = read_cstr(code, ip);
            Instr::LdFldADyn(name)
        }
        0xE4 => {
            let name = read_cstr(code, ip);
            Instr::CallDyn(name)
        }
        0xE5 => {
            let name = read_cstr(code, ip);
            Instr::CallVirtDyn(name)
        }

        _ => panic!("unknown opcode '0x{op:02x}'"),
    }
}

unsafe fn read_i32_le(data: *const u8, pos: *mut usize) -> i32 {
    let mut result: i32 = 0;
    for i in 0..4 {
        result |= (*data.add(*pos + i) as i32) << (i * 8);
    }
    *pos += 4;
    result
}

unsafe fn read_u32_le(data: *const u8, pos: *mut usize) -> u32 {
    let mut result: u32 = 0;
    for i in 0..4 {
        result |= (*data.add(*pos + i) as u32) << (i * 8);
    }
    *pos += 4;
    result
}

unsafe fn read_f32_le(data: *const u8, pos: *mut usize) -> f32 {
    let bits = read_u32_le(data, pos);
    f32::from_bits(bits)
}

unsafe fn read_f64_le(data: *const u8, pos: *mut usize) -> f64 {
    let mut result: u64 = 0;
    for i in 0..8 {
        result |= (*data.add(*pos + i) as u64) << (i * 8);
    }
    *pos += 8;
    f64::from_bits(result)
}

unsafe fn read_cstr(data: *const u8, pos: *mut usize) -> *const i8 {
    let start = *pos;
    while *data.add(*pos) != 0 {
        *pos += 1;
    }
    data.add(start) as *const i8
}

pub unsafe fn disasm_instr(code: *const u8, ip: &mut usize) -> &'static str {
    let op = *code.add(*ip);
    *ip += 1;

    match op {
        // Base Instructions - CIL compatible
        0x00 => "nop",
        0x38 => {
            let _offset = read_i32_le(code, ip);
            "br"
        }
        0x39 => {
            let _offset = read_i32_le(code, ip);
            "brtrue"
        }
        0x3A => {
            let _offset = read_i32_le(code, ip);
            "brfalse"
        }
        0x3B => {
            let _offset = read_i32_le(code, ip);
            "beq"
        }
        0x3C => {
            let _offset = read_i32_le(code, ip);
            "bge"
        }
        0x3D => {
            let _offset = read_i32_le(code, ip);
            "bgt"
        }
        0x3E => {
            let _offset = read_i32_le(code, ip);
            "ble"
        }
        0x3F => {
            let _offset = read_i32_le(code, ip);
            "blt"
        }
        0x40 => {
            let _offset = read_i32_le(code, ip);
            "bne"
        }
        0xC7 => {
            let _offset = read_i32_le(code, ip);
            "leave"
        }
        0x2A => "ret",
        0x7A => "throw",
        0x7B => "rethrow",
        0xC8 => "try",
        0xC9 => "defer",

        // Stack Operations
        0x25 => "dup",
        0x26 => "pop",

        // Load Constants
        0x14 => "ldnull",
        0x20 => {
            let _n = read_i32_le(code, ip);
            "ldc.i4"
        }
        0x21 => {
            let _n = read_i64_le(code, ip);
            "ldc.i8"
        }
        0x22 => {
            let _n = read_u32_le(code, ip);
            "ldc.n8"
        }
        0x23 => {
            let _n = read_u32_le(code, ip);
            "ldc.n16"
        }
        0x24 => {
            let _n = read_u32_le(code, ip);
            "ldc.n32"
        }
        0x2F => {
            let _n = read_u32_le(code, ip);
            "ldc.n64"
        }
        0x30 => {
            let _f = read_f32_le(code, ip);
            "ldc.b32"
        }
        0x31 => {
            let _f = read_f64_le(code, ip);
            "ldc.b64"
        }
        0x32 => {
            let _f = read_f32_le(code, ip);
            "ldc.d32"
        }
        0x33 => {
            let _f = read_f64_le(code, ip);
            "ldc.d64"
        }
        0x72 => {
            let _idx = read_u32_le(code, ip);
            "ldstr"
        }

        // Load/Store Variables
        0x0E => {
            let _idx = read_u32_le(code, ip);
            "ldloc"
        }
        0x0F => {
            let _idx = read_u32_le(code, ip);
            "stloc"
        }
        0x10 => {
            let _idx = read_u32_le(code, ip);
            "ldloca"
        }
        0x02 => {
            let _idx = read_u32_le(code, ip);
            "ldarg"
        }
        0x03 => {
            let _idx = read_u32_le(code, ip);
            "starg"
        }
        0x04 => {
            let _idx = read_u32_le(code, ip);
            "ldarga"
        }

        // Object Operations
        0x73 => {
            let _ctor = read_u32_le(code, ip);
            "newobj"
        }
        0x28 => {
            let _name = read_cstr(code, ip);
            "call"
        }
        0x6F => {
            let _name = read_cstr(code, ip);
            "callvirt"
        }
        0x29 => "calli",
        0x7B => {
            let _idx = read_u32_le(code, ip);
            "ldfld"
        }
        0x7D => {
            let _idx = read_u32_le(code, ip);
            "stfld"
        }
        0x7C => {
            let _idx = read_u32_le(code, ip);
            "ldflda"
        }
        0x80 => {
            let _idx = read_u32_le(code, ip);
            "ldsfld"
        }
        0x81 => {
            let _idx = read_u32_le(code, ip);
            "stsfld"
        }
        0x82 => {
            let _idx = read_u32_le(code, ip);
            "ldsflda"
        }
        0x8D => {
            let _type_id = read_u32_le(code, ip);
            "newarr"
        }
        0xA3 => "ldelem",
        0xA4 => "stelem",
        0x8F => "ldelema",
        0x8E => "ldlen",

        // Type Operations
        0xD1 => {
            let _type = read_u32_le(code, ip);
            "ldtype"
        }
        0x75 => {
            let _type = read_u32_le(code, ip);
            "isinst"
        }
        0x74 => {
            let _type = read_u32_le(code, ip);
            "castclass"
        }
        0x8C => {
            let _type = read_u32_le(code, ip);
            "box"
        }
        0xA5 => {
            let _type = read_u32_le(code, ip);
            "unbox.any"
        }
        0x67 => "conv.i8",
        0x68 => "conv.i16",
        0x69 => "conv.i32",
        0x6A => "conv.i64",
        0xD0 => "conv.i128",
        0xD2 => "conv.n8",
        0xD3 => "conv.n16",
        0xD4 => "conv.n32",
        0xD5 => "conv.n64",
        0xD6 => "conv.n128",
        0xB6 => "conv.b32",
        0xB7 => "conv.b64",
        0xD7 => "conv.b128",
        0xD8 => "conv.d32",
        0xD9 => "conv.d64",
        0xDA => "conv.d128",

        // Arithmetic Operations
        0x58 => "add",
        0xD6 => "add.ovf",
        0xD7 => "add.ovf.un",
        0x59 => "sub",
        0xD8 => "sub.ovf",
        0xD9 => "sub.ovf.un",
        0x5A => "mul",
        0xDA => "mul.ovf",
        0xDB => "mul.ovf.un",
        0x5B => "div",
        0x5C => "div.un",
        0x5D => "rem",
        0x5E => "rem.un",
        0xDC => "mod",
        0x65 => "neg",

        // Logical/Bitwise Operations
        0x5F => "and",
        0x60 => "or",
        0x61 => "xor",
        0x66 => "not",
        0x62 => "shl",
        0x63 => "shr",
        0x64 => "shr.un",

        // Comparison Operations
        0xFE => "ceq",
        0xC2 => "cgt",
        0xC3 => "cgt.un",
        0xC4 => "clt",
        0xC5 => "clt.un",

        // Memory Management
        0xDF => "pin",
        0xE0 => "unpin",

        // Dynamic Operations
        0xE1 => {
            let _name = read_cstr(code, ip);
            "ldfld.dyn"
        }
        0xE2 => {
            let _name = read_cstr(code, ip);
            "stfld.dyn"
        }
        0xE3 => {
            let _name = read_cstr(code, ip);
            "ldfld.addr.dyn"
        }
        0xE4 => {
            let _name = read_cstr(code, ip);
            "call.dyn"
        }
        0xE5 => {
            let _name = read_cstr(code, ip);
            "callvirt.dyn"
        }

        _ => "<unknown>",
    }
}

// Missing function implementations for new MSIL instructions
unsafe fn vm_exec_castclass(_vm: *mut VM, _type_id: u32) {
    // TODO: Implement type casting
    panic!("castclass not implemented");
}

unsafe fn vm_exec_box(_vm: *mut VM, _type_id: u32) {
    // TODO: Implement boxing
    panic!("box not implemented");
}

unsafe fn vm_exec_unbox_any(_vm: *mut VM, _type_id: u32) {
    // TODO: Implement unboxing
    panic!("unbox.any not implemented");
}

unsafe fn vm_exec_callvirt(_vm: *mut VM, _name: *const i8) {
    // TODO: Implement virtual method calls
    panic!("callvirt not implemented");
}

unsafe fn vm_exec_calli(_vm: *mut VM) {
    // TODO: Implement indirect calls
    panic!("calli not implemented");
}

// Helper functions for the new instructions
unsafe fn value_equals(a: Value, b: Value) -> bool {
    match (a, b) {
        (Value::Int64(a), Value::Int64(b)) => a == b,
        (Value::Bin64(a), Value::Bin64(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        _ => false,
    }
}

// Stub implementations for all the new instructions
unsafe fn vm_exec_starg(_vm: *mut VM, _idx: u32) { panic!("starg not implemented"); }
unsafe fn vm_exec_ldarga(_vm: *mut VM, _idx: u32) { panic!("ldarga not implemented"); }
unsafe fn vm_exec_ldloca(_vm: *mut VM, _idx: u32) { panic!("ldloca not implemented"); }
unsafe fn vm_exec_ldsfld(_vm: *mut VM, _idx: u32) { panic!("ldsfld not implemented"); }
unsafe fn vm_exec_stsfld(_vm: *mut VM, _idx: u32) { panic!("stsfld not implemented"); }
unsafe fn vm_exec_ldsflda(_vm: *mut VM, _idx: u32) { panic!("ldsflda not implemented"); }
unsafe fn vm_exec_newarr(_vm: *mut VM, _type_id: u32) { panic!("newarr not implemented"); }
unsafe fn vm_exec_ldelema(_vm: *mut VM) { panic!("ldelema not implemented"); }
unsafe fn vm_exec_ldtype(_vm: *mut VM, _type_id: u32) { panic!("ldtype not implemented"); }
unsafe fn vm_exec_conv_i8(_vm: *mut VM) { panic!("conv.i8 not implemented"); }
unsafe fn vm_exec_conv_i16(_vm: *mut VM) { panic!("conv.i16 not implemented"); }
unsafe fn vm_exec_conv_i32(_vm: *mut VM) { panic!("conv.i32 not implemented"); }
unsafe fn vm_exec_conv_i64(_vm: *mut VM) { panic!("conv.i64 not implemented"); }
unsafe fn vm_exec_conv_i128(_vm: *mut VM) { panic!("conv.i128 not implemented"); }
unsafe fn vm_exec_conv_n8(_vm: *mut VM) { panic!("conv.n8 not implemented"); }
unsafe fn vm_exec_conv_n16(_vm: *mut VM) { panic!("conv.n16 not implemented"); }
unsafe fn vm_exec_conv_n32(_vm: *mut VM) { panic!("conv.n32 not implemented"); }
unsafe fn vm_exec_conv_n64(_vm: *mut VM) { panic!("conv.n64 not implemented"); }
unsafe fn vm_exec_conv_n128(_vm: *mut VM) { panic!("conv.n128 not implemented"); }
unsafe fn vm_exec_conv_b32(_vm: *mut VM) { panic!("conv.b32 not implemented"); }
unsafe fn vm_exec_conv_b64(_vm: *mut VM) { panic!("conv.b64 not implemented"); }
unsafe fn vm_exec_conv_b128(_vm: *mut VM) { panic!("conv.b128 not implemented"); }
unsafe fn vm_exec_conv_d32(_vm: *mut VM) { panic!("conv.d32 not implemented"); }
unsafe fn vm_exec_conv_d64(_vm: *mut VM) { panic!("conv.d64 not implemented"); }
unsafe fn vm_exec_conv_d128(_vm: *mut VM) { panic!("conv.d128 not implemented"); }
unsafe fn vm_exec_add_ovf(_vm: *mut VM) { panic!("add.ovf not implemented"); }
unsafe fn vm_exec_add_ovf_un(_vm: *mut VM) { panic!("add.ovf.un not implemented"); }
unsafe fn vm_exec_sub_ovf(_vm: *mut VM) { panic!("sub.ovf not implemented"); }
unsafe fn vm_exec_sub_ovf_un(_vm: *mut VM) { panic!("sub.ovf.un not implemented"); }
unsafe fn vm_exec_mul_ovf(_vm: *mut VM) { panic!("mul.ovf not implemented"); }
unsafe fn vm_exec_mul_ovf_un(_vm: *mut VM) { panic!("mul.ovf.un not implemented"); }
unsafe fn vm_exec_div_un(_vm: *mut VM) { panic!("div.un not implemented"); }
unsafe fn vm_exec_rem_un(_vm: *mut VM) { panic!("rem.un not implemented"); }
unsafe fn vm_exec_shr_un(_vm: *mut VM) { panic!("shr.un not implemented"); }
unsafe fn vm_exec_pin(_vm: *mut VM) { panic!("pin not implemented"); }
unsafe fn vm_exec_unpin(_vm: *mut VM) { panic!("unpin not implemented"); }
unsafe fn vm_exec_ldfld_dyn(_vm: *mut VM, _name: *const i8) { panic!("ldfld.dyn not implemented"); }
unsafe fn vm_exec_stfld_dyn(_vm: *mut VM, _name: *const i8) { panic!("stfld.dyn not implemented"); }
unsafe fn vm_exec_ldflda_dyn(_vm: *mut VM, _name: *const i8) { panic!("ldfld.addr.dyn not implemented"); }
unsafe fn vm_exec_ldflda(_vm: *mut VM, _idx: u32) { panic!("ldflda not implemented"); }
unsafe fn vm_exec_call_dyn(_vm: *mut VM, _name: *const i8) { panic!("call.dyn not implemented"); }
unsafe fn vm_exec_callvirt_dyn(_vm: *mut VM, _name: *const i8) { panic!("callvirt.dyn not implemented"); }
