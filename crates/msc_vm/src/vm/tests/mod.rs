//! VM integration tests.
//!
//! All tests build minimal `.musi` binaries from raw bytes using the SEAM ISA:
//! big-endian operands, no WID/EXT prefixes, new opcode names.
//!
//! Instruction encoding reminders:
//!   LD_CONST  FI16 (3 bytes): op, idx_hi, idx_lo
//!   LD_LOC    FI8  (2 bytes): op, slot
//!   ST_LOC    FI8  (2 bytes): op, slot
//!   BR        FI16 (3 bytes): op, off_hi, off_lo  (signed BE relative to next instr)
//!   BR_FALSE  FI16 (3 bytes): op, off_hi, off_lo
//!   BR_TRUE   FI16 (3 bytes): op, off_hi, off_lo
//!   REC_NEW   FI8x2(3 bytes): op, tag, arity
//!   REC_GET   FI8  (2 bytes): op, idx
//!   REC_SET   FI8  (2 bytes): op, idx
//!   MAT_TAG   FI16 (3 bytes): op, tag_hi, tag_lo
//!   MAT_DATA  F0   (1 byte)
//!   ARR_NEW   F0   (1 byte)  — pops len from stack, pushes ref
//!   ARR_GET   F0   (1 byte)
//!   ARR_SET   F0   (1 byte)
//!   ARR_LEN   F0   (1 byte)
//!   CALL      FI8  (2 bytes): op, arity  — callee below args on stack
//!   CALL_TAIL FI8  (2 bytes): op, arity
//!   CLS_NEW   FI16 (3 bytes): op, fn_hi, fn_lo
//!   EFF_HDL   FI16 (3 bytes): op, eff_hi, eff_lo
//!   EFF_NEED  FI8x2(3 bytes): op, op_id, arity
//!   All arithmetic/compare: F0 (1 byte)
#![allow(clippy::panic)]

mod arith;
mod basic;
mod closure;
mod control;
mod data;
mod effects;
mod gc;

use std::iter;

use msc_bc::{self, Opcode, crc32_slice};

use crate::error::VmError;
use crate::loader::load;
use crate::value::Value;
use crate::verifier::verify;
use crate::vm::{StepResult, Vm};

// ---------------------------------------------------------------------------
// Test binary builder (SEAM format)
// ---------------------------------------------------------------------------

enum ConstEntry {
    Int(i64),
    F64(f64),
    Str(Vec<u8>),
}

struct FnDef {
    local_count: u16,
    param_count: u16,
    upvalue_count: u16,
    code: Vec<u8>,
    handlers: Vec<(u8, u32)>,
    max_stack: Option<u16>,
}

fn fn_def(local_count: u16, param_count: u16, code: Vec<u8>) -> FnDef {
    FnDef {
        local_count,
        param_count,
        upvalue_count: 0,
        code,
        handlers: vec![],
        max_stack: None,
    }
}

fn fn_def_with_max_stack(
    local_count: u16,
    param_count: u16,
    max_stack: u16,
    code: Vec<u8>,
) -> FnDef {
    FnDef {
        local_count,
        param_count,
        upvalue_count: 0,
        code,
        handlers: vec![],
        max_stack: Some(max_stack),
    }
}

fn fn_def_with_upvalues(
    local_count: u16,
    param_count: u16,
    upvalue_count: u16,
    code: Vec<u8>,
) -> FnDef {
    FnDef {
        local_count,
        param_count,
        upvalue_count,
        code,
        handlers: vec![],
        max_stack: None,
    }
}

fn write_seam_section(buf: &mut Vec<u8>, tag: &[u8; 4], payload: &[u8]) {
    buf.extend_from_slice(tag);
    buf.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    buf.extend_from_slice(payload);
}

fn make_seam(consts: &[ConstEntry], fns: &[FnDef]) -> Vec<u8> {
    make_seam_with_effects(consts, &[], fns)
}

fn make_msbc(consts: &[ConstEntry], fns: &[FnDef]) -> Vec<u8> {
    make_seam(consts, fns)
}

struct SeamEffectDef {
    id: u16,
    name: &'static str,
    ops: Vec<SeamEffectOpDef>,
}

struct SeamEffectOpDef {
    id: u16,
    name: &'static str,
}

fn make_seam_with_effects(
    consts: &[ConstEntry],
    effects: &[SeamEffectDef],
    fns: &[FnDef],
) -> Vec<u8> {
    let mut string_entries: Vec<Vec<u8>> = vec![];
    let mut str_lookup: std::collections::HashMap<Vec<u8>, u16> = std::collections::HashMap::new();

    let mut intern_str = |s: &[u8]| -> u16 {
        if let Some(&idx) = str_lookup.get(s) {
            return idx;
        }
        let idx = string_entries.len() as u16;
        let _ = str_lookup.insert(s.to_vec(), idx);
        string_entries.push(s.to_vec());
        idx
    };

    let _ = intern_str(b"");

    let mut effect_name_idxs: Vec<u16> = vec![];
    let mut op_name_idxs: Vec<Vec<u16>> = vec![];
    for eff in effects {
        effect_name_idxs.push(intern_str(eff.name.as_bytes()));
        let mut op_idxs = vec![];
        for op in &eff.ops {
            op_idxs.push(intern_str(op.name.as_bytes()));
        }
        op_name_idxs.push(op_idxs);
    }

    let mut const_str_idxs: Vec<u16> = vec![];
    for c in consts {
        if let ConstEntry::Str(bytes) = c {
            const_str_idxs.push(intern_str(bytes));
        } else {
            const_str_idxs.push(0);
        }
    }

    let mut strt: Vec<u8> = vec![];
    strt.extend_from_slice(&(string_entries.len() as u16).to_be_bytes());
    for entry in &string_entries {
        strt.extend_from_slice(&(entry.len() as u16).to_be_bytes());
        strt.extend_from_slice(entry);
    }

    let mut type_payload: Vec<u8> = vec![];
    type_payload.extend_from_slice(&0u16.to_be_bytes());

    let mut cnst: Vec<u8> = vec![];
    cnst.extend_from_slice(&(consts.len() as u16).to_be_bytes());
    let mut str_idx_iter = const_str_idxs.iter();
    for c in consts {
        let str_idx = str_idx_iter.next().copied().unwrap_or(0);
        match c {
            ConstEntry::Int(n) => {
                cnst.push(0x01);
                cnst.extend_from_slice(&n.to_be_bytes());
            }
            ConstEntry::F64(f) => {
                cnst.push(0x02);
                cnst.extend_from_slice(&f.to_bits().to_be_bytes());
            }
            ConstEntry::Str(_) => {
                cnst.push(0x03);
                cnst.extend_from_slice(&str_idx.to_be_bytes());
            }
        }
    }

    let deps: Vec<u8> = 0u16.to_be_bytes().to_vec();
    let glob: Vec<u8> = 0u16.to_be_bytes().to_vec();

    let mut meth: Vec<u8> = vec![];
    meth.extend_from_slice(&(fns.len() as u16).to_be_bytes());
    for f in fns {
        meth.extend_from_slice(&0u16.to_be_bytes()); // name_stridx
        meth.extend_from_slice(&0u16.to_be_bytes()); // type_id
        meth.extend_from_slice(&f.param_count.to_be_bytes());
        meth.extend_from_slice(&f.local_count.to_be_bytes());
        let max_stack: u16 = f.max_stack.unwrap_or(16);
        meth.extend_from_slice(&max_stack.to_be_bytes());
        meth.extend_from_slice(&f.upvalue_count.to_be_bytes());
        meth.extend_from_slice(&(f.code.len() as u32).to_be_bytes());
        meth.extend_from_slice(&f.code);
        meth.extend_from_slice(&(f.handlers.len() as u16).to_be_bytes());
        for &(eid, hfn) in &f.handlers {
            meth.push(eid);
            meth.extend_from_slice(&hfn.to_be_bytes());
        }
        meth.extend_from_slice(&0u16.to_be_bytes()); // safepoint_count
        meth.extend_from_slice(&0u16.to_be_bytes()); // effect_set_count
    }

    let mut efct: Vec<u8> = vec![];
    efct.extend_from_slice(&(effects.len() as u16).to_be_bytes());
    for (eff_i, eff) in effects.iter().enumerate() {
        efct.extend_from_slice(&eff.id.to_be_bytes());
        efct.extend_from_slice(&effect_name_idxs[eff_i].to_be_bytes());
        efct.extend_from_slice(&(eff.ops.len() as u16).to_be_bytes());
        for (op_i, op) in eff.ops.iter().enumerate() {
            efct.extend_from_slice(&op.id.to_be_bytes());
            efct.extend_from_slice(&op_name_idxs[eff_i][op_i].to_be_bytes());
            efct.extend_from_slice(&0u16.to_be_bytes()); // param_count
            efct.extend_from_slice(&0u32.to_be_bytes()); // ret_type_id
            efct.push(0u8); // fatal
        }
    }

    let clss: Vec<u8> = 0u16.to_be_bytes().to_vec();
    let frgn: Vec<u8> = 0u16.to_be_bytes().to_vec();
    let dbug: Vec<u8> = vec![];

    let mut sections: Vec<u8> = vec![];
    write_seam_section(&mut sections, b"STRT", &strt);
    write_seam_section(&mut sections, b"TYPE", &type_payload);
    write_seam_section(&mut sections, b"CNST", &cnst);
    write_seam_section(&mut sections, b"DEPS", &deps);
    write_seam_section(&mut sections, b"GLOB", &glob);
    write_seam_section(&mut sections, b"METH", &meth);
    write_seam_section(&mut sections, b"EFCT", &efct);
    write_seam_section(&mut sections, b"CLSS", &clss);
    write_seam_section(&mut sections, b"FRGN", &frgn);
    write_seam_section(&mut sections, b"DBUG", &dbug);

    let crc = crc32_slice(&sections);

    let flags: u8 = if fns.is_empty() { 0x02 } else { 0x04 };
    let mut header: Vec<u8> = Vec::with_capacity(16);
    header.extend_from_slice(b"SEAM");
    header.push(1u8);
    header.push(0u8);
    header.push(0u8);
    header.push(flags);
    header.extend_from_slice(&0u16.to_be_bytes());
    header.extend_from_slice(&0u16.to_be_bytes());
    header.extend_from_slice(&crc.to_be_bytes());

    debug_assert_eq!(header.len(), 16);

    let mut out = header;
    out.extend_from_slice(&sections);
    out
}

// ---------------------------------------------------------------------------
// Instruction encoding helpers (SEAM ISA, BE operands)
// ---------------------------------------------------------------------------

fn ld_const(idx: u16) -> [u8; 3] {
    let [hi, lo] = idx.to_be_bytes();
    [Opcode::LD_CONST.0, hi, lo]
}

fn br(offset: i16) -> [u8; 3] {
    let [hi, lo] = offset.to_be_bytes();
    [Opcode::BR.0, hi, lo]
}

fn br_false(offset: i16) -> [u8; 3] {
    let [hi, lo] = offset.to_be_bytes();
    [Opcode::BR_FALSE.0, hi, lo]
}

#[allow(dead_code)] // encoding helper kept for completeness alongside br_false
fn br_true(offset: i16) -> [u8; 3] {
    let [hi, lo] = offset.to_be_bytes();
    [Opcode::BR_TRUE.0, hi, lo]
}

fn rec_new(tag: u8, arity: u8) -> [u8; 3] {
    [Opcode::REC_NEW.0, tag, arity]
}

fn mat_tag(tag: u16) -> [u8; 3] {
    let [hi, lo] = tag.to_be_bytes();
    [Opcode::MAT_TAG.0, hi, lo]
}

fn cls_new(fn_id: u16) -> [u8; 3] {
    let [hi, lo] = fn_id.to_be_bytes();
    [Opcode::CLS_NEW.0, hi, lo]
}

fn cls_upv(kind: u8, idx: u8) -> [u8; 3] {
    [Opcode::CLS_UPV.0, kind, idx]
}

fn eff_hdl(effect_id: u16) -> [u8; 3] {
    let [hi, lo] = effect_id.to_be_bytes();
    [Opcode::EFF_HDL.0, hi, lo]
}

fn eff_need(op_id: u8, arity: u8) -> [u8; 3] {
    [Opcode::EFF_NEED.0, op_id, arity]
}

fn call(arity: u8) -> [u8; 2] {
    [Opcode::CALL.0, arity]
}

fn call_tail(arity: u8) -> [u8; 2] {
    [Opcode::CALL_TAIL.0, arity]
}

fn code(parts: &[&[u8]]) -> Vec<u8> {
    let mut out = vec![];
    for p in parts {
        out.extend_from_slice(p);
    }
    out
}

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn run_vm(bytes: &[u8]) -> (Vm, Result<Value, VmError>) {
    let module = load(bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.run();
    (vm, result)
}

fn run_vm_call(bytes: &[u8], fn_id: u32, args: &[Value]) -> (Vm, Result<Value, VmError>) {
    let module = load(bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.call_fn(fn_id, args);
    (vm, result)
}
