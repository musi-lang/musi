use crate::types::fsize;
use alloc::vec::Vec;

#[derive(Clone, Copy)]
pub enum Value {
    Int(isize),
    Int32(i32),
    Int64(i64),
    Nat(usize),
    Nat32(u32),
    Nat64(u64),
    Bin(fsize),
    Bin32(f32),
    Bin64(f64),
    Bool(bool),
    Unit,
}

pub type ValueList = Vec<Value>;

// TODO: add ref types (Str, Arr, Obj)
// TODO: add ARC ref counting
