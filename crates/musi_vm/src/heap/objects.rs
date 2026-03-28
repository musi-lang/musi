use std::ffi::c_void;
use std::ops::{Deref, DerefMut};

use music_il::format;

use crate::effect::EffectHandler;
use crate::frame::CallFrame;
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Closure {
    pub type_id: u16,
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

#[derive(Clone)]
pub struct Continuation {
    pub type_id: u16,
    pub frames: Vec<CallFrame>,
    pub resume_pc: usize,
    pub captured_handlers: Vec<EffectHandler>,
}

#[derive(Clone, Debug)]
pub struct VmArray {
    pub type_id: u16,
    pub tag: Value,
    pub elements: Vec<Value>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VmString {
    pub type_id: u16,
    pub data: String,
}

#[derive(Clone, Debug)]
pub struct VmSlice {
    pub type_id: u16,
    pub source: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug)]
pub struct VmCPtr {
    pub type_id: u16,
    pub ptr: *mut c_void,
}

#[derive(Clone, Debug)]
pub struct VmCell {
    pub type_id: u16,
    pub value: Value,
}

#[derive(Clone)]
pub enum HeapObject {
    Closure(Closure),
    Continuation(Continuation),
    Array(VmArray),
    String(VmString),
    Slice(VmSlice),
    CPtr(VmCPtr),
    Cell(VmCell),
}

impl HeapObject {
    #[must_use]
    pub const fn type_id(&self) -> u16 {
        match self {
            Self::Closure(v) => v.type_id,
            Self::Continuation(v) => v.type_id,
            Self::Array(v) => v.type_id,
            Self::String(v) => v.type_id,
            Self::Slice(v) => v.type_id,
            Self::CPtr(v) => v.type_id,
            Self::Cell(v) => v.type_id,
        }
    }
}

impl Deref for VmString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for VmString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl From<String> for VmString {
    fn from(data: String) -> Self {
        Self {
            type_id: format::BUILTIN_TYPE_STRING,
            data,
        }
    }
}

impl From<&str> for VmString {
    fn from(data: &str) -> Self {
        Self::from(data.to_owned())
    }
}

impl PartialEq<str> for VmString {
    fn eq(&self, other: &str) -> bool {
        self.data == other
    }
}

impl PartialEq<&str> for VmString {
    fn eq(&self, other: &&str) -> bool {
        self.data == *other
    }
}
