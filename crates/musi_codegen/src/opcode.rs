//! The Musi instruction set -- a minimal stack-based bytecode.

use crate::error::DeserError;

const NOP: u8 = 0x00;
const HALT: u8 = 0x01;
const RET: u8 = 0x02;
const DROP: u8 = 0x03;
const DUP: u8 = 0x04;
const HALT_ERROR: u8 = 0x05;
const LD_IMM_I64: u8 = 0x10;
const LD_IMM_F64: u8 = 0x11;
const LD_IMM_UNIT: u8 = 0x13;
const LD_CONST: u8 = 0x14;
const LD_LOC: u8 = 0x15;
const ST_LOC: u8 = 0x16;
const CALL: u8 = 0x20;
const LD_FN_IDX: u8 = 0x21;
const CALL_DYNAMIC: u8 = 0x22;
// Object ops
const NEW_OBJ: u8 = 0x80;
const LD_FLD: u8 = 0x81;
const LD_TAG: u8 = 0x82;
const CALL_METHOD: u8 = 0x83;
const NIL_COALESCE: u8 = 0x84;
const OPT_FIELD: u8 = 0x85;
// Array ops
const NEW_ARR: u8 = 0x90;
const ARR_GET: u8 = 0x91;
const ARR_SET: u8 = 0x92;
const ARR_LEN: u8 = 0x93;
const ARR_PUSH: u8 = 0x94;
const ARR_SLICE: u8 = 0x95;
// Arithmetic -- integer
const ADD_I64: u8 = 0x30;
const SUB_I64: u8 = 0x31;
const MUL_I64: u8 = 0x32;
const DIV_I64: u8 = 0x33;
const REM_I64: u8 = 0x34;
const NEG_I64: u8 = 0x35;
// Arithmetic -- float
const ADD_F64: u8 = 0x36;
const SUB_F64: u8 = 0x37;
const MUL_F64: u8 = 0x38;
const DIV_F64: u8 = 0x39;
const REM_F64: u8 = 0x3A;
const NEG_F64: u8 = 0x3B;
// Comparison -- integer
const EQ_I64: u8 = 0x40;
const NEQ_I64: u8 = 0x41;
const LT_I64: u8 = 0x42;
const GT_I64: u8 = 0x43;
const LEQ_I64: u8 = 0x44;
const GEQ_I64: u8 = 0x45;
// Comparison -- float
const EQ_F64: u8 = 0x46;
const NEQ_F64: u8 = 0x47;
const LT_F64: u8 = 0x48;
const GT_F64: u8 = 0x49;
const LEQ_F64: u8 = 0x4A;
const GEQ_F64: u8 = 0x4B;
// Comparison -- bool / string
const EQ_BOOL: u8 = 0x4C;
const NEQ_BOOL: u8 = 0x4D;
const EQ_STR: u8 = 0x4E;
const NEQ_STR: u8 = 0x4F;
// Logical / bitwise
const NOT: u8 = 0x50;
const BIT_AND: u8 = 0x51;
const BIT_OR: u8 = 0x52;
const BIT_XOR: u8 = 0x53;
const BIT_NOT: u8 = 0x54;
const SHL: u8 = 0x55;
const SHR: u8 = 0x56;
// Control flow -- relative jumps
const BR: u8 = 0x60;
const BR_TRUE: u8 = 0x61;
const BR_FALSE: u8 = 0x62;
// String
const CONCAT_STR: u8 = 0x70;

/// A single Musi bytecode instruction.
///
/// Instructions are encoded as a tag byte followed by an optional payload,
/// all in little-endian byte order.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    /// No operation.
    Nop,
    /// Terminate execution; top of stack (or `Unit`) is the result.
    Halt,
    /// Return from the current function; top of stack is the return value.
    Ret,
    /// Discard the top of the operand stack.
    Drop,
    /// Clone the top of the operand stack and push the copy.
    Dup,
    /// Terminate execution with a runtime error (non-exhaustive match).
    HaltError,
    /// Push an immediate 64-bit signed integer.
    LdImmI64(i64),
    /// Push an immediate 64-bit float.
    LdImmF64(f64),
    /// Push `Unit`.
    LdImmUnit,
    /// Push a value from the const pool by index.
    LdConst(u16),
    /// Push a local variable by slot index.
    LdLoc(u16),
    /// Pop the top of the stack into a local variable slot.
    StLoc(u16),
    /// Call a function by its function-table index.
    Call(u16),
    /// Push a function value (`Value::Function`) by its function-table index.
    LdFnIdx(u16),
    /// Pop a `Value::Function` from the stack and call it.
    CallDynamic,
    /// Pop `field_count` values from the stack and create a `Value::Object` with the given type tag.
    NewObj { type_tag: u16, field_count: u16 },
    /// Pop a `Value::Object` and push the field at the given index.
    LdFld(u16),
    /// Pop a `Value::Object` and push field[0] as an Int (discriminant).
    LdTag,
    /// Pop two i64, push their wrapping sum.
    AddI64,
    /// Pop two i64, push their wrapping difference.
    SubI64,
    /// Pop two i64, push their wrapping product.
    MulI64,
    /// Pop two i64, push their quotient; errors on division by zero.
    DivI64,
    /// Pop two i64, push their remainder; errors on division by zero.
    RemI64,
    /// Pop one i64, push its wrapping negation.
    NegI64,
    /// Pop two f64, push their sum.
    AddF64,
    /// Pop two f64, push their difference.
    SubF64,
    /// Pop two f64, push their product.
    MulF64,
    /// Pop two f64, push their quotient.
    DivF64,
    /// Pop two f64, push their remainder.
    RemF64,
    /// Pop one f64, push its negation.
    NegF64,
    /// Pop two i64, push `lhs == rhs` as bool.
    EqI64,
    /// Pop two i64, push `lhs != rhs` as bool.
    NeqI64,
    /// Pop two i64, push `lhs < rhs` as bool.
    LtI64,
    /// Pop two i64, push `lhs > rhs` as bool.
    GtI64,
    /// Pop two i64, push `lhs <= rhs` as bool.
    LeqI64,
    /// Pop two i64, push `lhs >= rhs` as bool.
    GeqI64,
    /// Pop two f64, push `lhs == rhs` as bool.
    EqF64,
    /// Pop two f64, push `lhs != rhs` as bool.
    NeqF64,
    /// Pop two f64, push `lhs < rhs` as bool.
    LtF64,
    /// Pop two f64, push `lhs > rhs` as bool.
    GtF64,
    /// Pop two f64, push `lhs <= rhs` as bool.
    LeqF64,
    /// Pop two f64, push `lhs >= rhs` as bool.
    GeqF64,
    /// Pop two bools, push `lhs == rhs`.
    EqBool,
    /// Pop two bools, push `lhs != rhs`.
    NeqBool,
    /// Pop two strings, push `lhs == rhs`.
    EqStr,
    /// Pop two strings, push `lhs != rhs`.
    NeqStr,
    /// Pop one bool, push `!value`.
    Not,
    /// Pop two i64, push `lhs & rhs`.
    BitAnd,
    /// Pop two i64, push `lhs | rhs`.
    BitOr,
    /// Pop two i64, push `lhs ^ rhs`.
    BitXor,
    /// Pop one i64, push `!value`.
    BitNot,
    /// Pop two i64, push `lhs << rhs`.
    Shl,
    /// Pop two i64, push `lhs >> rhs`.
    Shr,
    /// Unconditional jump; i32 offset from the byte after this instruction.
    Br(i32),
    /// Pop bool, jump if true; i32 offset from the byte after this instruction.
    BrTrue(i32),
    /// Pop bool, jump if false; i32 offset from the byte after this instruction.
    BrFalse(i32),
    /// Pop two strings (bottom then top), push their concatenation.
    ConcatStr,
    /// Pop `arg_count` values (receiver is the first arg pushed), dispatch on receiver type.
    /// `method_idx` is a const-pool index for the method name string.
    CallMethod { method_idx: u16, arg_count: u16 },
    /// Pop rhs (default), pop lhs (Option value): push lhs inner if Some, else push rhs.
    NilCoalesce,
    /// Pop an Option value; if None push None, else access field at index and wrap in Some.
    OptField(u16),
    /// Pop N values, create an Array of N elements, push it.
    NewArr(u16),
    /// Pop Int index, pop Array, push element (bounds-checked).
    ArrGet,
    /// Pop value, pop Int index, pop Array, set element in-place (bounds-checked).
    ArrSet,
    /// Pop Array, push Int length.
    ArrLen,
    /// Pop value, pop Array, append value in-place, push Unit.
    ArrPush,
    /// Pop Int end, pop Int start, pop Array, push new Array slice.
    ArrSlice,
}

fn push_u16_op(buf: &mut Vec<u8>, tag: u8, val: u16) {
    buf.push(tag);
    buf.extend_from_slice(&val.to_le_bytes());
}

fn push_i32_op(buf: &mut Vec<u8>, tag: u8, val: i32) {
    buf.push(tag);
    buf.extend_from_slice(&val.to_le_bytes());
}

/// Maps simple (no-payload) opcode variants to their byte constants.
///
/// Used by both `encode_into` and `decode` to avoid repeating 51 match arms twice.
macro_rules! for_simple_opcodes {
    ($m:ident) => {
        $m! {
            Nop => NOP, Halt => HALT, Ret => RET, Drop => DROP, Dup => DUP,
            HaltError => HALT_ERROR, LdImmUnit => LD_IMM_UNIT, CallDynamic => CALL_DYNAMIC,
            LdTag => LD_TAG, AddI64 => ADD_I64, SubI64 => SUB_I64, MulI64 => MUL_I64,
            DivI64 => DIV_I64, RemI64 => REM_I64, NegI64 => NEG_I64, AddF64 => ADD_F64,
            SubF64 => SUB_F64, MulF64 => MUL_F64, DivF64 => DIV_F64, RemF64 => REM_F64,
            NegF64 => NEG_F64, EqI64 => EQ_I64, NeqI64 => NEQ_I64, LtI64 => LT_I64,
            GtI64 => GT_I64, LeqI64 => LEQ_I64, GeqI64 => GEQ_I64, EqF64 => EQ_F64,
            NeqF64 => NEQ_F64, LtF64 => LT_F64, GtF64 => GT_F64, LeqF64 => LEQ_F64,
            GeqF64 => GEQ_F64, EqBool => EQ_BOOL, NeqBool => NEQ_BOOL, EqStr => EQ_STR,
            NeqStr => NEQ_STR, Not => NOT, BitAnd => BIT_AND, BitOr => BIT_OR,
            BitXor => BIT_XOR, BitNot => BIT_NOT, Shl => SHL, Shr => SHR,
            ConcatStr => CONCAT_STR, NilCoalesce => NIL_COALESCE, ArrGet => ARR_GET,
            ArrSet => ARR_SET, ArrLen => ARR_LEN, ArrPush => ARR_PUSH, ArrSlice => ARR_SLICE,
        }
    };
}

impl Opcode {
    /// Encodes this opcode into `buf` as little-endian bytes.
    pub fn encode_into(&self, buf: &mut Vec<u8>) {
        macro_rules! encode_simple {
            ($($var:ident => $byte:ident),+ $(,)?) => {
                match self {
                    $(Self::$var => buf.push($byte),)+
                    Self::LdImmI64(v) => { buf.push(LD_IMM_I64); buf.extend_from_slice(&v.to_le_bytes()); }
                    Self::LdImmF64(v) => { buf.push(LD_IMM_F64); buf.extend_from_slice(&v.to_le_bytes()); }
                    Self::LdConst(v)  => push_u16_op(buf, LD_CONST,  *v),
                    Self::LdLoc(v)    => push_u16_op(buf, LD_LOC,    *v),
                    Self::StLoc(v)    => push_u16_op(buf, ST_LOC,    *v),
                    Self::Call(v)     => push_u16_op(buf, CALL,      *v),
                    Self::LdFnIdx(v)  => push_u16_op(buf, LD_FN_IDX, *v),
                    Self::LdFld(v)    => push_u16_op(buf, LD_FLD,    *v),
                    Self::OptField(v) => push_u16_op(buf, OPT_FIELD, *v),
                    Self::NewArr(v)   => push_u16_op(buf, NEW_ARR,   *v),
                    Self::NewObj { type_tag, field_count } => {
                        buf.push(NEW_OBJ);
                        buf.extend_from_slice(&type_tag.to_le_bytes());
                        buf.extend_from_slice(&field_count.to_le_bytes());
                    }
                    Self::Br(offset)      => push_i32_op(buf, BR,       *offset),
                    Self::BrTrue(offset)  => push_i32_op(buf, BR_TRUE,  *offset),
                    Self::BrFalse(offset) => push_i32_op(buf, BR_FALSE, *offset),
                    Self::CallMethod { method_idx, arg_count } => {
                        buf.push(CALL_METHOD);
                        buf.extend_from_slice(&method_idx.to_le_bytes());
                        buf.extend_from_slice(&arg_count.to_le_bytes());
                    }
                }
            };
        }
        for_simple_opcodes!(encode_simple);
    }

    /// Decodes one opcode from `code[offset..]`.
    ///
    /// Returns `(opcode, bytes_consumed)`.
    ///
    /// # Errors
    ///
    /// Returns [`DeserError::UnexpectedEof`] if the slice is too short, or
    /// [`DeserError::UnknownOpcode`] if the tag byte is not recognised.
    pub fn decode(code: &[u8], offset: usize) -> Result<(Self, usize), DeserError> {
        let tag = code.get(offset).copied().ok_or(DeserError::UnexpectedEof)?;
        macro_rules! decode_simple {
            ($($var:ident => $byte:ident),+ $(,)?) => {
                match tag {
                    $($byte => Ok((Self::$var, 1)),)+
                    LD_IMM_I64 => {
                        let b = read_bytes::<8>(code, offset + 1)?;
                        Ok((Self::LdImmI64(i64::from_le_bytes(b)), 9))
                    }
                    LD_IMM_F64 => {
                        let b = read_bytes::<8>(code, offset + 1)?;
                        Ok((Self::LdImmF64(f64::from_le_bytes(b)), 9))
                    }
                    LD_CONST => Ok((Self::LdConst(read_u16(code, offset + 1)?), 3)),
                    LD_LOC => Ok((Self::LdLoc(read_u16(code, offset + 1)?), 3)),
                    ST_LOC => Ok((Self::StLoc(read_u16(code, offset + 1)?), 3)),
                    CALL => Ok((Self::Call(read_u16(code, offset + 1)?), 3)),
                    LD_FN_IDX => Ok((Self::LdFnIdx(read_u16(code, offset + 1)?), 3)),
                    LD_FLD => Ok((Self::LdFld(read_u16(code, offset + 1)?), 3)),
                    OPT_FIELD => Ok((Self::OptField(read_u16(code, offset + 1)?), 3)),
                    NEW_ARR => Ok((Self::NewArr(read_u16(code, offset + 1)?), 3)),
                    NEW_OBJ => {
                        let type_tag = read_u16(code, offset + 1)?;
                        let field_count = read_u16(code, offset + 3)?;
                        Ok((Self::NewObj { type_tag, field_count }, 5))
                    }
                    BR => Ok((Self::Br(read_i32(code, offset + 1)?), 5)),
                    BR_TRUE => Ok((Self::BrTrue(read_i32(code, offset + 1)?), 5)),
                    BR_FALSE => Ok((Self::BrFalse(read_i32(code, offset + 1)?), 5)),
                    CALL_METHOD => {
                        let method_idx = read_u16(code, offset + 1)?;
                        let arg_count = read_u16(code, offset + 3)?;
                        Ok((Self::CallMethod { method_idx, arg_count }, 5))
                    }
                    _ => Err(DeserError::UnknownOpcode { tag, offset }),
                }
            };
        }
        for_simple_opcodes!(decode_simple)
    }

    /// Returns the encoded byte length of this opcode.
    #[must_use]
    pub const fn encoded_len(&self) -> usize {
        match self {
            Self::LdImmI64(_) | Self::LdImmF64(_) => 9,
            Self::LdConst(_)
            | Self::LdLoc(_)
            | Self::StLoc(_)
            | Self::Call(_)
            | Self::LdFnIdx(_)
            | Self::LdFld(_)
            | Self::OptField(_)
            | Self::NewArr(_) => 3,
            Self::Br(_)
            | Self::BrTrue(_)
            | Self::BrFalse(_)
            | Self::NewObj { .. }
            | Self::CallMethod { .. } => 5,
            _ => 1,
        }
    }
}

fn read_bytes<const N: usize>(code: &[u8], offset: usize) -> Result<[u8; N], DeserError> {
    let end = offset.checked_add(N).ok_or(DeserError::UnexpectedEof)?;
    let slice = code.get(offset..end).ok_or(DeserError::UnexpectedEof)?;
    slice.try_into().map_err(|_| DeserError::UnexpectedEof)
}

fn read_u16(code: &[u8], o: usize) -> Result<u16, DeserError> {
    read_bytes(code, o).map(u16::from_le_bytes)
}

fn read_i32(code: &[u8], o: usize) -> Result<i32, DeserError> {
    read_bytes(code, o).map(i32::from_le_bytes)
}

#[cfg(test)]
mod tests;
