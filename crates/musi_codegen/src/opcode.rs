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
    /// Pop N values from the stack and create a `Value::Object`.
    NewObj(u16),
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
}

impl Opcode {
    /// Encodes this opcode into `buf` as little-endian bytes.
    pub fn encode_into(&self, buf: &mut Vec<u8>) {
        match self {
            Self::Nop => buf.push(NOP),
            Self::Halt => buf.push(HALT),
            Self::Ret => buf.push(RET),
            Self::Drop => buf.push(DROP),
            Self::Dup => buf.push(DUP),
            Self::HaltError => buf.push(HALT_ERROR),
            Self::LdImmUnit => buf.push(LD_IMM_UNIT),
            Self::LdImmI64(v) => {
                buf.push(LD_IMM_I64);
                buf.extend_from_slice(&v.to_le_bytes());
            }
            Self::LdImmF64(v) => {
                buf.push(LD_IMM_F64);
                buf.extend_from_slice(&v.to_le_bytes());
            }
            Self::LdConst(idx) => {
                buf.push(LD_CONST);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            Self::LdLoc(idx) => {
                buf.push(LD_LOC);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            Self::StLoc(idx) => {
                buf.push(ST_LOC);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            Self::Call(fn_idx) => {
                buf.push(CALL);
                buf.extend_from_slice(&fn_idx.to_le_bytes());
            }
            Self::LdFnIdx(idx) => {
                buf.push(LD_FN_IDX);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            Self::CallDynamic => buf.push(CALL_DYNAMIC),
            Self::NewObj(n) => {
                buf.push(NEW_OBJ);
                buf.extend_from_slice(&n.to_le_bytes());
            }
            Self::LdFld(idx) => {
                buf.push(LD_FLD);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            Self::LdTag => buf.push(LD_TAG),
            Self::AddI64 => buf.push(ADD_I64),
            Self::SubI64 => buf.push(SUB_I64),
            Self::MulI64 => buf.push(MUL_I64),
            Self::DivI64 => buf.push(DIV_I64),
            Self::RemI64 => buf.push(REM_I64),
            Self::NegI64 => buf.push(NEG_I64),
            Self::AddF64 => buf.push(ADD_F64),
            Self::SubF64 => buf.push(SUB_F64),
            Self::MulF64 => buf.push(MUL_F64),
            Self::DivF64 => buf.push(DIV_F64),
            Self::RemF64 => buf.push(REM_F64),
            Self::NegF64 => buf.push(NEG_F64),
            Self::EqI64 => buf.push(EQ_I64),
            Self::NeqI64 => buf.push(NEQ_I64),
            Self::LtI64 => buf.push(LT_I64),
            Self::GtI64 => buf.push(GT_I64),
            Self::LeqI64 => buf.push(LEQ_I64),
            Self::GeqI64 => buf.push(GEQ_I64),
            Self::EqF64 => buf.push(EQ_F64),
            Self::NeqF64 => buf.push(NEQ_F64),
            Self::LtF64 => buf.push(LT_F64),
            Self::GtF64 => buf.push(GT_F64),
            Self::LeqF64 => buf.push(LEQ_F64),
            Self::GeqF64 => buf.push(GEQ_F64),
            Self::EqBool => buf.push(EQ_BOOL),
            Self::NeqBool => buf.push(NEQ_BOOL),
            Self::EqStr => buf.push(EQ_STR),
            Self::NeqStr => buf.push(NEQ_STR),
            Self::Not => buf.push(NOT),
            Self::BitAnd => buf.push(BIT_AND),
            Self::BitOr => buf.push(BIT_OR),
            Self::BitXor => buf.push(BIT_XOR),
            Self::BitNot => buf.push(BIT_NOT),
            Self::Shl => buf.push(SHL),
            Self::Shr => buf.push(SHR),
            Self::Br(offset) => {
                buf.push(BR);
                buf.extend_from_slice(&offset.to_le_bytes());
            }
            Self::BrTrue(offset) => {
                buf.push(BR_TRUE);
                buf.extend_from_slice(&offset.to_le_bytes());
            }
            Self::BrFalse(offset) => {
                buf.push(BR_FALSE);
                buf.extend_from_slice(&offset.to_le_bytes());
            }
            Self::ConcatStr => buf.push(CONCAT_STR),
            Self::CallMethod { method_idx, arg_count } => {
                buf.push(CALL_METHOD);
                buf.extend_from_slice(&method_idx.to_le_bytes());
                buf.extend_from_slice(&arg_count.to_le_bytes());
            }
        }
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
        match tag {
            NOP => Ok((Self::Nop, 1)),
            HALT => Ok((Self::Halt, 1)),
            RET => Ok((Self::Ret, 1)),
            DROP => Ok((Self::Drop, 1)),
            DUP => Ok((Self::Dup, 1)),
            HALT_ERROR => Ok((Self::HaltError, 1)),
            LD_IMM_UNIT => Ok((Self::LdImmUnit, 1)),
            LD_IMM_I64 => {
                let b = read_8(code, offset + 1)?;
                Ok((Self::LdImmI64(i64::from_le_bytes(b)), 9))
            }
            LD_IMM_F64 => {
                let b = read_8(code, offset + 1)?;
                Ok((Self::LdImmF64(f64::from_le_bytes(b)), 9))
            }
            LD_CONST => Ok((Self::LdConst(read_u16(code, offset + 1)?), 3)),
            LD_LOC => Ok((Self::LdLoc(read_u16(code, offset + 1)?), 3)),
            ST_LOC => Ok((Self::StLoc(read_u16(code, offset + 1)?), 3)),
            CALL => Ok((Self::Call(read_u16(code, offset + 1)?), 3)),
            LD_FN_IDX => Ok((Self::LdFnIdx(read_u16(code, offset + 1)?), 3)),
            CALL_DYNAMIC => Ok((Self::CallDynamic, 1)),
            NEW_OBJ => Ok((Self::NewObj(read_u16(code, offset + 1)?), 3)),
            LD_FLD => Ok((Self::LdFld(read_u16(code, offset + 1)?), 3)),
            LD_TAG => Ok((Self::LdTag, 1)),
            ADD_I64 => Ok((Self::AddI64, 1)),
            SUB_I64 => Ok((Self::SubI64, 1)),
            MUL_I64 => Ok((Self::MulI64, 1)),
            DIV_I64 => Ok((Self::DivI64, 1)),
            REM_I64 => Ok((Self::RemI64, 1)),
            NEG_I64 => Ok((Self::NegI64, 1)),
            ADD_F64 => Ok((Self::AddF64, 1)),
            SUB_F64 => Ok((Self::SubF64, 1)),
            MUL_F64 => Ok((Self::MulF64, 1)),
            DIV_F64 => Ok((Self::DivF64, 1)),
            REM_F64 => Ok((Self::RemF64, 1)),
            NEG_F64 => Ok((Self::NegF64, 1)),
            EQ_I64 => Ok((Self::EqI64, 1)),
            NEQ_I64 => Ok((Self::NeqI64, 1)),
            LT_I64 => Ok((Self::LtI64, 1)),
            GT_I64 => Ok((Self::GtI64, 1)),
            LEQ_I64 => Ok((Self::LeqI64, 1)),
            GEQ_I64 => Ok((Self::GeqI64, 1)),
            EQ_F64 => Ok((Self::EqF64, 1)),
            NEQ_F64 => Ok((Self::NeqF64, 1)),
            LT_F64 => Ok((Self::LtF64, 1)),
            GT_F64 => Ok((Self::GtF64, 1)),
            LEQ_F64 => Ok((Self::LeqF64, 1)),
            GEQ_F64 => Ok((Self::GeqF64, 1)),
            EQ_BOOL => Ok((Self::EqBool, 1)),
            NEQ_BOOL => Ok((Self::NeqBool, 1)),
            EQ_STR => Ok((Self::EqStr, 1)),
            NEQ_STR => Ok((Self::NeqStr, 1)),
            NOT => Ok((Self::Not, 1)),
            BIT_AND => Ok((Self::BitAnd, 1)),
            BIT_OR => Ok((Self::BitOr, 1)),
            BIT_XOR => Ok((Self::BitXor, 1)),
            BIT_NOT => Ok((Self::BitNot, 1)),
            SHL => Ok((Self::Shl, 1)),
            SHR => Ok((Self::Shr, 1)),
            BR => Ok((Self::Br(read_i32(code, offset + 1)?), 5)),
            BR_TRUE => Ok((Self::BrTrue(read_i32(code, offset + 1)?), 5)),
            BR_FALSE => Ok((Self::BrFalse(read_i32(code, offset + 1)?), 5)),
            CONCAT_STR => Ok((Self::ConcatStr, 1)),
            CALL_METHOD => {
                let method_idx = read_u16(code, offset + 1)?;
                let arg_count = read_u16(code, offset + 3)?;
                Ok((Self::CallMethod { method_idx, arg_count }, 5))
            }
            _ => Err(DeserError::UnknownOpcode { tag, offset }),
        }
    }

    /// Returns the encoded byte length of this opcode.
    #[must_use]
    pub const fn encoded_len(&self) -> usize {
        match self {
            Self::Nop
            | Self::Halt
            | Self::Ret
            | Self::Drop
            | Self::Dup
            | Self::HaltError
            | Self::LdImmUnit
            | Self::CallDynamic
            | Self::LdTag
            | Self::AddI64
            | Self::SubI64
            | Self::MulI64
            | Self::DivI64
            | Self::RemI64
            | Self::NegI64
            | Self::AddF64
            | Self::SubF64
            | Self::MulF64
            | Self::DivF64
            | Self::RemF64
            | Self::NegF64
            | Self::EqI64
            | Self::NeqI64
            | Self::LtI64
            | Self::GtI64
            | Self::LeqI64
            | Self::GeqI64
            | Self::EqF64
            | Self::NeqF64
            | Self::LtF64
            | Self::GtF64
            | Self::LeqF64
            | Self::GeqF64
            | Self::EqBool
            | Self::NeqBool
            | Self::EqStr
            | Self::NeqStr
            | Self::Not
            | Self::BitAnd
            | Self::BitOr
            | Self::BitXor
            | Self::BitNot
            | Self::Shl
            | Self::Shr
            | Self::ConcatStr => 1,
            Self::LdImmI64(_) | Self::LdImmF64(_) => 9,
            Self::LdConst(_)
            | Self::LdLoc(_)
            | Self::StLoc(_)
            | Self::Call(_)
            | Self::LdFnIdx(_)
            | Self::NewObj(_)
            | Self::LdFld(_) => 3,
            Self::Br(_) | Self::BrTrue(_) | Self::BrFalse(_) => 5,
            Self::CallMethod { .. } => 5,
        }
    }
}

fn read_8(code: &[u8], offset: usize) -> Result<[u8; 8], DeserError> {
    let end = offset.checked_add(8).ok_or(DeserError::UnexpectedEof)?;
    let slice = code.get(offset..end).ok_or(DeserError::UnexpectedEof)?;
    let mut arr = [0u8; 8];
    arr.copy_from_slice(slice);
    Ok(arr)
}

fn read_u16(code: &[u8], offset: usize) -> Result<u16, DeserError> {
    let end = offset.checked_add(2).ok_or(DeserError::UnexpectedEof)?;
    let slice = code.get(offset..end).ok_or(DeserError::UnexpectedEof)?;
    let mut arr = [0u8; 2];
    arr.copy_from_slice(slice);
    Ok(u16::from_le_bytes(arr))
}

fn read_i32(code: &[u8], offset: usize) -> Result<i32, DeserError> {
    let end = offset.checked_add(4).ok_or(DeserError::UnexpectedEof)?;
    let slice = code.get(offset..end).ok_or(DeserError::UnexpectedEof)?;
    let mut arr = [0u8; 4];
    arr.copy_from_slice(slice);
    Ok(i32::from_le_bytes(arr))
}

#[cfg(test)]
mod tests;
