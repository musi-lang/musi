//! Opcode definitions for the SEAM bytecode format.
//!
//! Five instruction formats with fixed widths:
//! - `F0`   - `[op]`                        1 byte
//! - `FI8`  - `[op] [u8]`                   2 bytes
//! - `FI16` - `[op] [u16-hi] [u16-lo]`      3 bytes (big-endian)
//! - `FI8x2`- `[op] [u8-a] [u8-b]`          3 bytes (two independent u8 operands)
//! - `FI24` - `[op] [i24-hi] [i24-mid] [i24-lo]`  4 bytes (signed, big-endian)

use core::fmt;

/// A bytecode opcode (newtype around the raw byte value).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Opcode(pub u8);

impl Opcode {
    // §4.1 Data Movement (15)
    pub const LD_CONST: Self = Self(0x00);
    pub const LD_SMI: Self = Self(0x01);
    pub const LD_TRUE: Self = Self(0x02);
    pub const LD_FALSE: Self = Self(0x03);
    pub const LD_UNIT: Self = Self(0x04);
    pub const LD_NONE: Self = Self(0x05);
    pub const LD_LOC: Self = Self(0x06);
    pub const LD_UPV: Self = Self(0x07);
    pub const LD_GLB: Self = Self(0x08);
    /// Reserved: planned for loading the address of a local or global slot into
    /// an integer register (pointer-width, for FFI interop).
    pub const LD_ADDR: Self = Self(0x09);
    /// Reserved: planned for dereferencing a raw pointer value (unsafe FFI loads).
    pub const LD_IND: Self = Self(0x0A);
    pub const ST_LOC: Self = Self(0x0B);
    /// Reserved: planned for writing directly into an upvalue cell without closing it
    /// (supports mutable captured variables in a future closure model).
    pub const ST_UPV: Self = Self(0x0C);
    pub const ST_GLB: Self = Self(0x0D);
    /// Reserved: planned for writing through a raw pointer (unsafe FFI stores).
    pub const ST_IND: Self = Self(0x0E);

    // §4.2 Stack (3)
    pub const POP: Self = Self(0x0F);
    pub const DUP: Self = Self(0x10);
    /// Reserved: planned for swapping the top two stack values (register allocation helper).
    pub const SWAP: Self = Self(0x11);

    // §4.3 Arithmetic (6)
    pub const ADD: Self = Self(0x12);
    pub const SUB: Self = Self(0x13);
    pub const MUL: Self = Self(0x14);
    pub const DIV: Self = Self(0x15);
    pub const REM: Self = Self(0x16);
    pub const NEG: Self = Self(0x17);

    // §4.4 Bitwise (4) - Int only; Bool and/or/xor/not compile to branch sequences
    pub const BAND: Self = Self(0x18);
    pub const BOR: Self = Self(0x19);
    pub const BXOR: Self = Self(0x1A);
    pub const BNOT: Self = Self(0x1B);

    // §4.5 Class Dispatch (2)
    pub const CLS_DICT: Self = Self(0x1C);
    pub const CLS_DISP: Self = Self(0x1D);

    // §4.6 Comparison (6)
    pub const CMP_EQ: Self = Self(0x1E);
    pub const CMP_NE: Self = Self(0x1F);
    pub const CMP_LT: Self = Self(0x20);
    pub const CMP_GT: Self = Self(0x21);
    pub const CMP_LE: Self = Self(0x22);
    pub const CMP_GE: Self = Self(0x23);

    // §4.7 Branch (4)
    pub const BR: Self = Self(0x24);
    pub const BR_TRUE: Self = Self(0x25);
    pub const BR_FALSE: Self = Self(0x26);
    pub const BR_LONG: Self = Self(0x27);

    // §4.8 Call/Return (4)
    pub const CALL: Self = Self(0x28);
    pub const CALL_TAIL: Self = Self(0x29);
    pub const RET: Self = Self(0x2A);
    pub const RET_UNIT: Self = Self(0x2B);

    // §4.9 Closure (2)
    pub const CLS_NEW: Self = Self(0x2C);
    pub const CLS_UPV: Self = Self(0x2D);

    // §4.10 Record (4)
    pub const REC_NEW: Self = Self(0x2E);
    pub const REC_GET: Self = Self(0x2F);
    pub const REC_SET: Self = Self(0x30);
    /// Reserved: planned for taking the address of a record field (raw pointer for FFI structs).
    pub const REC_ADDR: Self = Self(0x31);

    // §4.11 Array (4)
    pub const ARR_NEW: Self = Self(0x32);
    pub const ARR_GET: Self = Self(0x33);
    pub const ARR_SET: Self = Self(0x34);
    pub const ARR_LEN: Self = Self(0x35);

    // §4.12 Tuple (2)
    pub const TUP_NEW: Self = Self(0x36);
    pub const TUP_GET: Self = Self(0x37);

    // §4.13 Type (5)
    pub const TY_OF: Self = Self(0x38);
    /// Reserved: planned for runtime type-identity comparison (pushes bool, both args are type descriptors).
    pub const TY_EQ: Self = Self(0x39);
    pub const TY_TEST: Self = Self(0x3A);
    pub const TY_CAST: Self = Self(0x3B);
    pub const TY_DESC: Self = Self(0x3C);

    // §4.14 Effect (4)
    pub const EFF_NEED: Self = Self(0x3D);
    pub const EFF_HDL: Self = Self(0x3E);
    pub const EFF_RES: Self = Self(0x3F);
    pub const EFF_POP: Self = Self(0x40);

    // §4.15 Match (2)
    pub const MAT_TAG: Self = Self(0x41);
    pub const MAT_DATA: Self = Self(0x42);

    // §4.16 Optional (4)
    pub const OPT_SOME: Self = Self(0x43);
    pub const OPT_NONE: Self = Self(0x44);
    pub const OPT_GET: Self = Self(0x45);
    pub const OPT_IS: Self = Self(0x46);

    // §4.17 String (2)
    /// Reserved: planned for in-VM string concatenation without an FFI call.
    pub const STR_CAT: Self = Self(0x47);
    /// Reserved: planned for querying the byte length of a heap string in O(1).
    pub const STR_LEN: Self = Self(0x48);

    // §4.18 Arena (3)
    pub const AR_NEW: Self = Self(0x49);
    pub const AR_ALLOC: Self = Self(0x4A);
    pub const AR_FREE: Self = Self(0x4B);

    // §4.19 GC (2)
    pub const GC_PIN: Self = Self(0x4C);
    pub const GC_UNPIN: Self = Self(0x4D);

    // §4.20 Foreign (1)
    pub const FFI_CALL: Self = Self(0x4E);

    // §4.21 Misc (2)
    /// Reserved: no-op placeholder; intended for patch-site alignment and debug breakpoints.
    pub const NOP: Self = Self(0x4F);
    pub const PANIC: Self = Self(0x50);

    /// Human-readable name for this opcode, if known.
    #[must_use]
    pub fn name(self) -> Option<&'static str> {
        OPCODE_NAMES[usize::from(self.0)]
    }
}

/// Instruction format tag, determining operand width and layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    /// No operand - 1 byte total.
    F0,
    /// Single u8 operand - 2 bytes total.
    FI8,
    /// Single u16 operand (big-endian) - 3 bytes total.
    FI16,
    /// Two independent u8 operands - 3 bytes total.
    FI8x2,
    /// Signed 24-bit operand (big-endian) - 4 bytes total.
    FI24,
}

/// Map every opcode byte to its instruction format.
///
/// Opcodes 0x51–0xFF are unassigned and map to `F0` (1 byte) so the
/// disassembler can always advance without panicking.
///
/// Arms are grouped by ISA section (§4.1–§4.21) for documentation; clippy
/// sees cross-section duplicates but merging them would destroy the structure.
#[must_use]
#[expect(clippy::match_same_arms)]
pub const fn format(op: u8) -> Format {
    match op {
        // §4.1 Data Movement
        0x00 | 0x01 => Format::FI16, // LD_CONST, LD_SMI
        0x02..=0x05 => Format::F0,   // LD_TRUE, LD_FALSE, LD_UNIT, LD_NONE
        0x06..=0x09 => Format::FI8,  // LD_LOC, LD_UPV, LD_GLB, LD_ADDR
        0x0A => Format::F0,          // LD_IND
        0x0B..=0x0D => Format::FI8,  // ST_LOC, ST_UPV, ST_GLB
        0x0E => Format::F0,          // ST_IND
        // §4.2 Stack – §4.4 Bitwise (all F0)
        0x0F..=0x1B => Format::F0, // POP..BNOT
        // §4.5 Class Dispatch
        0x1C => Format::FI16,  // CLS_DICT
        0x1D => Format::FI8x2, // CLS_DISP
        // §4.6 Comparison (all F0)
        0x1E..=0x23 => Format::F0, // CMP_EQ..CMP_GE
        // §4.7 Branch
        0x24..=0x26 => Format::FI16, // BR, BR_TRUE, BR_FALSE
        0x27 => Format::FI24,        // BR_LONG
        // §4.8 Call/Return
        0x28 | 0x29 => Format::FI8, // CALL, CALL_TAIL
        0x2A | 0x2B => Format::F0,  // RET, RET_UNIT
        // §4.9 Closure
        0x2C => Format::FI16,  // CLS_NEW
        0x2D => Format::FI8x2, // CLS_UPV (kind:u8, idx:u8)
        // §4.10 Record
        0x2E => Format::FI8x2,      // REC_NEW
        0x2F..=0x31 => Format::FI8, // REC_GET, REC_SET, REC_ADDR
        // §4.11 Array (all F0)
        0x32..=0x35 => Format::F0, // ARR_NEW..ARR_LEN
        // §4.12 Tuple
        0x36 | 0x37 => Format::FI8, // TUP_NEW, TUP_GET
        // §4.13 Type
        0x38..=0x3B => Format::F0, // TY_OF, TY_EQ, TY_TEST, TY_CAST
        0x3C => Format::FI16,      // TY_DESC
        // §4.14 Effect
        0x3D => Format::FI8x2,     // EFF_NEED
        0x3E => Format::FI16,      // EFF_HDL
        0x3F | 0x40 => Format::F0, // EFF_RES, EFF_POP
        // §4.15 Match
        0x41 => Format::FI16, // MAT_TAG
        0x42 => Format::F0,   // MAT_DATA
        // §4.16 Optional (all F0)
        0x43..=0x46 => Format::F0, // OPT_SOME..OPT_IS
        // §4.17 String (all F0)
        0x47 | 0x48 => Format::F0, // STR_CAT, STR_LEN
        // §4.18 Arena (all F0)
        0x49..=0x4B => Format::F0, // AR_NEW..AR_FREE
        // §4.19 GC (all F0)
        0x4C | 0x4D => Format::F0, // GC_PIN, GC_UNPIN
        // §4.20 Foreign
        0x4E => Format::FI8x2, // FFI_CALL
        // §4.21 Misc (all F0)
        0x4F | 0x50 => Format::F0, // NOP, PANIC
        // All unassigned opcodes - treat as 1-byte F0 so the disassembler
        // can always advance by at least one byte.
        _ => Format::F0,
    }
}

/// Total instruction length in bytes (including the opcode byte itself).
#[must_use]
pub const fn instr_len(op: u8) -> usize {
    match format(op) {
        Format::F0 => 1,
        Format::FI8 => 2,
        Format::FI16 | Format::FI8x2 => 3,
        Format::FI24 => 4,
    }
}

pub const OPCODE_NAMES: [Option<&str>; 256] = {
    let mut t: [Option<&str>; 256] = [None; 256];

    // §4.1 Data Movement
    t[0x00] = Some("ld.const");
    t[0x01] = Some("ld.smi");
    t[0x02] = Some("ld.true");
    t[0x03] = Some("ld.false");
    t[0x04] = Some("ld.unit");
    t[0x05] = Some("ld.none");
    t[0x06] = Some("ld.loc");
    t[0x07] = Some("ld.upv");
    t[0x08] = Some("ld.glb");
    t[0x09] = Some("ld.addr");
    t[0x0A] = Some("ld.ind");
    t[0x0B] = Some("st.loc");
    t[0x0C] = Some("st.upv");
    t[0x0D] = Some("st.glb");
    t[0x0E] = Some("st.ind");

    // §4.2 Stack
    t[0x0F] = Some("pop");
    t[0x10] = Some("dup");
    t[0x11] = Some("swap");

    // §4.3 Arithmetic
    t[0x12] = Some("add");
    t[0x13] = Some("sub");
    t[0x14] = Some("mul");
    t[0x15] = Some("div");
    t[0x16] = Some("rem");
    t[0x17] = Some("neg");

    // §4.4 Bitwise
    t[0x18] = Some("band");
    t[0x19] = Some("bor");
    t[0x1A] = Some("bxor");
    t[0x1B] = Some("bnot");

    // §4.5 Class Dispatch
    t[0x1C] = Some("cls.dict");
    t[0x1D] = Some("cls.disp");

    // §4.6 Comparison
    t[0x1E] = Some("cmp.eq");
    t[0x1F] = Some("cmp.ne");
    t[0x20] = Some("cmp.lt");
    t[0x21] = Some("cmp.gt");
    t[0x22] = Some("cmp.le");
    t[0x23] = Some("cmp.ge");

    // §4.7 Branch
    t[0x24] = Some("br");
    t[0x25] = Some("br.true");
    t[0x26] = Some("br.false");
    t[0x27] = Some("br.long");

    // §4.8 Call/Return
    t[0x28] = Some("call");
    t[0x29] = Some("call.tail");
    t[0x2A] = Some("ret");
    t[0x2B] = Some("ret.unit");

    // §4.9 Closure
    t[0x2C] = Some("cls.new");
    t[0x2D] = Some("cls.upv");

    // §4.10 Record
    t[0x2E] = Some("rec.new");
    t[0x2F] = Some("rec.get");
    t[0x30] = Some("rec.set");
    t[0x31] = Some("rec.addr");

    // §4.11 Array
    t[0x32] = Some("arr.new");
    t[0x33] = Some("arr.get");
    t[0x34] = Some("arr.set");
    t[0x35] = Some("arr.len");

    // §4.12 Tuple
    t[0x36] = Some("tup.new");
    t[0x37] = Some("tup.get");

    // §4.13 Type
    t[0x38] = Some("ty.of");
    t[0x39] = Some("ty.eq");
    t[0x3A] = Some("ty.test");
    t[0x3B] = Some("ty.cast");
    t[0x3C] = Some("ty.desc");

    // §4.14 Effect
    t[0x3D] = Some("eff.need");
    t[0x3E] = Some("eff.hdl");
    t[0x3F] = Some("eff.res");
    t[0x40] = Some("eff.pop");

    // §4.15 Match
    t[0x41] = Some("mat.tag");
    t[0x42] = Some("mat.data");

    // §4.16 Optional
    t[0x43] = Some("opt.some");
    t[0x44] = Some("opt.none");
    t[0x45] = Some("opt.get");
    t[0x46] = Some("opt.is");

    // §4.17 String
    t[0x47] = Some("str.cat");
    t[0x48] = Some("str.len");

    // §4.18 Arena
    t[0x49] = Some("ar.new");
    t[0x4A] = Some("ar.alloc");
    t[0x4B] = Some("ar.free");

    // §4.19 GC
    t[0x4C] = Some("gc.pin");
    t[0x4D] = Some("gc.unpin");

    // §4.20 Foreign
    t[0x4E] = Some("ffi.call");

    // §4.21 Misc
    t[0x4F] = Some("nop");
    t[0x50] = Some("panic");

    t
};

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name() {
            Some(n) => f.write_str(n),
            None => write!(f, "0x{:02X}", self.0),
        }
    }
}

#[cfg(test)]
mod tests;
