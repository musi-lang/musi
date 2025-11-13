// Base instructions
pub const NOP: u8 = 0x00;
pub const BR: u8 = 0x38;
pub const BRTRUE: u8 = 0x39;
pub const BRFALSE: u8 = 0x3A;
pub const BEQ: u8 = 0x3B;
pub const BGE: u8 = 0x3C;
pub const BGT: u8 = 0x3D;
pub const BLE: u8 = 0x3E;
pub const BLT: u8 = 0x3F;
pub const BNE: u8 = 0x40;
pub const LEAVE: u8 = 0xC7;
pub const RET: u8 = 0x2A;
pub const THROW: u16 = 0x7A00;
pub const RETHROW: u16 = 0x7A01;
pub const TRY: u8 = 0xC8;
pub const DEFER: u8 = 0xC9;

// Stack operations
pub const DUP: u8 = 0x25;
pub const POP: u8 = 0x26;

// Load constants
pub const LDNULL: u8 = 0x14;
pub const LDC_I4: u8 = 0x20;
pub const LDC_I8: u8 = 0x21;
pub const LDC_N8: u8 = 0x22;
pub const LDC_N16: u8 = 0x23;
pub const LDC_N32: u8 = 0x24;
pub const LDC_N64: u8 = 0x2F;
pub const LDC_B32: u8 = 0x30;
pub const LDC_B64: u8 = 0x31;
pub const LDC_D32: u8 = 0x32;
pub const LDC_D64: u8 = 0x33;
pub const LDSTR: u8 = 0x72;

// Load/store variables
pub const LDLOC: u8 = 0x0E;
pub const STLOC: u8 = 0x0F;
pub const LDLOCA: u8 = 0x10;
pub const LDARG: u8 = 0x02;
pub const STARG: u8 = 0x03;
pub const LDARGA: u8 = 0x04;

// Object operations
pub const NEWOBJ: u8 = 0x73;
pub const CALL: u8 = 0x28;
pub const CALLI: u8 = 0x29;
pub const LDFLD: u8 = 0x7B;
pub const STFLD: u8 = 0x7D;
pub const LDFLDA: u8 = 0x7C;
pub const LDSFLD: u8 = 0x80;
pub const STSFLD: u8 = 0x81;
pub const LDSFLDA: u8 = 0x82;
pub const NEWARR: u8 = 0x8D;
pub const LDELEM: u8 = 0xA3;
pub const STELEM: u8 = 0xA4;
pub const LDELEMA: u8 = 0x8F;
pub const LDLEN: u8 = 0x8E;

// Type operations
pub const LDTYPE: u8 = 0xD1;
pub const ISINST: u8 = 0x75;
pub const CASTCLASS: u8 = 0x74;
pub const BOX: u8 = 0x8C;
pub const UNBOX_ANY: u8 = 0xA5;
pub const CONV_I8: u8 = 0x67;
pub const CONV_I16: u8 = 0x68;
pub const CONV_I32: u8 = 0x69;
pub const CONV_I64: u8 = 0x6A;
pub const CONV_I128: u8 = 0xD0;
pub const CONV_N8: u8 = 0xD2;
pub const CONV_N16: u8 = 0xD3;
pub const CONV_N32: u8 = 0xD4;
pub const CONV_N64: u8 = 0xD5;
pub const CONV_N128: u8 = 0xD6;
pub const CONV_B32: u8 = 0xB6;
pub const CONV_B64: u8 = 0xB7;
pub const CONV_B128: u8 = 0xD7;
pub const CONV_D32: u8 = 0xD8;
pub const CONV_D64: u8 = 0xD9;
pub const CONV_D128: u8 = 0xDA;

// Arithmetic operations
pub const ADD: u8 = 0x58;
pub const ADD_OVF: u8 = 0xD6;
pub const ADD_OVF_UN: u8 = 0xD7;
pub const SUB: u8 = 0x59;
pub const SUB_OVF: u8 = 0xD8;
pub const SUB_OVF_UN: u8 = 0xD9;
pub const MUL: u8 = 0x5A;
pub const MUL_OVF: u8 = 0xDA;
pub const MUL_OVF_UN: u8 = 0xDB;
pub const DIV: u8 = 0x5B;
pub const DIV_UN: u8 = 0x5C;
pub const REM: u8 = 0x5D;
pub const REM_UN: u8 = 0x5E;
pub const NEG: u8 = 0x65;

// Logical/bitwise operations
pub const AND: u8 = 0x5F;
pub const OR: u8 = 0x60;
pub const XOR: u8 = 0x61;
pub const NOT: u8 = 0x66;
pub const SHL: u8 = 0x62;
pub const SHR: u8 = 0x63;
pub const SHR_UN: u8 = 0x64;

// Comparison operations
pub const CEQ: u8 = 0xFE;
pub const CGT: u8 = 0xC2;
pub const CGT_UN: u8 = 0xC3;
pub const CLT: u8 = 0xC4;
pub const CLT_UN: u8 = 0xC5;

// Memory management
pub const PIN: u8 = 0xDF;
pub const UNPIN: u8 = 0xE0;
pub const REFINC: u8 = 0xA0;
pub const REFDEC: u8 = 0xA1;
