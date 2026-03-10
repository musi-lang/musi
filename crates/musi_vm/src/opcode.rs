//! Opcode constants (mirrors `music_emit::opcode`).
//!
//! Shared within `musi_vm` so that `verifier.rs` and `vm/tests.rs` use one
//! canonical set of constants instead of maintaining separate copies.

pub const NOP: u8 = 0x00;
pub const HLT: u8 = 0x01;
pub const RET: u8 = 0x02;
pub const RET_U: u8 = 0x03;
pub const UNR: u8 = 0x04;
pub const BRK: u8 = 0x05;
pub const DUP: u8 = 0x06;
pub const POP: u8 = 0x07;
pub const SWP: u8 = 0x08;

pub const I_ADD: u8 = 0x10;
pub const I_ADD_UN: u8 = 0x11;
pub const I_SUB: u8 = 0x12;
pub const I_SUB_UN: u8 = 0x13;
pub const I_MUL: u8 = 0x14;
pub const I_MUL_UN: u8 = 0x15;
pub const I_DIV: u8 = 0x16;
pub const I_DIV_UN: u8 = 0x17;
pub const I_REM: u8 = 0x18;
pub const I_REM_UN: u8 = 0x19;
pub const I_NEG: u8 = 0x1A;

pub const F_ADD: u8 = 0x20;
pub const F_SUB: u8 = 0x21;
pub const F_MUL: u8 = 0x22;
pub const F_DIV: u8 = 0x23;
pub const F_REM: u8 = 0x24;
pub const F_NEG: u8 = 0x25;

pub const B_AND: u8 = 0x30;
pub const B_OR: u8 = 0x31;
pub const B_XOR: u8 = 0x32;
pub const B_NOT: u8 = 0x33;
pub const B_SHL: u8 = 0x34;
pub const B_SHR: u8 = 0x36;
pub const B_SHR_UN: u8 = 0x37;

pub const CMP_EQ: u8 = 0x3B;
pub const CMP_NE: u8 = 0x3C;

pub const LD_LOC: u8 = 0x40;
pub const ST_LOC: u8 = 0x41;
pub const LD_CST: u8 = 0x42;
pub const ST_FLD: u8 = 0x43;
pub const MK_PRD: u8 = 0x44;
pub const LD_FLD: u8 = 0x45;
pub const MK_VAR: u8 = 0x46;
pub const LD_PAY: u8 = 0x47;
pub const CMP_TAG: u8 = 0x48;
pub const CNV_WDN: u8 = 0x49;
pub const CNV_WDN_UN: u8 = 0x4A;
pub const CNV_NRW: u8 = 0x4B;
pub const EFF_PSH: u8 = 0x4C;
pub const EFF_POP: u8 = 0x4D;

pub const CMP_LT: u8 = 0x50;
pub const CMP_LT_UN: u8 = 0x51;
pub const CMP_LE: u8 = 0x52;
pub const CMP_LE_UN: u8 = 0x53;
pub const CMP_GT: u8 = 0x54;
pub const CMP_GT_UN: u8 = 0x55;
pub const CMP_GE: u8 = 0x56;
pub const CMP_GE_UN: u8 = 0x57;

pub const CMP_F_EQ: u8 = 0x58;
pub const CMP_F_NE: u8 = 0x59;
pub const CMP_F_LT: u8 = 0x5A;
pub const CMP_F_LE: u8 = 0x5B;
pub const CMP_F_GT: u8 = 0x5C;
pub const CMP_F_GE: u8 = 0x5D;

pub const CNV_ITF: u8 = 0x5E;
pub const CNV_FTI: u8 = 0x5F;
pub const CNV_TRM: u8 = 0x60;

pub const LD_TAG: u8 = 0x61;
pub const LD_LEN: u8 = 0x62;
pub const LD_IDX: u8 = 0x63;
pub const ST_IDX: u8 = 0x64;
pub const FRE: u8 = 0x65;
pub const EFF_RES_C: u8 = 0x66;
pub const EFF_ABT: u8 = 0x67;
pub const TSK_AWT: u8 = 0x68;
pub const INV_DYN: u8 = 0x69;

pub const LD_LOC_W: u8 = 0x80;
pub const ST_LOC_W: u8 = 0x81;
pub const LD_CST_W: u8 = 0x82;
pub const ST_FLD_W: u8 = 0x83;
pub const MK_VAR_W: u8 = 0x84;
pub const CMP_TAG_W: u8 = 0x85;
pub const JMP: u8 = 0x86;
pub const JMP_T: u8 = 0x87;
pub const JMP_F: u8 = 0x88;

pub const INV: u8 = 0xC0;
pub const INV_EFF: u8 = 0xC1;
pub const INV_TAL: u8 = 0xC2;
pub const INV_TAL_EFF: u8 = 0xC3;
pub const LD_GLB: u8 = 0xC4;
pub const ST_GLB: u8 = 0xC5;
pub const MK_ARR: u8 = 0xC6;
pub const ALC_REF: u8 = 0xC7;
pub const ALC_MAN: u8 = 0xC8;
pub const ALC_ARN: u8 = 0xC9;
pub const EFF_DO: u8 = 0xCA;
pub const EFF_RES: u8 = 0xCB;
pub const TSK_SPN: u8 = 0xCC;
pub const TSK_CHS: u8 = 0xCD;
pub const TSK_CHR: u8 = 0xCE;
pub const TSK_CMK: u8 = 0xCF;
pub const JMP_W: u8 = 0xD0;
pub const JMP_T_W: u8 = 0xD1;
pub const JMP_F_W: u8 = 0xD2;
