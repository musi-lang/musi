pub const NAN_BOX_PTR: u8 = 0b000;
pub const NAN_BOX_SMI: u8 = 0b001;
pub const NAN_BOX_BOOL: u8 = 0b010;
pub const NAN_BOX_UNIT: u8 = 0b011;
pub const NAN_BOX_TAG: u8 = 0b100;
pub const NAN_BOX_CHAR: u8 = 0b101;

pub const BUILTIN_TYPE_TYPE: u16 = 0xFFF0;
pub const BUILTIN_TYPE_ANY: u16 = 0xFFF1;
pub const BUILTIN_TYPE_UNKNOWN: u16 = 0xFFF2;
pub const BUILTIN_TYPE_NEVER: u16 = 0xFFF3;
pub const BUILTIN_TYPE_UNIT: u16 = 0xFFF4;
pub const BUILTIN_TYPE_BOOL: u16 = 0xFFF5;
pub const BUILTIN_TYPE_INT: u16 = 0xFFF6;
pub const BUILTIN_TYPE_FLOAT: u16 = 0xFFF7;
pub const BUILTIN_TYPE_STRING: u16 = 0xFFF8;

pub const FIRST_EMITTED_TYPE_ID: u16 = 0x0100;
