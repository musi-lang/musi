pub const MAGIC: [u8; 4] = *b"SEAM";
pub const VERSION_MAJOR: u8 = 0;
pub const VERSION_MINOR: u8 = 1;
pub const HEADER_SIZE: usize = 16;

pub const ENTRY_METHOD_NAME: u32 = u32::MAX;
pub const ANON_METHOD_NAME: u32 = u32::MAX - 1;

pub const STRING_SECTION_TAG: [u8; 4] = *b"STRT";
pub const TYPE_SECTION_TAG: [u8; 4] = *b"TYPE";
pub const CONST_SECTION_TAG: [u8; 4] = *b"CNST";
pub const GLOBAL_SECTION_TAG: [u8; 4] = *b"GLOB";
pub const METHOD_SECTION_TAG: [u8; 4] = *b"METH";
pub const EFFECT_SECTION_TAG: [u8; 4] = *b"EFCT";
pub const CLASS_SECTION_TAG: [u8; 4] = *b"CLSS";
pub const FOREIGN_SECTION_TAG: [u8; 4] = *b"FRGN";
