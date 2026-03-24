/// `.seam` binary file magic bytes.
pub const MAGIC: [u8; 4] = *b"SEAM";

/// Current major version of the `.seam` format.
pub const VERSION_MAJOR: u8 = 0;

/// Current minor version of the `.seam` format.
pub const VERSION_MINOR: u8 = 1;

/// Fixed header size in bytes: `magic(4)` + `version(2)` + `flags(2)` + `section_count(4)` + `total_size(4)`.
pub const HEADER_SIZE: usize = 16;

/// Four-byte section tags identifying each section in a `.seam` file.
pub mod section {
    /// String table: interned strings, null-separated.
    pub const STRT: [u8; 4] = *b"STRT";
    /// Type table: type descriptors (u8 kind + members).
    pub const TYPE: [u8; 4] = *b"TYPE";
    /// Constant pool: NaN-boxed constant values.
    pub const CNST: [u8; 4] = *b"CNST";
    /// Dependencies: module dependency paths (string table refs).
    pub const DEPS: [u8; 4] = *b"DEPS";
    /// Global table: global bindings with export/opaque flags.
    pub const GLOB: [u8; 4] = *b"GLOB";
    /// Method table: bytecode + stack maps + safepoint bitmaps.
    pub const METH: [u8; 4] = *b"METH";
    /// Effect table: effect descriptors (optional section).
    pub const EFCT: [u8; 4] = *b"EFCT";
    /// Class table: class declarations + instance table (optional section).
    pub const CLSS: [u8; 4] = *b"CLSS";
    /// Foreign table: FFI symbol descriptors.
    pub const FRGN: [u8; 4] = *b"FRGN";
    /// Debug section: line maps, local names (strippable).
    pub const DBUG: [u8; 4] = *b"DBUG";
}

/// NaN-box tag bits (stored in the 3 quiet-NaN tag bits).
pub const NAN_BOX_PTR: u8 = 0b000;
pub const NAN_BOX_SMI: u8 = 0b001;
pub const NAN_BOX_BOOL: u8 = 0b010;
pub const NAN_BOX_UNIT: u8 = 0b011;
pub const NAN_BOX_TAG: u8 = 0b100;
pub const NAN_BOX_CHAR: u8 = 0b101;

/// Well-known builtin type IDs in the `.seam` type table.
pub const BUILTIN_TYPE_TYPE: u16 = 0xFFF0;
pub const BUILTIN_TYPE_ANY: u16 = 0xFFF1;
pub const BUILTIN_TYPE_UNKNOWN: u16 = 0xFFF2;
pub const BUILTIN_TYPE_NEVER: u16 = 0xFFF3;
pub const BUILTIN_TYPE_UNIT: u16 = 0xFFF4;
pub const BUILTIN_TYPE_BOOL: u16 = 0xFFF5;
pub const BUILTIN_TYPE_INT: u16 = 0xFFF6;
pub const BUILTIN_TYPE_FLOAT: u16 = 0xFFF7;
pub const BUILTIN_TYPE_STRING: u16 = 0xFFF8;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
