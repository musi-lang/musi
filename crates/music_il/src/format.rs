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

/// Discriminator for entries in the TYPE section.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Builtin = 0,
    Record = 1,
    Choice = 2,
}

/// A single entry in the TYPE section: identifies a type for runtime checks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub id: u16,
    pub kind: TypeKind,
    pub member_count: u16,
}

/// A method implementation within a class instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMethod {
    /// String table index of the method name.
    pub name_idx: u32,
    /// Index into the METH section, or `0xFFFF` for abstract.
    pub method_idx: u16,
}

/// A type class instance: which type implements the class and how.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassInstance {
    /// Which type this instance is for.
    pub type_id: u16,
    /// Method implementations for this instance.
    pub methods: Vec<ClassMethod>,
}

/// A type class descriptor in the CLSS section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    pub id: u16,
    /// String table index of the class name.
    pub name_idx: u32,
    /// Number of methods declared by the class.
    pub method_count: u16,
    /// String table indices for method names.
    pub method_names: Vec<u32>,
    /// Registered instances.
    pub instances: Vec<ClassInstance>,
}

/// ABI for foreign function calls.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForeignAbi {
    Default = 0,
    Cdecl = 1,
    Stdcall = 2,
    Fastcall = 3,
}

impl ForeignAbi {
    /// Decode a byte into a `ForeignAbi`, defaulting to `Default` for unknown values.
    #[must_use]
    pub const fn from_byte(byte: u8) -> Self {
        match byte {
            1 => Self::Cdecl,
            2 => Self::Stdcall,
            3 => Self::Fastcall,
            _ => Self::Default,
        }
    }
}

/// Type tag for FFI parameter and return types in the FRGN section.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FfiType {
    Void = 0,
    Int = 1,
    Float = 2,
    Bool = 3,
    Ptr = 4,
    Str = 5,
}

impl FfiType {
    /// Decode a byte into an `FfiType`, defaulting to `Void` for unknown values.
    #[must_use]
    pub const fn from_byte(byte: u8) -> Self {
        match byte {
            1 => Self::Int,
            2 => Self::Float,
            3 => Self::Bool,
            4 => Self::Ptr,
            5 => Self::Str,
            _ => Self::Void,
        }
    }
}

/// An FFI symbol descriptor in the FRGN section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    /// Musi identifier (string table index).
    pub name_idx: u32,
    /// C symbol name (string table index), or same as `name_idx` if no `as`.
    pub symbol_idx: u32,
    /// Library name (string table index), `0xFFFF_FFFF` if none.
    pub lib_idx: u32,
    pub abi: ForeignAbi,
    pub arity: u8,
    /// `export foreign = true`.
    pub exported: bool,
    /// Type tags for each parameter.
    pub param_types: Vec<FfiType>,
    /// Return type tag.
    pub return_type: FfiType,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
