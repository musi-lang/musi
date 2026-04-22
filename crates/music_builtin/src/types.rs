#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinTypeId {
    Type,
    Array,
    Any,
    Unknown,
    Syntax,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Float,
    Float32,
    Float64,
    String,
    Rune,
    Range,
    Pin,
    ClosedRange,
    PartialRangeFrom,
    PartialRangeUpTo,
    PartialRangeThru,
    CString,
    CPtr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinTypeDef {
    pub id: BuiltinTypeId,
    pub name: &'static str,
    pub compiler_prelude: bool,
}

impl BuiltinTypeId {
    #[must_use]
    pub fn def(self) -> &'static BuiltinTypeDef {
        builtin_type_by_id(self)
    }

    #[must_use]
    pub fn name(self) -> &'static str {
        self.def().name
    }
}

pub const BUILTIN_TYPES: &[BuiltinTypeDef] = &[
    BuiltinTypeDef::new(BuiltinTypeId::Type, "Type"),
    BuiltinTypeDef::new(BuiltinTypeId::Array, "Array"),
    BuiltinTypeDef::new(BuiltinTypeId::Any, "Any"),
    BuiltinTypeDef::new(BuiltinTypeId::Unknown, "Unknown"),
    BuiltinTypeDef::new(BuiltinTypeId::Syntax, "Syntax"),
    BuiltinTypeDef::new(BuiltinTypeId::Empty, "Empty"),
    BuiltinTypeDef::new(BuiltinTypeId::Unit, "Unit"),
    BuiltinTypeDef::new(BuiltinTypeId::Bool, "Bool"),
    BuiltinTypeDef::new(BuiltinTypeId::Nat, "Nat"),
    BuiltinTypeDef::new(BuiltinTypeId::Int, "Int"),
    BuiltinTypeDef::new(BuiltinTypeId::Int8, "Int8"),
    BuiltinTypeDef::new(BuiltinTypeId::Int16, "Int16"),
    BuiltinTypeDef::new(BuiltinTypeId::Int32, "Int32"),
    BuiltinTypeDef::new(BuiltinTypeId::Int64, "Int64"),
    BuiltinTypeDef::new(BuiltinTypeId::Nat8, "Nat8"),
    BuiltinTypeDef::new(BuiltinTypeId::Nat16, "Nat16"),
    BuiltinTypeDef::new(BuiltinTypeId::Nat32, "Nat32"),
    BuiltinTypeDef::new(BuiltinTypeId::Nat64, "Nat64"),
    BuiltinTypeDef::new(BuiltinTypeId::Float, "Float"),
    BuiltinTypeDef::new(BuiltinTypeId::Float32, "Float32"),
    BuiltinTypeDef::new(BuiltinTypeId::Float64, "Float64"),
    BuiltinTypeDef::new(BuiltinTypeId::String, "String"),
    BuiltinTypeDef::new(BuiltinTypeId::Rune, "Rune"),
    BuiltinTypeDef::new(BuiltinTypeId::Range, "Range"),
    BuiltinTypeDef::new(BuiltinTypeId::Pin, "Pin"),
    BuiltinTypeDef::new(BuiltinTypeId::ClosedRange, "ClosedRange"),
    BuiltinTypeDef::new(BuiltinTypeId::PartialRangeFrom, "PartialRangeFrom"),
    BuiltinTypeDef::new(BuiltinTypeId::PartialRangeUpTo, "PartialRangeUpTo"),
    BuiltinTypeDef::new(BuiltinTypeId::PartialRangeThru, "PartialRangeThru"),
    BuiltinTypeDef::new(BuiltinTypeId::CString, "CString"),
    BuiltinTypeDef::new(BuiltinTypeId::CPtr, "CPtr"),
];

impl BuiltinTypeDef {
    const fn new(id: BuiltinTypeId, name: &'static str) -> Self {
        Self {
            id,
            name,
            compiler_prelude: true,
        }
    }
}

#[must_use]
pub const fn all_builtin_types() -> &'static [BuiltinTypeDef] {
    BUILTIN_TYPES
}

#[must_use]
pub fn builtin_type_by_name(name: &str) -> Option<&'static BuiltinTypeDef> {
    BUILTIN_TYPES.iter().find(|def| def.name == name)
}

#[must_use]
pub fn is_builtin_type_name(name: &str) -> bool {
    builtin_type_by_name(name).is_some()
}

fn builtin_type_by_id(id: BuiltinTypeId) -> &'static BuiltinTypeDef {
    BUILTIN_TYPES
        .iter()
        .find(|def| def.id == id)
        .expect("builtin type id must have definition")
}
