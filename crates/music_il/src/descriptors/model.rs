#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Builtin = 0,
    Record = 1,
    Choice = 2,
}

impl TypeKind {
    #[must_use]
    pub const fn to_byte(self) -> u8 {
        match self {
            Self::Builtin => 0,
            Self::Record => 1,
            Self::Choice => 2,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub id: u16,
    pub key: String,
    pub kind: TypeKind,
    pub member_count: u16,
}

pub type EffectOps = Vec<EffectOpDescriptor>;
pub type ClassMethods = Vec<ClassMethod>;
pub type ClassInstances = Vec<ClassInstance>;
pub type MethodNames = Vec<String>;
pub type ForeignParamTypes = Vec<FfiType>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpDescriptor {
    pub id: u16,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDescriptor {
    pub id: u16,
    pub module_name: String,
    pub name: String,
    pub operations: EffectOps,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMethod {
    pub name: String,
    pub method_idx: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassInstance {
    pub ty_id: u16,
    pub methods: ClassMethods,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    pub id: u16,
    pub name: String,
    pub method_names: MethodNames,
    pub instances: ClassInstances,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForeignAbi {
    Default = 0,
    Cdecl = 1,
    Stdcall = 2,
    Fastcall = 3,
}

impl ForeignAbi {
    #[must_use]
    pub const fn to_byte(self) -> u8 {
        match self {
            Self::Default => 0,
            Self::Cdecl => 1,
            Self::Stdcall => 2,
            Self::Fastcall => 3,
        }
    }

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
    #[must_use]
    pub const fn to_byte(self) -> u8 {
        match self {
            Self::Void => 0,
            Self::Int => 1,
            Self::Float => 2,
            Self::Bool => 3,
            Self::Ptr => 4,
            Self::Str => 5,
        }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: String,
    pub symbol: Option<String>,
    pub library: Option<String>,
    pub abi: ForeignAbi,
    pub arity: u8,
    pub exported: bool,
    pub param_types: ForeignParamTypes,
    pub return_type: FfiType,
}
