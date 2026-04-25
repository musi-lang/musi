use music_seam::TypeId;
use music_term::TypeTermKind;

use super::layout::ProgramDataLayout;
use super::model::Program;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramTypeAbiKind {
    Unsupported,
    Unit,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    CString,
    CPtr,
    DataTransparent,
    DataReprCProduct,
}

impl ProgramTypeAbiKind {
    #[must_use]
    pub const fn is_supported_native_abi(self) -> bool {
        !matches!(self, Self::Unsupported)
    }

    #[must_use]
    pub const fn uses_data_layout(self) -> bool {
        matches!(self, Self::DataTransparent | Self::DataReprCProduct)
    }
}

const fn fsize_bits() -> u8 {
    if cfg!(target_pointer_width = "32") {
        32
    } else {
        64
    }
}

const fn target_pointer_bits() -> u8 {
    if cfg!(target_pointer_width = "16") {
        16
    } else if cfg!(target_pointer_width = "32") {
        32
    } else {
        64
    }
}

impl Program {
    #[must_use]
    pub fn type_abi_kind(&self, ty: TypeId) -> ProgramTypeAbiKind {
        match self.type_term(ty).kind {
            TypeTermKind::Unit => ProgramTypeAbiKind::Unit,
            TypeTermKind::Bool => ProgramTypeAbiKind::Bool,
            TypeTermKind::Nat => ProgramTypeAbiKind::Int {
                signed: false,
                bits: target_pointer_bits(),
            },
            TypeTermKind::Int => ProgramTypeAbiKind::Int {
                signed: true,
                bits: target_pointer_bits(),
            },
            TypeTermKind::Int8 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 8,
            },
            TypeTermKind::Int16 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 16,
            },
            TypeTermKind::Int32 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 32,
            },
            TypeTermKind::Int64 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 64,
            },
            TypeTermKind::Nat8 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 8,
            },
            TypeTermKind::Nat16 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 16,
            },
            TypeTermKind::Nat32 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 32,
            },
            TypeTermKind::Nat64 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 64,
            },
            TypeTermKind::Float => ProgramTypeAbiKind::Float { bits: fsize_bits() },
            TypeTermKind::Float32 => ProgramTypeAbiKind::Float { bits: 32 },
            TypeTermKind::Float64 => ProgramTypeAbiKind::Float { bits: 64 },
            TypeTermKind::CString => ProgramTypeAbiKind::CString,
            TypeTermKind::CPtr => ProgramTypeAbiKind::CPtr,
            TypeTermKind::Named { .. } => self.type_data_layout(ty).map_or(
                ProgramTypeAbiKind::Unsupported,
                ProgramDataLayout::native_abi_kind,
            ),
            _ => ProgramTypeAbiKind::Unsupported,
        }
    }
}
