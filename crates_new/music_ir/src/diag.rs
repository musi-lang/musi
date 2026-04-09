use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrDiagKind {
    InvalidSurfaceTypeId,
}

impl IrDiagKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::InvalidSurfaceTypeId => 3400,
        })
    }

    #[must_use]
    pub const fn message(self) -> &'static str {
        match self {
            Self::InvalidSurfaceTypeId => "invalid surface type id",
        }
    }

    #[must_use]
    pub const fn label(self) -> &'static str {
        self.message()
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        match diag.code()?.raw() {
            3400 => Some(Self::InvalidSurfaceTypeId),
            _ => None,
        }
    }
}
