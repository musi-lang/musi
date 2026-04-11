use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrDiagKind {
    InvalidSurfaceTypeId,
    LoweringRequiresSemaCleanModule,
    LoweringInvariantViolated,
}

impl IrDiagKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::InvalidSurfaceTypeId => 3400,
            Self::LoweringRequiresSemaCleanModule => 3401,
            Self::LoweringInvariantViolated => 3402,
        })
    }

    #[must_use]
    pub const fn message(self) -> &'static str {
        match self {
            Self::InvalidSurfaceTypeId => "invalid surface type id",
            Self::LoweringRequiresSemaCleanModule => "ir lowering requires sema-clean module",
            Self::LoweringInvariantViolated => "ir lowering invariant violated",
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
            3401 => Some(Self::LoweringRequiresSemaCleanModule),
            3402 => Some(Self::LoweringInvariantViolated),
            _ => None,
        }
    }
}
