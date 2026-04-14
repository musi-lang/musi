use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolveDiagKind {
    ExpectedName,
    InvalidStmt,
    UnboundName,
    ImportResolveFailed,
    InvalidImportSpec,
}

impl ResolveDiagKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::ExpectedName => 3200,
            Self::InvalidStmt => 3201,
            Self::UnboundName => 3202,
            Self::ImportResolveFailed => 3203,
            Self::InvalidImportSpec => 3204,
        })
    }

    #[must_use]
    pub const fn message(self) -> &'static str {
        match self {
            Self::ExpectedName => "expected name",
            Self::InvalidStmt => "invalid stmt position",
            Self::UnboundName => "unbound name",
            Self::ImportResolveFailed => "import resolve failed",
            Self::InvalidImportSpec => "invalid import spec",
        }
    }

    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::ExpectedName => "name starts here",
            Self::InvalidStmt => "stmt is not valid here",
            Self::UnboundName => "name is not bound in this scope",
            Self::ImportResolveFailed => "import is not resolved here",
            Self::InvalidImportSpec => "import spec must be string literal",
        }
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        match diag.code()?.raw() {
            3200 => Some(Self::ExpectedName),
            3201 => Some(Self::InvalidStmt),
            3202 => Some(Self::UnboundName),
            3203 => Some(Self::ImportResolveFailed),
            3204 => Some(Self::InvalidImportSpec),
            _ => None,
        }
    }
}
