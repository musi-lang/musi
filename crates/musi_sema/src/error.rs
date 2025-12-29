use musi_basic::error::Level;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[non_exhaustive]
pub enum SemaErrorKind {
    #[error("undefined identifier '{0}'")]
    UndefinedIdent(String),

    #[error("undefined type '{0}'")]
    UndefinedType(String),

    #[error("duplicate definition of '{0}'")]
    DuplicateDef(String),

    #[error("cannot convert type '{from}' to type '{to}'")]
    TypeMismatch { from: String, to: String },

    #[error("cannot infer type for '{0}'")]
    CannotInfer(String),

    #[error("cannot apply '{callee}' to non-function type '{ty}'")]
    NotCallable { callee: String, ty: String },

    #[error("cannot index type '{0}'")]
    NotIndexable(String),

    #[error("cannot access field '{field}' on type '{ty}'")]
    NoSuchField { ty: String, field: String },

    #[error("expected {expected} argument(s), got {got}")]
    ArityMismatch { expected: usize, got: usize },

    #[error("cannot apply binary operator '{op}' to types '{lhs}' and '{rhs}'")]
    InvalidBinaryOp {
        op: String,
        lhs: String,
        rhs: String,
    },

    #[error("cannot apply unary operator '{op}' to type '{operand}'")]
    InvalidUnaryOp { op: String, operand: String },

    #[error("'return' outside of function")]
    ReturnOutsideFn,

    #[error("'break' outside of loop")]
    BreakOutsideLoop,

    #[error("'cycle' outside of loop")]
    CycleOutsideLoop,

    #[error("pattern match not exhaustive")]
    NonExhaustiveMatch,

    #[error("cannot assign to immutable binding '{0}'")]
    AssignmentToImmutable(String),

    #[error("unused name '{0}'")]
    UnusedName(String),

    #[error("invalid pipe target")]
    InvalidPipeTarget,
}

impl SemaErrorKind {
    #[must_use]
    pub const fn hint(&self) -> Option<&'static str> {
        match self {
            Self::NonExhaustiveMatch => Some("add missing case(s) or use '_' wildcard"),
            Self::InvalidPipeTarget => Some("use function that returns value"),
            _ => None,
        }
    }

    #[must_use]
    pub const fn level(&self) -> Level {
        match self {
            Self::NonExhaustiveMatch | Self::UnusedName(_) => Level::Warning,
            _ => Level::Error,
        }
    }
}
