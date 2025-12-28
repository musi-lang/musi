use musi_basic::error::Level;
use musi_macros::MusiError;
use thiserror::Error;

#[derive(Debug, Clone, Error, MusiError)]
#[non_exhaustive]
pub enum SemaErrorKind {
    #[ms(3001)]
    #[error("undefined identifier '{0}'")]
    UndefinedIdent(String),

    #[ms(3002)]
    #[error("undefined type '{0}'")]
    UndefinedType(String),

    #[ms(3003)]
    #[error("duplicate definition of '{0}'")]
    DuplicateDef(String),

    #[ms(3004)]
    #[error("cannot convert type '{from}' to type '{to}'")]
    TypeMismatch { from: String, to: String },

    #[ms(3005)]
    #[error("cannot infer type for '{0}'")]
    CannotInfer(String),

    #[ms(3006)]
    #[error("cannot apply '{callee}' to non-function type '{ty}'")]
    NotCallable { callee: String, ty: String },

    #[ms(3007)]
    #[error("cannot index type '{0}'")]
    NotIndexable(String),

    #[ms(3008)]
    #[error("cannot access field '{field}' on type '{ty}'")]
    NoSuchField { ty: String, field: String },

    #[ms(3009)]
    #[error("expected {expected} argument(s), got {got}")]
    ArityMismatch { expected: usize, got: usize },

    #[ms(3010)]
    #[error("cannot apply binary operator '{op}' to types '{lhs}' and '{rhs}'")]
    InvalidBinaryOp {
        op: String,
        lhs: String,
        rhs: String,
    },

    #[ms(3011)]
    #[error("cannot apply unary operator '{op}' to type '{operand}'")]
    InvalidUnaryOp { op: String, operand: String },

    #[ms(3012)]
    #[error("'return' outside of function")]
    ReturnOutsideFn,

    #[ms(3013)]
    #[error("'break' outside of loop")]
    BreakOutsideLoop,

    #[ms(3014)]
    #[error("'cycle' outside of loop")]
    CycleOutsideLoop,

    #[ms(3015)]
    #[error("pattern match not exhaustive")]
    NonExhaustiveMatch,

    #[ms(3016)]
    #[error("cannot assign to immutable binding '{0}'")]
    AssignmentToImmutable(String),

    #[ms(3017)]
    #[error("unused name '{0}'")]
    UnusedName(String),

    #[ms(3018)]
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
