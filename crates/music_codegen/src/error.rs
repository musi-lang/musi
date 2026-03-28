use std::error::Error;
use std::fmt;

/// Errors produced during bytecode emission.
#[derive(Debug)]
pub enum EmitError {
    /// A language feature has no codegen implementation yet.
    Unimplemented(&'static str),
}

impl fmt::Display for EmitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unimplemented(feature) => write!(f, "not implemented; {feature}"),
        }
    }
}

impl Error for EmitError {}
