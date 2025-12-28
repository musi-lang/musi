pub mod binder;
pub mod builtins;
pub mod error;
pub mod phase1;
pub mod phase2;
pub mod phase3;
pub mod semantic;
pub mod symbol;
pub mod ty_repr;
pub mod types;
pub mod unifier;

#[cfg(test)]
pub(crate) mod test_utils;

pub use binder::*;
pub use builtins::Builtins;
pub use error::SemaErrorKind;
pub use semantic::*;
pub use symbol::*;
pub use ty_repr::*;
pub use types::*;
pub use unifier::Unifier;
