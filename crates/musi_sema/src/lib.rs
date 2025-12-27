pub mod binder;
pub mod builtins;
pub mod error;
pub mod model;
pub mod symbol;
pub mod ty_repr;
pub mod types;
pub mod unifier;

#[cfg(test)]
mod tests;

pub use builtins::Builtins;
pub use error::SemaErrorKind;
pub use model::SemanticModel;
pub use symbol::*;
pub use ty_repr::*;
pub use types::*;
pub use unifier::Unifier;
