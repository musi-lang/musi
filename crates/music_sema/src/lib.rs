pub mod check;
pub mod dispatch;
pub mod effects;
pub mod env;
pub mod errors;
pub mod types;
pub mod unify;

pub use env::TypeEnv;
pub use errors::{SemaError, SemaErrorKind};
pub use types::{SemaTypeId, Ty, TyVarId};

use music_config::CompilerOptions;
use music_db::Db;
use music_resolve::queries::ResolutionMap;

/// Runs bidirectional type checking on a resolved module.
///
/// Returns the populated type environment and any errors found.
#[must_use]
pub fn type_check(
    db: &Db,
    resolution: &ResolutionMap,
    _config: Option<&CompilerOptions>,
) -> (TypeEnv, Vec<SemaError>) {
    let mut checker = check::Checker::new(db, resolution);
    checker.check_module();
    checker.finish()
}
