pub mod check;
pub mod dispatch;
pub mod effects;
pub mod env;
pub mod errors;
pub mod types;
pub mod unify;

pub use check::SemaDb;
pub use env::TypeEnv;
pub use errors::{SemaError, SemaErrorKind};
pub use types::{SemaTypeId, Ty, TyVarId};

use music_config::CompilerOptions;
use music_db::Db;
use music_resolve::queries::ResolutionMap;

/// Runs bidirectional type checking on a resolved module.
///
/// Takes ownership of `Db` and `ResolutionMap`, returning them alongside
/// the type environment and errors.
#[must_use]
pub fn type_check(
    db: Db,
    resolution: ResolutionMap,
    _config: Option<&CompilerOptions>,
) -> (Db, ResolutionMap, TypeEnv, Vec<SemaError>) {
    let mut sdb = SemaDb::new(db, resolution);
    sdb.check_module();
    sdb.finish()
}
