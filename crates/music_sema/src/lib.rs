mod check;
mod dispatch;
mod effects;
mod env;
mod errors;
mod types;
mod unify;

pub use check::SemaDb;
pub use env::{DispatchInfo, EffectUse, TypeEnv, TypeKey, VariantInfo};
pub use errors::{SemaError, SemaErrorKind};
pub use types::{NominalKey, SemaTypeId, Ty, TyVarId};

use music_config::CompilerOptions;
use music_db::Db;
use music_resolve::ResolutionMap;

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
