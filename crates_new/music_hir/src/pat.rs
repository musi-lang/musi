use music_names::Ident;
use music_storage::Idx;

use super::*;

pub type HirPatId = Idx<HirPat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirPat {
    pub origin: HirOrigin,
    pub ty: HirTyId,
    pub kind: HirPatKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPatKind {
    Wildcard,
    Bind { name: Ident, sub: Option<HirPatId> },
    Lit { lit: HirLit },
    Variant { name: Ident, args: HirPatIds },
    Record { fields: Box<[HirRecordPatField]> },
    Tuple { items: HirPatIds },
    Array { items: HirPatIds },
    Or { alts: HirPatIds },
    Error,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
