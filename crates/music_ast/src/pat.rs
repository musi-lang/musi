use music_found::{Ident, Literal};

use crate::{PatId, PatList};

#[derive(Debug, Clone, PartialEq)]
pub enum PatKind {
    Wildcard,
    Lit(Literal),
    Bind(Ident),
    As { name: Ident, pat: PatId },
    Variant { tag: Ident, fields: PatList },
    Record(Vec<RecordPatField>),
    Tuple(PatList),
    Array(PatList),
    Or(Vec<PatId>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordPatField {
    pub mutable: bool,
    pub name: Ident,
    pub pat: Option<PatId>,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
