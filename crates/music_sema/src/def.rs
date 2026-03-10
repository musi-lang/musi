//! Definition identifiers and metadata for resolved bindings.

#[cfg(test)]
mod tests;

use music_ast::expr::ParamMode;
use music_shared::{Span, Symbol};

use crate::types::{Obligation, TyVarId, TypeIdx};

/// A unique identifier for a definition (binding, function, type, variant, ...).
///
/// `DefId`s are allocated monotonically; `DefId(n)` corresponds to index `n`
/// in the `Vec<DefInfo>` owned by the analysis result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// The syntactic category of a definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    /// Immutable binding (`let`).
    Let,
    /// Mutable binding (`var`).
    Var,
    /// Named function.
    Fn,
    /// Function parameter.
    Param,
    /// Type definition (record, choice, alias).
    Type,
    /// Choice variant constructor.
    Variant,
    /// Type class definition.
    Class,
    /// Type class instance.
    Given,
    /// Effect definition.
    Effect,
    /// Effect operation.
    EffectOp,
    /// Import binding.
    Import,
    /// Foreign function binding.
    ForeignFn,
    /// Opaque foreign type.
    OpaqueType,
}

/// Type-checker annotations for a definition, filled during type checking.
#[derive(Debug, Clone, Default)]
pub struct DefTyInfo {
    /// Filled by the type checker once the type has been inferred/checked.
    pub ty: Option<TypeIdx>,
    /// For generic definitions: the type variables that are universally quantified.
    pub ty_params: Vec<TyVarId>,
    /// Typeclass constraints on this definition.
    pub constraints: Vec<Obligation>,
}

/// All metadata the compiler knows about a single definition.
#[derive(Debug, Clone)]
pub struct DefInfo {
    pub id: DefId,
    pub name: Symbol,
    pub kind: DefKind,
    pub span: Span,
    /// For variants/members: the enclosing type or class.
    pub parent: Option<DefId>,
    /// Type-checking data (filled incrementally by the checker).
    pub ty_info: DefTyInfo,
    /// How many times this definition is referenced (for unused warnings).
    pub use_count: u32,
    /// Whether this definition is exported from the module.
    pub exported: bool,
    /// For parameters: the calling-convention mode (`var`, `inout`, `ref`).
    pub param_mode: Option<ParamMode>,
}

/// Registry of all definitions encountered during analysis.
pub struct DefTable {
    defs: Vec<DefInfo>,
}

impl DefTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { defs: vec![] }
    }

    /// # Panics
    ///
    /// Panics if the number of definitions exceeds `u32::MAX`.
    #[must_use]
    pub fn alloc(&mut self, name: Symbol, kind: DefKind, span: Span) -> DefId {
        let id = DefId(u32::try_from(self.defs.len()).expect("def count overflow"));
        self.defs.push(DefInfo {
            id,
            name,
            kind,
            span,
            parent: None,
            ty_info: DefTyInfo::default(),
            use_count: 0,
            exported: false,
            param_mode: None,
        });
        id
    }

    /// # Panics
    ///
    /// Panics if `id` was not allocated by this table.
    #[must_use]
    pub fn get(&self, id: DefId) -> &DefInfo {
        let idx = usize::try_from(id.0).expect("DefId in range");
        &self.defs[idx]
    }

    /// # Panics
    ///
    /// Panics if `id` was not allocated by this table.
    pub fn get_mut(&mut self, id: DefId) -> &mut DefInfo {
        let idx = usize::try_from(id.0).expect("DefId in range");
        &mut self.defs[idx]
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.defs.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.defs.is_empty()
    }

    #[must_use]
    pub fn into_vec(self) -> Vec<DefInfo> {
        self.defs
    }

    pub fn iter(&self) -> impl Iterator<Item = &DefInfo> {
        self.defs.iter()
    }
}

impl Default for DefTable {
    fn default() -> Self {
        Self::new()
    }
}
