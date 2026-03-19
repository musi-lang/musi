//! Definition identifiers and metadata for resolved bindings.

#[cfg(test)]
mod tests;

use msc_ast::expr::ParamMode;
use msc_shared::{FileId, Span, Symbol};

use crate::types::{LawObligation, Obligation, TypeIdx};

/// Bitfield of behavioural flags for a definition.
///
/// Kept as a thin `u8` wrapper so adding flags is a non-breaking change.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DefFlags(pub u8);

impl DefFlags {
    /// Function should be inlined at every call site.
    pub const INLINE: u8 = 0x01;
    /// Function must never be inlined.
    pub const INLINE_NEVER: u8 = 0x02;
    /// Type may not be constructed directly outside the defining module.
    pub const ABSTRACT: u8 = 0x04;
    /// Type uses C-compatible memory layout (`#[repr("C")]`).
    pub const REPR_C: u8 = 0x08;
    /// Type uses packed layout (`#[repr("packed")]`).
    pub const REPR_PACKED: u8 = 0x10;

    /// Returns `true` when the given flag constant is set.
    #[must_use]
    pub const fn has(self, flag: u8) -> bool {
        self.0 & flag != 0
    }

    /// Returns a copy with the given flag set.
    #[must_use]
    pub const fn with(self, flag: u8) -> Self {
        Self(self.0 | flag)
    }
}

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
    Instance,
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
    /// Typeclass law (named property).
    Law,
    /// Universally quantified law variable (value witness of class type param).
    LawVar,
    /// Compiler-internal primitive type (not user-visible).
    Primitive,
}

/// Type-checker annotations for a definition, filled during type checking.
#[derive(Debug, Clone, Default)]
pub struct DefTyInfo {
    /// Filled by the type checker once the type has been inferred/checked.
    pub ty: Option<TypeIdx>,
    /// For generic definitions: the `DefId`s of the type parameter defs.
    pub ty_params: Vec<DefId>,
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
    /// The source file this definition was declared in.
    pub file_id: FileId,
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
    /// If this definition carries `#[lang := "..."]`, the interned name.
    pub lang_item: Option<Symbol>,
    /// Law obligations for class definitions (empty for non-class defs).
    pub law_obligations: Vec<LawObligation>,
    /// Behavioural flags: inline, inline-never, abstract.
    pub flags: DefFlags,
    /// If this definition carries `#[deprecated]` or `#[deprecated := "msg"]`,
    /// the deprecation message symbol.
    pub deprecated: Option<Symbol>,
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
    pub fn alloc(&mut self, name: Symbol, kind: DefKind, span: Span, file_id: FileId) -> DefId {
        let id = DefId(u32::try_from(self.defs.len()).expect("def count overflow"));
        self.defs.push(DefInfo {
            id,
            name,
            kind,
            span,
            file_id,
            parent: None,
            ty_info: DefTyInfo::default(),
            use_count: 0,
            exported: false,
            param_mode: None,
            lang_item: None,
            law_obligations: vec![],
            flags: DefFlags(0),
            deprecated: None,
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
