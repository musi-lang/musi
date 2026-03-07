//! Definition identifiers and metadata for resolved bindings.

use musi_shared::{Span, Symbol};

use crate::types::{Type, TypeVarId};

/// A unique identifier for a definition (binding, function, type, variant, ...).
///
/// `DefId`s are allocated monotonically; `DefId(n)` corresponds to index `n`
/// in the `Vec<DefInfo>` owned by the analysis result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

/// The syntactic category of a definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    /// `const` binding.
    Const,
    /// `var` binding.
    Var,
    /// Named function (`fn name`).
    Fn,
    /// Record or choice type definition.
    Type,
    /// Function parameter.
    Param,
    /// Choice variant constructor.
    Variant,
    /// Glob namespace import (`import * as Name from "path"`).
    Namespace,
}

/// All metadata the compiler knows about a single definition.
#[derive(Debug, Clone)]
pub struct DefInfo {
    pub id: DefId,
    pub name: Symbol,
    pub kind: DefKind,
    pub span: Span,
    /// Filled by the type checker once the type has been inferred/checked.
    pub ty: Option<Type>,
    /// For generic functions: the [`TypeVarId`]s that are still free after
    /// type-checking the body, representing universally-quantified type params.
    /// Empty for monomorphic definitions.
    pub scheme_vars: Vec<TypeVarId>,
}
