//! Semantic type representation.
//!
//! Types are arena-allocated via `Arena<Type>`, so recursive positions use
//! `Idx<Type>` (which is `Copy`).  There is no `PrimTy` enum — all types
//! including `Int`, `Bool`, `String` are represented as `Type::Named`.

#[cfg(test)]
mod tests;

use std::fmt;

use music_shared::{Arena, Idx, Interner, Symbol};

use crate::def::{DefId, DefInfo};

/// A unique identifier for a type variable (unification or rigid).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(pub u32);

/// A resolved semantic type.
///
/// All recursive children use `Idx<Type>` (arena indices) rather than `Box`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A named type (user-defined or prelude-registered), with type arguments.
    Named { def: DefId, args: Vec<Idx<Self>> },
    /// A function type with parameters, return type, and effect row.
    Fn {
        params: Vec<Idx<Self>>,
        ret: Idx<Self>,
        effects: EffectRow,
    },
    /// A tuple type.
    Tuple { elems: Vec<Idx<Self>> },
    /// A record type (structural).
    Record {
        fields: Vec<RecordField>,
        open: bool,
    },
    /// A sum type (structural).
    Sum { variants: Vec<SumVariant> },
    /// An array type with optional fixed length.
    Array { elem: Idx<Self>, len: Option<u32> },
    /// A reference type.
    Ref { inner: Idx<Self> },
    /// A unification variable (solvable during inference).
    Var(TyVarId),
    /// A skolem (rigid) variable from `forall` (not solvable).
    Rigid(TyVarId),
    /// A quantified type (`forall` or `exists`).
    Quantified {
        kind: Quantifier,
        params: Vec<TyVarId>,
        constraints: Vec<Obligation>,
        body: Idx<Self>,
    },
    /// Poison type that absorbs all unification (suppresses cascading errors).
    Error,
}

/// Quantifier kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Quantifier {
    Forall,
    Exists,
}

/// A field in a structural record type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub name: Symbol,
    pub ty: Idx<Type>,
}

/// A variant in a structural sum type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SumVariant {
    pub name: Symbol,
    pub fields: Vec<Idx<Type>>,
}

/// An effect row: a list of concrete effects plus an optional row variable
/// for polymorphic effects.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectRow {
    pub effects: Vec<EffectEntry>,
    pub row_var: Option<TyVarId>,
}

impl EffectRow {
    /// The empty, pure effect row.
    pub const PURE: Self = Self {
        effects: Vec::new(),
        row_var: None,
    };

    /// Returns `true` if this effect row is definitely pure (no effects, no row variable).
    #[must_use]
    pub const fn is_pure(&self) -> bool {
        self.effects.is_empty() && self.row_var.is_none()
    }
}

/// A concrete effect in an effect row.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectEntry {
    pub def: DefId,
    pub args: Vec<Idx<Type>>,
}

/// A typeclass obligation: "`class` must be satisfied for `args`".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obligation {
    pub class: DefId,
    pub args: Vec<Idx<Type>>,
    pub span: music_shared::Span,
}

/// A typeclass instance.
#[derive(Debug, Clone)]
pub struct InstanceInfo {
    pub class: DefId,
    pub target: Idx<Type>,
    pub params: Vec<TyVarId>,
    pub constraints: Vec<Obligation>,
    pub members: Vec<(Symbol, DefId)>,
    pub span: music_shared::Span,
}

/// Helper for displaying types with access to the type arena and def table.
pub struct TypeDisplay<'a> {
    pub ty: Idx<Type>,
    pub arena: &'a Arena<Type>,
    pub defs: &'a [DefInfo],
    pub interner: &'a Interner,
}

impl fmt::Display for TypeDisplay<'_> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.arena[self.ty] {
            Type::Named { def, args } => {
                let idx = usize::try_from(def.0).map_err(|_| fmt::Error)?;
                let name = self.interner.resolve(self.defs[idx].name);
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, &arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        let d = TypeDisplay {
                            ty: arg,
                            arena: self.arena,
                            defs: self.defs,
                            interner: self.interner,
                        };
                        write!(f, "{d}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Type::Fn { params, ret, .. } => {
                write!(f, "(")?;
                for (i, &p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let d = TypeDisplay {
                        ty: p,
                        arena: self.arena,
                        defs: self.defs,
                        interner: self.interner,
                    };
                    write!(f, "{d}")?;
                }
                write!(f, ") -> ")?;
                let d = TypeDisplay {
                    ty: *ret,
                    arena: self.arena,
                    defs: self.defs,
                    interner: self.interner,
                };
                write!(f, "{d}")
            }
            Type::Tuple { elems } => {
                write!(f, "(")?;
                for (i, &e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let d = TypeDisplay {
                        ty: e,
                        arena: self.arena,
                        defs: self.defs,
                        interner: self.interner,
                    };
                    write!(f, "{d}")?;
                }
                write!(f, ")")
            }
            Type::Record { fields, open } => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let name = self.interner.resolve(field.name);
                    let d = TypeDisplay {
                        ty: field.ty,
                        arena: self.arena,
                        defs: self.defs,
                        interner: self.interner,
                    };
                    write!(f, "{name}: {d}")?;
                }
                if *open {
                    write!(f, ", ..")?;
                }
                write!(f, " }}")
            }
            Type::Sum { variants } => {
                for (i, v) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    let name = self.interner.resolve(v.name);
                    write!(f, "{name}")?;
                    if !v.fields.is_empty() {
                        write!(f, "(")?;
                        for (j, &ft) in v.fields.iter().enumerate() {
                            if j > 0 {
                                write!(f, ", ")?;
                            }
                            let d = TypeDisplay {
                                ty: ft,
                                arena: self.arena,
                                defs: self.defs,
                                interner: self.interner,
                            };
                            write!(f, "{d}")?;
                        }
                        write!(f, ")")?;
                    }
                }
                Ok(())
            }
            Type::Array { elem, len } => {
                if let Some(n) = len {
                    write!(f, "[{n}]")?;
                } else {
                    write!(f, "[]")?;
                }
                let d = TypeDisplay {
                    ty: *elem,
                    arena: self.arena,
                    defs: self.defs,
                    interner: self.interner,
                };
                write!(f, "{d}")
            }
            Type::Ref { inner } => {
                write!(f, "&")?;
                let d = TypeDisplay {
                    ty: *inner,
                    arena: self.arena,
                    defs: self.defs,
                    interner: self.interner,
                };
                write!(f, "{d}")
            }
            Type::Var(v) => write!(f, "?{}", v.0),
            Type::Rigid(v) => write!(f, "'{}", v.0),
            Type::Quantified { body, .. } => {
                // Simplified display
                let d = TypeDisplay {
                    ty: *body,
                    arena: self.arena,
                    defs: self.defs,
                    interner: self.interner,
                };
                write!(f, "forall .. {d}")
            }
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Convenience function to format a type as a `Box<str>` for error messages.
#[must_use]
pub fn fmt_type(
    ty: Idx<Type>,
    arena: &Arena<Type>,
    defs: &[DefInfo],
    interner: &Interner,
) -> Box<str> {
    let d = TypeDisplay {
        ty,
        arena,
        defs,
        interner,
    };
    Box::from(d.to_string())
}
