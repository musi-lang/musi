//! Semantic type representation.
//!
//! Types are arena-allocated via `Arena<Type>`, so recursive positions use
//! `TypeIdx` (which is `Copy`).  There is no `PrimTy` enum — all types
//! including `Int`, `Bool`, `String` are represented as `Type::Named`.

#[cfg(test)]
mod tests;

use std::fmt;

use music_shared::{Arena, Idx, Interner, Span, Symbol};

use crate::def::{DefId, DefInfo};

/// Index into the semantic type arena.
pub type TypeIdx = Idx<Type>;

/// A unique identifier for a type variable (unification or rigid).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(pub u32);

/// A resolved semantic type.
///
/// All recursive children use `TypeIdx` (arena indices) rather than `Box`.
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
    /// A sum type (structural, named variants).
    Sum { variants: Vec<SumVariant> },
    /// An anonymous sum type (e.g. `Int + String`).
    AnonSum { variants: Vec<Idx<Self>> },
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
    pub ty: TypeIdx,
}

/// A variant in a structural sum type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SumVariant {
    pub name: Symbol,
    pub fields: Vec<TypeIdx>,
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
        effects: vec![],
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
    pub args: Vec<TypeIdx>,
}

/// A typeclass obligation: "`class` must be satisfied for `args`".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obligation {
    pub class: DefId,
    pub args: Vec<TypeIdx>,
    pub span: Span,
}

/// A typeclass instance.
#[derive(Debug, Clone)]
pub struct InstanceInfo {
    pub class: DefId,
    pub target: TypeIdx,
    pub params: Vec<TyVarId>,
    pub constraints: Vec<Obligation>,
    pub members: Vec<(Symbol, DefId)>,
    pub span: Span,
}

/// Helper for displaying types with access to the type arena and def table.
pub struct TypeDisplay<'a> {
    pub ty: TypeIdx,
    pub arena: &'a Arena<Type>,
    pub defs: &'a [DefInfo],
    pub interner: &'a Interner,
}

impl TypeDisplay<'_> {
    fn write_ty(&self, f: &mut fmt::Formatter<'_>, ty: TypeIdx) -> fmt::Result {
        let d = TypeDisplay {
            ty,
            arena: self.arena,
            defs: self.defs,
            interner: self.interner,
        };
        write!(f, "{d}")
    }

    fn resolve_def_name(&self, def: DefId) -> Result<&str, fmt::Error> {
        let idx = usize::try_from(def.0).map_err(|_| fmt::Error)?;
        Ok(self.interner.resolve(self.defs[idx].name))
    }

    fn resolve_symbol(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    /// Writes types separated by `sep` (e.g. `", "` or `" | "`).
    fn fmt_types_sep(&self, f: &mut fmt::Formatter<'_>, tys: &[TypeIdx], sep: &str) -> fmt::Result {
        for (i, &ty) in tys.iter().enumerate() {
            if i > 0 {
                write!(f, "{sep}")?;
            }
            self.write_ty(f, ty)?;
        }
        Ok(())
    }

    fn fmt_bracketed_types(&self, f: &mut fmt::Formatter<'_>, tys: &[TypeIdx]) -> fmt::Result {
        write!(f, "[")?;
        self.fmt_types_sep(f, tys, ", ")?;
        write!(f, "]")
    }

    fn fmt_paren_types(&self, f: &mut fmt::Formatter<'_>, tys: &[TypeIdx]) -> fmt::Result {
        write!(f, "(")?;
        self.fmt_types_sep(f, tys, ", ")?;
        write!(f, ")")
    }

    fn fmt_quantifier_params(f: &mut fmt::Formatter<'_>, params: &[TyVarId]) -> fmt::Result {
        for (i, &v) in params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "'{}", v.0)?;
        }
        Ok(())
    }

    fn fmt_obligation(&self, f: &mut fmt::Formatter<'_>, ob: &Obligation) -> fmt::Result {
        write!(f, "{}", self.resolve_def_name(ob.class)?)?;
        if !ob.args.is_empty() {
            self.fmt_bracketed_types(f, &ob.args)?;
        }
        Ok(())
    }

    fn fmt_obligations(
        &self,
        f: &mut fmt::Formatter<'_>,
        constraints: &[Obligation],
    ) -> fmt::Result {
        for (i, ob) in constraints.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            self.fmt_obligation(f, ob)?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.arena[self.ty] {
            Type::Named { def, args } => {
                write!(f, "{}", self.resolve_def_name(*def)?)?;
                if !args.is_empty() {
                    self.fmt_bracketed_types(f, args)?;
                }
                Ok(())
            }
            Type::Fn { params, ret, .. } => {
                self.fmt_paren_types(f, params)?;
                write!(f, " -> ")?;
                self.write_ty(f, *ret)
            }
            Type::Tuple { elems } => self.fmt_paren_types(f, elems),
            Type::Record { fields, open } => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", self.resolve_symbol(field.name))?;
                    self.write_ty(f, field.ty)?;
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
                    write!(f, "{}", self.resolve_symbol(v.name))?;
                    if !v.fields.is_empty() {
                        self.fmt_paren_types(f, &v.fields)?;
                    }
                }
                Ok(())
            }
            Type::AnonSum { variants } => self.fmt_types_sep(f, variants, " + "),
            Type::Array { elem, len } => {
                if let Some(n) = len {
                    write!(f, "[{n}]")?;
                } else {
                    write!(f, "[]")?;
                }
                self.write_ty(f, *elem)
            }
            Type::Ref { inner } => {
                write!(f, "&")?;
                self.write_ty(f, *inner)
            }
            Type::Var(v) => write!(f, "?{}", v.0),
            Type::Rigid(v) => write!(f, "'{}", v.0),
            Type::Quantified {
                kind,
                params,
                constraints,
                body,
            } => {
                let kw = match kind {
                    Quantifier::Forall => "forall",
                    Quantifier::Exists => "exists",
                };
                write!(f, "{kw} ")?;
                Self::fmt_quantifier_params(f, params)?;
                if !params.is_empty() || !constraints.is_empty() {
                    write!(f, ". ")?;
                }
                if !constraints.is_empty() {
                    self.fmt_obligations(f, constraints)?;
                    write!(f, " => ")?;
                }
                self.write_ty(f, *body)
            }
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Convenience function to format a type as a `Box<str>` for error messages.
#[must_use]
pub fn fmt_type(
    ty: TypeIdx,
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
