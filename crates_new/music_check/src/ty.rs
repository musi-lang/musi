use std::collections::BTreeMap;
use std::fmt;

use music_basic::Span;
use music_hir::{HirArrowFlavor, HirDim, HirTyBinOp};
use music_names::{Interner, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SemTyId(u32);

pub type SemTyIds = Box<[SemTyId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemTy {
    Error,
    Unknown,
    Any,

    Named {
        name: Symbol,
        args: SemTyIds,
    },
    Tuple {
        items: SemTyIds,
    },
    Array {
        dims: Box<[HirDim]>,
        elem: SemTyId,
    },
    Arrow {
        flavor: HirArrowFlavor,
        input: SemTyId,
        output: SemTyId,
    },
    Binary {
        op: HirTyBinOp,
        left: SemTyId,
        right: SemTyId,
    },
    Mut {
        base: SemTyId,
    },
    Record {
        fields: BTreeMap<Symbol, SemTyId>,
    },

    InferVar(InferVarId),
    Generic(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferVarId(u32);

#[derive(Debug, Clone)]
pub struct SemTys {
    tys: Vec<SemTy>,
    infer: Vec<Option<SemTyId>>,
}

impl SemTys {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            tys: Vec::new(),
            infer: Vec::new(),
        }
    }

    /// Allocates `ty` in the semantic type table.
    ///
    /// # Panics
    /// Panics if semantic type table size exceeds `u32::MAX`.
    #[must_use]
    pub fn alloc(&mut self, ty: SemTy) -> SemTyId {
        let id = u32::try_from(self.tys.len()).expect("SemTys overflow");
        self.tys.push(ty);
        SemTyId(id)
    }

    /// Returns semantic type for `id`.
    ///
    /// # Panics
    /// Panics if `id` not allocated by this `SemTys`.
    #[must_use]
    pub fn get(&self, id: SemTyId) -> &SemTy {
        self.tys
            .get(usize::try_from(id.0).unwrap_or(usize::MAX))
            .expect("SemTyId out of bounds")
    }

    /// Allocates a fresh inference variable.
    ///
    /// # Panics
    /// Panics if inference variable table size exceeds `u32::MAX`.
    pub fn fresh_infer_var(&mut self, span: Span) -> SemTyId {
        let id = u32::try_from(self.infer.len()).expect("InferVar overflow");
        self.infer.push(None);
        let _ = span;
        self.alloc(SemTy::InferVar(InferVarId(id)))
    }

    #[must_use]
    pub fn infer_binding(&self, var: InferVarId) -> Option<SemTyId> {
        self.infer
            .get(usize::try_from(var.0).unwrap_or(usize::MAX))
            .copied()
            .flatten()
    }

    pub fn bind_infer(&mut self, var: InferVarId, to: SemTyId) {
        let Some(slot) = self
            .infer
            .get_mut(usize::try_from(var.0).unwrap_or(usize::MAX))
        else {
            return;
        };
        *slot = Some(to);
    }
}

impl Default for SemTys {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SemTyDisplay<'a> {
    pub tys: &'a SemTys,
    pub interner: &'a Interner,
    pub ty: SemTyId,
}

impl fmt::Display for SemTyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_ty(self.tys, self.interner, self.ty, f)
    }
}

fn fmt_ty(
    tys: &SemTys,
    interner: &Interner,
    ty: SemTyId,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match tys.get(ty) {
        SemTy::Error => write!(f, "<error>"),
        SemTy::Unknown => write!(f, "Unknown"),
        SemTy::Any => write!(f, "Any"),
        SemTy::InferVar(var) => {
            if let Some(bound) = tys.infer_binding(*var) {
                return fmt_ty(tys, interner, bound, f);
            }
            write!(f, "_")
        }
        SemTy::Generic(i) => write!(f, "T{i}"),
        SemTy::Named { name, args } => {
            write!(f, "{}", interner.resolve(*name))?;
            if !args.is_empty() {
                write!(f, "[")?;
                for (i, arg) in args.iter().copied().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    fmt_ty(tys, interner, arg, f)?;
                }
                write!(f, "]")?;
            }
            Ok(())
        }
        SemTy::Tuple { items } => {
            write!(f, "(")?;
            for (i, item) in items.iter().copied().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                fmt_ty(tys, interner, item, f)?;
            }
            write!(f, ")")
        }
        SemTy::Array { elem: _, .. } => {
            let SemTy::Array { dims, elem } = tys.get(ty) else {
                return write!(f, "[]<error>");
            };
            fmt_array_dims(dims, interner, f)?;
            fmt_ty(tys, interner, *elem, f)
        }
        SemTy::Arrow {
            flavor,
            input,
            output,
        } => {
            fmt_ty(tys, interner, *input, f)?;
            match flavor {
                HirArrowFlavor::Pure => write!(f, " -> ")?,
                HirArrowFlavor::Effectful => write!(f, " ~> ")?,
            }
            fmt_ty(tys, interner, *output, f)
        }
        SemTy::Binary { op, left, right } => {
            fmt_ty(tys, interner, *left, f)?;
            match op {
                HirTyBinOp::Sum => write!(f, " + ")?,
                HirTyBinOp::Product => write!(f, " * ")?,
            }
            fmt_ty(tys, interner, *right, f)
        }
        SemTy::Mut { base } => {
            write!(f, "mut ")?;
            fmt_ty(tys, interner, *base, f)
        }
        SemTy::Record { fields } => {
            write!(f, "{{")?;
            for (i, (name, ty)) in fields.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", interner.resolve(*name))?;
                write!(f, " := ")?;
                fmt_ty(tys, interner, *ty, f)?;
            }
            write!(f, "}}")
        }
    }
}

fn fmt_array_dims(dims: &[HirDim], interner: &Interner, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if dims.len() == 1 && matches!(dims[0], HirDim::Inferred { .. }) {
        return write!(f, "[]");
    }

    write!(f, "[")?;
    for (i, dim) in dims.iter().enumerate() {
        if i != 0 {
            write!(f, ", ")?;
        }
        match dim {
            HirDim::IntLit {
                value: Some(value), ..
            } => write!(f, "{value}")?,
            HirDim::IntLit { value: None, .. } | HirDim::Inferred { .. } => write!(f, "_")?,
            HirDim::Name { name } => write!(f, "{}", interner.resolve(name.name))?,
        }
    }
    write!(f, "]")
}
