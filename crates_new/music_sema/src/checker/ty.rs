use std::fmt;

use music_basic::Span;
use music_hir::{HirArrowFlavor, HirDim, HirTyBinOp};
use music_names::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SemTyId(u32);

#[derive(Debug, Clone, PartialEq)]
pub enum SemTy {
    Error,
    Unknown,
    Any,

    Named { name: Symbol, args: Box<[SemTyId]> },
    Tuple { items: Box<[SemTyId]> },
    Array { dims: Box<[HirDim]>, elem: SemTyId },
    Arrow { flavor: HirArrowFlavor, input: SemTyId, output: SemTyId },
    Binary { op: HirTyBinOp, left: SemTyId, right: SemTyId },
    Mut { base: SemTyId },

    InferVar(InferVarId),
    Generic(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferVarId(u32);

#[derive(Debug)]
pub struct SemTys {
    tys: Vec<SemTy>,
    infer: Vec<Option<SemTyId>>,
}

impl SemTys {
    #[must_use]
    pub fn new() -> Self {
        Self {
            tys: vec![],
            infer: vec![],
        }
    }

    #[must_use]
    pub fn alloc(&mut self, ty: SemTy) -> SemTyId {
        let id = u32::try_from(self.tys.len()).expect("SemTys overflow");
        self.tys.push(ty);
        SemTyId(id)
    }

    #[must_use]
    pub fn get(&self, id: SemTyId) -> &SemTy {
        self.tys
            .get(usize::try_from(id.0).unwrap_or(0))
            .expect("SemTyId out of bounds")
    }

    pub fn fresh_infer_var(&mut self, span: Span) -> SemTyId {
        let id = u32::try_from(self.infer.len()).expect("InferVar overflow");
        self.infer.push(None);
        let _ = span;
        self.alloc(SemTy::InferVar(InferVarId(id)))
    }

    #[must_use]
    pub fn infer_binding(&self, var: InferVarId) -> Option<SemTyId> {
        self.infer.get(var.0 as usize).copied().flatten()
    }

    pub fn bind_infer(&mut self, var: InferVarId, to: SemTyId) {
        let slot = self
            .infer
            .get_mut(var.0 as usize)
            .expect("InferVarId out of bounds");
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
    pub interner: &'a music_names::Interner,
    pub ty: SemTyId,
}

impl fmt::Display for SemTyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_ty(self.tys, self.interner, self.ty, f)
    }
}

fn fmt_ty(
    tys: &SemTys,
    interner: &music_names::Interner,
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
        SemTy::Array { elem, .. } => {
            write!(f, "[]")?;
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
    }
}
