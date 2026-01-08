use musi_core::{MusiResult, Span};

use crate::errors;
use crate::table::UnificationTable;
use crate::ty::{TyArena, TyId, TyKind};

pub struct Unifier<'a> {
    arena: &'a TyArena,
    table: &'a mut UnificationTable,
}

impl<'a> Unifier<'a> {
    #[must_use]
    pub const fn new(arena: &'a TyArena, table: &'a mut UnificationTable) -> Self {
        Self { arena, table }
    }

    /// # Errors
    /// Returns error if types cannot be unified.
    pub fn unify(&mut self, t1: TyId, t2: TyId, span: Span) -> MusiResult<()> {
        let ty1 = self.resolve(t1);
        let ty2 = self.resolve(t2);

        if ty1 == ty2 {
            return Ok(());
        }

        let k1 = &self.arena.get(ty1).kind;
        let k2 = &self.arena.get(ty2).kind;

        tracing::trace!(?k1, ?k2, "unify_structural");

        match (k1, k2) {
            (TyKind::Var(v1), TyKind::Var(v2)) => {
                self.table.unify_var_var(*v1, *v2);
                Ok(())
            }
            (TyKind::Var(v), _) => {
                self.table.unify_var_ty(*v, ty2);
                Ok(())
            }
            (_, TyKind::Var(v)) => {
                self.table.unify_var_ty(*v, ty1);
                Ok(())
            }
            _ => self.unify_structural(ty1, ty2, span),
        }
    }

    fn resolve(&mut self, ty: TyId) -> TyId {
        if let TyKind::Var(v) = &self.arena.get(ty).kind
            && let Some(bound_ty) = self.table.probe(*v)
        {
            return self.resolve(bound_ty);
        }
        ty
    }

    fn unify_structural(&mut self, t1: TyId, t2: TyId, span: Span) -> MusiResult<()> {
        let k1 = self.arena.get(t1).kind.clone();
        let k2 = self.arena.get(t2).kind.clone();

        match (&k1, &k2) {
            (TyKind::Any | TyKind::Never, _)
            | (_, TyKind::Any | TyKind::Never)
            | (TyKind::Int, TyKind::Int)
            | (TyKind::Real, TyKind::Real)
            | (TyKind::String, TyKind::String)
            | (TyKind::Rune, TyKind::Rune)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Unit, TyKind::Unit) => Ok(()),

            (TyKind::Tuple(fs1), TyKind::Tuple(fs2)) => {
                self.unify_sequences(fs1, fs2, span, &k1, &k2)
            }
            (TyKind::Array(e1), TyKind::Array(e2)) => self.unify(*e1, *e2, span),

            (TyKind::Record { fields: f1 }, TyKind::Record { fields: f2 }) => {
                self.unify_records(f1, f2, span, &k1, &k2)
            }

            (
                TyKind::Fn {
                    params: p1,
                    ret: r1,
                },
                TyKind::Fn {
                    params: p2,
                    ret: r2,
                },
            ) => {
                self.unify_sequences(p1, p2, span, &k1, &k2)?;
                self.unify(*r1, *r2, span)
            }

            (TyKind::Optional(i1), TyKind::Optional(i2))
            | (TyKind::Ptr(i1), TyKind::Ptr(i2))
            | (TyKind::Range(i1), TyKind::Range(i2)) => self.unify(*i1, *i2, span),

            (TyKind::Named(n1), TyKind::Named(n2)) if n1 == n2 => Ok(()),

            _ => Err(errors::type_mismatch(&k1, &k2, span)),
        }
    }

    fn unify_sequences(
        &mut self,
        s1: &[TyId],
        s2: &[TyId],
        span: Span,
        k1: &TyKind,
        k2: &TyKind,
    ) -> MusiResult<()> {
        if s1.len() != s2.len() {
            return Err(errors::type_mismatch(k1, k2, span));
        }
        for (&sub1, &sub2) in s1.iter().zip(s2) {
            self.unify(sub1, sub2, span)?;
        }
        Ok(())
    }

    fn unify_records(
        &mut self,
        f1: &[(musi_core::Name, TyId)],
        f2: &[(musi_core::Name, TyId)],
        span: Span,
        k1: &TyKind,
        k2: &TyKind,
    ) -> MusiResult<()> {
        if f1.len() != f2.len() {
            return Err(errors::type_mismatch(k1, k2, span));
        }
        for ((n1, ty1), (n2, ty2)) in f1.iter().zip(f2) {
            if n1 != n2 {
                return Err(errors::type_mismatch(k1, k2, span));
            }
            self.unify(*ty1, *ty2, span)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
