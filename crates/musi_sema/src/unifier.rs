use std::collections::HashMap;

use crate::error::SemaErrorKind;
use crate::ty_repr::{TyRepr, TyReprKind, TyVarId};

#[derive(Debug)]
pub struct Unifier {
    substitutions: HashMap<TyVarId, TyRepr>,
    next_var: u32,
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}

impl Unifier {
    #[must_use]
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
            next_var: 0,
        }
    }

    pub const fn fresh_var(&mut self) -> TyRepr {
        let id = TyVarId::new(self.next_var);
        self.next_var += 1;
        TyRepr::var(id)
    }

    /// Unifies two types.
    ///
    /// # Errors
    ///
    /// Returns `SemaErrorKind::TypeMismatch` if types cannot unify.
    pub fn unify(&mut self, a: &TyRepr, b: &TyRepr) -> Result<(), SemaErrorKind> {
        let a = self.apply(a);
        let b = self.apply(b);
        match (&a.kind, &b.kind) {
            _ if a == b => Ok(()),
            (TyReprKind::Error | TyReprKind::Any, _) | (_, TyReprKind::Error | TyReprKind::Any) => {
                Ok(())
            }
            (TyReprKind::Var(id), _) => {
                self.bind(*id, b);
                Ok(())
            }
            (_, TyReprKind::Var(id)) => {
                self.bind(*id, a);
                Ok(())
            }
            (TyReprKind::Never, _) | (_, TyReprKind::Never) => Ok(()),
            (TyReprKind::Optional(inner_a), TyReprKind::Optional(inner_b)) => {
                self.unify(inner_a, inner_b)
            }
            (TyReprKind::Ptr(inner_a), TyReprKind::Ptr(inner_b)) => self.unify(inner_a, inner_b),
            (
                TyReprKind::Array {
                    elem: elem_a,
                    size: size_a,
                },
                TyReprKind::Array {
                    elem: elem_b,
                    size: size_b,
                },
            ) => {
                if size_a != size_b {
                    return Err(SemaErrorKind::TypeMismatch {
                        from: format!("{a}"),
                        to: format!("{b}"),
                    });
                }
                self.unify(elem_a, elem_b)
            }
            (TyReprKind::Tuple(elems_a), TyReprKind::Tuple(elems_b)) => {
                if elems_a.len() != elems_b.len() {
                    return Err(SemaErrorKind::TypeMismatch {
                        from: format!("{a}"),
                        to: format!("{b}"),
                    });
                }
                for (ea, eb) in elems_a.iter().zip(elems_b.iter()) {
                    self.unify(ea, eb)?;
                }
                Ok(())
            }
            (
                TyReprKind::Fn {
                    params: params_a,
                    ret: ret_a,
                },
                TyReprKind::Fn {
                    params: params_b,
                    ret: ret_b,
                },
            ) => {
                if params_a.len() != params_b.len() {
                    return Err(SemaErrorKind::ArityMismatch {
                        expected: params_a.len(),
                        got: params_b.len(),
                    });
                }
                for (pa, pb) in params_a.iter().zip(params_b.iter()) {
                    self.unify(pa, pb)?;
                }
                self.unify(ret_a, ret_b)
            }
            (
                TyReprKind::Named {
                    symbol: sym_a,
                    args: args_a,
                },
                TyReprKind::Named {
                    symbol: sym_b,
                    args: args_b,
                },
            ) => {
                if sym_a != sym_b || args_a.len() != args_b.len() {
                    return Err(SemaErrorKind::TypeMismatch {
                        from: format!("{a}"),
                        to: format!("{b}"),
                    });
                }
                for (aa, ab) in args_a.iter().zip(args_b.iter()) {
                    self.unify(aa, ab)?;
                }
                Ok(())
            }
            _ => Err(SemaErrorKind::TypeMismatch {
                from: format!("{a}"),
                to: format!("{b}"),
            }),
        }
    }

    #[must_use]
    pub fn apply(&self, ty: &TyRepr) -> TyRepr {
        match &ty.kind {
            TyReprKind::Var(id) => self
                .substitutions
                .get(id)
                .map_or_else(|| ty.clone(), |resolved| self.apply(resolved)),
            TyReprKind::Optional(inner) => TyRepr::optional(self.apply(inner)),
            TyReprKind::Ptr(inner) => TyRepr::ptr(self.apply(inner)),
            TyReprKind::Array { elem, size } => TyRepr::array(self.apply(elem), *size),
            TyReprKind::Tuple(elems) => {
                TyRepr::tuple(elems.iter().map(|e| self.apply(e)).collect())
            }
            TyReprKind::Fn { params, ret } => TyRepr::func(
                params.iter().map(|p| self.apply(p)).collect(),
                self.apply(ret),
            ),
            TyReprKind::Named { symbol, args } => TyRepr::new(TyReprKind::Named {
                symbol: *symbol,
                args: args.iter().map(|a| self.apply(a)).collect(),
            }),
            _ => ty.clone(),
        }
    }

    #[must_use]
    pub fn finalize(&self, ty: &TyRepr) -> TyRepr {
        let applied = self.apply(ty);
        finalize_inner(&applied)
    }

    fn bind(&mut self, id: TyVarId, ty: TyRepr) {
        let _ = self.substitutions.insert(id, ty);
    }
}

fn finalize_inner(ty: &TyRepr) -> TyRepr {
    match &ty.kind {
        TyReprKind::Var(_) => TyRepr::error(),
        TyReprKind::Optional(inner) => TyRepr::optional(finalize_inner(inner)),
        TyReprKind::Ptr(inner) => TyRepr::ptr(finalize_inner(inner)),
        TyReprKind::Array { elem, size } => TyRepr::array(finalize_inner(elem), *size),
        TyReprKind::Tuple(elems) => TyRepr::tuple(elems.iter().map(finalize_inner).collect()),
        TyReprKind::Fn { params, ret } => TyRepr::func(
            params.iter().map(finalize_inner).collect(),
            finalize_inner(ret),
        ),
        TyReprKind::Named { symbol, args } => TyRepr::new(TyReprKind::Named {
            symbol: *symbol,
            args: args.iter().map(finalize_inner).collect(),
        }),
        _ => ty.clone(),
    }
}
