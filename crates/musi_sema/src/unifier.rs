use std::collections::HashMap;

use crate::SymbolId;
use crate::error::SemaErrorKind;
use crate::ty_repr::{TyParamId, TyRepr, TyReprKind, TyVarId};

#[derive(Debug)]
pub struct Unifier {
    substs: HashMap<TyVarId, TyRepr>,
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
            substs: HashMap::new(),
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
            (TyReprKind::Optional(inner_a), TyReprKind::Optional(inner_b))
            | (TyReprKind::Ptr(inner_a), TyReprKind::Ptr(inner_b)) => self.unify(inner_a, inner_b),
            (TyReprKind::Array(elem_a, size_a), TyReprKind::Array(elem_b, size_b)) => {
                self.unify_ty_array(&a, &b, elem_a, elem_b, *size_a, *size_b)
            }
            (TyReprKind::Tuple(elems_a), TyReprKind::Tuple(elems_b)) => {
                self.unify_slices(&a, &b, elems_a, elems_b)
            }
            (TyReprKind::Fn(params_a, ret_a), TyReprKind::Fn(params_b, ret_b)) => {
                self.unify_ty_fn(params_a, params_b, ret_a, ret_b)
            }
            (TyReprKind::Named(sym_a, args_a), TyReprKind::Named(sym_b, args_b)) => {
                self.unify_ty_named(&a, &b, *sym_a, *sym_b, args_a, args_b)
            }
            _ => Err(type_mismatch(&a, &b)),
        }
    }

    #[must_use]
    pub fn apply(&self, ty: &TyRepr) -> TyRepr {
        self.transform(ty, &|s, id| {
            s.substs
                .get(&id)
                .map_or_else(|| TyRepr::var(id), |resolved| s.apply(resolved))
        })
    }

    #[must_use]
    pub fn finalize(&self, ty: &TyRepr) -> TyRepr {
        self.transform(ty, &|s, id| {
            s.substs
                .get(&id)
                .map_or(TyRepr::any(), |resolved| s.finalize(resolved))
        })
    }

    #[must_use]
    /// Generalize type by replacing free type variables with type parameters.
    ///
    /// # Panics
    ///
    /// Panics if there are too many type parameters.
    pub fn generalize(&self, ty: &TyRepr) -> TyRepr {
        let applied = self.apply(ty);
        let free_vars = collect_free_vars(&applied);
        if free_vars.is_empty() {
            return applied;
        }
        let param_ids: Vec<_> = free_vars
            .iter()
            .enumerate()
            .map(|(i, _)| TyParamId::new(u32::try_from(i).expect("too many type parameters")))
            .collect();
        let substituted = substitute_vars_to_params(&applied, &free_vars, &param_ids);
        TyRepr::poly(param_ids, substituted)
    }

    fn unify_ty_array(
        &mut self,
        a: &TyRepr,
        b: &TyRepr,
        elem_a: &TyRepr,
        elem_b: &TyRepr,
        size_a: Option<usize>,
        size_b: Option<usize>,
    ) -> Result<(), SemaErrorKind> {
        if size_a != size_b {
            return Err(type_mismatch(a, b));
        }
        self.unify(elem_a, elem_b)
    }

    fn unify_slices(
        &mut self,
        a: &TyRepr,
        b: &TyRepr,
        elems_a: &[TyRepr],
        elems_b: &[TyRepr],
    ) -> Result<(), SemaErrorKind> {
        if elems_a.len() != elems_b.len() {
            return Err(type_mismatch(a, b));
        }
        for (ea, eb) in elems_a.iter().zip(elems_b.iter()) {
            self.unify(ea, eb)?;
        }
        Ok(())
    }

    fn unify_ty_fn(
        &mut self,
        params_a: &[TyRepr],
        params_b: &[TyRepr],
        ret_a: &TyRepr,
        ret_b: &TyRepr,
    ) -> Result<(), SemaErrorKind> {
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

    fn unify_ty_named(
        &mut self,
        a: &TyRepr,
        b: &TyRepr,
        sym_a: SymbolId,
        sym_b: SymbolId,
        args_a: &[TyRepr],
        args_b: &[TyRepr],
    ) -> Result<(), SemaErrorKind> {
        if sym_a != sym_b || args_a.len() != args_b.len() {
            return Err(type_mismatch(a, b));
        }
        for (aa, ab) in args_a.iter().zip(args_b.iter()) {
            self.unify(aa, ab)?;
        }
        Ok(())
    }

    fn transform<F>(&self, ty: &TyRepr, f: &F) -> TyRepr
    where
        F: Fn(&Self, TyVarId) -> TyRepr,
    {
        match &ty.kind {
            TyReprKind::Var(id) => f(self, *id),
            TyReprKind::Optional(inner) => TyRepr::optional(self.transform(inner, f)),
            TyReprKind::Ptr(inner) => TyRepr::ptr(self.transform(inner, f)),
            TyReprKind::Array(elem, size) => TyRepr::array(self.transform(elem, f), *size),
            TyReprKind::Tuple(elems) => {
                TyRepr::tuple(elems.iter().map(|e| self.transform(e, f)).collect())
            }
            TyReprKind::Fn(params, ret) => TyRepr::func(
                params.iter().map(|p| self.transform(p, f)).collect(),
                self.transform(ret, f),
            ),
            TyReprKind::Named(symbol, args) => {
                TyRepr::named(*symbol, args.iter().map(|a| self.transform(a, f)).collect())
            }
            TyReprKind::Poly { params, body } => {
                TyRepr::poly(params.clone(), self.transform(body, f))
            }
            _ => ty.clone(),
        }
    }

    fn bind(&mut self, id: TyVarId, ty: TyRepr) {
        let _ = self.substs.insert(id, ty);
    }
}

fn type_mismatch(a: &TyRepr, b: &TyRepr) -> SemaErrorKind {
    SemaErrorKind::TypeMismatch {
        from: format!("{a}"),
        to: format!("{b}"),
    }
}

fn collect_free_vars(ty: &TyRepr) -> Vec<TyVarId> {
    let mut vars = Vec::new();
    collect_free_vars_rec(ty, &mut vars);
    vars
}

fn collect_free_vars_rec(ty: &TyRepr, vars: &mut Vec<TyVarId>) {
    match &ty.kind {
        TyReprKind::Var(id) if !vars.contains(id) => vars.push(*id),
        TyReprKind::Optional(inner) | TyReprKind::Ptr(inner) | TyReprKind::Array(inner, _) => {
            collect_free_vars_rec(inner, vars);
        }
        TyReprKind::Tuple(elems) | TyReprKind::Fn(elems, _) => {
            for e in elems {
                collect_free_vars_rec(e, vars);
            }
            if let TyReprKind::Fn(_, ret) = &ty.kind {
                collect_free_vars_rec(ret, vars);
            }
        }
        TyReprKind::Named(_, args) => {
            for a in args {
                collect_free_vars_rec(a, vars);
            }
        }
        TyReprKind::Poly { body, .. } => collect_free_vars_rec(body, vars),
        _ => {}
    }
}

fn substitute_vars_to_params(ty: &TyRepr, vars: &[TyVarId], params: &[TyParamId]) -> TyRepr {
    match &ty.kind {
        TyReprKind::Var(id) => vars
            .iter()
            .position(|v| v == id)
            .map_or_else(|| ty.clone(), |pos| TyRepr::type_param(params[pos])),
        TyReprKind::Optional(inner) => {
            TyRepr::optional(substitute_vars_to_params(inner, vars, params))
        }
        TyReprKind::Ptr(inner) => TyRepr::ptr(substitute_vars_to_params(inner, vars, params)),
        TyReprKind::Array(elem, size) => {
            TyRepr::array(substitute_vars_to_params(elem, vars, params), *size)
        }
        TyReprKind::Tuple(elems) => TyRepr::tuple(
            elems
                .iter()
                .map(|e| substitute_vars_to_params(e, vars, params))
                .collect(),
        ),
        TyReprKind::Fn(fn_params, ret) => TyRepr::func(
            fn_params
                .iter()
                .map(|p| substitute_vars_to_params(p, vars, params))
                .collect(),
            substitute_vars_to_params(ret, vars, params),
        ),
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests;
