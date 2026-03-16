//! Unification table for type inference.

#[cfg(test)]
mod tests;

use music_shared::{Arena, Span};

use crate::DefId;
use crate::types::{RecordField, SumVariant, TyVarId, Type, TypeIdx};
use crate::well_known::WellKnown;

/// Whether a type variable is solvable (unification) or rigid (skolem).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TyVarKind {
    /// Generated during inference; can be solved by unification.
    Unification,
    /// Introduced by `forall`; can only unify with itself.
    Rigid,
}

/// A single entry in the unification table.
struct TyVarEntry {
    _kind: TyVarKind,
    binding: Option<TypeIdx>,
    _origin: Span,
}

/// Unification table: maps type variable ids to their bindings.
pub struct UnifyTable {
    vars: Vec<TyVarEntry>,
}

impl UnifyTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { vars: vec![] }
    }

    #[must_use]
    pub fn fresh(&mut self, span: Span, arena: &mut Arena<Type>) -> TypeIdx {
        let id = self.alloc_var(TyVarKind::Unification, span);
        arena.alloc(Type::Var(id))
    }

    /// Allocates a fresh rigid (skolem) variable.
    #[must_use]
    pub fn fresh_rigid(&mut self, span: Span, arena: &mut Arena<Type>) -> (TyVarId, TypeIdx) {
        let id = self.alloc_var(TyVarKind::Rigid, span);
        let idx = arena.alloc(Type::Rigid(id));
        (id, idx)
    }

    #[must_use]
    pub fn fresh_var_id(&mut self, span: Span) -> TyVarId {
        self.alloc_var(TyVarKind::Unification, span)
    }

    /// # Panics
    ///
    /// Panics if `var` is out of range.
    #[must_use]
    pub fn is_bound(&self, var: TyVarId) -> bool {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        self.vars[idx].binding.is_some()
    }

    /// # Panics
    ///
    /// Panics if `var` is out of range.
    #[must_use]
    pub fn probe(&self, var: TyVarId) -> Option<TypeIdx> {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        self.vars[idx].binding
    }

    /// # Panics
    ///
    /// Panics if `var` is already bound.
    pub fn bind(&mut self, var: TyVarId, ty: TypeIdx) {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        assert!(self.vars[idx].binding.is_none(), "variable already bound");
        self.vars[idx].binding = Some(ty);
    }

    fn alloc_var(&mut self, kind: TyVarKind, origin: Span) -> TyVarId {
        let id = TyVarId(u32::try_from(self.vars.len()).expect("type variable count overflow"));
        self.vars.push(TyVarEntry {
            _kind: kind,
            binding: None,
            _origin: origin,
        });
        id
    }
}

impl UnifyTable {
    /// Resolves a type through any chain of bindings.
    ///
    /// If `ty` is `Var(v)` and `v` is bound, follows the binding recursively.
    /// Otherwise returns `ty` unchanged.
    #[must_use]
    pub fn resolve(&self, ty: TypeIdx, arena: &Arena<Type>) -> TypeIdx {
        match &arena[ty] {
            Type::Var(v) => self
                .probe(*v)
                .map_or(ty, |bound| self.resolve(bound, arena)),
            _ => ty,
        }
    }

    /// Checks whether `var` occurs anywhere in `ty` (occurs check).
    ///
    /// Returns `true` if `var` is found, meaning binding `var` to `ty`
    /// would create an infinite type.
    #[must_use]
    pub fn occurs(&self, var: TyVarId, ty: TypeIdx, arena: &Arena<Type>) -> bool {
        let ty = self.resolve(ty, arena);
        match &arena[ty] {
            Type::Var(v) | Type::Rigid(v) => *v == var,
            Type::Named { args, .. } => args.iter().any(|&a| self.occurs(var, a, arena)),
            Type::Fn {
                params,
                ret,
                effects: _,
            } => {
                params.iter().any(|&p| self.occurs(var, p, arena)) || self.occurs(var, *ret, arena)
            }
            Type::Tuple { elems } | Type::AnonSum { variants: elems } => {
                elems.iter().any(|&e| self.occurs(var, e, arena))
            }
            Type::Record { fields, .. } => fields.iter().any(|f| self.occurs(var, f.ty, arena)),
            Type::Sum { variants } => variants
                .iter()
                .any(|v| v.fields.iter().any(|&f| self.occurs(var, f, arena))),
            Type::Array { elem, .. } => self.occurs(var, *elem, arena),
            Type::Ref { inner } => self.occurs(var, *inner, arena),
            Type::Quantified { body, .. } => self.occurs(var, *body, arena),
            Type::Error => false,
        }
    }
}

impl UnifyTable {
    /// Unifies two types, returning `true` on success.
    ///
    /// On failure, returns `false` and the caller should report a diagnostic.
    pub fn unify(
        &mut self,
        a: TypeIdx,
        b: TypeIdx,
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        let a = self.resolve(a, arena);
        let b = self.resolve(b, arena);

        if a == b {
            return true;
        }

        match (&arena[a], &arena[b]) {
            (Type::Error, _) | (_, Type::Error) => true,

            (Type::Var(v), _) => self.try_bind_var(*v, b, arena),
            (_, Type::Var(v)) => self.try_bind_var(*v, a, arena),

            (Type::Rigid(_), _) | (_, Type::Rigid(_)) => false,

            (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => true,

            (Type::Named { def, args }, _) if *def == well_known.never && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.never && args.is_empty() => true,

            (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
                if d1 != d2 || a1.len() != a2.len() {
                    return false;
                }
                let pairs: Vec<_> = a1.iter().copied().zip(a2.iter().copied()).collect();
                self.unify_pairwise(&pairs, arena, well_known)
            }

            (
                Type::Fn {
                    params: p1,
                    ret: r1,
                    ..
                },
                Type::Fn {
                    params: p2,
                    ret: r2,
                    ..
                },
            ) => {
                let (p1, r1, p2, r2) = (p1.clone(), *r1, p2.clone(), *r2);
                self.unify_fn(&p1, r1, &p2, r2, arena, well_known)
            }

            (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => {
                if e1.len() != e2.len() {
                    return false;
                }
                let pairs: Vec<_> = e1.iter().copied().zip(e2.iter().copied()).collect();
                self.unify_pairwise(&pairs, arena, well_known)
            }

            (
                Type::Record {
                    fields: f1,
                    open: o1,
                },
                Type::Record {
                    fields: f2,
                    open: o2,
                },
            ) => self.unify_record(&f1.clone(), *o1, &f2.clone(), *o2, arena, well_known),

            (Type::Array { elem: e1, len: l1 }, Type::Array { elem: e2, len: l2 }) => {
                if l1 != l2 {
                    return false;
                }
                let (e1, e2) = (*e1, *e2);
                self.unify(e1, e2, arena, well_known)
            }

            (Type::Ref { inner: i1 }, Type::Ref { inner: i2 }) => {
                let (i1, i2) = (*i1, *i2);
                self.unify(i1, i2, arena, well_known)
            }

            (Type::AnonSum { variants: v1 }, Type::AnonSum { variants: v2 }) => {
                if v1.len() != v2.len() {
                    return false;
                }
                let pairs: Vec<_> = v1.iter().copied().zip(v2.iter().copied()).collect();
                self.unify_pairwise(&pairs, arena, well_known)
            }

            (Type::Sum { variants: v1 }, Type::Sum { variants: v2 }) => {
                self.unify_sum(&v1.clone(), &v2.clone(), arena, well_known)
            }

            _ => false,
        }
    }

    /// Attempts to bind a unification variable to a target type.
    ///
    /// Returns `false` if the occurs check fails (would create infinite type).
    fn try_bind_var(&mut self, var: TyVarId, target: TypeIdx, arena: &Arena<Type>) -> bool {
        if self.occurs(var, target, arena) {
            return false;
        }
        self.bind(var, target);
        true
    }

    fn unify_fn(
        &mut self,
        p1: &[TypeIdx],
        r1: TypeIdx,
        p2: &[TypeIdx],
        r2: TypeIdx,
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        if p1.len() != p2.len() {
            return false;
        }
        let pairs: Vec<_> = p1.iter().copied().zip(p2.iter().copied()).collect();
        self.unify_pairwise(&pairs, arena, well_known) && self.unify(r1, r2, arena, well_known)
    }

    fn unify_pairwise(
        &mut self,
        pairs: &[(TypeIdx, TypeIdx)],
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        pairs
            .iter()
            .all(|&(x, y)| self.unify(x, y, arena, well_known))
    }

    fn unify_record(
        &mut self,
        f1: &[RecordField],
        o1: bool,
        f2: &[RecordField],
        o2: bool,
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        if !o1 && !o2 && f1.len() != f2.len() {
            return false;
        }
        f1.iter().all(|field1| {
            f2.iter()
                .find(|field2| field2.name == field1.name)
                .is_some_and(|field2| self.unify(field1.ty, field2.ty, arena, well_known))
        })
    }

    fn unify_sum(
        &mut self,
        v1: &[SumVariant],
        v2: &[SumVariant],
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        if v1.len() != v2.len() {
            return false;
        }
        v1.iter().all(|var1| {
            v2.iter()
                .find(|var2| var2.name == var1.name)
                .is_some_and(|var2| {
                    var1.fields.len() == var2.fields.len()
                        && var1
                            .fields
                            .iter()
                            .zip(var2.fields.iter())
                            .all(|(&f1, &f2)| self.unify(f1, f2, arena, well_known))
                })
        })
    }
}

impl UnifyTable {
    /// Recursively replaces all solved `Var` with their bindings.
    ///
    /// Unsolved `Var` defaults to `Any` (the top type). Used before exporting types.
    #[must_use]
    pub fn freeze(&self, ty: TypeIdx, arena: &mut Arena<Type>, any_def: DefId) -> TypeIdx {
        match arena[ty].clone() {
            Type::Var(v) => {
                if let Some(bound) = self.probe(v) {
                    self.freeze(bound, arena, any_def)
                } else {
                    arena.alloc(Type::Named {
                        def: any_def,
                        args: vec![],
                    })
                }
            }
            Type::Named { def, args } => {
                let args: Vec<_> = args
                    .iter()
                    .map(|&a| self.freeze(a, arena, any_def))
                    .collect();
                arena.alloc(Type::Named { def, args })
            }
            Type::Fn {
                params,
                ret,
                effects,
            } => {
                let params: Vec<_> = params
                    .iter()
                    .map(|&p| self.freeze(p, arena, any_def))
                    .collect();
                let ret = self.freeze(ret, arena, any_def);
                arena.alloc(Type::Fn {
                    params,
                    ret,
                    effects,
                })
            }
            Type::Tuple { elems } => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|&e| self.freeze(e, arena, any_def))
                    .collect();
                arena.alloc(Type::Tuple { elems })
            }
            Type::Record { fields, open } => {
                let fields = fields
                    .iter()
                    .map(|f| RecordField {
                        name: f.name,
                        ty: self.freeze(f.ty, arena, any_def),
                    })
                    .collect();
                arena.alloc(Type::Record { fields, open })
            }
            Type::Array { elem, len } => {
                let elem = self.freeze(elem, arena, any_def);
                arena.alloc(Type::Array { elem, len })
            }
            Type::Ref { inner } => {
                let inner = self.freeze(inner, arena, any_def);
                arena.alloc(Type::Ref { inner })
            }
            Type::Quantified {
                kind,
                params,
                constraints,
                body,
            } => {
                let body = self.freeze(body, arena, any_def);
                arena.alloc(Type::Quantified {
                    kind,
                    params,
                    constraints,
                    body,
                })
            }
            Type::AnonSum { variants } => {
                let variants: Vec<_> = variants
                    .iter()
                    .map(|&v| self.freeze(v, arena, any_def))
                    .collect();
                arena.alloc(Type::AnonSum { variants })
            }
            Type::Sum { variants } => {
                let variants = variants
                    .iter()
                    .map(|v| SumVariant {
                        name: v.name,
                        fields: v
                            .fields
                            .iter()
                            .map(|&f| self.freeze(f, arena, any_def))
                            .collect(),
                    })
                    .collect();
                arena.alloc(Type::Sum { variants })
            }
            Type::Rigid(_) | Type::Error => ty,
        }
    }
}

impl Default for UnifyTable {
    fn default() -> Self {
        Self::new()
    }
}
