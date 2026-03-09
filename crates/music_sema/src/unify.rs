//! Unification table for type inference.

#[cfg(test)]
mod tests;

use music_shared::{Arena, Idx, Span};

use crate::types::{RecordField, SumVariant, TyVarId, Type};
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
    binding: Option<Idx<Type>>,
    _origin: Span,
}

/// Unification table: maps type variable ids to their bindings.
pub struct UnifyTable {
    vars: Vec<TyVarEntry>,
}

impl UnifyTable {
    /// Creates an empty unification table.
    #[must_use]
    pub const fn new() -> Self {
        Self { vars: Vec::new() }
    }

    /// Allocates a fresh unification variable.
    #[must_use]
    pub fn fresh(&mut self, span: Span, arena: &mut Arena<Type>) -> Idx<Type> {
        let id = self.alloc_var(TyVarKind::Unification, span);
        arena.alloc(Type::Var(id))
    }

    /// Allocates a fresh rigid (skolem) variable.
    #[must_use]
    pub fn fresh_rigid(&mut self, span: Span, arena: &mut Arena<Type>) -> (TyVarId, Idx<Type>) {
        let id = self.alloc_var(TyVarKind::Rigid, span);
        let idx = arena.alloc(Type::Rigid(id));
        (id, idx)
    }

    /// Returns the [`TyVarId`] for a freshly allocated unification variable.
    #[must_use]
    pub fn fresh_var_id(&mut self, span: Span) -> TyVarId {
        self.alloc_var(TyVarKind::Unification, span)
    }

    /// Returns `true` if `var` has been bound to a type.
    ///
    /// # Panics
    ///
    /// Panics if `var` is out of range.
    #[must_use]
    pub fn is_bound(&self, var: TyVarId) -> bool {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        self.vars[idx].binding.is_some()
    }

    /// Returns the binding for `var`, if any.
    ///
    /// # Panics
    ///
    /// Panics if `var` is out of range.
    #[must_use]
    pub fn probe(&self, var: TyVarId) -> Option<Idx<Type>> {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        self.vars[idx].binding
    }

    /// Binds `var` to `ty`.
    ///
    /// # Panics
    ///
    /// Panics if `var` is already bound.
    pub fn bind(&mut self, var: TyVarId, ty: Idx<Type>) {
        let idx = usize::try_from(var.0).expect("TyVarId in range");
        assert!(self.vars[idx].binding.is_none(), "variable already bound");
        self.vars[idx].binding = Some(ty);
    }

    /// Resolves a type through any chain of bindings.
    ///
    /// If `ty` is `Var(v)` and `v` is bound, follows the binding recursively.
    /// Otherwise returns `ty` unchanged.
    #[must_use]
    pub fn resolve(&self, ty: Idx<Type>, arena: &Arena<Type>) -> Idx<Type> {
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
    pub fn occurs(&self, var: TyVarId, ty: Idx<Type>, arena: &Arena<Type>) -> bool {
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
            Type::Tuple { elems } => elems.iter().any(|&e| self.occurs(var, e, arena)),
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

    /// Unifies two types, returning `true` on success.
    ///
    /// On failure, returns `false` and the caller should report a diagnostic.
    #[allow(clippy::too_many_lines)]
    pub fn unify(
        &mut self,
        a: Idx<Type>,
        b: Idx<Type>,
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        let a = self.resolve(a, arena);
        let b = self.resolve(b, arena);

        if a == b {
            return true;
        }

        match (&arena[a], &arena[b]) {
            // Error absorbs everything
            (Type::Error, _) | (_, Type::Error) => true,

            // Unification variable: bind (with occurs check)
            (Type::Var(v), _) => {
                let v = *v;
                if self.occurs(v, b, arena) {
                    return false;
                }
                self.bind(v, b);
                true
            }
            (_, Type::Var(v)) => {
                let v = *v;
                if self.occurs(v, a, arena) {
                    return false;
                }
                self.bind(v, a);
                true
            }

            // Rigid variable: only unifies with itself (already handled by a == b)
            (Type::Rigid(_), _) | (_, Type::Rigid(_)) => false,

            // Any (top) unifies with anything
            (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => true,

            // Never (bottom) unifies with anything
            (Type::Named { def, args }, _) if *def == well_known.never && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.never && args.is_empty() => true,

            // Named: same DefId, recursively unify args
            (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
                if d1 != d2 || a1.len() != a2.len() {
                    return false;
                }
                let pairs: Vec<_> = a1.iter().copied().zip(a2.iter().copied()).collect();
                pairs
                    .iter()
                    .all(|&(x, y)| self.unify(x, y, arena, well_known))
            }

            // Fn: same arity, unify params + ret
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
                if p1.len() != p2.len() {
                    return false;
                }
                let r1 = *r1;
                let r2 = *r2;
                let pairs: Vec<_> = p1.iter().copied().zip(p2.iter().copied()).collect();
                pairs
                    .iter()
                    .all(|&(x, y)| self.unify(x, y, arena, well_known))
                    && self.unify(r1, r2, arena, well_known)
            }

            // Tuple: same length
            (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => {
                if e1.len() != e2.len() {
                    return false;
                }
                let pairs: Vec<_> = e1.iter().copied().zip(e2.iter().copied()).collect();
                pairs
                    .iter()
                    .all(|&(x, y)| self.unify(x, y, arena, well_known))
            }

            // Record: structural match by field name
            (
                Type::Record {
                    fields: f1,
                    open: o1,
                },
                Type::Record {
                    fields: f2,
                    open: o2,
                },
            ) => {
                if !o1 && !o2 && f1.len() != f2.len() {
                    return false;
                }
                let f1: Vec<RecordField> = f1.clone();
                let f2: Vec<RecordField> = f2.clone();
                f1.iter().all(|field1| {
                    f2.iter()
                        .find(|field2| field2.name == field1.name)
                        .is_some_and(|field2| self.unify(field1.ty, field2.ty, arena, well_known))
                })
            }

            // Array: unify element type
            (Type::Array { elem: e1, len: l1 }, Type::Array { elem: e2, len: l2 }) => {
                if l1 != l2 {
                    return false;
                }
                let (e1, e2) = (*e1, *e2);
                self.unify(e1, e2, arena, well_known)
            }

            // Ref: unify inner
            (Type::Ref { inner: i1 }, Type::Ref { inner: i2 }) => {
                let (i1, i2) = (*i1, *i2);
                self.unify(i1, i2, arena, well_known)
            }

            // Sum: structural match by variant name
            (Type::Sum { variants: v1 }, Type::Sum { variants: v2 }) => {
                if v1.len() != v2.len() {
                    return false;
                }
                let v1: Vec<SumVariant> = v1.clone();
                let v2: Vec<SumVariant> = v2.clone();
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

            _ => false,
        }
    }

    /// Recursively replaces all solved `Var` with their bindings.
    ///
    /// Unsolved `Var` → `Error` (erasure). Used before exporting types.
    #[must_use]
    pub fn freeze(&self, ty: Idx<Type>, arena: &mut Arena<Type>) -> Idx<Type> {
        match arena[ty].clone() {
            Type::Var(v) => {
                if let Some(bound) = self.probe(v) {
                    self.freeze(bound, arena)
                } else {
                    arena.alloc(Type::Error)
                }
            }
            Type::Named { def, args } => {
                let args: Vec<_> = args.iter().map(|&a| self.freeze(a, arena)).collect();
                arena.alloc(Type::Named { def, args })
            }
            Type::Fn {
                params,
                ret,
                effects,
            } => {
                let params: Vec<_> = params.iter().map(|&p| self.freeze(p, arena)).collect();
                let ret = self.freeze(ret, arena);
                arena.alloc(Type::Fn {
                    params,
                    ret,
                    effects,
                })
            }
            Type::Tuple { elems } => {
                let elems: Vec<_> = elems.iter().map(|&e| self.freeze(e, arena)).collect();
                arena.alloc(Type::Tuple { elems })
            }
            Type::Record { fields, open } => {
                let fields = fields
                    .iter()
                    .map(|f| RecordField {
                        name: f.name,
                        ty: self.freeze(f.ty, arena),
                    })
                    .collect();
                arena.alloc(Type::Record { fields, open })
            }
            Type::Array { elem, len } => {
                let elem = self.freeze(elem, arena);
                arena.alloc(Type::Array { elem, len })
            }
            Type::Ref { inner } => {
                let inner = self.freeze(inner, arena);
                arena.alloc(Type::Ref { inner })
            }
            Type::Quantified {
                kind,
                params,
                constraints,
                body,
            } => {
                let body = self.freeze(body, arena);
                arena.alloc(Type::Quantified {
                    kind,
                    params,
                    constraints,
                    body,
                })
            }
            Type::Sum { variants } => {
                let variants = variants
                    .iter()
                    .map(|v| SumVariant {
                        name: v.name,
                        fields: v.fields.iter().map(|&f| self.freeze(f, arena)).collect(),
                    })
                    .collect();
                arena.alloc(Type::Sum { variants })
            }
            Type::Rigid(_) | Type::Error => ty,
        }
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

impl Default for UnifyTable {
    fn default() -> Self {
        Self::new()
    }
}
