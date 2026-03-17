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
            Type::Record { fields, rest } => {
                fields.iter().any(|f| self.occurs(var, f.ty, arena))
                    || rest.is_some_and(|r| self.occurs(var, r, arena))
            }
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

            // Any/Never checked before Var to prevent variable erasure.
            (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => true,

            (Type::Named { def, args }, _) if *def == well_known.never && args.is_empty() => true,
            (_, Type::Named { def, args }) if *def == well_known.never && args.is_empty() => true,

            (Type::Var(v), _) => {
                let v = *v;
                let b = self.freshen_any(b, arena, well_known.any);
                self.try_bind_var(v, b, arena)
            }
            (_, Type::Var(v)) => {
                let v = *v;
                let a = self.freshen_any(a, arena, well_known.any);
                self.try_bind_var(v, a, arena)
            }

            // Rigid type variables are intentionally non-unifiable with anything.
            #[allow(clippy::match_same_arms)]
            (Type::Rigid(_), _) | (_, Type::Rigid(_)) => false,

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
                    rest: r1,
                },
                Type::Record {
                    fields: f2,
                    rest: r2,
                },
            ) => {
                let (f1, r1, f2, r2) = (f1.clone(), *r1, f2.clone(), *r2);
                self.unify_record(&f1, r1, &f2, r2, arena, well_known)
            }

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

    /// Replaces `Any` positions in `ty` with fresh unification variables.
    /// Returns the original type unchanged if it contains no `Any`.
    fn freshen_any(
        &mut self,
        ty: TypeIdx,
        arena: &mut Arena<Type>,
        any_def: crate::DefId,
    ) -> TypeIdx {
        match arena[ty].clone() {
            Type::Named { def, args } if def == any_def && args.is_empty() => {
                self.fresh(Span::DUMMY, arena)
            }
            Type::Array { elem, len } => {
                let new_elem = self.freshen_any(elem, arena, any_def);
                if new_elem == elem {
                    ty
                } else {
                    arena.alloc(Type::Array {
                        elem: new_elem,
                        len,
                    })
                }
            }
            Type::Fn {
                params,
                ret,
                effects,
            } => {
                let new_params: Vec<_> = params
                    .iter()
                    .map(|&p| self.freshen_any(p, arena, any_def))
                    .collect();
                let new_ret = self.freshen_any(ret, arena, any_def);
                if new_params == params && new_ret == ret {
                    ty
                } else {
                    arena.alloc(Type::Fn {
                        params: new_params,
                        ret: new_ret,
                        effects,
                    })
                }
            }
            Type::Tuple { elems } => {
                let new_elems: Vec<_> = elems
                    .iter()
                    .map(|&e| self.freshen_any(e, arena, any_def))
                    .collect();
                if new_elems == elems {
                    ty
                } else {
                    arena.alloc(Type::Tuple { elems: new_elems })
                }
            }
            Type::Record { fields, rest } => {
                let new_fields: Vec<_> = fields
                    .iter()
                    .map(|f| RecordField {
                        name: f.name,
                        ty: self.freshen_any(f.ty, arena, any_def),
                        ty_params: f.ty_params.clone(),
                    })
                    .collect();
                let new_rest = rest.map(|r| self.freshen_any(r, arena, any_def));
                if new_fields
                    .iter()
                    .zip(fields.iter())
                    .all(|(a, b)| a.ty == b.ty)
                    && new_rest == rest
                {
                    ty
                } else {
                    arena.alloc(Type::Record {
                        fields: new_fields,
                        rest: new_rest,
                    })
                }
            }
            _ => ty,
        }
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
        rest1: Option<TypeIdx>,
        f2: &[RecordField],
        rest2: Option<TypeIdx>,
        arena: &mut Arena<Type>,
        well_known: &WellKnown,
    ) -> bool {
        // Partition into common fields, only-in-1, only-in-2.
        let mut common: Vec<(TypeIdx, TypeIdx)> = vec![];
        let mut only1: Vec<RecordField> = vec![];
        let mut only2: Vec<RecordField> = vec![];

        for field in f1 {
            if let Some(f2_field) = f2.iter().find(|f| f.name == field.name) {
                common.push((field.ty, f2_field.ty));
            } else {
                only1.push(field.clone());
            }
        }
        for field in f2 {
            if !f1.iter().any(|f| f.name == field.name) {
                only2.push(field.clone());
            }
        }

        // Unify common field types.
        for (t1, t2) in common {
            if !self.unify(t1, t2, arena, well_known) {
                return false;
            }
        }

        // Handle excess fields.
        match (only1.is_empty(), only2.is_empty(), rest1, rest2) {
            // No excess on either side.
            (true, true, None, None) => true,
            (true, true, Some(r1), None) => {
                let empty = arena.alloc(Type::Record {
                    fields: vec![],
                    rest: None,
                });
                self.unify(r1, empty, arena, well_known)
            }
            (true, true, None, Some(r2)) => {
                let empty = arena.alloc(Type::Record {
                    fields: vec![],
                    rest: None,
                });
                self.unify(r2, empty, arena, well_known)
            }
            (true, true, Some(r1), Some(r2)) => self.unify(r1, r2, arena, well_known),

            // Excess only in f1, f2 is closed - fail; or excess only in f2, f1 is closed - fail.
            (false, _, _, None) | (_, false, None, _) => false,

            // Excess only in f1, rest2 is open - push only1 into rest2.
            (false, true, _, Some(r2)) => {
                let new_rec = arena.alloc(Type::Record {
                    fields: only1,
                    rest: rest1,
                });
                self.unify(r2, new_rec, arena, well_known)
            }

            // Excess only in f2, rest1 is open - push only2 into rest1.
            (true, false, Some(r1), _) => {
                let new_rec = arena.alloc(Type::Record {
                    fields: only2,
                    rest: rest2,
                });
                self.unify(r1, new_rec, arena, well_known)
            }

            // Excess on both sides, both open - introduce shared tail variable.
            (false, false, Some(r1), Some(r2)) => {
                let rho = self.fresh(Span::DUMMY, arena);
                let rec1 = arena.alloc(Type::Record {
                    fields: only2,
                    rest: Some(rho),
                });
                let rec2 = arena.alloc(Type::Record {
                    fields: only1,
                    rest: Some(rho),
                });
                self.unify(r1, rec1, arena, well_known) && self.unify(r2, rec2, arena, well_known)
            }
        }
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
            Type::Record { fields, rest } => self.freeze_record(&fields, rest, arena, any_def),
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

    /// Freezes a `Record` type by flattening its row chain into a closed, sorted record.
    fn freeze_record(
        &self,
        fields: &[RecordField],
        rest: Option<TypeIdx>,
        arena: &mut Arena<Type>,
        any_def: DefId,
    ) -> TypeIdx {
        // Collect all fields from this record and any linked rest records.
        let mut all_fields: Vec<RecordField> = fields
            .iter()
            .map(|f| RecordField {
                name: f.name,
                ty: self.freeze(f.ty, arena, any_def),
                ty_params: f.ty_params.clone(),
            })
            .collect();
        // Walk the rest chain.
        let mut cur_rest = rest;
        while let Some(rest_idx) = cur_rest {
            let resolved = self.resolve(rest_idx, arena);
            match arena[resolved].clone() {
                Type::Record {
                    fields: rf,
                    rest: rr,
                } => {
                    for f in &rf {
                        if !all_fields.iter().any(|ef| ef.name == f.name) {
                            all_fields.push(RecordField {
                                name: f.name,
                                ty: self.freeze(f.ty, arena, any_def),
                                ty_params: f.ty_params.clone(),
                            });
                        }
                    }
                    cur_rest = rr;
                }
                // Unsolved row variable - close the record.
                _ => break,
            }
        }
        // Sort by name index for canonical ordering. Full string sort
        // requires interner access which freeze doesn't have; sort by
        // Symbol (interning order) as a stable tie-breaker. The
        // canonical sort-by-string is enforced at construction sites.
        all_fields.sort_by_key(|f| f.name.0);
        arena.alloc(Type::Record {
            fields: all_fields,
            rest: None,
        })
    }
}

impl Default for UnifyTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Structural type equality after resolving unification variables.
///
/// Unlike raw `TypeIdx` comparison, this follows `Named { def, args }` structure
/// so that two independently-allocated slots for the same type match.
#[must_use]
pub fn types_match(types: &Arena<Type>, unify: &UnifyTable, a: TypeIdx, b: TypeIdx) -> bool {
    let a = unify.resolve(a, types);
    let b = unify.resolve(b, types);
    if a == b {
        return true;
    }
    match (&types[a], &types[b]) {
        (Type::Named { def: da, args: aa }, Type::Named { def: db, args: ab }) => {
            da == db
                && aa.len() == ab.len()
                && aa
                    .iter()
                    .zip(ab)
                    .all(|(x, y)| types_match(types, unify, *x, *y))
        }
        _ => false,
    }
}
