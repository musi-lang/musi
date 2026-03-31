use std::collections::{BTreeMap, BTreeSet, HashMap};

use music_basic::Span;
use music_names::{NameBindingId, Symbol};

use crate::{
    ty::{InferVarId, SemTyIds},
    unify,
};

use super::{EffectKey, EffectRow, SemTy, SemTyId, SemTys};

#[derive(Debug, Clone)]
pub struct ValueScheme {
    pub generic_count: u32,
    pub ty: SemTyId,
    pub declared_effects: Option<EffectRow>,
    pub constraints: Box<[SemConstraint]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemTyNamed {
    pub name: Symbol,
    pub args: SemTyIds,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemConstraint {
    Subtype {
        name: Symbol,
        idx: u32,
        bound: SemTyNamed,
    },
    Implements {
        name: Symbol,
        idx: u32,
        class: SemTyNamed,
    },
}

#[derive(Debug, Clone)]
pub struct EffectOpSig {
    pub params: SemTyIds,
    pub ret: SemTyId,
}

#[derive(Debug, Clone)]
pub struct ClassOpSig {
    pub params: SemTyIds,
    pub ret: SemTyId,
}

#[derive(Debug, Clone)]
pub struct EffectFamily {
    pub generic_count: u32,
    pub ops: HashMap<Symbol, EffectOpSig>,
}

#[derive(Debug, Clone)]
pub struct ClassFamily {
    pub generic_count: u32,
    pub ops: HashMap<Symbol, ClassOpSig>,
}

#[derive(Debug, Clone)]
pub struct InstanceScheme {
    pub generic_count: u32,
    pub args: SemTyIds,
    pub constraints: Box<[SemConstraint]>,
}

#[derive(Debug, Clone)]
pub struct DataDef {
    pub generic_count: u32,
    pub variants: Option<HashMap<Symbol, Option<SemTyId>>>,
    pub fields: Option<HashMap<Symbol, DataFieldDef>>,
}

#[derive(Debug, Clone)]
pub struct DataFieldDef {
    pub ty: SemTyId,
}

#[derive(Debug, Default)]
pub struct TypeEnv {
    values: HashMap<NameBindingId, ValueScheme>,
    effects: HashMap<NameBindingId, EffectFamily>,
    classes: HashMap<NameBindingId, ClassFamily>,
    instances: HashMap<Symbol, Vec<InstanceScheme>>,
    data_defs: HashMap<Symbol, DataDef>,
}

impl TypeEnv {
    #[must_use]
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            effects: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
            data_defs: HashMap::new(),
        }
    }

    pub fn insert_value(&mut self, binding: NameBindingId, scheme: ValueScheme) {
        let _prev = self.values.insert(binding, scheme);
    }

    #[must_use]
    pub fn get_value(&self, binding: NameBindingId) -> Option<&ValueScheme> {
        self.values.get(&binding)
    }

    pub fn insert_effect_family(
        &mut self,
        binding: NameBindingId,
        generic_count: u32,
        ops: HashMap<Symbol, EffectOpSig>,
    ) {
        let _prev = self
            .effects
            .insert(binding, EffectFamily { generic_count, ops });
    }

    #[must_use]
    pub fn get_effect_family(&self, binding: NameBindingId) -> Option<&EffectFamily> {
        self.effects.get(&binding)
    }

    pub fn insert_class_family(
        &mut self,
        binding: NameBindingId,
        generic_count: u32,
        ops: HashMap<Symbol, ClassOpSig>,
    ) {
        let _prev = self
            .classes
            .insert(binding, ClassFamily { generic_count, ops });
    }

    #[must_use]
    pub fn get_class_family(&self, binding: NameBindingId) -> Option<&ClassFamily> {
        self.classes.get(&binding)
    }

    pub fn insert_instance(&mut self, class: Symbol, scheme: InstanceScheme) {
        self.instances.entry(class).or_default().push(scheme);
    }

    pub fn insert_data_def(&mut self, name: Symbol, def: DataDef) {
        let _prev = self.data_defs.insert(name, def);
    }

    #[must_use]
    pub fn get_data_def(&self, name: Symbol) -> Option<&DataDef> {
        self.data_defs.get(&name)
    }

    #[must_use]
    pub fn instances_for_class(&self, class: Symbol) -> Option<&[InstanceScheme]> {
        self.instances.get(&class).map(|v| v.as_slice())
    }
}

impl ValueScheme {
    pub fn instantiate(&self, tys: &mut SemTys, span: Span) -> InstantiatedScheme {
        if self.generic_count == 0 {
            return InstantiatedScheme {
                ty: self.ty,
                declared_effects: self.declared_effects.clone(),
                obligations: self
                    .constraints
                    .iter()
                    .cloned()
                    .map(|c| instantiate_constraint(tys, span, c, &[]))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            };
        }

        let cap = usize::try_from(self.generic_count).unwrap_or(0);
        let mut subst: Vec<SemTyId> = Vec::with_capacity(cap);
        for _ in 0..self.generic_count {
            subst.push(tys.fresh_infer_var(span));
        }

        let ty = substitute_generics(tys, self.ty, &subst);
        let declared_effects = self
            .declared_effects
            .as_ref()
            .map(|row| substitute_effect_row(tys, row, &subst));

        let obligations = self
            .constraints
            .iter()
            .cloned()
            .map(|c| instantiate_constraint(tys, span, c, &subst))
            .collect::<Vec<_>>()
            .into_boxed_slice();

        InstantiatedScheme {
            ty,
            declared_effects,
            obligations,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstantiatedScheme {
    pub ty: SemTyId,
    pub declared_effects: Option<EffectRow>,
    pub obligations: Box<[SemObligation]>,
}

#[derive(Debug, Clone)]
pub enum SemObligationKind {
    Subtype { left: SemTyId, right: SemTyNamed },
    Implements { ty: SemTyId, class: SemTyNamed },
}

#[derive(Debug, Clone)]
pub struct SemObligation {
    pub span: Span,
    pub kind: SemObligationKind,
}

pub(crate) fn instantiate_constraint(
    tys: &mut SemTys,
    span: Span,
    c: SemConstraint,
    subst: &[SemTyId],
) -> SemObligation {
    match c {
        SemConstraint::Subtype { idx, bound, .. } => {
            let left = subst
                .get(usize::try_from(idx).unwrap_or(usize::MAX))
                .copied()
                .unwrap_or_else(|| tys.fresh_infer_var(span));
            let right = SemTyNamed {
                name: bound.name,
                args: bound
                    .args
                    .iter()
                    .copied()
                    .map(|t| substitute_generics(tys, t, subst))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            };
            SemObligation {
                span,
                kind: SemObligationKind::Subtype { left, right },
            }
        }
        SemConstraint::Implements { idx, class, .. } => {
            let ty = subst
                .get(usize::try_from(idx).unwrap_or(usize::MAX))
                .copied()
                .unwrap_or_else(|| tys.fresh_infer_var(span));
            let class = SemTyNamed {
                name: class.name,
                args: class
                    .args
                    .iter()
                    .copied()
                    .map(|t| substitute_generics(tys, t, subst))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            };
            SemObligation {
                span,
                kind: SemObligationKind::Implements { ty, class },
            }
        }
    }
}

fn substitute_effect_row(tys: &mut SemTys, row: &EffectRow, subst: &[SemTyId]) -> EffectRow {
    let mut out = EffectRow::empty();
    out.is_open = row.is_open;
    for EffectKey { name, arg } in row.items.iter().cloned() {
        let arg = arg.map(|t| substitute_generics(tys, t, subst));
        out.add(EffectKey { name, arg });
    }
    out
}

pub fn substitute_generics(tys: &mut SemTys, ty: SemTyId, subst: &[SemTyId]) -> SemTyId {
    let ty = unify::resolve(tys, ty);
    match tys.get(ty).clone() {
        SemTy::Error | SemTy::Unknown | SemTy::Any | SemTy::InferVar(_) => ty,
        SemTy::Generic(i) => subst
            .get(usize::try_from(i).unwrap_or(usize::MAX))
            .copied()
            .unwrap_or(ty),
        SemTy::Named { name, args } => {
            let new_args: Vec<_> = args
                .iter()
                .copied()
                .map(|a| substitute_generics(tys, a, subst))
                .collect();
            tys.alloc(SemTy::Named {
                name,
                args: new_args.into_boxed_slice(),
            })
        }
        SemTy::Tuple { items } => {
            let new_items: Vec<_> = items
                .iter()
                .copied()
                .map(|a| substitute_generics(tys, a, subst))
                .collect();
            tys.alloc(SemTy::Tuple {
                items: new_items.into_boxed_slice(),
            })
        }
        SemTy::Array { dims, elem } => {
            let elem = substitute_generics(tys, elem, subst);
            tys.alloc(SemTy::Array { dims, elem })
        }
        SemTy::Arrow {
            flavor,
            input,
            output,
        } => {
            let input = substitute_generics(tys, input, subst);
            let output = substitute_generics(tys, output, subst);
            tys.alloc(SemTy::Arrow {
                flavor,
                input,
                output,
            })
        }
        SemTy::Binary { op, left, right } => {
            let left = substitute_generics(tys, left, subst);
            let right = substitute_generics(tys, right, subst);
            tys.alloc(SemTy::Binary { op, left, right })
        }
        SemTy::Mut { base } => {
            let base = substitute_generics(tys, base, subst);
            tys.alloc(SemTy::Mut { base })
        }
        SemTy::Record { fields } => {
            let mut new_fields = BTreeMap::new();
            for (k, v) in fields {
                let v = substitute_generics(tys, v, subst);
                let _prev = new_fields.insert(k, v);
            }
            tys.alloc(SemTy::Record { fields: new_fields })
        }
    }
}

pub fn generalize_infer_vars(tys: &mut SemTys, ty: SemTyId, start_index: u32) -> (u32, SemTyId) {
    let ty = unify::resolve(tys, ty);
    let mut vars = BTreeSet::<InferVarId>::new();
    collect_unbound_infer_vars(tys, ty, &mut vars);
    if vars.is_empty() {
        return (0, ty);
    }

    let mut mapping = BTreeMap::<InferVarId, u32>::new();
    for (i, var) in vars.into_iter().enumerate() {
        let idx = start_index + u32::try_from(i).unwrap_or(0);
        let _prev = mapping.insert(var, idx);
    }

    let out = replace_infer_with_generics(tys, ty, &mapping);
    (u32::try_from(mapping.len()).unwrap_or(0), out)
}

fn collect_unbound_infer_vars(tys: &SemTys, ty: SemTyId, out: &mut BTreeSet<InferVarId>) {
    let ty = unify::resolve(tys, ty);
    match tys.get(ty) {
        SemTy::Error | SemTy::Unknown | SemTy::Any | SemTy::Generic(_) => {}
        SemTy::InferVar(var) => {
            if let Some(bound) = tys.infer_binding(*var) {
                collect_unbound_infer_vars(tys, bound, out);
            } else {
                let _did_insert = out.insert(*var);
            }
        }
        SemTy::Named { args, .. } => {
            for a in args.iter().copied() {
                collect_unbound_infer_vars(tys, a, out);
            }
        }
        SemTy::Tuple { items } => {
            for a in items.iter().copied() {
                collect_unbound_infer_vars(tys, a, out);
            }
        }
        SemTy::Array { elem, .. } => collect_unbound_infer_vars(tys, *elem, out),
        SemTy::Arrow { input, output, .. } => {
            collect_unbound_infer_vars(tys, *input, out);
            collect_unbound_infer_vars(tys, *output, out);
        }
        SemTy::Binary { left, right, .. } => {
            collect_unbound_infer_vars(tys, *left, out);
            collect_unbound_infer_vars(tys, *right, out);
        }
        SemTy::Mut { base } => collect_unbound_infer_vars(tys, *base, out),
        SemTy::Record { fields } => {
            for ty in fields.values().copied() {
                collect_unbound_infer_vars(tys, ty, out);
            }
        }
    }
}

fn replace_infer_with_generics(
    tys: &mut SemTys,
    ty: SemTyId,
    mapping: &BTreeMap<InferVarId, u32>,
) -> SemTyId {
    let ty = unify::resolve(tys, ty);
    match tys.get(ty).clone() {
        SemTy::Error | SemTy::Unknown | SemTy::Any | SemTy::Generic(_) => ty,
        SemTy::InferVar(var) => {
            if let Some(bound) = tys.infer_binding(var) {
                return replace_infer_with_generics(tys, bound, mapping);
            }
            if let Some(&idx) = mapping.get(&var) {
                return tys.alloc(SemTy::Generic(idx));
            }
            ty
        }
        SemTy::Named { name, args } => {
            let new_args: Vec<_> = args
                .iter()
                .copied()
                .map(|a| replace_infer_with_generics(tys, a, mapping))
                .collect();
            tys.alloc(SemTy::Named {
                name,
                args: new_args.into_boxed_slice(),
            })
        }
        SemTy::Tuple { items } => {
            let new_items: Vec<_> = items
                .iter()
                .copied()
                .map(|a| replace_infer_with_generics(tys, a, mapping))
                .collect();
            tys.alloc(SemTy::Tuple {
                items: new_items.into_boxed_slice(),
            })
        }
        SemTy::Array { dims, elem } => {
            let elem = replace_infer_with_generics(tys, elem, mapping);
            tys.alloc(SemTy::Array { dims, elem })
        }
        SemTy::Arrow {
            flavor,
            input,
            output,
        } => {
            let input = replace_infer_with_generics(tys, input, mapping);
            let output = replace_infer_with_generics(tys, output, mapping);
            tys.alloc(SemTy::Arrow {
                flavor,
                input,
                output,
            })
        }
        SemTy::Binary { op, left, right } => {
            let left = replace_infer_with_generics(tys, left, mapping);
            let right = replace_infer_with_generics(tys, right, mapping);
            tys.alloc(SemTy::Binary { op, left, right })
        }
        SemTy::Mut { base } => {
            let base = replace_infer_with_generics(tys, base, mapping);
            tys.alloc(SemTy::Mut { base })
        }
        SemTy::Record { fields } => {
            let mut out = BTreeMap::new();
            for (k, v) in fields {
                let v = replace_infer_with_generics(tys, v, mapping);
                let _prev = out.insert(k, v);
            }
            tys.alloc(SemTy::Record { fields: out })
        }
    }
}
