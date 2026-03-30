use std::collections::HashMap;

use music_names::{NameBindingId, Symbol};

use super::{EffectRow, SemTy, SemTyId, SemTys};

#[derive(Debug, Clone)]
pub struct ValueScheme {
    pub generic_count: u32,
    pub ty: SemTyId,
    pub declared_effects: Option<EffectRow>,
}

#[derive(Debug, Clone)]
pub struct EffectOpSig {
    pub params: Box<[SemTyId]>,
    pub ret: SemTyId,
}

#[derive(Debug, Default)]
pub struct TypeEnv {
    values: HashMap<NameBindingId, ValueScheme>,
    effects: HashMap<NameBindingId, HashMap<Symbol, EffectOpSig>>,
}

impl TypeEnv {
    #[must_use]
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            effects: HashMap::new(),
        }
    }

    pub fn insert_value(&mut self, binding: NameBindingId, scheme: ValueScheme) {
        let _prev = self.values.insert(binding, scheme);
    }

    #[must_use]
    pub fn get_value(&self, binding: NameBindingId) -> Option<&ValueScheme> {
        self.values.get(&binding)
    }

    pub fn insert_effect_ops(&mut self, binding: NameBindingId, ops: HashMap<Symbol, EffectOpSig>) {
        let _prev = self.effects.insert(binding, ops);
    }

    #[must_use]
    pub fn get_effect_ops(&self, binding: NameBindingId) -> Option<&HashMap<Symbol, EffectOpSig>> {
        self.effects.get(&binding)
    }

    pub fn instantiate(&self, tys: &mut SemTys, scheme: &ValueScheme, span: music_basic::Span) -> SemTyId {
        if scheme.generic_count == 0 {
            return scheme.ty;
        }
        let mut subst: Vec<SemTyId> = Vec::with_capacity(scheme.generic_count as usize);
        for _ in 0..scheme.generic_count {
            subst.push(tys.fresh_infer_var(span));
        }
        instantiate_ty(tys, scheme.ty, &subst)
    }
}

fn instantiate_ty(tys: &mut SemTys, ty: SemTyId, subst: &[SemTyId]) -> SemTyId {
    let ty = super::unify::resolve(tys, ty);
    match tys.get(ty).clone() {
        SemTy::Error | SemTy::Unknown | SemTy::Any | SemTy::InferVar(_) => ty,
        SemTy::Generic(i) => subst.get(i as usize).copied().unwrap_or(ty),
        SemTy::Named { name, args } => {
            let new_args: Vec<_> = args.iter().copied().map(|a| instantiate_ty(tys, a, subst)).collect();
            tys.alloc(SemTy::Named {
                name,
                args: new_args.into_boxed_slice(),
            })
        }
        SemTy::Tuple { items } => {
            let new_items: Vec<_> = items.iter().copied().map(|a| instantiate_ty(tys, a, subst)).collect();
            tys.alloc(SemTy::Tuple {
                items: new_items.into_boxed_slice(),
            })
        }
        SemTy::Array { dims, elem } => {
            let elem = instantiate_ty(tys, elem, subst);
            tys.alloc(SemTy::Array { dims, elem })
        }
        SemTy::Arrow { flavor, input, output } => {
            let input = instantiate_ty(tys, input, subst);
            let output = instantiate_ty(tys, output, subst);
            tys.alloc(SemTy::Arrow { flavor, input, output })
        }
        SemTy::Binary { op, left, right } => {
            let left = instantiate_ty(tys, left, subst);
            let right = instantiate_ty(tys, right, subst);
            tys.alloc(SemTy::Binary { op, left, right })
        }
        SemTy::Mut { base } => {
            let base = instantiate_ty(tys, base, subst);
            tys.alloc(SemTy::Mut { base })
        }
    }
}

