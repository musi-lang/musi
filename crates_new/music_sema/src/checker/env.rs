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

#[derive(Debug, Clone)]
pub struct ClassOpSig {
    pub params: Box<[SemTyId]>,
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
    instances: HashMap<NameBindingId, Vec<Box<[SemTyId]>>>,
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

    pub fn insert_instance(&mut self, class: NameBindingId, args: Box<[SemTyId]>) {
        self.instances.entry(class).or_default().push(args);
    }

    pub fn insert_data_def(&mut self, name: Symbol, def: DataDef) {
        let _prev = self.data_defs.insert(name, def);
    }

    #[must_use]
    pub fn get_data_def(&self, name: Symbol) -> Option<&DataDef> {
        self.data_defs.get(&name)
    }

    pub fn instantiate(
        &self,
        tys: &mut SemTys,
        scheme: &ValueScheme,
        span: music_basic::Span,
    ) -> SemTyId {
        if scheme.generic_count == 0 {
            return scheme.ty;
        }
        let mut subst: Vec<SemTyId> = Vec::with_capacity(scheme.generic_count as usize);
        for _ in 0..scheme.generic_count {
            subst.push(tys.fresh_infer_var(span));
        }
        substitute_generics(tys, scheme.ty, &subst)
    }
}

pub(super) fn substitute_generics(tys: &mut SemTys, ty: SemTyId, subst: &[SemTyId]) -> SemTyId {
    let ty = super::unify::resolve(tys, ty);
    match tys.get(ty).clone() {
        SemTy::Error | SemTy::Unknown | SemTy::Any | SemTy::InferVar(_) => ty,
        SemTy::Generic(i) => subst.get(i as usize).copied().unwrap_or(ty),
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
            let mut new_fields = std::collections::BTreeMap::new();
            for (k, v) in fields {
                let v = substitute_generics(tys, v, subst);
                let _prev = new_fields.insert(k, v);
            }
            tys.alloc(SemTy::Record { fields: new_fields })
        }
    }
}
