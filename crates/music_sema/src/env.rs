use std::cell::Cell;
use std::collections::{HashMap, HashSet};

use music_arena::Arena;
use music_ast::ExprId;
use music_builtins::types::BuiltinType;
use music_il::opcode::Opcode;
use music_resolve::def::DefId;
use music_shared::{Span, Symbol, SymbolList};

use crate::types::{SemaTypeId, SemaTypeList, Ty, TyVarId};

/// Tracks a registered type class instance for coherence checking.
#[derive(Debug, Clone)]
pub struct InstanceEntry {
    pub span: Span,
    pub methods: SymbolList,
}

/// How a call site should be dispatched at codegen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchInfo {
    /// Resolved to a VM opcode at compile time.
    Static { opcode: Opcode },
    /// Resolved via a type class dictionary at runtime.
    Dictionary { class: Symbol, method_idx: usize },
    /// Fully dynamic dispatch (e.g. through `Any`).
    Dynamic,
}

/// Resolved variant constructor info for codegen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantInfo {
    pub parent_type: SemaTypeId,
    pub tag_index: u16,
    pub arity: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpInfo {
    pub id: u16,
    pub name: Symbol,
    pub param_ty: Option<SemaTypeId>,
    pub ret_ty: SemaTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDefInfo {
    pub id: u16,
    pub name: Symbol,
    pub module_name: String,
    pub operations: Vec<EffectOpInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectUse {
    pub effect_id: u16,
    pub op_id: u16,
}

/// Central type-checking environment.
///
/// Owns the type arena, unification variable bindings, and maps from AST
/// nodes to their inferred types, effects, and dispatch decisions.
pub struct TypeEnv {
    pub types: Arena<Ty>,
    pub vars: Vec<Cell<Option<SemaTypeId>>>,
    pub type_map: HashMap<ExprId, SemaTypeId>,
    pub effect_map: HashMap<ExprId, SemaTypeList>,
    pub dispatch: HashMap<ExprId, DispatchInfo>,
    pub builtin_types: HashMap<BuiltinType, SemaTypeId>,
    /// Registered type class instances: `(class, type) -> entry`.
    pub instances: HashMap<(Symbol, Symbol), InstanceEntry>,
    /// Effect name -> semantic metadata for the current module.
    pub effect_defs: HashMap<Symbol, EffectDefInfo>,
    /// Effects in deterministic local-ID order.
    pub effect_order: SymbolList,
    /// Handle `ExprId` -> effect ID for the handler.
    pub handle_effects: HashMap<ExprId, u16>,
    /// Perform `ExprId` -> explicit effect operation being invoked.
    pub perform_effects: HashMap<ExprId, EffectUse>,
    /// Variant literal `ExprId` -> resolved parent type and tag index.
    pub variant_info: HashMap<ExprId, VariantInfo>,
    /// Stable class IDs for type class dispatch in the emitter.
    pub class_ids: HashMap<Symbol, u16>,
    /// Variant name -> numeric tag index, populated during data def processing.
    pub variant_tags: HashMap<Symbol, u16>,
    /// Mutable bindings captured across lambda boundaries, requiring heap boxing.
    pub captured_mutables: HashSet<DefId>,
    /// Names of mutable bindings captured across lambda boundaries (for emitter lookup).
    pub captured_mutable_names: HashSet<Symbol>,
    next_var: TyVarId,
    next_effect_idx: u16,
    next_effect_op_idx: HashMap<u16, u16>,
    next_class_id: u16,
}

impl TypeEnv {
    /// Creates an empty type environment.
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Creates a type environment with pre-sized maps.
    ///
    /// `hint` is typically the number of AST expressions; maps that track
    /// per-expression data are sized from it, while smaller maps use
    /// proportional fractions.
    #[must_use]
    pub fn with_capacity(hint: usize) -> Self {
        Self {
            types: Arena::with_capacity(hint),
            vars: Vec::new(),
            type_map: HashMap::with_capacity(hint),
            effect_map: HashMap::new(),
            dispatch: HashMap::with_capacity(hint / 4),
            builtin_types: HashMap::with_capacity(32),
            instances: HashMap::new(),
            effect_defs: HashMap::new(),
            effect_order: Vec::new(),
            handle_effects: HashMap::new(),
            perform_effects: HashMap::new(),
            variant_info: HashMap::with_capacity(hint / 8),
            class_ids: HashMap::new(),
            variant_tags: HashMap::new(),
            captured_mutables: HashSet::new(),
            captured_mutable_names: HashSet::new(),
            next_var: 0,
            next_effect_idx: 0,
            next_effect_op_idx: HashMap::new(),
            next_class_id: 0,
        }
    }

    /// Allocates a type in the arena and returns its interned handle.
    pub fn intern(&mut self, ty: Ty) -> SemaTypeId {
        self.types.alloc(ty)
    }

    /// Creates a fresh unification variable and returns its interned handle.
    ///
    /// # Panics
    ///
    /// Panics if more than `u32::MAX` type variables are created.
    pub fn fresh_var(&mut self) -> SemaTypeId {
        let id = self.next_var;
        self.next_var = self
            .next_var
            .checked_add(1)
            .expect("type variable overflow");
        self.vars.push(Cell::new(None));
        self.intern(Ty::Var(id))
    }

    /// Pre-populates the environment with a `SemaTypeId` for every `BuiltinType`.
    pub fn seed_builtins(&mut self) {
        for &bt in BuiltinType::ALL {
            let id = self.intern(Ty::Builtin(bt));
            let _prev = self.builtin_types.insert(bt, id);
        }
    }

    /// Returns the `SemaTypeId` for a built-in type.
    ///
    /// # Panics
    ///
    /// Panics if `seed_builtins` has not been called.
    #[must_use]
    pub fn builtin(&self, bt: BuiltinType) -> SemaTypeId {
        *self
            .builtin_types
            .get(&bt)
            .expect("builtin type not seeded; call seed_builtins first")
    }

    /// Follows unification variable chains to find the resolved type.
    ///
    /// If `id` refers to a `Ty::Var(v)` that has been bound, follows the
    /// binding recursively. Returns the final non-variable type id, or the
    /// unbound variable itself.
    ///
    /// # Panics
    ///
    /// Panics if a `TyVarId` exceeds `usize::MAX` (unreachable on 32+ bit
    /// platforms since `TyVarId` is `u32`).
    #[must_use]
    pub fn resolve_var(&self, id: SemaTypeId) -> SemaTypeId {
        let ty = self.types.get(id);
        if let Ty::Var(v) = ty {
            let idx = usize::try_from(*v).expect("TyVarId always fits in usize");
            if let Some(bound) = self.vars.get(idx).and_then(Cell::get) {
                let resolved = self.resolve_var(bound);
                // Path compression: point directly to the final target
                if resolved != bound {
                    self.vars[idx].set(Some(resolved));
                }
                return resolved;
            }
        }
        id
    }

    /// Returns the stable numeric ID for an effect, assigning one if first seen.
    pub fn assign_effect_id(&mut self, name: Symbol) -> u16 {
        if let Some(effect) = self.effect_defs.get(&name) {
            return effect.id;
        }
        let id = self.next_effect_idx;
        self.next_effect_idx = self.next_effect_idx.wrapping_add(1);
        self.effect_order.push(name);
        let _ = self.effect_defs.insert(
            name,
            EffectDefInfo {
                id,
                name,
                module_name: String::new(),
                operations: Vec::new(),
            },
        );
        id
    }

    pub fn set_effect_module_name(&mut self, name: Symbol, module_name: String) {
        let effect_id = self.assign_effect_id(name);
        let effect = self
            .effect_defs
            .entry(name)
            .or_insert_with(|| EffectDefInfo {
                id: effect_id,
                name,
                module_name: String::new(),
                operations: Vec::new(),
            });
        effect.module_name = module_name;
    }

    pub fn register_effect_ops(
        &mut self,
        effect_name: Symbol,
        operations: Vec<(Symbol, Option<SemaTypeId>, SemaTypeId)>,
    ) -> u16 {
        let effect_id = self.assign_effect_id(effect_name);
        let next_op = self.next_effect_op_idx.entry(effect_id).or_insert(0);
        let effect = self
            .effect_defs
            .entry(effect_name)
            .or_insert_with(|| EffectDefInfo {
                id: effect_id,
                name: effect_name,
                module_name: String::new(),
                operations: Vec::new(),
            });
        effect.operations.clear();
        for (op_name, param_ty, ret_ty) in operations {
            let op_id = *next_op;
            *next_op = next_op.wrapping_add(1);
            effect.operations.push(EffectOpInfo {
                id: op_id,
                name: op_name,
                param_ty,
                ret_ty,
            });
        }
        effect_id
    }

    pub fn ensure_effect_op(
        &mut self,
        effect_name: Symbol,
        op_name: Symbol,
        ret_ty: SemaTypeId,
    ) -> EffectUse {
        if let Some(existing) = self.resolve_effect_op(effect_name, op_name) {
            return existing;
        }
        let effect_id = self.assign_effect_id(effect_name);
        let next_op = self.next_effect_op_idx.entry(effect_id).or_insert(0);
        let op_id = *next_op;
        *next_op = next_op.wrapping_add(1);
        let effect = self
            .effect_defs
            .entry(effect_name)
            .or_insert_with(|| EffectDefInfo {
                id: effect_id,
                name: effect_name,
                module_name: String::new(),
                operations: Vec::new(),
            });
        effect.operations.push(EffectOpInfo {
            id: op_id,
            name: op_name,
            param_ty: None,
            ret_ty,
        });
        EffectUse { effect_id, op_id }
    }

    #[must_use]
    pub fn effect_by_id(&self, effect_id: u16) -> Option<&EffectDefInfo> {
        self.effect_order
            .iter()
            .filter_map(|name| self.effect_defs.get(name))
            .find(|effect| effect.id == effect_id)
    }

    #[must_use]
    pub fn resolve_effect_op(&self, effect_name: Symbol, op_name: Symbol) -> Option<EffectUse> {
        self.effect_defs.get(&effect_name).and_then(|effect| {
            effect
                .operations
                .iter()
                .find(|op| op.name == op_name)
                .map(|op| EffectUse {
                    effect_id: effect.id,
                    op_id: op.id,
                })
        })
    }

    #[must_use]
    pub fn effect_op_info(&self, effect_name: Symbol, op_name: Symbol) -> Option<&EffectOpInfo> {
        self.effect_defs
            .get(&effect_name)
            .and_then(|effect| effect.operations.iter().find(|op| op.name == op_name))
    }

    /// Returns the stable numeric ID for a type class, assigning one if first seen.
    pub fn assign_class_id(&mut self, name: Symbol) -> u16 {
        let next = &mut self.next_class_id;
        *self.class_ids.entry(name).or_insert_with(|| {
            let id = *next;
            *next = next.wrapping_add(1);
            id
        })
    }

    /// Binds a unification variable to a type.
    ///
    /// # Panics
    ///
    /// Panics if `var` is out of range for the current variable set.
    pub fn bind_var(&mut self, var: TyVarId, ty: SemaTypeId) {
        let idx = usize::try_from(var).expect("TyVarId always fits in usize");
        assert!(idx < self.vars.len(), "variable index out of bounds");
        self.vars[idx].set(Some(ty));
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
