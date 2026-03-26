use std::collections::HashMap;

use music_arena::Arena;
use music_ast::ExprId;
use music_builtins::types::BuiltinType;
use music_found::{Span, Symbol};
use music_il::opcode::Opcode;

use crate::types::{SemaTypeId, Ty, TyVarId};

/// Tracks a registered type class instance for coherence checking.
#[derive(Debug, Clone)]
pub struct InstanceEntry {
    pub span: Span,
    pub methods: Vec<Symbol>,
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

/// Central type-checking environment.
///
/// Owns the type arena, unification variable bindings, and maps from AST
/// nodes to their inferred types, effects, and dispatch decisions.
pub struct TypeEnv {
    pub types: Arena<Ty>,
    pub vars: Vec<Option<SemaTypeId>>,
    pub type_map: HashMap<ExprId, SemaTypeId>,
    pub effect_map: HashMap<ExprId, Vec<SemaTypeId>>,
    pub dispatch: HashMap<ExprId, DispatchInfo>,
    pub builtin_types: HashMap<BuiltinType, SemaTypeId>,
    /// Registered type class instances: `(class, type) -> entry`.
    pub instances: HashMap<(Symbol, Symbol), InstanceEntry>,
    /// Effect name -> operation names.
    pub effect_ops: HashMap<Symbol, Vec<Symbol>>,
    /// Effect name -> stable numeric ID (assigned in declaration order).
    pub effect_indices: HashMap<Symbol, u16>,
    /// Handle `ExprId` -> effect ID for the handler.
    pub handle_effects: HashMap<ExprId, u16>,
    /// Need `ExprId` -> effect ID being invoked.
    pub need_effects: HashMap<ExprId, u16>,
    /// Variant literal `ExprId` -> resolved parent type and tag index.
    pub variant_info: HashMap<ExprId, VariantInfo>,
    next_var: TyVarId,
    next_effect_idx: u16,
}

impl TypeEnv {
    /// Creates an empty type environment.
    #[must_use]
    pub fn new() -> Self {
        Self {
            types: Arena::new(),
            vars: Vec::new(),
            type_map: HashMap::new(),
            effect_map: HashMap::new(),
            dispatch: HashMap::new(),
            builtin_types: HashMap::new(),
            instances: HashMap::new(),
            effect_ops: HashMap::new(),
            effect_indices: HashMap::new(),
            handle_effects: HashMap::new(),
            need_effects: HashMap::new(),
            variant_info: HashMap::new(),
            next_var: 0,
            next_effect_idx: 0,
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
        self.vars.push(None);
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
            if let Some(bound) = self.vars.get(idx).and_then(|slot| *slot) {
                return self.resolve_var(bound);
            }
        }
        id
    }

    /// Returns the stable numeric ID for an effect, assigning one if first seen.
    pub fn assign_effect_id(&mut self, name: Symbol) -> u16 {
        let next = &mut self.next_effect_idx;
        *self.effect_indices.entry(name).or_insert_with(|| {
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
        self.vars[idx] = Some(ty);
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
