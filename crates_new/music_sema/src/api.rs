use std::collections::{BTreeSet, HashMap};

use music_arena::Idx;
use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirOrigin, HirPatId, HirTy, HirTyId};
use music_names::Symbol;
use music_resolve::ResolvedModule;

use crate::effects::EffectRow;

pub type SemaDiagList = Vec<Diag>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TargetInfo {
    pub os: Option<Box<str>>,
    pub arch: Option<Box<str>>,
    pub env: Option<Box<str>>,
    pub abi: Option<Box<str>>,
    pub vendor: Option<Box<str>>,
    pub features: BTreeSet<Box<str>>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SemaOptions {
    pub target: Option<TargetInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprFacts {
    pub ty: HirTyId,
    pub effects: EffectRow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatFacts {
    pub ty: HirTyId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintKind {
    Subtype,
    Implements,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintFacts {
    pub name: Symbol,
    pub kind: ConstraintKind,
    pub value: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberFacts {
    pub name: Symbol,
    pub params: Box<[HirTyId]>,
    pub result: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassFacts {
    pub name: Symbol,
    pub constraints: Box<[ConstraintFacts]>,
    pub members: Box<[ClassMemberFacts]>,
    pub laws: Box<[Symbol]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceFacts {
    pub origin: HirOrigin,
    pub class_name: Symbol,
    pub class_args: Box<[HirTyId]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub member_names: Box<[Symbol]>,
}

#[derive(Debug)]
pub struct SemaModule {
    resolved: ResolvedModule,
    expr_facts: Box<[ExprFacts]>,
    pat_facts: Box<[PatFacts]>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
    diags: SemaDiagList,
}

impl SemaModule {
    #[must_use]
    pub const fn resolved(&self) -> &ResolvedModule {
        &self.resolved
    }

    #[must_use]
    pub const fn module(&self) -> &HirModule {
        &self.resolved.module
    }

    #[must_use]
    pub fn diags(&self) -> &[Diag] {
        &self.diags
    }

    #[must_use]
    pub fn ty(&self, id: HirTyId) -> &HirTy {
        self.module().store.tys.get(id)
    }

    /// # Panics
    /// Panics if `id` does not refer to an expression in this module.
    #[must_use]
    pub fn expr_ty(&self, id: HirExprId) -> HirTyId {
        self.expr_facts
            .get(idx_to_usize(id))
            .expect("expr facts missing for HIR expr id")
            .ty
    }

    /// # Panics
    /// Panics if `id` does not refer to an expression in this module.
    #[must_use]
    pub fn expr_effects(&self, id: HirExprId) -> &EffectRow {
        &self
            .expr_facts
            .get(idx_to_usize(id))
            .expect("expr facts missing for HIR expr id")
            .effects
    }

    /// # Panics
    /// Panics if `id` does not refer to a pattern in this module.
    #[must_use]
    pub fn pat_ty(&self, id: HirPatId) -> HirTyId {
        self.pat_facts
            .get(idx_to_usize(id))
            .expect("pat facts missing for HIR pat id")
            .ty
    }

    #[must_use]
    pub fn has_class_facts(&self, id: HirExprId) -> bool {
        self.class_facts.contains_key(&id)
    }

    #[must_use]
    pub fn has_instance_facts(&self, id: HirExprId) -> bool {
        self.instance_facts.contains_key(&id)
    }

    #[must_use]
    pub(crate) fn from_parts(
        resolved: ResolvedModule,
        expr_facts: Vec<ExprFacts>,
        pat_facts: Vec<PatFacts>,
        class_facts: HashMap<HirExprId, ClassFacts>,
        instance_facts: HashMap<HirExprId, InstanceFacts>,
        diags: SemaDiagList,
    ) -> Self {
        Self {
            resolved,
            expr_facts: expr_facts.into_boxed_slice(),
            pat_facts: pat_facts.into_boxed_slice(),
            class_facts,
            instance_facts,
            diags,
        }
    }
}

fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
