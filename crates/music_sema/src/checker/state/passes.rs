use std::ops::{Deref, DerefMut};

use music_base::{SourceId, Span};
use music_hir::{HirExprId, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, NameBindingKind, Symbol};
use music_resolve::ResolvedImportBindingList;

use crate::api::{ConstraintAnswer, ConstraintKey};

use super::aliases::{
    ConstraintAnswerScope, ConstraintAnswerScopeList, ExpectedTyList, StaticImportList,
};

use super::{DeclState, FactState, ModuleState, ResumeCtx, ResumeState, RuntimeEnv, TypingState};

pub struct PassBase<'ctx, 'interner, 'env> {
    pub module: &'ctx mut ModuleState,
    pub runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
    pub typing: &'ctx mut TypingState,
    pub decls: &'ctx mut DeclState,
    pub facts: &'ctx mut FactState,
}

pub struct PassParts<'ctx, 'interner, 'env> {
    pub module: &'ctx mut ModuleState,
    pub runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
    pub typing: &'ctx mut TypingState,
    pub decls: &'ctx mut DeclState,
    pub facts: &'ctx mut FactState,
}

pub struct CollectPass<'ctx, 'interner, 'env> {
    pub base: PassBase<'ctx, 'interner, 'env>,
}

pub struct CheckPass<'ctx, 'interner, 'env> {
    pub collect: CollectPass<'ctx, 'interner, 'env>,
    pub resume: &'ctx mut ResumeState,
    pub expected: ExpectedTyList,
    pub answer_scopes: ConstraintAnswerScopeList,
    pub module_stmt_depth: u32,
    pub unsafe_depth: u32,
}

impl<'ctx, 'interner, 'env> PassBase<'ctx, 'interner, 'env> {
    pub const fn new(parts: PassParts<'ctx, 'interner, 'env>) -> Self {
        let PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = parts;
        Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        }
    }

    pub const fn root_expr_id(&self) -> HirExprId {
        self.module.resolved.module.root
    }

    pub const fn source_id(&self) -> SourceId {
        self.module.resolved.module.source_id
    }

    pub const fn module_key(&self) -> &ModuleKey {
        &self.module.resolved.module_key
    }

    pub fn static_import_target(&self, span: Span) -> Option<ModuleKey> {
        self.module.import_targets.get(&span).cloned()
    }

    pub fn static_imports(&self) -> StaticImportList {
        self.module
            .resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect()
    }

    pub fn import_bindings(&self) -> ResolvedImportBindingList {
        self.module.resolved.import_bindings.clone()
    }

    pub fn prelude_bindings(&self) -> Box<[(NameBindingId, Symbol)]> {
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .filter_map(|(id, binding)| {
                (binding.kind == NameBindingKind::Prelude).then_some((id, binding.name))
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

impl<'ctx, 'interner, 'env> CollectPass<'ctx, 'interner, 'env> {
    pub const fn new(base: PassBase<'ctx, 'interner, 'env>) -> Self {
        Self { base }
    }
}

impl<'ctx, 'interner, 'env> Deref for CollectPass<'ctx, 'interner, 'env> {
    type Target = PassBase<'ctx, 'interner, 'env>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for CollectPass<'_, '_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl<'ctx, 'interner, 'env> CheckPass<'ctx, 'interner, 'env> {
    pub const fn new(
        collect: CollectPass<'ctx, 'interner, 'env>,
        resume: &'ctx mut ResumeState,
    ) -> Self {
        Self {
            collect,
            resume,
            expected: Vec::new(),
            answer_scopes: Vec::new(),
            module_stmt_depth: 0,
            unsafe_depth: 0,
        }
    }

    pub const fn enter_module_stmt(&mut self) {
        self.module_stmt_depth = self.module_stmt_depth.saturating_add(1);
    }

    pub const fn exit_module_stmt(&mut self) {
        self.module_stmt_depth = self.module_stmt_depth.saturating_sub(1);
    }

    pub const fn in_module_stmt(&self) -> bool {
        self.module_stmt_depth > 0
    }

    pub const fn enter_unsafe_block(&mut self) {
        self.unsafe_depth = self.unsafe_depth.saturating_add(1);
    }

    pub const fn exit_unsafe_block(&mut self) {
        self.unsafe_depth = self.unsafe_depth.saturating_sub(1);
    }

    pub const fn in_unsafe_block(&self) -> bool {
        self.unsafe_depth > 0
    }

    pub fn push_answer_scope(&mut self, scope: ConstraintAnswerScope) {
        self.answer_scopes.push(scope);
    }

    pub fn pop_answer_scope(&mut self) -> Option<ConstraintAnswerScope> {
        self.answer_scopes.pop()
    }

    pub fn resolve_in_scope_answer(&self, key: &ConstraintKey) -> Option<ConstraintAnswer> {
        self.answer_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(key).cloned())
    }

    pub fn answer_entries_in_scope(&self) -> Vec<(ConstraintKey, ConstraintAnswer)> {
        self.answer_scopes
            .iter()
            .rev()
            .flat_map(|scope| {
                scope
                    .iter()
                    .map(|(key, answer)| (key.clone(), answer.clone()))
            })
            .collect()
    }

    pub fn push_resume(&mut self, ctx: ResumeCtx) {
        self.resume.stack.push(ctx);
    }

    pub fn pop_resume(&mut self) -> Option<ResumeCtx> {
        self.resume.stack.pop()
    }

    pub fn resume_top(&self) -> Option<ResumeCtx> {
        self.resume.stack.last().cloned()
    }

    pub fn push_expected_ty(&mut self, ty: HirTyId) {
        self.expected.push(ty);
    }

    pub fn pop_expected_ty(&mut self) -> Option<HirTyId> {
        self.expected.pop()
    }

    pub fn expected_ty(&self) -> Option<HirTyId> {
        self.expected.last().copied()
    }
}

impl<'ctx, 'interner, 'env> Deref for CheckPass<'ctx, 'interner, 'env> {
    type Target = CollectPass<'ctx, 'interner, 'env>;

    fn deref(&self) -> &Self::Target {
        &self.collect
    }
}

impl DerefMut for CheckPass<'_, '_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.collect
    }
}
