use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use music_arena::{Idx, SliceRange};
use music_base::diag::Diag;
use music_base::{SourceId, Span};
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirAttrArg, HirCaseArm, HirConstraint, HirDim, HirEffectItem,
    HirEffectSet, HirExpr, HirExprId, HirFieldDef, HirForeignDecl, HirHandleClause, HirLit,
    HirLitId, HirLitKind, HirMemberDef, HirOrigin, HirParam, HirPat, HirPatId, HirRecordItem,
    HirRecordPatField, HirTemplatePart, HirTy, HirTyField, HirTyId, HirTyKind, HirVariantDef,
};
use music_names::{Ident, Interner, KnownSymbols, NameBindingId, NameSite, Symbol};
use music_resolve::ResolvedModule;

use crate::api::{
    ClassFacts, ExprFacts, InstanceFacts, PatFacts, SemaDiagList, SemaModule, SemaOptions,
};
use crate::effects::EffectRow;

#[derive(Debug, Clone, Copy)]
pub struct Builtins {
    pub error: HirTyId,
    pub unknown: HirTyId,
    pub type_: HirTyId,
    pub module: HirTyId,
    pub syntax: HirTyId,
    pub any: HirTyId,
    pub empty: HirTyId,
    pub unit: HirTyId,
    pub bool_: HirTyId,
    pub int_: HirTyId,
    pub float_: HirTyId,
    pub string_: HirTyId,
    pub cstring: HirTyId,
    pub cptr: HirTyId,
}

#[derive(Debug, Clone)]
pub struct EffectOpDef {
    pub params: Box<[HirTyId]>,
    pub result: HirTyId,
}

#[derive(Debug, Clone)]
pub struct EffectDef {
    pub ops: HashMap<Symbol, EffectOpDef>,
}

#[derive(Debug, Clone)]
pub struct ResumeCtx {
    pub arg: HirTyId,
    pub result: HirTyId,
}

pub struct ModuleState {
    resolved: ResolvedModule,
    binding_ids: HashMap<NameSite, NameBindingId>,
}

pub struct RuntimeEnv<'a> {
    interner: &'a mut Interner,
    known: KnownSymbols,
    builtins: Builtins,
    options: SemaOptions,
}

#[derive(Default)]
pub struct TypingState {
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_effects: HashMap<NameBindingId, EffectRow>,
}

#[derive(Default)]
pub struct DeclState {
    effect_defs: HashMap<Symbol, EffectDef>,
    class_index: HashMap<Symbol, HirExprId>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
}

pub struct FactState {
    diags: SemaDiagList,
    expr_facts: Vec<ExprFacts>,
    pat_facts: Vec<PatFacts>,
}

#[derive(Default)]
pub struct ResumeState {
    stack: Vec<ResumeCtx>,
}

pub struct PassBase<'a, 'env> {
    module: &'a mut ModuleState,
    runtime: &'a mut RuntimeEnv<'env>,
    typing: &'a mut TypingState,
    decls: &'a mut DeclState,
    facts: &'a mut FactState,
}

pub struct CollectPass<'a, 'env> {
    base: PassBase<'a, 'env>,
}

pub struct CheckPass<'a, 'env> {
    base: PassBase<'a, 'env>,
    resume: &'a mut ResumeState,
}

pub fn prepare_module(
    mut resolved: ResolvedModule,
    interner: &mut Interner,
    options: SemaOptions,
) -> (
    ModuleState,
    RuntimeEnv<'_>,
    TypingState,
    DeclState,
    FactState,
    ResumeState,
) {
    let known = KnownSymbols::new(interner);
    let builtins = Builtins {
        error: alloc_builtin(&mut resolved, HirTyKind::Error),
        unknown: alloc_builtin(&mut resolved, HirTyKind::Unknown),
        type_: alloc_builtin(&mut resolved, HirTyKind::Type),
        module: alloc_builtin(&mut resolved, HirTyKind::Module),
        syntax: alloc_builtin(&mut resolved, HirTyKind::Syntax),
        any: alloc_builtin(&mut resolved, HirTyKind::Any),
        empty: alloc_builtin(&mut resolved, HirTyKind::Empty),
        unit: alloc_builtin(&mut resolved, HirTyKind::Unit),
        bool_: alloc_builtin(&mut resolved, HirTyKind::Bool),
        int_: alloc_builtin(&mut resolved, HirTyKind::Int),
        float_: alloc_builtin(&mut resolved, HirTyKind::Float),
        string_: alloc_builtin(&mut resolved, HirTyKind::String),
        cstring: alloc_builtin(&mut resolved, HirTyKind::CString),
        cptr: alloc_builtin(&mut resolved, HirTyKind::CPtr),
    };
    let binding_ids = resolved
        .names
        .bindings
        .iter()
        .map(|(id, binding)| (binding.site, id))
        .collect::<HashMap<_, _>>();
    let expr_facts = vec![
        ExprFacts {
            ty: builtins.unknown,
            effects: EffectRow::empty(),
        };
        resolved.module.store.exprs.len()
    ];
    let pat_facts = vec![
        PatFacts {
            ty: builtins.unknown
        };
        resolved.module.store.pats.len()
    ];

    (
        ModuleState {
            resolved,
            binding_ids,
        },
        RuntimeEnv {
            interner,
            known,
            builtins,
            options,
        },
        TypingState::default(),
        DeclState::default(),
        FactState {
            diags: Vec::new(),
            expr_facts,
            pat_facts,
        },
        ResumeState::default(),
    )
}

pub fn finish_module(module: ModuleState, decls: DeclState, facts: FactState) -> SemaModule {
    SemaModule::from_parts(
        module.resolved,
        facts.expr_facts,
        facts.pat_facts,
        decls.class_facts,
        decls.instance_facts,
        facts.diags,
    )
}

impl<'a, 'env> PassBase<'a, 'env> {
    const fn new(
        module: &'a mut ModuleState,
        runtime: &'a mut RuntimeEnv<'env>,
        typing: &'a mut TypingState,
        decls: &'a mut DeclState,
        facts: &'a mut FactState,
    ) -> Self {
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

    pub fn expr(&self, id: HirExprId) -> HirExpr {
        self.module.resolved.module.store.exprs.get(id).clone()
    }

    pub fn pat(&self, id: HirPatId) -> HirPat {
        self.module.resolved.module.store.pats.get(id).clone()
    }

    pub fn ty(&self, id: HirTyId) -> HirTy {
        self.module.resolved.module.store.tys.get(id).clone()
    }

    pub fn lit(&self, id: HirLitId) -> HirLit {
        self.module.resolved.module.store.lits.get(id).clone()
    }

    pub const fn builtins(&self) -> Builtins {
        self.runtime.builtins
    }

    pub const fn known(&self) -> KnownSymbols {
        self.runtime.known
    }

    pub const fn options(&self) -> &SemaOptions {
        &self.runtime.options
    }

    pub fn intern(&mut self, text: &str) -> Symbol {
        self.runtime.interner.intern(text)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.runtime.interner.resolve(symbol)
    }

    pub fn expr_ids(&self, range: SliceRange<HirExprId>) -> Vec<HirExprId> {
        self.module
            .resolved
            .module
            .store
            .expr_ids
            .get(range)
            .to_vec()
    }

    pub fn args(&self, range: SliceRange<HirArg>) -> Vec<HirArg> {
        self.module.resolved.module.store.args.get(range).to_vec()
    }

    pub fn arg_count(&self, range: SliceRange<HirArg>) -> usize {
        self.module.resolved.module.store.args.get(range).len()
    }

    pub fn dims(&self, range: SliceRange<HirDim>) -> Vec<HirDim> {
        self.module.resolved.module.store.dims.get(range).to_vec()
    }

    pub fn ty_ids(&self, range: SliceRange<HirTyId>) -> Vec<HirTyId> {
        self.module.resolved.module.store.ty_ids.get(range).to_vec()
    }

    pub fn ty_fields(&self, range: SliceRange<HirTyField>) -> Vec<HirTyField> {
        self.module
            .resolved
            .module
            .store
            .ty_fields
            .get(range)
            .to_vec()
    }

    pub fn array_items(&self, range: SliceRange<HirArrayItem>) -> Vec<HirArrayItem> {
        self.module
            .resolved
            .module
            .store
            .array_items
            .get(range)
            .to_vec()
    }

    pub fn record_items(&self, range: SliceRange<HirRecordItem>) -> Vec<HirRecordItem> {
        self.module
            .resolved
            .module
            .store
            .record_items
            .get(range)
            .to_vec()
    }

    pub fn params(&self, range: SliceRange<HirParam>) -> Vec<HirParam> {
        self.module.resolved.module.store.params.get(range).to_vec()
    }

    pub fn attrs(&self, range: SliceRange<HirAttr>) -> Vec<HirAttr> {
        self.module.resolved.module.store.attrs.get(range).to_vec()
    }

    pub fn attr_args(&self, range: SliceRange<HirAttrArg>) -> Vec<HirAttrArg> {
        self.module
            .resolved
            .module
            .store
            .attr_args
            .get(range)
            .to_vec()
    }

    pub fn members(&self, range: SliceRange<HirMemberDef>) -> Vec<HirMemberDef> {
        self.module
            .resolved
            .module
            .store
            .members
            .get(range)
            .to_vec()
    }

    pub fn foreign_decls(&self, range: SliceRange<HirForeignDecl>) -> Vec<HirForeignDecl> {
        self.module
            .resolved
            .module
            .store
            .foreign_decls
            .get(range)
            .to_vec()
    }

    pub fn handle_clauses(&self, range: SliceRange<HirHandleClause>) -> Vec<HirHandleClause> {
        self.module
            .resolved
            .module
            .store
            .handle_clauses
            .get(range)
            .to_vec()
    }

    pub fn case_arms(&self, range: SliceRange<HirCaseArm>) -> Vec<HirCaseArm> {
        self.module
            .resolved
            .module
            .store
            .case_arms
            .get(range)
            .to_vec()
    }

    pub fn constraints(&self, range: SliceRange<HirConstraint>) -> Vec<HirConstraint> {
        self.module
            .resolved
            .module
            .store
            .constraints
            .get(range)
            .to_vec()
    }

    pub fn variants(&self, range: SliceRange<HirVariantDef>) -> Vec<HirVariantDef> {
        self.module
            .resolved
            .module
            .store
            .variants
            .get(range)
            .to_vec()
    }

    pub fn fields(&self, range: SliceRange<HirFieldDef>) -> Vec<HirFieldDef> {
        self.module.resolved.module.store.fields.get(range).to_vec()
    }

    pub fn effect_items(&self, set: &HirEffectSet) -> Vec<HirEffectItem> {
        self.module
            .resolved
            .module
            .store
            .effect_items
            .get(set.items.clone())
            .to_vec()
    }

    pub fn pat_ids(&self, range: SliceRange<HirPatId>) -> Vec<HirPatId> {
        self.module
            .resolved
            .module
            .store
            .pat_ids
            .get(range)
            .to_vec()
    }

    pub fn record_pat_fields(
        &self,
        range: SliceRange<HirRecordPatField>,
    ) -> Vec<HirRecordPatField> {
        self.module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(range)
            .to_vec()
    }

    pub fn idents(&self, range: SliceRange<Ident>) -> Vec<Ident> {
        self.module.resolved.module.store.idents.get(range).to_vec()
    }

    pub fn template_parts(&self, range: SliceRange<HirTemplatePart>) -> Vec<HirTemplatePart> {
        self.module
            .resolved
            .module
            .store
            .template_parts
            .get(range)
            .to_vec()
    }

    pub fn set_expr_facts(&mut self, id: HirExprId, facts: ExprFacts) {
        let slot = self
            .facts
            .expr_facts
            .get_mut(idx_to_usize(id))
            .expect("expr facts slot missing");
        *slot = facts;
    }

    pub fn set_pat_facts(&mut self, id: HirPatId, facts: PatFacts) {
        let slot = self
            .facts
            .pat_facts
            .get_mut(idx_to_usize(id))
            .expect("pat facts slot missing");
        *slot = facts;
    }

    pub fn alloc_dims<I>(&mut self, dims: I) -> SliceRange<HirDim>
    where
        I: IntoIterator<Item = HirDim>,
    {
        self.module.resolved.module.store.dims.alloc_from_iter(dims)
    }

    pub fn alloc_ty(&mut self, kind: HirTyKind) -> HirTyId {
        self.module.resolved.module.store.alloc_ty(HirTy {
            origin: HirOrigin::dummy(),
            kind,
        })
    }

    pub fn alloc_ty_list<I>(&mut self, tys: I) -> SliceRange<HirTyId>
    where
        I: IntoIterator<Item = HirTyId>,
    {
        self.module.resolved.module.store.alloc_ty_list(tys)
    }

    pub fn alloc_ty_fields<I>(&mut self, fields: I) -> SliceRange<HirTyField>
    where
        I: IntoIterator<Item = HirTyField>,
    {
        self.module
            .resolved
            .module
            .store
            .alloc_ty_field_list(fields)
    }

    pub fn binding_id_for_decl(&self, ident: Ident) -> Option<NameBindingId> {
        self.module
            .binding_ids
            .get(&NameSite::new(self.source_id(), ident.span))
            .copied()
    }

    pub fn binding_id_for_use(&self, ident: Ident) -> Option<NameBindingId> {
        self.module
            .resolved
            .names
            .refs
            .get(&NameSite::new(self.source_id(), ident.span))
            .copied()
    }

    pub fn binding_type(&self, id: NameBindingId) -> Option<HirTyId> {
        self.typing.binding_types.get(&id).copied()
    }

    pub fn insert_binding_type(&mut self, id: NameBindingId, ty: HirTyId) {
        let _prev = self.typing.binding_types.insert(id, ty);
    }

    pub fn binding_effects(&self, id: NameBindingId) -> Option<EffectRow> {
        self.typing.binding_effects.get(&id).cloned()
    }

    pub fn insert_binding_effects(&mut self, id: NameBindingId, effects: EffectRow) {
        let _prev = self.typing.binding_effects.insert(id, effects);
    }

    pub fn effect_def(&self, symbol: Symbol) -> Option<&EffectDef> {
        self.decls.effect_defs.get(&symbol)
    }

    pub fn insert_effect_def(&mut self, symbol: Symbol, def: EffectDef) {
        let _prev = self.decls.effect_defs.insert(symbol, def);
    }

    pub fn class_id(&self, symbol: Symbol) -> Option<HirExprId> {
        self.decls.class_index.get(&symbol).copied()
    }

    pub fn insert_class_id(&mut self, symbol: Symbol, id: HirExprId) {
        let _prev = self.decls.class_index.insert(symbol, id);
    }

    pub fn insert_class_facts(&mut self, id: HirExprId, facts: ClassFacts) {
        let _prev = self.decls.class_facts.insert(id, facts);
    }

    pub fn class_facts(&self, id: HirExprId) -> Option<&ClassFacts> {
        self.decls.class_facts.get(&id)
    }

    pub fn insert_instance_facts(&mut self, id: HirExprId, facts: InstanceFacts) {
        let _prev = self.decls.instance_facts.insert(id, facts);
    }

    pub fn lit_kind(&self, lit: HirLitId) -> HirLitKind {
        self.lit(lit).kind
    }

    pub fn lit_is_string(&self, lit: HirLitId) -> bool {
        matches!(self.lit_kind(lit), HirLitKind::String { .. })
    }

    pub fn diag(&mut self, span: Span, message: &str, label: &str) {
        self.facts
            .diags
            .push(Diag::error(message).with_label(span, self.source_id(), label));
    }
}

impl<'a, 'env> CollectPass<'a, 'env> {
    pub const fn new(
        module: &'a mut ModuleState,
        runtime: &'a mut RuntimeEnv<'env>,
        typing: &'a mut TypingState,
        decls: &'a mut DeclState,
        facts: &'a mut FactState,
    ) -> Self {
        Self {
            base: PassBase::new(module, runtime, typing, decls, facts),
        }
    }
}

impl<'a, 'env> Deref for CollectPass<'a, 'env> {
    type Target = PassBase<'a, 'env>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for CollectPass<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl<'a, 'env> CheckPass<'a, 'env> {
    pub const fn new(
        module: &'a mut ModuleState,
        runtime: &'a mut RuntimeEnv<'env>,
        typing: &'a mut TypingState,
        decls: &'a mut DeclState,
        facts: &'a mut FactState,
        resume: &'a mut ResumeState,
    ) -> Self {
        Self {
            base: PassBase::new(module, runtime, typing, decls, facts),
            resume,
        }
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
}

impl<'a, 'env> Deref for CheckPass<'a, 'env> {
    type Target = PassBase<'a, 'env>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for CheckPass<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

fn alloc_builtin(resolved: &mut ResolvedModule, kind: HirTyKind) -> HirTyId {
    resolved.module.store.alloc_ty(HirTy {
        origin: HirOrigin::dummy(),
        kind,
    })
}

fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
