use std::collections::{BTreeSet, HashMap, HashSet};
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
use music_module::ModuleKey;
use music_names::{Ident, Interner, KnownSymbols, NameBindingId, NameSite, Symbol};
use music_resolve::ResolvedModule;

use super::schemes::BindingScheme;
use super::surface::build_module_surface;
use crate::api::{
    ClassFacts, ExprFacts, InstanceFacts, PatFacts, SemaDiagList, SemaEnv, SemaModule,
    SemaDataDef, SemaDataVariantDef, SemaEffectDef, SemaEffectOpDef, SemaModuleParts, SemaOptions,
    DefinitionKey, ForeignLinkInfo, TargetInfo,
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

pub type EffectOpDef = SemaEffectOpDef;
pub type EffectDef = SemaEffectDef;
pub type DataVariantDef = SemaDataVariantDef;
pub type DataDef = SemaDataDef;

#[derive(Debug, Clone)]
pub struct ResumeCtx {
    pub arg: HirTyId,
    pub result: HirTyId,
}

pub struct ModuleState {
    pub(crate) resolved: ResolvedModule,
    binding_ids: HashMap<NameSite, NameBindingId>,
    import_targets: HashMap<Span, ModuleKey>,
}

pub struct RuntimeEnv<'interner, 'env> {
    interner: &'interner mut Interner,
    known: KnownSymbols,
    builtins: Builtins,
    target: Option<TargetInfo>,
    env: Option<&'env dyn SemaEnv>,
}

#[derive(Default)]
pub struct TypingState {
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_effects: HashMap<NameBindingId, EffectRow>,
    binding_schemes: HashMap<NameBindingId, BindingScheme>,
    binding_module_targets: HashMap<NameBindingId, ModuleKey>,
    mutable_bindings: HashSet<NameBindingId>,
    sealed_classes: HashSet<DefinitionKey>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    next_open_row_id: u32,
}

#[derive(Default)]
pub struct DeclState {
    effect_defs: HashMap<Box<str>, EffectDef>,
    data_defs: HashMap<Box<str>, DataDef>,
    class_index: HashMap<Symbol, HirExprId>,
    class_facts_by_name: HashMap<Symbol, ClassFacts>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
}

pub struct FactState {
    diags: SemaDiagList,
    expr_facts: Vec<ExprFacts>,
    pat_facts: Vec<PatFacts>,
    expr_callable_effects: HashMap<HirExprId, EffectRow>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
}

#[derive(Default)]
pub struct ResumeState {
    stack: Vec<ResumeCtx>,
}

pub struct PassBase<'ctx, 'interner, 'env> {
    module: &'ctx mut ModuleState,
    runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
    typing: &'ctx mut TypingState,
    decls: &'ctx mut DeclState,
    facts: &'ctx mut FactState,
}

pub struct CollectPass<'ctx, 'interner, 'env> {
    base: PassBase<'ctx, 'interner, 'env>,
}

pub struct CheckPass<'ctx, 'interner, 'env> {
    base: PassBase<'ctx, 'interner, 'env>,
    resume: &'ctx mut ResumeState,
    expected: Vec<HirTyId>,
    module_stmt_depth: u32,
}

pub fn prepare_module<'interner, 'env>(
    mut resolved: ResolvedModule,
    interner: &'interner mut Interner,
    options: SemaOptions<'env>,
) -> (
    ModuleState,
    RuntimeEnv<'interner, 'env>,
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
    let import_targets = resolved
        .imports
        .iter()
        .map(|import| (import.span, import.to.clone()))
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
            import_targets,
        },
        RuntimeEnv {
            interner,
            known,
            builtins,
            target: options.target.or_else(|| Some(host_target_info())),
            env: options.env,
        },
        TypingState::default(),
        DeclState::default(),
        FactState {
            diags: Vec::new(),
            expr_facts,
            pat_facts,
            expr_callable_effects: HashMap::new(),
            expr_module_targets: HashMap::new(),
        },
        ResumeState::default(),
    )
}

fn host_target_info() -> TargetInfo {
    use std::env::consts::{ARCH, OS};

    let os = match OS {
        "macos" => "mac",
        other => other,
    };
    TargetInfo {
        os: Some(os.into()),
        arch: Some(ARCH.into()),
        env: None,
        abi: None,
        vendor: None,
        features: BTreeSet::default(),
    }
}

pub fn finish_module(
    module: ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: DeclState,
    facts: FactState,
) -> SemaModule {
    let surface = build_module_surface(&module, runtime, typing, &decls);
    SemaModule::from_parts(SemaModuleParts {
        resolved: module.resolved,
        target: runtime.target.clone(),
        gated_bindings: typing.gated_bindings.clone(),
        foreign_links: typing.foreign_links.clone(),
        expr_facts: facts.expr_facts,
        pat_facts: facts.pat_facts,
        expr_module_targets: facts.expr_module_targets,
        effect_defs: decls.effect_defs,
        data_defs: decls.data_defs,
        class_facts: decls.class_facts,
        instance_facts: decls.instance_facts,
        surface,
        diags: facts.diags,
    })
}

impl ModuleState {
    pub(super) fn binding_id_at_site(&self, site: NameSite) -> Option<NameBindingId> {
        self.binding_ids.get(&site).copied()
    }
}

impl<'ctx, 'interner, 'env> PassBase<'ctx, 'interner, 'env> {
    const fn new(
        module: &'ctx mut ModuleState,
        runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
        typing: &'ctx mut TypingState,
        decls: &'ctx mut DeclState,
        facts: &'ctx mut FactState,
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

    pub const fn module_key(&self) -> &ModuleKey {
        &self.module.resolved.module_key
    }

    pub fn static_import_target(&self, span: Span) -> Option<ModuleKey> {
        self.module.import_targets.get(&span).cloned()
    }

    pub fn static_imports(&self) -> Vec<ModuleKey> {
        self.module
            .resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect()
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

    pub const fn target(&self) -> Option<&TargetInfo> {
        self.runtime.target.as_ref()
    }

    pub const fn sema_env(&self) -> Option<&'env dyn SemaEnv> {
        self.runtime.env
    }

    pub const fn interner(&self) -> &Interner {
        self.runtime.interner
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

    pub fn expr_module_target(&self, id: HirExprId) -> Option<&ModuleKey> {
        self.facts.expr_module_targets.get(&id)
    }

    pub fn set_expr_module_target(&mut self, id: HirExprId, target: ModuleKey) {
        let _prev = self.facts.expr_module_targets.insert(id, target);
    }

    pub fn expr_callable_effects(&self, id: HirExprId) -> Option<EffectRow> {
        self.facts.expr_callable_effects.get(&id).cloned()
    }

    pub fn set_expr_callable_effects(&mut self, id: HirExprId, effects: EffectRow) {
        let _prev = self.facts.expr_callable_effects.insert(id, effects);
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
        let site = NameSite::new(self.source_id(), ident.span);
        if let Some(id) = self.module.binding_ids.get(&site).copied() {
            return Some(id);
        }
        // Some decl sites in resolve are recorded on the enclosing syntactic form rather than the
        // identifier token span. Fall back to a name+containment lookup so sema can still attach
        // binding facts (notably for params) to the correct resolved binding.
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .filter_map(|(id, binding)| {
                if binding.name != ident.name || binding.site.source_id != site.source_id {
                    return None;
                }
                let overlaps = binding.site.span.start <= ident.span.end
                    && binding.site.span.end >= ident.span.start;
                if overlaps {
                    Some((id, binding.site.span.len()))
                } else {
                    None
                }
            })
            .min_by_key(|(_, span_len)| *span_len)
            .map(|(id, _)| id)
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

    pub fn is_binding_mutable(&self, id: NameBindingId) -> bool {
        self.typing.mutable_bindings.contains(&id)
    }

    pub fn mark_binding_mutable(&mut self, id: NameBindingId) {
        let _ = self.typing.mutable_bindings.insert(id);
    }

    pub fn binding_effects(&self, id: NameBindingId) -> Option<EffectRow> {
        self.typing.binding_effects.get(&id).cloned()
    }

    pub fn insert_binding_effects(&mut self, id: NameBindingId, effects: EffectRow) {
        let _prev = self.typing.binding_effects.insert(id, effects);
    }

    pub fn binding_scheme(&self, id: NameBindingId) -> Option<&BindingScheme> {
        self.typing.binding_schemes.get(&id)
    }

    pub fn insert_binding_scheme(&mut self, id: NameBindingId, scheme: BindingScheme) {
        let _prev = self.typing.binding_schemes.insert(id, scheme);
    }

    pub fn binding_module_target(&self, id: NameBindingId) -> Option<&ModuleKey> {
        self.typing.binding_module_targets.get(&id)
    }

    pub fn insert_binding_module_target(&mut self, id: NameBindingId, target: ModuleKey) {
        let _prev = self.typing.binding_module_targets.insert(id, target);
    }

    pub fn mark_sealed_class(&mut self, key: DefinitionKey) {
        let _ = self.typing.sealed_classes.insert(key);
    }

    pub fn is_sealed_class(&self, key: &DefinitionKey) -> bool {
        self.typing.sealed_classes.contains(key)
    }

    pub fn mark_gated_binding(&mut self, id: NameBindingId) {
        let _ = self.typing.gated_bindings.insert(id);
    }

    pub fn is_gated_binding(&self, id: NameBindingId) -> bool {
        self.typing.gated_bindings.contains(&id)
    }

    pub fn set_foreign_link(&mut self, binding: NameBindingId, link: ForeignLinkInfo) {
        let _prev = self.typing.foreign_links.insert(binding, link);
    }

    pub fn effect_def(&self, name: &str) -> Option<&EffectDef> {
        self.decls.effect_defs.get(name)
    }

    pub fn insert_effect_def(&mut self, name: impl Into<Box<str>>, def: EffectDef) {
        let _prev = self.decls.effect_defs.insert(name.into(), def);
    }

    pub fn data_def(&self, name: &str) -> Option<&DataDef> {
        self.decls.data_defs.get(name)
    }

    pub const fn data_defs(&self) -> &HashMap<Box<str>, DataDef> {
        &self.decls.data_defs
    }

    pub fn insert_data_def(&mut self, name: impl Into<Box<str>>, def: DataDef) {
        let _prev = self.decls.data_defs.insert(name.into(), def);
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

    pub fn insert_class_facts_by_name(&mut self, name: Symbol, facts: ClassFacts) {
        let _prev = self.decls.class_facts_by_name.insert(name, facts);
    }

    pub fn class_facts(&self, id: HirExprId) -> Option<&ClassFacts> {
        self.decls.class_facts.get(&id)
    }

    pub fn class_facts_by_name(&self, name: Symbol) -> Option<&ClassFacts> {
        self.decls.class_facts_by_name.get(&name)
    }

    pub fn insert_instance_facts(&mut self, id: HirExprId, facts: InstanceFacts) {
        let _prev = self.decls.instance_facts.insert(id, facts);
    }

    pub const fn instance_facts(&self) -> &HashMap<HirExprId, InstanceFacts> {
        &self.decls.instance_facts
    }

    pub fn lit_kind(&self, lit: HirLitId) -> HirLitKind {
        self.lit(lit).kind
    }

    pub fn lit_is_string(&self, lit: HirLitId) -> bool {
        matches!(self.lit_kind(lit), HirLitKind::String { .. })
    }

    pub fn lit_string_value(&self, lit: HirLitId) -> Option<String> {
        match self.lit_kind(lit) {
            HirLitKind::String { value } => Some(value.into()),
            _ => None,
        }
    }

    pub fn diag(&mut self, span: Span, message: &str, label: &str) {
        self.facts
            .diags
            .push(Diag::error(message).with_label(span, self.source_id(), label));
    }

    pub fn fresh_open_row_name(&mut self, base: &str) -> Box<str> {
        let next = self.typing.next_open_row_id;
        self.typing.next_open_row_id = self.typing.next_open_row_id.saturating_add(1);
        format!("{base}#{next}").into_boxed_str()
    }
}

impl RuntimeEnv<'_, '_> {
    pub const fn interner(&self) -> &Interner {
        self.interner
    }
}

impl TypingState {
    pub const fn binding_types(&self) -> &HashMap<NameBindingId, HirTyId> {
        &self.binding_types
    }

    pub const fn binding_schemes(&self) -> &HashMap<NameBindingId, BindingScheme> {
        &self.binding_schemes
    }

    pub const fn binding_module_targets(&self) -> &HashMap<NameBindingId, ModuleKey> {
        &self.binding_module_targets
    }

    pub(super) fn is_gated_binding(&self, id: NameBindingId) -> bool {
        self.gated_bindings.contains(&id)
    }
}

impl DeclState {
    pub fn effect_def(&self, name: &str) -> Option<&EffectDef> {
        self.effect_defs.get(name)
    }

    pub fn data_def(&self, name: &str) -> Option<&DataDef> {
        self.data_defs.get(name)
    }

    pub const fn class_facts_by_name(&self) -> &HashMap<Symbol, ClassFacts> {
        &self.class_facts_by_name
    }

    pub const fn instance_facts(&self) -> &HashMap<HirExprId, InstanceFacts> {
        &self.instance_facts
    }
}

impl<'ctx, 'interner, 'env> CollectPass<'ctx, 'interner, 'env> {
    pub const fn new(
        module: &'ctx mut ModuleState,
        runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
        typing: &'ctx mut TypingState,
        decls: &'ctx mut DeclState,
        facts: &'ctx mut FactState,
    ) -> Self {
        Self {
            base: PassBase::new(module, runtime, typing, decls, facts),
        }
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
        module: &'ctx mut ModuleState,
        runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
        typing: &'ctx mut TypingState,
        decls: &'ctx mut DeclState,
        facts: &'ctx mut FactState,
        resume: &'ctx mut ResumeState,
    ) -> Self {
        Self {
            base: PassBase::new(module, runtime, typing, decls, facts),
            resume,
            expected: Vec::new(),
            module_stmt_depth: 0,
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
    type Target = PassBase<'ctx, 'interner, 'env>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl DerefMut for CheckPass<'_, '_, '_> {
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
