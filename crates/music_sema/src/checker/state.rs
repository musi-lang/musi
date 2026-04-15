use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::{Deref, DerefMut};

use music_arena::{Idx, SliceRange};
use music_base::diag::Diag;
use music_base::{SourceId, Span};
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinder, HirConstraint, HirDim, HirEffectItem,
    HirEffectSet, HirExpr, HirExprId, HirFieldDef, HirHandleClause, HirLit, HirLitId, HirLitKind,
    HirMatchArm, HirMemberDef, HirOrigin, HirParam, HirPat, HirPatId, HirRecordItem,
    HirRecordPatField, HirTemplatePart, HirTy, HirTyField, HirTyId, HirTyKind, HirVariantDef,
    HirVariantFieldDef, HirVariantPatArg,
};
use music_module::ModuleKey;
use music_names::{
    Ident, Interner, KnownSymbols, NameBindingId, NameBindingKind, NameSite, Symbol,
};
use music_resolve::ResolvedModule;

use super::DiagKind;
use super::schemes::BindingScheme;
use super::surface::build_module_surface;
use crate::api::{
    ClassFacts, ConstraintEvidence, ConstraintKey, DefinitionKey, ExprFacts, ForeignLinkInfo,
    InstanceFacts, PatFacts, SemaDataDef, SemaDataVariantDef, SemaDiagList, SemaEffectDef,
    SemaEffectOpDef, SemaEnv, SemaModule, SemaOptions, TargetInfo,
};
use crate::effects::EffectRow;

const SYNTH_SUM_PREFIX: &str = "__sum__";

type BindingIdMap = HashMap<NameSite, NameBindingId>;
type ImportTargetMap = HashMap<Span, ModuleKey>;
type BindingTypeMap = HashMap<NameBindingId, HirTyId>;
type BindingEffectsMap = HashMap<NameBindingId, EffectRow>;
type BindingSchemeMap = HashMap<NameBindingId, BindingScheme>;
type TypeParamKindScope = HashMap<Symbol, HirTyId>;
type TypeParamKindScopeList = Vec<TypeParamKindScope>;
type BindingEvidenceKeyMap = HashMap<NameBindingId, Box<[ConstraintKey]>>;
type BindingModuleTargetMap = HashMap<NameBindingId, ModuleKey>;
type BindingAttachedReceiverMap = HashMap<NameBindingId, AttachedReceiverInfo>;
type SealedClassSet = HashSet<DefinitionKey>;
type GatedBindingSet = HashSet<NameBindingId>;
type ForeignLinkMap = HashMap<NameBindingId, ForeignLinkInfo>;
type UnsafeBindingSet = HashSet<NameBindingId>;
type EffectDefMap = HashMap<Box<str>, EffectDef>;
type DataDefMap = HashMap<Box<str>, DataDef>;
type AttachedMethodMap = HashMap<AttachedMethodKey, Vec<NameBindingId>>;
type ClassIndexMap = HashMap<Symbol, HirExprId>;
type ClassFactsByNameMap = HashMap<Symbol, ClassFacts>;
type ClassFactsMap = HashMap<HirExprId, ClassFacts>;
type InstanceFactsMap = HashMap<HirExprId, InstanceFacts>;
type ExprFactsList = Vec<ExprFacts>;
type PatFactsList = Vec<PatFacts>;
type ExprCallableEffectsMap = HashMap<HirExprId, EffectRow>;
type ExprModuleTargetMap = HashMap<HirExprId, ModuleKey>;
type TypeTestTargetMap = HashMap<HirExprId, HirTyId>;
type ExprEvidenceMap = HashMap<HirExprId, Box<[ConstraintEvidence]>>;
type ExprAttachedBindingMap = HashMap<HirExprId, NameBindingId>;
type ResumeCtxList = Vec<ResumeCtx>;
type ExpectedTyList = Vec<HirTyId>;
type EvidenceScope = HashMap<ConstraintKey, ConstraintEvidence>;
type EvidenceScopeList = Vec<EvidenceScope>;
type StaticImportList = Vec<ModuleKey>;
type ExprIdList = Vec<HirExprId>;
type ArgList = Vec<HirArg>;
type DimList = Vec<HirDim>;
type TyIdList = Vec<HirTyId>;
type TyFieldList = Vec<HirTyField>;
type ArrayItemList = Vec<HirArrayItem>;
type RecordItemList = Vec<HirRecordItem>;
type ParamList = Vec<HirParam>;
type AttrList = Vec<HirAttr>;
type AttrArgList = Vec<HirAttrArg>;
type MemberDefList = Vec<HirMemberDef>;
type HandleClauseList = Vec<HirHandleClause>;
type MatchArmList = Vec<HirMatchArm>;
type ConstraintList = Vec<HirConstraint>;
type VariantDefList = Vec<HirVariantDef>;
type VariantFieldDefList = Vec<HirVariantFieldDef>;
type FieldDefList = Vec<HirFieldDef>;
type EffectItemList = Vec<HirEffectItem>;
type PatIdList = Vec<HirPatId>;
type RecordPatFieldList = Vec<HirRecordPatField>;
type VariantPatArgList = Vec<HirVariantPatArg>;
type IdentList = Vec<Ident>;
type BinderList = Vec<HirBinder>;
type TemplatePartList = Vec<HirTemplatePart>;
type TypeParamKindList = Vec<(Symbol, HirTyId)>;

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
    pub nat: HirTyId,
    pub int_: HirTyId,
    pub float_: HirTyId,
    pub string_: HirTyId,
    pub cstring: HirTyId,
    pub cptr: HirTyId,
}

impl Builtins {
    fn from_resolved(resolved: &mut ResolvedModule, _known: KnownSymbols) -> Self {
        Self {
            error: alloc_builtin(resolved, HirTyKind::Error),
            unknown: alloc_builtin(resolved, HirTyKind::Unknown),
            type_: alloc_builtin(resolved, HirTyKind::Type),
            module: alloc_builtin(resolved, HirTyKind::Module),
            syntax: alloc_builtin(resolved, HirTyKind::Syntax),
            any: alloc_builtin(resolved, HirTyKind::Any),
            empty: alloc_builtin(resolved, HirTyKind::Empty),
            unit: alloc_builtin(resolved, HirTyKind::Unit),
            bool_: alloc_builtin(resolved, HirTyKind::Bool),
            nat: alloc_builtin(resolved, HirTyKind::Nat),
            int_: alloc_builtin(resolved, HirTyKind::Int),
            float_: alloc_builtin(resolved, HirTyKind::Float),
            string_: alloc_builtin(resolved, HirTyKind::String),
            cstring: alloc_builtin(resolved, HirTyKind::CString),
            cptr: alloc_builtin(resolved, HirTyKind::CPtr),
        }
    }
}

pub type EffectOpDef = SemaEffectOpDef;
pub type EffectDef = SemaEffectDef;
pub type DataVariantDef = SemaDataVariantDef;
pub type DataDef = SemaDataDef;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct AttachedMethodKey {
    receiver: HirTyId,
    name: Symbol,
}

impl AttachedMethodKey {
    const fn new(receiver: HirTyId, name: Symbol) -> Self {
        Self { receiver, name }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AttachedReceiverInfo {
    pub receiver: HirTyId,
    pub is_mut: bool,
}

impl AttachedReceiverInfo {
    const fn new(receiver: HirTyId, is_mut: bool) -> Self {
        Self { receiver, is_mut }
    }
}

#[derive(Debug, Clone)]
pub struct ResumeCtx {
    pub arg: HirTyId,
    pub result: HirTyId,
}

impl ResumeCtx {
    #[must_use]
    pub const fn new(arg: HirTyId, result: HirTyId) -> Self {
        Self { arg, result }
    }
}

pub struct ModuleState {
    pub(crate) resolved: ResolvedModule,
    binding_ids: BindingIdMap,
    import_targets: ImportTargetMap,
}

impl ModuleState {
    const fn new(
        resolved: ResolvedModule,
        binding_ids: BindingIdMap,
        import_targets: ImportTargetMap,
    ) -> Self {
        Self {
            resolved,
            binding_ids,
            import_targets,
        }
    }
}

pub struct RuntimeEnv<'interner, 'env> {
    interner: &'interner mut Interner,
    known: KnownSymbols,
    builtins: Builtins,
    target: Option<TargetInfo>,
    env: Option<&'env dyn SemaEnv>,
}

impl<'interner, 'env> RuntimeEnv<'interner, 'env> {
    fn new(interner: &'interner mut Interner, known: KnownSymbols, builtins: Builtins) -> Self {
        Self {
            interner,
            known,
            builtins,
            target: None,
            env: None,
        }
    }

    fn with_target(mut self, target: TargetInfo) -> Self {
        self.target = Some(target);
        self
    }

    const fn with_env(mut self, env: Option<&'env dyn SemaEnv>) -> Self {
        self.env = env;
        self
    }
}

#[derive(Default)]
pub struct TypingState {
    binding_types: BindingTypeMap,
    binding_effects: BindingEffectsMap,
    binding_schemes: BindingSchemeMap,
    type_param_kind_scopes: TypeParamKindScopeList,
    binding_evidence_keys: BindingEvidenceKeyMap,
    binding_module_targets: BindingModuleTargetMap,
    binding_attached_receivers: BindingAttachedReceiverMap,
    sealed_classes: SealedClassSet,
    gated_bindings: GatedBindingSet,
    foreign_links: ForeignLinkMap,
    unsafe_bindings: UnsafeBindingSet,
    next_open_row_id: u32,
}

impl TypingState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Default)]
pub struct DeclState {
    effect_defs: EffectDefMap,
    data_defs: DataDefMap,
    attached_methods: AttachedMethodMap,
    class_index: ClassIndexMap,
    class_facts_by_name: ClassFactsByNameMap,
    class_facts: ClassFactsMap,
    instance_facts: InstanceFactsMap,
}

impl DeclState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct FactState {
    diags: SemaDiagList,
    expr_facts: ExprFactsList,
    pat_facts: PatFactsList,
    expr_callable_effects: ExprCallableEffectsMap,
    expr_module_targets: ExprModuleTargetMap,
    type_test_targets: TypeTestTargetMap,
    expr_evidence: ExprEvidenceMap,
    expr_attached_bindings: ExprAttachedBindingMap,
}

impl FactState {
    #[must_use]
    fn new(expr_facts: ExprFactsList, pat_facts: PatFactsList) -> Self {
        Self {
            diags: Vec::new(),
            expr_facts,
            pat_facts,
            expr_callable_effects: HashMap::new(),
            expr_module_targets: HashMap::new(),
            type_test_targets: HashMap::new(),
            expr_evidence: HashMap::new(),
            expr_attached_bindings: HashMap::new(),
        }
    }
}

#[derive(Default)]
pub struct ResumeState {
    stack: ResumeCtxList,
}

impl ResumeState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct PassBase<'ctx, 'interner, 'env> {
    module: &'ctx mut ModuleState,
    runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
    typing: &'ctx mut TypingState,
    decls: &'ctx mut DeclState,
    facts: &'ctx mut FactState,
}

pub struct PassParts<'ctx, 'interner, 'env> {
    pub module: &'ctx mut ModuleState,
    pub runtime: &'ctx mut RuntimeEnv<'interner, 'env>,
    pub typing: &'ctx mut TypingState,
    pub decls: &'ctx mut DeclState,
    pub facts: &'ctx mut FactState,
}

pub struct CollectPass<'ctx, 'interner, 'env> {
    base: PassBase<'ctx, 'interner, 'env>,
}

pub struct CheckPass<'ctx, 'interner, 'env> {
    collect: CollectPass<'ctx, 'interner, 'env>,
    resume: &'ctx mut ResumeState,
    expected: ExpectedTyList,
    evidence_scopes: EvidenceScopeList,
    module_stmt_depth: u32,
    unsafe_depth: u32,
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
    let builtins = Builtins::from_resolved(&mut resolved, known);
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
        ExprFacts::new(builtins.unknown, EffectRow::empty());
        resolved.module.store.exprs.len()
    ];
    let pat_facts = vec![PatFacts::new(builtins.unknown); resolved.module.store.pats.len()];

    let mut decls = DeclState::new();
    let module_key = resolved.module_key.clone();
    seed_builtin_data_defs(&mut decls, &module_key);

    (
        ModuleState::new(resolved, binding_ids, import_targets),
        RuntimeEnv::new(interner, known, builtins)
            .with_target(options.target.unwrap_or_else(host_target_info))
            .with_env(options.env),
        TypingState::new(),
        decls,
        FactState::new(expr_facts, pat_facts),
        ResumeState::new(),
    )
}

fn seed_builtin_data_defs(decls: &mut DeclState, module: &ModuleKey) {
    let bool_variants = BTreeMap::from([
        (
            "False".into(),
            SemaDataVariantDef::new(None, None, Box::default(), Box::default()),
        ),
        (
            "True".into(),
            SemaDataVariantDef::new(None, None, Box::default(), Box::default()),
        ),
    ]);
    let _ = decls.data_defs.insert(
        "Bool".into(),
        SemaDataDef::new(
            DefinitionKey::new(module.clone(), "Bool"),
            bool_variants,
            None,
            None,
            None,
            false,
        ),
    );
}

fn host_target_info() -> TargetInfo {
    use std::env::consts::{ARCH, OS};

    let os = match OS {
        "macos" => "mac",
        other => other,
    };
    TargetInfo::new().with_os(os).with_arch(ARCH)
}

pub fn finish_module(
    module: ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: DeclState,
    facts: FactState,
) -> SemaModule {
    let surface = build_module_surface(&module, runtime, typing, &decls);
    crate::build_sema_module(crate::SemaModuleBuild {
        resolved: module.resolved,
        context: crate::SemaContextBuild {
            target: runtime.target.clone(),
            gated_bindings: typing.gated_bindings.clone(),
            foreign_links: typing.foreign_links.clone(),
            binding_types: typing.binding_types().clone(),
            binding_schemes: typing.binding_schemes().clone(),
            binding_evidence_keys: typing.binding_evidence_keys().clone(),
            binding_module_targets: typing.binding_module_targets().clone(),
        },
        facts: crate::SemaFactsBuild {
            expr_facts: facts.expr_facts,
            pat_facts: facts.pat_facts,
            expr_module_targets: facts.expr_module_targets,
            type_test_targets: facts.type_test_targets,
            expr_evidence: facts.expr_evidence,
            expr_attached_bindings: facts.expr_attached_bindings,
        },
        decls: crate::SemaDeclsBuild {
            effect_defs: decls.effect_defs,
            data_defs: decls.data_defs,
            class_facts: decls.class_facts,
            instance_facts: decls.instance_facts,
        },
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
    pub(super) const fn new(parts: PassParts<'ctx, 'interner, 'env>) -> Self {
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

impl PassBase<'_, '_, '_> {
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

    pub const fn interner(&self) -> &Interner {
        self.runtime.interner
    }

    pub fn intern(&mut self, text: &str) -> Symbol {
        self.runtime.interner.intern(text)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.runtime.interner.resolve(symbol)
    }
}

impl<'env> PassBase<'_, '_, 'env> {
    pub const fn sema_env(&self) -> Option<&'env dyn SemaEnv> {
        self.runtime.env
    }
}

impl PassBase<'_, '_, '_> {
    pub fn expr_ids(&self, range: SliceRange<HirExprId>) -> ExprIdList {
        self.module
            .resolved
            .module
            .store
            .expr_ids
            .get(range)
            .to_vec()
    }

    pub fn args(&self, range: SliceRange<HirArg>) -> ArgList {
        self.module.resolved.module.store.args.get(range).to_vec()
    }

    pub fn dims(&self, range: SliceRange<HirDim>) -> DimList {
        self.module.resolved.module.store.dims.get(range).to_vec()
    }

    pub fn ty_ids(&self, range: SliceRange<HirTyId>) -> TyIdList {
        self.module.resolved.module.store.ty_ids.get(range).to_vec()
    }

    pub fn ty_fields(&self, range: SliceRange<HirTyField>) -> TyFieldList {
        self.module
            .resolved
            .module
            .store
            .ty_fields
            .get(range)
            .to_vec()
    }

    pub fn array_items(&self, range: SliceRange<HirArrayItem>) -> ArrayItemList {
        self.module
            .resolved
            .module
            .store
            .array_items
            .get(range)
            .to_vec()
    }

    pub fn record_items(&self, range: SliceRange<HirRecordItem>) -> RecordItemList {
        self.module
            .resolved
            .module
            .store
            .record_items
            .get(range)
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn params(&self, range: SliceRange<HirParam>) -> ParamList {
        self.module.resolved.module.store.params.get(range).to_vec()
    }

    pub fn attrs(&self, range: SliceRange<HirAttr>) -> AttrList {
        self.module.resolved.module.store.attrs.get(range).to_vec()
    }

    pub fn attr_args(&self, range: SliceRange<HirAttrArg>) -> AttrArgList {
        self.module
            .resolved
            .module
            .store
            .attr_args
            .get(range)
            .to_vec()
    }

    pub fn members(&self, range: SliceRange<HirMemberDef>) -> MemberDefList {
        self.module
            .resolved
            .module
            .store
            .members
            .get(range)
            .to_vec()
    }

    pub fn handle_clauses(&self, range: SliceRange<HirHandleClause>) -> HandleClauseList {
        self.module
            .resolved
            .module
            .store
            .handle_clauses
            .get(range)
            .to_vec()
    }

    pub fn match_arms(&self, range: SliceRange<HirMatchArm>) -> MatchArmList {
        self.module
            .resolved
            .module
            .store
            .match_arms
            .get(range)
            .to_vec()
    }

    pub fn constraints(&self, range: SliceRange<HirConstraint>) -> ConstraintList {
        self.module
            .resolved
            .module
            .store
            .constraints
            .get(range)
            .to_vec()
    }

    pub fn variants(&self, range: SliceRange<HirVariantDef>) -> VariantDefList {
        self.module
            .resolved
            .module
            .store
            .variants
            .get(range)
            .to_vec()
    }

    pub fn variant_fields(&self, range: SliceRange<HirVariantFieldDef>) -> VariantFieldDefList {
        self.module
            .resolved
            .module
            .store
            .variant_fields
            .get(range)
            .to_vec()
    }

    pub fn fields(&self, range: SliceRange<HirFieldDef>) -> FieldDefList {
        self.module.resolved.module.store.fields.get(range).to_vec()
    }

    pub fn effect_items(&self, set: &HirEffectSet) -> EffectItemList {
        self.module
            .resolved
            .module
            .store
            .effect_items
            .get(set.items.clone())
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn pat_ids(&self, range: SliceRange<HirPatId>) -> PatIdList {
        self.module
            .resolved
            .module
            .store
            .pat_ids
            .get(range)
            .to_vec()
    }

    pub fn record_pat_fields(&self, range: SliceRange<HirRecordPatField>) -> RecordPatFieldList {
        self.module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(range)
            .to_vec()
    }

    pub fn variant_pat_args(&self, range: SliceRange<HirVariantPatArg>) -> VariantPatArgList {
        self.module
            .resolved
            .module
            .store
            .variant_pat_args
            .get(range)
            .to_vec()
    }

    pub fn idents(&self, range: SliceRange<Ident>) -> IdentList {
        self.module.resolved.module.store.idents.get(range).to_vec()
    }

    pub fn binders(&self, range: SliceRange<HirBinder>) -> BinderList {
        self.module
            .resolved
            .module
            .store
            .binders
            .get(range)
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn lower_type_param_kinds(&mut self, range: SliceRange<HirBinder>) -> TypeParamKindList {
        let builtins = self.builtins();
        self.binders(range)
            .into_iter()
            .map(|binder| {
                let kind = binder.ty.map_or(builtins.type_, |expr| {
                    let origin = self.expr(expr).origin;
                    self.lower_type_expr(expr, origin)
                });
                (binder.name.name, kind)
            })
            .collect()
    }

    pub fn push_type_param_kinds(&mut self, kinds: &[(Symbol, HirTyId)]) {
        self.typing
            .type_param_kind_scopes
            .push(kinds.iter().copied().collect());
    }

    pub fn pop_type_param_kinds(&mut self) {
        let _ = self.typing.type_param_kind_scopes.pop();
    }

    pub fn type_constructor_scheme_arity(&self, symbol: Symbol) -> Option<usize> {
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .find(|(id, binding)| {
                binding.name == symbol && self.typing.binding_schemes.contains_key(id)
            })
            .and_then(|(id, _)| self.typing.binding_schemes.get(&id))
            .map(|scheme| scheme.type_params.len())
    }

    pub fn type_param_kind(&self, symbol: Symbol) -> Option<HirTyId> {
        self.typing
            .type_param_kind_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&symbol).copied())
    }

    pub fn template_parts(&self, range: SliceRange<HirTemplatePart>) -> TemplatePartList {
        self.module
            .resolved
            .module
            .store
            .template_parts
            .get(range)
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
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

    pub fn set_type_test_target(&mut self, id: HirExprId, target: HirTyId) {
        let _prev = self.facts.type_test_targets.insert(id, target);
    }

    pub fn set_expr_evidence(
        &mut self,
        id: HirExprId,
        evidence: impl Into<Box<[ConstraintEvidence]>>,
    ) {
        let _prev = self.facts.expr_evidence.insert(id, evidence.into());
    }

    pub fn set_expr_attached_binding(&mut self, id: HirExprId, binding: NameBindingId) {
        let _prev = self.facts.expr_attached_bindings.insert(id, binding);
    }

    pub fn expr_attached_binding(&self, id: HirExprId) -> Option<NameBindingId> {
        self.facts.expr_attached_bindings.get(&id).copied()
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
        self.module
            .resolved
            .module
            .store
            .alloc_ty(HirTy::new(HirOrigin::dummy(), kind))
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
}

impl PassBase<'_, '_, '_> {
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
}

impl PassBase<'_, '_, '_> {
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

    pub fn binding_scheme(&self, id: NameBindingId) -> Option<&BindingScheme> {
        self.typing.binding_schemes.get(&id)
    }

    pub fn insert_binding_scheme(&mut self, id: NameBindingId, scheme: BindingScheme) {
        let _prev = self.typing.binding_schemes.insert(id, scheme);
    }

    pub fn set_binding_evidence_keys(
        &mut self,
        id: NameBindingId,
        keys: impl Into<Box<[ConstraintKey]>>,
    ) {
        let _prev = self.typing.binding_evidence_keys.insert(id, keys.into());
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

    pub fn mark_unsafe_binding(&mut self, binding: NameBindingId) {
        let _ = self.typing.unsafe_bindings.insert(binding);
    }

    pub fn is_unsafe_binding(&self, binding: NameBindingId) -> bool {
        self.typing.unsafe_bindings.contains(&binding)
    }
}

impl PassBase<'_, '_, '_> {
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

    pub fn insert_attached_method_binding(
        &mut self,
        receiver: HirTyId,
        name: Symbol,
        binding: NameBindingId,
        is_mut: bool,
    ) {
        let _prev = self
            .typing
            .binding_attached_receivers
            .insert(binding, AttachedReceiverInfo::new(receiver, is_mut));
        self.decls
            .attached_methods
            .entry(AttachedMethodKey::new(receiver, name))
            .or_default()
            .push(binding);
    }

    pub fn attached_method_bindings(&self, receiver: HirTyId, name: Symbol) -> &[NameBindingId] {
        self.decls
            .attached_methods
            .get(&AttachedMethodKey::new(receiver, name))
            .map_or(&[], Vec::as_slice)
    }

    pub fn ensure_sum_data_def(&mut self, left: HirTyId, right: HirTyId) -> Box<str> {
        let name: Box<str> = format!("{SYNTH_SUM_PREFIX}{}_{}", left.raw(), right.raw()).into();
        if self.decls.data_defs.contains_key(name.as_ref()) {
            return name;
        }

        let key = DefinitionKey::new(self.module_key().clone(), name.clone());
        let variants = BTreeMap::from([
            (
                "Left".into(),
                SemaDataVariantDef::new(
                    Some(left),
                    None,
                    vec![left].into_boxed_slice(),
                    Box::default(),
                ),
            ),
            (
                "Right".into(),
                SemaDataVariantDef::new(
                    Some(right),
                    None,
                    vec![right].into_boxed_slice(),
                    Box::default(),
                ),
            ),
        ]);
        let _prev = self.decls.data_defs.insert(
            name.clone(),
            SemaDataDef::new(key, variants, None, None, None, false),
        );
        name
    }
}

impl PassBase<'_, '_, '_> {
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
}

impl PassBase<'_, '_, '_> {
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

    pub fn diag_builder(&self, span: Span, kind: DiagKind, label: &str) -> Diag {
        let label = if label.is_empty() {
            kind.label()
        } else {
            label
        };
        let mut diag = Diag::error(kind.message())
            .with_code(kind.code())
            .with_label(span, self.source_id(), label);
        if let Some(hint) = kind.hint() {
            diag = diag.with_hint(hint);
        }
        diag
    }

    pub fn push_diag(&mut self, diag: Diag) {
        self.facts.diags.push(diag);
    }

    pub fn diag(&mut self, span: Span, kind: DiagKind, label: &str) {
        self.push_diag(self.diag_builder(span, kind, label));
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

    pub const fn binding_evidence_keys(&self) -> &HashMap<NameBindingId, Box<[ConstraintKey]>> {
        &self.binding_evidence_keys
    }

    pub const fn binding_module_targets(&self) -> &HashMap<NameBindingId, ModuleKey> {
        &self.binding_module_targets
    }

    pub fn binding_attached_receiver(&self, id: NameBindingId) -> Option<AttachedReceiverInfo> {
        self.binding_attached_receivers.get(&id).copied()
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
            evidence_scopes: Vec::new(),
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

    pub fn push_evidence_scope(&mut self, scope: EvidenceScope) {
        self.evidence_scopes.push(scope);
    }

    pub fn pop_evidence_scope(&mut self) -> Option<EvidenceScope> {
        self.evidence_scopes.pop()
    }

    pub fn resolve_in_scope_evidence(&self, key: &ConstraintKey) -> Option<ConstraintEvidence> {
        self.evidence_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(key).cloned())
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

fn alloc_builtin(resolved: &mut ResolvedModule, kind: HirTyKind) -> HirTyId {
    resolved
        .module
        .store
        .alloc_ty(HirTy::new(HirOrigin::dummy(), kind))
}

fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
