use std::collections::{HashMap, HashSet, VecDeque};

use music_basic::{SourceId, SourceMap, Span, string_lit};
use music_hir::{HirExprId, HirPatId, HirStore};
use music_known::KnownSymbols;
use music_names::{Interner, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol};

use crate::iface::{ModuleExportSummary, SemaImportEnv};
use crate::{SemaError, SemaErrorKind};

use super::env::{
    EffectOpSig, InstanceScheme, InstantiatedScheme, SemObligation, SemObligationKind, SemTyNamed,
    TypeEnv, ValueScheme, substitute_generics,
};
use super::lang::LangItems;
use super::{EffectRow, SemTy, SemTyId, SemTys, binding_by_site, site};

#[derive(Debug, Clone)]
pub struct ResumeCtx {
    pub arg: SemTyId,
    pub result: SemTyId,
}

#[derive(Debug, Clone)]
pub(crate) struct ImportRecordEntry {
    pub scheme: ValueScheme,
    pub effect_family: Option<super::env::EffectFamily>,
    pub class_family: Option<super::env::ClassFamily>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImportRecord {
    pub entries: HashMap<Symbol, ImportRecordEntry>,
}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinTys {
    pub error: SemTyId,
    pub unknown: SemTyId,
    pub any: SemTyId,

    pub type_: SemTyId,
    pub syntax: SemTyId,
    pub empty: SemTyId,
    pub unit: SemTyId,
    pub bool_: SemTyId,
    pub int_: SemTyId,
    pub float_: SemTyId,
    pub string_: SemTyId,

    pub cstring: SemTyId,
    pub cptr: SemTyId,
}

#[derive(Debug, Default)]
pub struct FlowState {
    pub expr_tys: HashMap<HirExprId, SemTyId>,
    pub callable_effs: HashMap<HirExprId, EffectRow>,
    pub import_records: HashMap<HirExprId, ImportRecord>,
    pub instance_schemes: HashMap<HirExprId, InstanceScheme>,
    pub resume_stack: Vec<ResumeCtx>,
}

pub struct CheckerCtx<'a> {
    pub source_id: SourceId,
    pub sources: &'a SourceMap,
    pub interner: &'a mut Interner,
    pub names: &'a NameResolution,
    pub import_env: Option<&'a dyn SemaImportEnv>,
    pub binding_by_site: HashMap<NameSite, NameBindingId>,
    pub import_binding_by_key: HashMap<(Span, Symbol), NameBindingId>,
    pub store: &'a mut HirStore,
    pub errors: &'a mut Vec<SemaError>,
}

pub struct CheckerState {
    pub semtys: SemTys,
    pub env: TypeEnv,
    pub known: KnownSymbols,
    pub lang: LangItems,
    pub builtins: BuiltinTys,
    pub flow: FlowState,
    pub obligations: Vec<SemObligation>,
    pub binding_mut: HashMap<NameBindingId, bool>,
    pub opaque_imports: HashSet<Symbol>,
    pub imported_modules: HashSet<String>,
}

pub struct Checker<'a> {
    pub ctx: CheckerCtx<'a>,
    pub state: CheckerState,
}

impl Checker<'_> {
    pub fn new<'a>(
        source_id: SourceId,
        sources: &'a SourceMap,
        interner: &'a mut Interner,
        names: &'a NameResolution,
        store: &'a mut HirStore,
        errors: &'a mut Vec<SemaError>,
        import_env: Option<&'a dyn SemaImportEnv>,
    ) -> Checker<'a> {
        let known = KnownSymbols::new(interner);
        let mut semtys = SemTys::new();
        let error = semtys.alloc(SemTy::Error);
        let unknown = semtys.alloc(SemTy::Unknown);
        let any = semtys.alloc(SemTy::Any);

        let type_ = builtin_named(&mut semtys, known.type_);
        let syntax = builtin_named(&mut semtys, known.syntax);
        let empty = builtin_named(&mut semtys, known.empty);
        let unit = builtin_named(&mut semtys, known.unit);
        let bool_ = builtin_named(&mut semtys, known.bool_);
        let int_ = builtin_named(&mut semtys, known.int_);
        let float_ = builtin_named(&mut semtys, known.float_);
        let string_ = builtin_named(&mut semtys, known.string_);
        let cstring = builtin_named(&mut semtys, known.cstring);
        let cptr = builtin_named(&mut semtys, known.cptr);

        let mut checker = Checker {
            ctx: CheckerCtx {
                source_id,
                sources,
                interner,
                names,
                import_env,
                binding_by_site: binding_by_site(names),
                import_binding_by_key: HashMap::new(),
                store,
                errors,
            },
            state: CheckerState {
                semtys,
                env: TypeEnv::new(),
                known,
                lang: LangItems::default(),
                builtins: BuiltinTys {
                    error,
                    unknown,
                    any,
                    type_,
                    syntax,
                    empty,
                    unit,
                    bool_,
                    int_,
                    float_,
                    string_,
                    cstring,
                    cptr,
                },
                flow: FlowState::default(),
                obligations: Vec::new(),
                binding_mut: HashMap::new(),
                opaque_imports: HashSet::new(),
                imported_modules: HashSet::new(),
            },
        };

        for (id, binding) in &checker.ctx.names.bindings {
            if !matches!(binding.kind, NameBindingKind::Import { .. }) {
                continue;
            }
            let _prev = checker
                .ctx
                .import_binding_by_key
                .insert((binding.site.span, binding.name), id);
        }

        for (_id, binding) in &checker.ctx.names.bindings {
            let NameBindingKind::Import { opaque: true } = binding.kind else {
                continue;
            };
            let _did_insert = checker.state.opaque_imports.insert(binding.name);
        }

        checker.seed_compiler_prelude_values();
        checker.seed_abort_effect_family();
        checker
    }

    pub fn check_module(&mut self, root: HirExprId) -> ModuleExportSummary {
        self.validate_public_attrs();
        let _ = self.synth_expr(root);
        self.solve_obligations();
        self.finalize_expr_types();
        self.build_export_summary(root)
    }

    pub(crate) fn instantiate_scheme(
        &mut self,
        span: Span,
        scheme: &ValueScheme,
    ) -> InstantiatedScheme {
        let inst = scheme.instantiate(&mut self.state.semtys, span);
        self.state
            .obligations
            .extend(inst.obligations.iter().cloned());
        inst
    }

    fn solve_obligations(&mut self) {
        let mut queue = VecDeque::from(std::mem::take(&mut self.state.obligations));
        while let Some(ob) = queue.pop_front() {
            match ob.kind {
                SemObligationKind::Subtype { left, right } => {
                    if self.obligation_satisfied_subtype(left, &right) {
                        continue;
                    }

                    let found = crate::ty::SemTyDisplay {
                        tys: &self.state.semtys,
                        interner: self.ctx.interner,
                        ty: crate::unify::resolve(&self.state.semtys, left),
                    }
                    .to_string();

                    let bound_ty = self.state.semtys.alloc(SemTy::Named {
                        name: right.name,
                        args: right.args.clone(),
                    });
                    let bound = crate::ty::SemTyDisplay {
                        tys: &self.state.semtys,
                        interner: self.ctx.interner,
                        ty: bound_ty,
                    }
                    .to_string();

                    self.error(
                        ob.span,
                        SemaErrorKind::SubtypeConstraintNotSatisfied { found, bound },
                    );
                }
                SemObligationKind::Implements { ty, class } => {
                    if self.obligation_is_trivially_satisfied(ty) {
                        continue;
                    }

                    if let Some(new_obs) = self.try_apply_instance(ob.span, ty, &class) {
                        for ob in new_obs {
                            queue.push_back(ob);
                        }
                        continue;
                    }

                    self.error(
                        ob.span,
                        SemaErrorKind::InstanceMissingForClass {
                            class: String::from(self.ctx.interner.resolve(class.name)),
                        },
                    );
                }
            }
        }
    }

    fn obligation_is_trivially_satisfied(&self, ty: SemTyId) -> bool {
        let ty = crate::unify::resolve(&self.state.semtys, ty);
        match self.state.semtys.get(ty) {
            SemTy::Error | SemTy::Unknown | SemTy::Any => true,
            SemTy::InferVar(var) => self.state.semtys.infer_binding(*var).is_none(),
            _ => false,
        }
    }

    fn obligation_satisfied_subtype(&self, left: SemTyId, right: &SemTyNamed) -> bool {
        let left = crate::unify::resolve(&self.state.semtys, left);
        match self.state.semtys.get(left) {
            SemTy::Error | SemTy::Unknown | SemTy::Any => return true,
            SemTy::InferVar(var) => {
                if self.state.semtys.infer_binding(*var).is_none() {
                    return true;
                }
            }
            SemTy::Mut { base } => {
                return self.obligation_satisfied_subtype(*base, right);
            }
            _ => {}
        }

        let mut tmp = self.state.semtys.clone();
        let left = crate::unify::resolve(&tmp, left);
        let right_ty = tmp.alloc(SemTy::Named {
            name: right.name,
            args: right.args.clone(),
        });
        crate::unify::unify(&mut tmp, left, right_ty).is_ok()
    }

    fn try_apply_instance(
        &mut self,
        span: Span,
        ty: SemTyId,
        class: &SemTyNamed,
    ) -> Option<Box<[SemObligation]>> {
        let ty = crate::unify::resolve(&self.state.semtys, ty);
        let mut target_args = Vec::with_capacity(1 + class.args.len());
        target_args.push(ty);
        target_args.extend(class.args.iter().copied());

        let instances = self
            .state
            .env
            .instances_for_class(class.name)
            .map(<[InstanceScheme]>::to_vec)?;

        for scheme in instances.iter() {
            let obs = self.try_apply_instance_scheme(span, scheme, &target_args);
            if obs.is_some() {
                return obs;
            }
        }
        None
    }

    fn try_apply_instance_scheme(
        &mut self,
        span: Span,
        scheme: &InstanceScheme,
        target_args: &[SemTyId],
    ) -> Option<Box<[SemObligation]>> {
        if scheme.args.len() != target_args.len() {
            return None;
        }

        let cap = usize::try_from(scheme.generic_count).unwrap_or(0);
        let mut subst = Vec::with_capacity(cap);
        for _ in 0..scheme.generic_count {
            subst.push(self.state.semtys.fresh_infer_var(span));
        }

        let inst_args = scheme
            .args
            .iter()
            .copied()
            .map(|t| substitute_generics(&mut self.state.semtys, t, &subst))
            .collect::<Vec<_>>();

        for (&need, &have) in target_args.iter().zip(inst_args.iter()) {
            if crate::unify::unify(&mut self.state.semtys, need, have).is_err() {
                return None;
            }
        }

        let obs = scheme
            .constraints
            .iter()
            .cloned()
            .map(|c| super::env::instantiate_constraint(&mut self.state.semtys, span, c, &subst))
            .collect::<Vec<_>>()
            .into_boxed_slice();

        Some(obs)
    }

    fn finalize_expr_types(&mut self) {
        for (expr_id, sem_ty) in self.state.flow.expr_tys.clone() {
            let hir_ty = self.lower_sem_ty_to_hir(sem_ty);
            self.ctx.store.exprs.get_mut(expr_id).ty = hir_ty;
        }
    }

    pub fn record_type(&mut self, expr_id: HirExprId, ty: SemTyId) {
        let _prev = self.state.flow.expr_tys.insert(expr_id, ty);
    }

    pub fn error(&mut self, span: Span, kind: SemaErrorKind) {
        self.ctx.errors.push(SemaError {
            kind,
            source_id: self.ctx.source_id,
            span,
        });
    }

    pub fn error_if_opaque_repr_access(&mut self, span: Span, ty_name: Symbol) -> bool {
        if !self.state.opaque_imports.contains(&ty_name) {
            return false;
        }
        self.error(
            span,
            SemaErrorKind::OpaqueTypeBlocksRepresentation {
                name: self.ctx.interner.resolve(ty_name).to_owned(),
            },
        );
        true
    }

    pub fn binding_for_use(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .names
            .refs
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub fn binding_for_def(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .binding_by_site
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub fn mark_binding_mut(&mut self, binding: NameBindingId, mutable: bool) {
        let _prev = self.state.binding_mut.insert(binding, mutable);
    }

    #[must_use]
    pub fn binding_is_mut(&self, binding: NameBindingId) -> bool {
        self.state
            .binding_mut
            .get(&binding)
            .copied()
            .unwrap_or(false)
    }

    pub fn mark_pat_bindings_mut(&mut self, pat: HirPatId, mutable: bool) {
        let mut sites = Vec::new();
        self.collect_bind_sites(pat, &mut sites);
        for span in sites {
            if let Some(binding) = self.binding_for_def(span) {
                self.mark_binding_mut(binding, mutable);
            }
        }
    }

    pub fn slice(&self, span: Span) -> &str {
        let Some(source) = self.ctx.sources.get(self.ctx.source_id) else {
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }

    pub fn decode_string_span(&self, span: Span) -> String {
        string_lit::decode(self.slice(span))
    }

    fn seed_compiler_prelude_values(&mut self) {
        let prelude = self.state.known.compiler_prelude();
        for (id, binding) in &self.ctx.names.bindings {
            if binding.kind != NameBindingKind::Prelude {
                continue;
            }
            if !prelude.contains(&binding.name) {
                continue;
            }
            self.state.env.insert_value(
                id,
                ValueScheme {
                    generic_count: 0,
                    ty: self.state.builtins.type_,
                    declared_effects: None,
                    constraints: Box::new([]),
                },
            );
        }
    }

    fn seed_abort_effect_family(&mut self) {
        let abort_sym = self.state.known.abort;
        let abort_op = self.state.known.abort_op;

        let mut abort_binding = None;
        for (id, binding) in &self.ctx.names.bindings {
            if binding.kind == NameBindingKind::Prelude && binding.name == abort_sym {
                abort_binding = Some(id);
                break;
            }
        }

        let Some(abort_binding) = abort_binding else {
            return;
        };

        let mut ops = HashMap::new();
        let _prev = ops.insert(
            abort_op,
            EffectOpSig {
                params: Box::new([self.state.builtins.string_]),
                ret: self.state.builtins.empty,
            },
        );
        self.state.env.insert_effect_family(abort_binding, 0, ops);
    }
}

fn builtin_named(semtys: &mut SemTys, name: Symbol) -> SemTyId {
    semtys.alloc(SemTy::Named {
        name,
        args: Box::new([]),
    })
}
