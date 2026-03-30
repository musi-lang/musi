use std::collections::HashMap;

use music_basic::{SourceId, SourceMap, Span};
use music_hir::{HirExprId, HirStore};
use music_known::KnownSymbols;
use music_names::{Interner, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol};

use crate::{SemaError, SemaErrorKind};

use super::env::{EffectOpSig, TypeEnv, ValueScheme};
use super::ir;
use super::lang::LangItems;
use super::{EffectRow, SemTy, SemTyId, SemTys, binding_by_site, site};

#[derive(Debug, Clone)]
pub(crate) struct ResumeCtx {
    pub(crate) arg: SemTyId,
    pub(crate) result: SemTyId,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BuiltinTys {
    pub(crate) error: SemTyId,
    pub(crate) unknown: SemTyId,
    pub(crate) any: SemTyId,

    pub(crate) type_: SemTyId,
    pub(crate) syntax: SemTyId,
    pub(crate) empty: SemTyId,
    pub(crate) unit: SemTyId,
    pub(crate) bool_: SemTyId,
    pub(crate) int_: SemTyId,
    pub(crate) float_: SemTyId,
    pub(crate) string_: SemTyId,

    pub(crate) cstring: SemTyId,
    pub(crate) cptr: SemTyId,
}

#[derive(Debug, Default)]
pub(crate) struct FlowState {
    pub(crate) expr_tys: HashMap<HirExprId, SemTyId>,
    pub(crate) callable_effs: HashMap<HirExprId, EffectRow>,
    pub(crate) resume_stack: Vec<ResumeCtx>,
}

pub(crate) struct CheckerCtx<'a> {
    pub(crate) source_id: SourceId,
    pub(crate) sources: &'a SourceMap,
    pub(crate) interner: &'a mut Interner,
    pub(crate) names: &'a NameResolution,
    pub(crate) binding_by_site: HashMap<NameSite, NameBindingId>,
    pub(crate) store: &'a mut HirStore,
    pub(crate) errors: &'a mut Vec<SemaError>,
}

pub(crate) struct CheckerState {
    pub(crate) semtys: SemTys,
    pub(crate) env: TypeEnv,
    pub(crate) known: KnownSymbols,
    pub(crate) lang: LangItems,
    pub(crate) builtins: BuiltinTys,
    pub(crate) flow: FlowState,
    pub(crate) binding_mut: HashMap<NameBindingId, bool>,
    pub(crate) opaque_imports: std::collections::HashSet<Symbol>,
}

pub(crate) struct Checker<'a> {
    pub(crate) ctx: CheckerCtx<'a>,
    pub(crate) state: CheckerState,
}

impl<'a> Checker<'a> {
    pub(crate) fn new(
        source_id: SourceId,
        sources: &'a SourceMap,
        interner: &'a mut Interner,
        names: &'a NameResolution,
        store: &'a mut HirStore,
        errors: &'a mut Vec<SemaError>,
    ) -> Self {
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

        let mut checker = Self {
            ctx: CheckerCtx {
                source_id,
                sources,
                interner,
                names,
                binding_by_site: binding_by_site(names),
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
                binding_mut: HashMap::new(),
                opaque_imports: std::collections::HashSet::new(),
            },
        };

        for (_id, binding) in &checker.ctx.names.bindings {
            let music_names::NameBindingKind::Import { opaque: true } = binding.kind else {
                continue;
            };
            let _did_insert = checker.state.opaque_imports.insert(binding.name);
        }

        checker.seed_compiler_prelude_values();
        checker.seed_abort_effect_family();
        checker
    }

    pub(crate) fn check_module(&mut self, root: HirExprId) -> music_ir::IrModuleInfo {
        self.validate_public_attrs();
        let _ = self.synth_expr(root);
        self.finalize_expr_types();
        ir::build_ir_module(
            self.state.known,
            self.ctx.interner,
            &self.state.semtys,
            &self.state.flow.expr_tys,
            self.ctx.store,
            self.ctx.names,
            self.ctx.source_id,
        )
    }

    fn finalize_expr_types(&mut self) {
        for (expr_id, sem_ty) in self.state.flow.expr_tys.clone() {
            let hir_ty = self.lower_sem_ty_to_hir(sem_ty);
            self.ctx.store.exprs.get_mut(expr_id).ty = hir_ty;
        }
    }

    pub(crate) fn record_type(&mut self, expr_id: HirExprId, ty: SemTyId) {
        let _prev = self.state.flow.expr_tys.insert(expr_id, ty);
    }

    pub(crate) fn error(&mut self, span: Span, kind: SemaErrorKind) {
        self.ctx.errors.push(SemaError {
            kind,
            source_id: self.ctx.source_id,
            span,
        });
    }

    pub(crate) fn error_if_opaque_repr_access(&mut self, span: Span, ty_name: Symbol) -> bool {
        if !self.state.opaque_imports.contains(&ty_name) {
            return false;
        }
        self.error(
            span,
            SemaErrorKind::OpaqueTypeBlocksRepresentation {
                name: self.ctx.interner.resolve(ty_name).to_string(),
            },
        );
        true
    }

    pub(crate) fn binding_for_use(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .names
            .refs
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub(crate) fn binding_for_def(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .binding_by_site
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub(crate) fn mark_binding_mut(&mut self, binding: NameBindingId, mutable: bool) {
        let _prev = self.state.binding_mut.insert(binding, mutable);
    }

    #[must_use]
    pub(crate) fn binding_is_mut(&self, binding: NameBindingId) -> bool {
        self.state
            .binding_mut
            .get(&binding)
            .copied()
            .unwrap_or(false)
    }

    pub(crate) fn mark_pat_bindings_mut(&mut self, pat: music_hir::HirPatId, mutable: bool) {
        let mut sites = Vec::new();
        self.collect_bind_sites(pat, &mut sites);
        for span in sites {
            if let Some(binding) = self.binding_for_def(span) {
                self.mark_binding_mut(binding, mutable);
            }
        }
    }

    pub(crate) fn slice(&self, span: Span) -> &str {
        let Some(source) = self.ctx.sources.get(self.ctx.source_id) else {
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }

    pub(crate) fn decode_string_span(&self, span: Span) -> String {
        music_basic::string_lit::decode(self.slice(span))
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
