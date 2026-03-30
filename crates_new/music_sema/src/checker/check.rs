use std::collections::HashMap;

use music_basic::{SourceId, SourceMap, Span};
use music_hir::{HirExprId, HirStore};
use music_known::KnownSymbols;
use music_names::{Interner, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol};

use crate::{SemaError, SemaErrorKind};

use super::env::{EffectOpSig, TypeEnv, ValueScheme};
use super::lang::LangItems;
use super::{EffectRow, SemTy, SemTyId, SemTys, binding_by_site, site};

#[derive(Debug, Clone)]
pub(super) struct ResumeCtx {
    pub(super) arg: SemTyId,
    pub(super) result: SemTyId,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct BuiltinTys {
    pub(super) error: SemTyId,
    pub(super) unknown: SemTyId,
    pub(super) any: SemTyId,

    pub(super) type_: SemTyId,
    pub(super) empty: SemTyId,
    pub(super) unit: SemTyId,
    pub(super) bool_: SemTyId,
    pub(super) int_: SemTyId,
    pub(super) float_: SemTyId,
    pub(super) string_: SemTyId,

    pub(super) cstring: SemTyId,
    pub(super) cptr: SemTyId,
}

#[derive(Debug, Default)]
pub(super) struct FlowState {
    pub(super) expr_tys: HashMap<HirExprId, SemTyId>,
    pub(super) callable_effs: HashMap<HirExprId, EffectRow>,
    pub(super) resume_stack: Vec<ResumeCtx>,
}

pub(super) struct CheckerCtx<'a> {
    pub(super) source_id: SourceId,
    pub(super) sources: &'a SourceMap,
    pub(super) interner: &'a mut Interner,
    pub(super) names: &'a NameResolution,
    pub(super) binding_by_site: HashMap<NameSite, NameBindingId>,
    pub(super) store: &'a mut HirStore,
    pub(super) errors: &'a mut Vec<SemaError>,
}

pub(super) struct CheckerState {
    pub(super) semtys: SemTys,
    pub(super) env: TypeEnv,
    pub(super) known: KnownSymbols,
    pub(super) lang: LangItems,
    pub(super) builtins: BuiltinTys,
    pub(super) flow: FlowState,
}

pub(super) struct Checker<'a> {
    pub(super) ctx: CheckerCtx<'a>,
    pub(super) state: CheckerState,
}

impl<'a> Checker<'a> {
    pub(super) fn new(
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
            },
        };

        checker.seed_compiler_prelude_values();
        checker.seed_abort_effect_family();
        checker
    }

    pub(super) fn check_module(&mut self, root: HirExprId) {
        let _ = self.synth_expr(root);
        self.finalize_expr_types();
    }

    fn finalize_expr_types(&mut self) {
        for (expr_id, sem_ty) in self.state.flow.expr_tys.clone() {
            let hir_ty = self.lower_sem_ty_to_hir(sem_ty);
            self.ctx.store.exprs.get_mut(expr_id).ty = hir_ty;
        }
    }

    pub(super) fn record_type(&mut self, expr_id: HirExprId, ty: SemTyId) {
        let _prev = self.state.flow.expr_tys.insert(expr_id, ty);
    }

    pub(super) fn error(&mut self, span: Span, kind: SemaErrorKind) {
        self.ctx.errors.push(SemaError {
            kind,
            source_id: self.ctx.source_id,
            span,
        });
    }

    pub(super) fn binding_for_use(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .names
            .refs
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub(super) fn binding_for_def(&self, span: Span) -> Option<NameBindingId> {
        self.ctx
            .binding_by_site
            .get(&site(self.ctx.source_id, span))
            .copied()
    }

    pub(super) fn slice(&self, span: Span) -> &str {
        let Some(source) = self.ctx.sources.get(self.ctx.source_id) else {
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }

    pub(super) fn decode_string_span(&self, span: Span) -> String {
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
