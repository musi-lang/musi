use std::collections::HashMap;

use music_basic::{SourceId, Span};
use music_hir::{HirExprId, HirStore};
use music_names::{Interner, NameBindingId, NameResolution, NameSite, Symbol};

use crate::{SemaError, SemaErrorKind};

use super::{EffectRow, SemTy, SemTyId, SemTys, binding_by_site, site};
use super::env::TypeEnv;

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
    pub(super) unit: SemTyId,
    pub(super) bool_: SemTyId,
    pub(super) int_: SemTyId,
    pub(super) float_: SemTyId,
    pub(super) string_: SemTyId,
}

#[derive(Debug, Default)]
pub(super) struct FlowState {
    pub(super) expr_tys: HashMap<HirExprId, SemTyId>,
    pub(super) callable_effs: HashMap<HirExprId, EffectRow>,
    pub(super) resume_stack: Vec<ResumeCtx>,
}

pub(super) struct CheckerCtx<'a> {
    pub(super) source_id: SourceId,
    pub(super) interner: &'a mut Interner,
    pub(super) names: &'a NameResolution,
    pub(super) binding_by_site: HashMap<NameSite, NameBindingId>,
    pub(super) store: &'a mut HirStore,
    pub(super) errors: &'a mut Vec<SemaError>,
}

pub(super) struct CheckerState {
    pub(super) semtys: SemTys,
    pub(super) env: TypeEnv,
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
        interner: &'a mut Interner,
        names: &'a NameResolution,
        store: &'a mut HirStore,
        errors: &'a mut Vec<SemaError>,
    ) -> Self {
        let mut semtys = SemTys::new();
        let error = semtys.alloc(SemTy::Error);
        let unknown = semtys.alloc(SemTy::Unknown);
        let any = semtys.alloc(SemTy::Any);

        let type_ = builtin_named(&mut semtys, interner, "Type");
        let unit = builtin_named(&mut semtys, interner, "Unit");
        let bool_ = builtin_named(&mut semtys, interner, "Bool");
        let int_ = builtin_named(&mut semtys, interner, "Int");
        let float_ = builtin_named(&mut semtys, interner, "Float");
        let string_ = builtin_named(&mut semtys, interner, "String");

        Self {
            ctx: CheckerCtx {
                source_id,
                interner,
                names,
                binding_by_site: binding_by_site(names),
                store,
                errors,
            },
            state: CheckerState {
                semtys,
                env: TypeEnv::new(),
                builtins: BuiltinTys {
                    error,
                    unknown,
                    any,
                    type_,
                    unit,
                    bool_,
                    int_,
                    float_,
                    string_,
                },
                flow: FlowState::default(),
            },
        }
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

    pub(super) fn sym(&mut self, name: &str) -> Symbol {
        self.ctx.interner.intern(name)
    }
}

fn builtin_named(semtys: &mut SemTys, interner: &mut Interner, name: &str) -> SemTyId {
    let sym = interner.intern(name);
    semtys.alloc(SemTy::Named {
        name: sym,
        args: Box::new([]),
    })
}
