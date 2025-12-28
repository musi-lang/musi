use musi_ast::{AstArena, ExprId, Ident};
use musi_basic::diagnostic::{Diagnostic, DiagnosticBag};
use musi_basic::error::IntoMusiError;
use musi_basic::interner::Interner;
use musi_basic::span::Span;

use crate::error::SemaErrorKind;
use crate::semantic::SemanticModel;
use crate::symbol::{ScopeId, SymbolId, SymbolKind, SymbolTable};
use crate::ty_repr::TyRepr;
use crate::unifier::Unifier;

pub struct DeferredTask {
    pub body: ExprId,
    pub scope: ScopeId,
    pub expected_ret: TyRepr,
}

pub struct BindCtx<'a> {
    pub arena: &'a AstArena,
    pub interner: &'a Interner,
    pub model: &'a mut SemanticModel,
    pub symbols: &'a mut SymbolTable,
    pub unifier: &'a mut Unifier,
    pub diags: &'a mut DiagnosticBag,
    pub deferred: &'a mut Vec<DeferredTask>,
    pub in_loop: bool,
    pub in_fn: bool,
}

impl BindCtx<'_> {
    pub fn fork(&mut self) -> (SemanticModel, Unifier, SymbolTable, DiagnosticBag) {
        (
            self.model.fork(),
            self.unifier.fork(),
            self.symbols.fork(),
            DiagnosticBag::default(),
        )
    }

    pub fn merge_forked(
        &mut self,
        model: SemanticModel,
        unifier: Unifier,
        symbols: SymbolTable,
        diags: DiagnosticBag,
        deferred: Vec<DeferredTask>,
    ) {
        self.model.merge(model);
        self.unifier.merge(unifier);
        self.symbols.merge(symbols);
        self.diags.merge(diags);
        self.deferred.extend(deferred);
    }

    pub const fn reenter_scope(&mut self, id: ScopeId) {
        self.symbols.reenter(id);
    }

    pub fn mark_used(&mut self, id: SymbolId) {
        self.symbols.mark_used(id);
    }
}

impl BindCtx<'_> {
    pub(crate) fn define_and_record(
        &mut self,
        name: Ident,
        kind: SymbolKind,
        ty: TyRepr,
        def_span: Span,
        mutable: bool,
    ) -> Result<SymbolId, SymbolId> {
        let res = self.symbols.define(name, kind, ty, def_span, mutable);
        if let Ok(sym_id) = res {
            self.model.set_ident_symbol(name, sym_id);
        }
        res
    }

    pub fn error(&mut self, kind: SemaErrorKind, span: Span) {
        let diag = Diagnostic::from(kind.into_musi_error(span));
        self.diags.add(diag);
    }

    pub fn unify_or_err(&mut self, a: &TyRepr, b: &TyRepr, span: Span) {
        if let Err(err) = self.unifier.unify(a, b) {
            self.error(err, span);
        }
    }
}
