use musi_ast::AstArena;
use musi_basic::diagnostic::{Diagnostic, DiagnosticBag};
use musi_basic::error::IntoMusiError;
use musi_basic::interner::Interner;
use musi_basic::span::Span;

use crate::SymbolId;
use crate::error::SemaErrorKind;
use crate::semantic::SemanticModel;
use crate::symbol::SymbolKind;
use crate::symbol::SymbolTable;
use crate::ty_repr::TyRepr;
use crate::unifier::Unifier;
use musi_ast::Ident;

pub struct BindCtx<'a> {
    pub arena: &'a AstArena,
    pub interner: &'a Interner,
    pub model: &'a mut SemanticModel,
    pub symbols: &'a mut SymbolTable,
    pub unifier: &'a mut Unifier,
    pub diags: &'a mut DiagnosticBag,
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

    pub fn with_forked<T>(
        &mut self,
        model: &mut SemanticModel,
        unifier: &mut Unifier,
        symbols: &mut SymbolTable,
        diags: &mut DiagnosticBag,
        f: impl FnOnce(&mut BindCtx) -> T,
    ) -> T {
        let mut ctx = BindCtx {
            arena: self.arena,
            interner: self.interner,
            model,
            symbols,
            unifier,
            diags,
            in_loop: self.in_loop,
            in_fn: self.in_fn,
        };
        f(&mut ctx)
    }

    pub fn merge_forked(
        &mut self,
        model: SemanticModel,
        unifier: Unifier,
        symbols: SymbolTable,
        diags: DiagnosticBag,
    ) {
        self.model.merge(model);
        self.unifier.merge(unifier);
        self.symbols.merge(symbols);
        self.diags.merge(diags);
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
