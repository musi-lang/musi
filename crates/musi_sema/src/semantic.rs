use musi_ast::{AstArena, AstVisitor, ExprId, Ident, PatId, Prog, TyExprId, TyExprKind};
use musi_basic::span::Span;

use std::collections::HashMap;

use crate::symbol::SymbolId;
use crate::ty_repr::{TyRepr, TyReprKind};
use crate::{Symbol, SymbolKind, SymbolTable};

#[derive(Debug)]
pub struct SemanticModel {
    expr_types: Vec<Option<TyRepr>>,
    expr_symbols: Vec<Option<SymbolId>>,
    pat_types: Vec<Option<TyRepr>>,
    pat_symbols: Vec<Option<SymbolId>>,
    ty_expr_types: Vec<Option<TyRepr>>,
    ty_expr_symbols: Vec<Option<SymbolId>>,
    ident_symbols: HashMap<Span, SymbolId>,
}

impl SemanticModel {
    #[must_use]
    pub fn new(expr_count: usize, pat_count: usize, ty_expr_count: usize) -> Self {
        Self {
            expr_types: vec![None; expr_count],
            expr_symbols: vec![None; expr_count],
            pat_types: vec![None; pat_count],
            pat_symbols: vec![None; pat_count],
            ty_expr_types: vec![None; ty_expr_count],
            ty_expr_symbols: vec![None; ty_expr_count],
            ident_symbols: HashMap::new(),
        }
    }

    #[must_use]
    pub fn type_of_expr(&self, id: ExprId) -> Option<&TyRepr> {
        self.expr_types.get(id.as_usize()).and_then(|t| t.as_ref())
    }

    #[must_use]
    pub fn symbol_of_expr(&self, id: ExprId) -> Option<SymbolId> {
        self.expr_symbols.get(id.as_usize()).copied().flatten()
    }

    pub fn set_expr_type(&mut self, id: ExprId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.expr_types.len() {
            self.expr_types[idx] = Some(ty);
        }
    }

    pub fn set_expr_symbol(&mut self, id: ExprId, symbol: SymbolId) {
        let idx = id.as_usize();
        if idx < self.expr_symbols.len() {
            self.expr_symbols[idx] = Some(symbol);
        }
    }

    #[must_use]
    pub fn type_of_pat(&self, id: PatId) -> Option<&TyRepr> {
        self.pat_types.get(id.as_usize()).and_then(|t| t.as_ref())
    }

    #[must_use]
    pub fn symbol_of_pat(&self, id: PatId) -> Option<SymbolId> {
        self.pat_symbols.get(id.as_usize()).copied().flatten()
    }

    pub fn set_pat_type(&mut self, id: PatId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.pat_types.len() {
            self.pat_types[idx] = Some(ty);
        }
    }

    pub fn set_pat_symbol(&mut self, id: PatId, symbol: SymbolId) {
        let idx = id.as_usize();
        if idx < self.pat_symbols.len() {
            self.pat_symbols[idx] = Some(symbol);
        }
    }

    #[must_use]
    pub fn type_of_ty_expr(&self, id: TyExprId) -> Option<&TyRepr> {
        self.ty_expr_types
            .get(id.as_usize())
            .and_then(|t| t.as_ref())
    }

    pub fn set_ty_expr_type(&mut self, id: TyExprId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.ty_expr_types.len() {
            self.ty_expr_types[idx] = Some(ty);
        }
    }

    #[must_use]
    pub fn symbol_of_ty_expr(&self, id: TyExprId) -> Option<SymbolId> {
        self.ty_expr_symbols.get(id.as_usize()).copied().flatten()
    }

    pub fn set_ty_expr_symbol(&mut self, id: TyExprId, symbol: SymbolId) {
        let idx = id.as_usize();
        if idx < self.ty_expr_symbols.len() {
            self.ty_expr_symbols[idx] = Some(symbol);
        }
    }

    #[must_use]
    pub fn symbol_of_ident(&self, ident: Ident) -> Option<SymbolId> {
        self.ident_symbols.get(&ident.span).copied()
    }

    pub fn set_ident_symbol(&mut self, ident: Ident, symbol: SymbolId) {
        let _ = self.ident_symbols.insert(ident.span, symbol);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticTokenKind {
    Variable,
    Parameter,
    Function,
    Type,
    Property,
    EnumMember,
}

pub enum SemanticTokenModifier {
    Declaration,
    ReadOnly,
    DefaultLibrary,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct SemanticToken {
    pub span: Span,
    pub kind: SemanticTokenKind,
    pub modifiers: u32,
}

impl SemanticToken {
    pub const MOD_DECLARATION: u32 = 1 << 0;
    pub const MOD_READONLY: u32 = 1 << 1;
    pub const MOD_DEFAULT_LIBRARY: u32 = 1 << 2;
    pub const MOD_MUTABLE: u32 = 1 << 3;
}

#[must_use]
pub fn collect_tokens(
    arena: &AstArena,
    prog: &Prog,
    model: &SemanticModel,
    symbols: &SymbolTable,
) -> Vec<SemanticToken> {
    let mut collector = TokenCollector {
        model,
        symbols,
        tokens: vec![],
    };
    collector.visit_prog(arena, prog);
    collector.tokens.sort_by_key(|t| t.span.lo);
    collector.tokens
}

struct TokenCollector<'a> {
    model: &'a SemanticModel,
    symbols: &'a SymbolTable,
    tokens: Vec<SemanticToken>,
}

impl TokenCollector<'_> {
    fn classify_with_type(kind: SymbolKind, ty: &TyRepr) -> SemanticTokenKind {
        match kind {
            SymbolKind::Param | SymbolKind::Local => {
                if matches!(ty.kind, TyReprKind::Fn(..)) {
                    SemanticTokenKind::Function
                } else if kind == SymbolKind::Param {
                    SemanticTokenKind::Parameter
                } else {
                    SemanticTokenKind::Variable
                }
            }
            SymbolKind::Fn | SymbolKind::Builtin => SemanticTokenKind::Function,
            SymbolKind::Type => SemanticTokenKind::Type,
            SymbolKind::Field => SemanticTokenKind::Property,
            SymbolKind::Variant => SemanticTokenKind::EnumMember,
        }
    }

    fn add_token_with_mods(
        &mut self,
        span: Span,
        kind: SemanticTokenKind,
        symbol: &Symbol,
        extra_mods: u32,
    ) {
        let mut modifiers = extra_mods;
        if symbol.mutable {
            modifiers |= SemanticToken::MOD_MUTABLE;
        } else {
            modifiers |= SemanticToken::MOD_READONLY;
        }
        if symbol.def_span == Span::default() {
            modifiers |= SemanticToken::MOD_DEFAULT_LIBRARY;
        }
        self.tokens.push(SemanticToken {
            span,
            kind,
            modifiers,
        });
    }
}

impl AstVisitor for TokenCollector<'_> {
    fn visit_ident(&mut self, _arena: &AstArena, ident: Ident) {
        let sym_id = self
            .model
            .symbol_of_ident(ident)
            .or_else(|| self.symbols.lookup(ident));
        if let Some(sym_id) = sym_id
            && let Some(sym) = self.symbols.get(sym_id)
        {
            let mut kind = Self::classify_with_type(sym.kind, &sym.ty);
            let mut mods = 0;
            if sym.def_span == ident.span {
                mods |= SemanticToken::MOD_DECLARATION;
            }
            if sym.kind == SymbolKind::Variant {
                kind = SemanticTokenKind::EnumMember;
            }
            self.add_token_with_mods(ident.span, kind, sym, mods);
        }
    }

    fn visit_expr_id(&mut self, arena: &AstArena, id: ExprId) {
        let expr = arena.exprs.get(id);
        musi_ast::walk_expr(self, arena, expr);
    }

    fn visit_pat_id(&mut self, arena: &AstArena, id: PatId) {
        let pat = arena.pats.get(id);
        musi_ast::walk_pat(self, arena, pat);
    }

    fn visit_ty_expr_id(&mut self, arena: &AstArena, id: TyExprId) {
        let ty_expr = arena.ty_exprs.get(id);
        if let Some(sym_id) = self.model.symbol_of_ty_expr(id)
            && let Some(sym) = self.symbols.get(sym_id)
        {
            let kind = Self::classify_with_type(sym.kind, &sym.ty);
            if let TyExprKind::Ident(ident) = &ty_expr.kind {
                let mut mods = 0;
                if sym.def_span == ident.span {
                    mods |= SemanticToken::MOD_DECLARATION;
                }
                self.add_token_with_mods(ident.span, kind, sym, mods);
            } else {
                self.add_token_with_mods(ty_expr.span, kind, sym, 0);
            }
        }
        musi_ast::walk_ty_expr(self, arena, ty_expr);
    }
}
