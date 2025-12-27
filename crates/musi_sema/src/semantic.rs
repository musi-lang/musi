use musi_ast::{
    AstArena, AstVisitor, ExprId, ExprKind, PatId, PatKind, Prog, TyExprId, TyExprKind,
};
use musi_basic::span::Span;

use crate::symbol::SymbolId;
use crate::ty_repr::TyRepr;
use crate::{SemanticTokens, Symbol, SymbolKind, SymbolTable};

#[derive(Debug)]
pub struct SemanticModel {
    expr_types: Vec<Option<TyRepr>>,
    expr_symbols: Vec<Option<SymbolId>>,
    pat_types: Vec<Option<TyRepr>>,
    pat_symbols: Vec<Option<SymbolId>>,
    ty_expr_types: Vec<Option<TyRepr>>,
    ty_expr_symbols: Vec<Option<SymbolId>>,
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
}

#[must_use]
pub fn collect_tokens(
    arena: &AstArena,
    prog: &Prog,
    model: &SemanticModel,
    symbols: &SymbolTable,
) -> SemanticTokens {
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
    tokens: SemanticTokens,
}

impl TokenCollector<'_> {
    const fn classify_kind(kind: SymbolKind) -> SemanticTokenKind {
        match kind {
            SymbolKind::Local => SemanticTokenKind::Variable,
            SymbolKind::Param => SemanticTokenKind::Parameter,
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
        if !symbol.mutable {
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
    fn visit_expr_id(&mut self, arena: &AstArena, id: ExprId) {
        let expr = arena.exprs.get(id);
        if let Some(sym_id) = self.model.symbol_of_expr(id)
            && let Some(sym) = self.symbols.get(sym_id)
        {
            let kind = Self::classify_kind(sym.kind);
            if let ExprKind::Ident(ident) = &expr.kind {
                let mut mods = 0;
                if sym.def_span == ident.span {
                    mods |= SemanticToken::MOD_DECLARATION;
                }
                self.add_token_with_mods(ident.span, kind, sym, mods);
            }
        }
        musi_ast::walk_expr(self, arena, expr);
    }

    fn visit_pat_id(&mut self, arena: &AstArena, id: PatId) {
        let pat = arena.pats.get(id);
        if let Some(sym_id) = self.model.symbol_of_pat(id)
            && let Some(sym) = self.symbols.get(sym_id)
        {
            let kind = Self::classify_kind(sym.kind);
            let mods = SemanticToken::MOD_DECLARATION;
            if let PatKind::Ident(ident) = &pat.kind {
                self.add_token_with_mods(ident.span, kind, sym, mods);
            } else if let PatKind::Choice { name, .. } = &pat.kind {
                self.add_token_with_mods(name.span, SemanticTokenKind::EnumMember, sym, mods);
            }
        }
        musi_ast::walk_pat(self, arena, pat);
    }

    fn visit_ty_expr_id(&mut self, arena: &AstArena, id: TyExprId) {
        let ty_expr = arena.ty_exprs.get(id);
        if let Some(sym_id) = self.model.symbol_of_ty_expr(id)
            && let Some(sym) = self.symbols.get(sym_id)
        {
            let kind = Self::classify_kind(sym.kind);
            if let TyExprKind::Ident(ident) = &ty_expr.kind {
                self.add_token_with_mods(ident.span, kind, sym, 0);
            } else {
                self.add_token_with_mods(ty_expr.span, kind, sym, 0);
            }
        }
        musi_ast::walk_ty_expr(self, arena, ty_expr);
    }
}
