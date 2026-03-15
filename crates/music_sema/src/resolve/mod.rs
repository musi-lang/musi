//! Two-pass name resolution.
//!
//! Pass 1: collect all top-level definitions (bindings, functions, types,
//! classes, effects) into the module scope.
//!
//! Pass 2: walk the full AST, resolving every name reference to a [`DefId`]
//! and creating child scopes for blocks, functions, and match arms.

#[cfg(test)]
mod tests;

mod collect;
mod decl;
mod expr;
mod pat;
mod ty;

use std::collections::HashMap;

use music_ast::expr::{BindKind, Expr};
use music_ast::ty::{Constraint, TyParam};
use music_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx};
use music_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::def::{DefId, DefKind, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};

/// Imported names from dependency modules, keyed by import path symbol.
/// Each entry is a list of `(name, DefId)` pairs from the dependency's exports.
pub type ImportNames = HashMap<Symbol, Vec<(Symbol, DefId)>>;

/// Output accumulators from the resolution pass.
pub struct ResolveOutput {
    pub expr_defs: HashMap<ExprIdx, DefId>,
    pub pat_defs: HashMap<Span, DefId>,
    /// Maps law span -> inferred (implicit) law variables, for LSP inlay hints.
    pub law_inferred_vars: HashMap<Span, Vec<(Symbol, DefId)>>,
    /// Maps (class `DefId`, operator `Symbol`) -> member `DefId` for operator dispatch.
    pub class_op_members: HashMap<(DefId, Symbol), DefId>,
}

/// Mutable resolution state bundling the definition table, scope tree, and
/// current module scope. Passed together to avoid exceeding argument limits.
pub struct ResolveState<'a> {
    pub defs: &'a mut DefTable,
    pub scopes: &'a mut ScopeTree,
    pub module_scope: ScopeId,
}

/// Runs two-pass name resolution over a parsed module.
#[must_use]
pub fn resolve(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    defs: &mut DefTable,
    scopes: &mut ScopeTree,
    module_scope: ScopeId,
) -> ResolveOutput {
    let empty = HashMap::new();
    let mut state = ResolveState {
        defs,
        scopes,
        module_scope,
    };
    resolve_with_imports(module, interner, file_id, diags, &mut state, &empty)
}

/// Like [`resolve`], but with pre-computed import names for cross-module resolution.
#[must_use]
pub fn resolve_with_imports(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    state: &mut ResolveState<'_>,
    import_names: &ImportNames,
) -> ResolveOutput {
    let mut resolver = Resolver {
        ast: &module.arenas,
        interner,
        file_id,
        diags,
        defs: state.defs,
        scopes: state.scopes,
        output: ResolveOutput {
            expr_defs: HashMap::new(),
            pat_defs: HashMap::new(),
            law_inferred_vars: HashMap::new(),
            class_op_members: HashMap::new(),
        },
        current_scope: state.module_scope,
        import_names,
    };

    for stmt in &module.stmts {
        resolver.collect_top_level(stmt.expr);
    }
    for stmt in &module.stmts {
        resolver.resolve_expr(stmt.expr);
    }

    resolver.output
}

pub(super) struct Resolver<'a> {
    pub(super) ast: &'a AstArenas,
    pub(super) interner: &'a mut Interner,
    pub(super) file_id: FileId,
    pub(super) diags: &'a mut DiagnosticBag,
    pub(super) defs: &'a mut DefTable,
    pub(super) scopes: &'a mut ScopeTree,
    pub(super) output: ResolveOutput,
    pub(super) current_scope: ScopeId,
    pub(super) import_names: &'a ImportNames,
}

impl Resolver<'_> {
    /// If `pat` is a function-like pattern (`Pat::Variant` with args),
    /// enters a child scope and defines all arg bindings as params.
    /// Returns `Some(parent_scope)` if a scope was entered, `None` otherwise.
    fn enter_fn_pat_scope(&mut self, pat: PatIdx) -> Option<ScopeId> {
        use music_ast::pat::Pat;
        if let Pat::Variant { args, .. } = &self.ast.pats[pat] {
            if args.is_empty() {
                return None;
            }
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            for &arg in args {
                self.define_pat(arg, DefKind::Param);
            }
            Some(parent)
        } else {
            None
        }
    }

    fn enter_ty_param_scope(&mut self, params: &[TyParam], constraints: &[Constraint]) -> ScopeId {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self.defs.alloc(param.name, DefKind::Type, param.span);
            self.define_in_scope(param.name, id, param.span);
        }
        for constraint in constraints {
            self.resolve_ty_named_ref(&constraint.bound);
        }
        parent
    }

    fn define_in_scope(&mut self, name: Symbol, def_id: DefId, span: Span) {
        if let Some(prev) = self.scopes.define(self.current_scope, name, def_id) {
            let prev_span = self.defs.get(prev).span;
            // Well-known placeholder (Span::DUMMY) being redefined by real user code — allow.
            if prev_span == Span::DUMMY && span != Span::DUMMY {
                return;
            }
            // Skip operator-sentinel symbols that the interner cannot resolve.
            if name == Symbol(u32::MAX) {
                return;
            }
            let name_str = self.interner.resolve(name);
            let d = self.diags.report(
                &SemaError::DuplicateDefinition {
                    name: Box::from(name_str),
                },
                span,
                self.file_id,
            );
            let _s = d.add_secondary(prev_span, self.file_id, "previous definition here");
        }
    }

    fn span_of_expr(&self, idx: ExprIdx) -> Span {
        expr_span(&self.ast.exprs[idx])
    }
}

const fn binding_def_kind(kind: BindKind) -> DefKind {
    match kind {
        BindKind::Immut => DefKind::Let,
        BindKind::Mut => DefKind::Var,
    }
}

pub(crate) const fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Lit { span, .. }
        | Expr::Name { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Let { span, .. }
        | Expr::Fn { span, .. }
        | Expr::Call { span, .. }
        | Expr::Field { span, .. }
        | Expr::Index { span, .. }
        | Expr::Update { span, .. }
        | Expr::Record { span, .. }
        | Expr::Array { span, .. }
        | Expr::Variant { span, .. }
        | Expr::Choice { span, .. }
        | Expr::RecordDef { span, .. }
        | Expr::BinOp { span, .. }
        | Expr::UnaryOp { span, .. }
        | Expr::Piecewise { span, .. }
        | Expr::Match { span, .. }
        | Expr::Return { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Annotated { span, .. }
        | Expr::Binding { span, .. }
        | Expr::Class { span, .. }
        | Expr::Instance { span, .. }
        | Expr::Effect { span, .. }
        | Expr::Foreign { span, .. }
        | Expr::TypeCheck { span, .. }
        | Expr::Handle { span, .. }
        | Expr::Error { span, .. } => *span,
    }
}
