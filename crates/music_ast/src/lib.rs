//! Abstract syntax tree for the Musi compiler.
//!
//! All recursive children use [`Idx<T>`] arena indices rather than `Box<T>`,
//! giving cache-coherent storage and `Copy` references.

pub mod attr;
pub mod decl;
pub mod expr;
pub mod lit;
pub mod pat;
pub mod ty;
pub mod util;
pub mod visitor;

pub use expr::Expr;
pub use lit::Lit;
pub use pat::Pat;
pub use ty::Ty;

use music_shared::{Arena, Idx, Span, Symbol};

/// Index into the expression arena.
pub type ExprIdx = Idx<Expr>;
/// Index into the type-syntax arena.
pub type TyIdx = Idx<Ty>;
/// Index into the pattern arena.
pub type PatIdx = Idx<Pat>;
/// Index into the name-reference arena.
pub type NameRefIdx = Idx<NameRef>;

/// A reference to a name that will be resolved via scope lookup.
///
/// Definition sites keep plain `Symbol`; reference sites use `NameRefIdx`
/// so the resolver can attach a `DefId` to every resolved name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameRef {
    pub name: Symbol,
    pub span: Span,
}

/// A list of expression indices.
pub type ExprList = Vec<ExprIdx>;
/// A list of type indices.
pub type TyList = Vec<TyIdx>;
/// A list of pattern indices.
pub type PatList = Vec<PatIdx>;

/// Bundled arenas for all recursive AST node categories.
#[derive(Default)]
pub struct AstArenas {
    pub exprs: Arena<Expr>,
    pub tys: Arena<Ty>,
    pub pats: Arena<Pat>,
    pub name_refs: Arena<NameRef>,
}

impl AstArenas {
    /// Creates empty arenas.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

/// A statement: an expression followed by a semicolon (not stored).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub expr: ExprIdx,
    pub span: Span,
}

/// The top-level result of parsing a source file.
pub struct ParsedModule {
    pub stmts: Vec<Stmt>,
    pub arenas: AstArenas,
    pub span: Span,
}
