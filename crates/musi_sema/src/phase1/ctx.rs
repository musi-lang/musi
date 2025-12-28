use musi_ast::AstArena;
use musi_basic::interner::Interner;

use crate::symbol::SymbolTable;

pub struct ResolveCtx<'a> {
    pub arena: &'a AstArena,
    pub symbols: &'a SymbolTable,
    pub interner: &'a Interner,
}
