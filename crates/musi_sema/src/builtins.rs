use musi_ast::Ident;
use musi_basic::span::Span;

use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::{FloatWidth, IntWidth, TyRepr};

pub struct Builtins {
    pub int8: Ident,
    pub int16: Ident,
    pub int32: Ident,
    pub int64: Ident,
    pub nat8: Ident,
    pub nat16: Ident,
    pub nat32: Ident,
    pub nat64: Ident,
    pub float32: Ident,
    pub float64: Ident,
    pub bool: Ident,
    pub rune: Ident,
    pub string: Ident,
    pub unit: Ident,
    pub never: Ident,
    pub any: Ident,
}

impl Builtins {
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub const fn new(
        int8: Ident,
        int16: Ident,
        int32: Ident,
        int64: Ident,
        nat8: Ident,
        nat16: Ident,
        nat32: Ident,
        nat64: Ident,
        float32: Ident,
        float64: Ident,
        bool_id: Ident,
        rune: Ident,
        string: Ident,
        unit: Ident,
        never: Ident,
        any: Ident,
    ) -> Self {
        Self {
            int8,
            int16,
            int32,
            int64,
            nat8,
            nat16,
            nat32,
            nat64,
            float32,
            float64,
            bool: bool_id,
            rune,
            string,
            unit,
            never,
            any,
        }
    }

    /// Registers all builtin types in symbol table.
    ///
    /// # Panics
    ///
    /// Panics if builtin is already defined (indicates bug).
    pub fn register(&self, symbols: &mut SymbolTable) {
        let span = Span::default();
        let _ = symbols
            .define(
                self.int8,
                SymbolKind::Builtin,
                TyRepr::int(IntWidth::I8),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.int16,
                SymbolKind::Builtin,
                TyRepr::int(IntWidth::I16),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.int32,
                SymbolKind::Builtin,
                TyRepr::int(IntWidth::I32),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.int64,
                SymbolKind::Builtin,
                TyRepr::int(IntWidth::I64),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.nat8,
                SymbolKind::Builtin,
                TyRepr::nat(IntWidth::I8),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.nat16,
                SymbolKind::Builtin,
                TyRepr::nat(IntWidth::I16),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.nat32,
                SymbolKind::Builtin,
                TyRepr::nat(IntWidth::I32),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.nat64,
                SymbolKind::Builtin,
                TyRepr::nat(IntWidth::I64),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.float32,
                SymbolKind::Builtin,
                TyRepr::float(FloatWidth::F32),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(
                self.float64,
                SymbolKind::Builtin,
                TyRepr::float(FloatWidth::F64),
                span,
            )
            .expect("builtin already defined");
        let _ = symbols
            .define(self.bool, SymbolKind::Builtin, TyRepr::bool(), span)
            .expect("builtin already defined");
        let _ = symbols
            .define(self.rune, SymbolKind::Builtin, TyRepr::rune(), span)
            .expect("builtin already defined");
        let _ = symbols
            .define(self.string, SymbolKind::Builtin, TyRepr::string(), span)
            .expect("builtin already defined");
        let _ = symbols
            .define(self.unit, SymbolKind::Builtin, TyRepr::unit(), span)
            .expect("builtin already defined");
        let _ = symbols
            .define(self.never, SymbolKind::Builtin, TyRepr::never(), span)
            .expect("builtin already defined");
        let _ = symbols
            .define(self.any, SymbolKind::Builtin, TyRepr::any(), span)
            .expect("builtin already defined");
    }
}
