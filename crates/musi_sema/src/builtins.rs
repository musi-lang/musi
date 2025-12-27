use musi_ast::Ident;
use musi_basic::interner::Interner;
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
    pub fn from_interner(interner: &mut Interner) -> Self {
        let span = Span::default();
        let mut i = |s| Ident::new(interner.intern(s), span);
        Self {
            int8: i("Int8"),
            int16: i("Int16"),
            int32: i("Int"),
            int64: i("Int64"),
            nat8: i("Nat8"),
            nat16: i("Nat16"),
            nat32: i("Nat32"),
            nat64: i("Nat64"),
            float32: i("Float32"),
            float64: i("Float64"),
            bool: i("Bool"),
            rune: i("Rune"),
            string: i("String"),
            unit: i("Unit"),
            never: i("Never"),
            any: i("Any"),
        }
    }

    /// Registers all builtin types in symbol table.
    ///
    /// # Panics
    ///
    /// Panics if builtin is already defined (indicates bug).
    pub fn register(&self, symbols: &mut SymbolTable) {
        let span = Span::default();

        let builtins = [
            (self.int8, TyRepr::int(IntWidth::I8)),
            (self.int16, TyRepr::int(IntWidth::I16)),
            (self.int32, TyRepr::int(IntWidth::native())),
            (self.int64, TyRepr::int(IntWidth::I64)),
            (self.nat8, TyRepr::nat(IntWidth::I8)),
            (self.nat16, TyRepr::nat(IntWidth::I16)),
            (self.nat32, TyRepr::nat(IntWidth::native())),
            (self.nat64, TyRepr::nat(IntWidth::I64)),
            (self.float32, TyRepr::float(FloatWidth::F32)),
            (self.float64, TyRepr::float(FloatWidth::F64)),
            (self.bool, TyRepr::bool()),
            (self.rune, TyRepr::rune()),
            (self.string, TyRepr::string()),
            (self.unit, TyRepr::unit()),
            (self.never, TyRepr::never()),
            (self.any, TyRepr::any()),
        ];

        for (name, ty) in builtins {
            let _ = symbols
                .define(name, SymbolKind::Type, ty, span, false)
                .expect("builtin already defined");
        }
    }
}
