use musi_ast::Ident;
use musi_basic::interner::Interner;
use musi_basic::span::Span;

use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::{FloatWidth, IntWidth, TyRepr};

pub struct Builtins {
    pub ints: [Ident; 4],
    pub nats: [Ident; 4],
    pub floats: [Ident; 2],
    pub int: Ident,
    pub nat: Ident,
    pub float: Ident,
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
            ints: [i("Int8"), i("Int16"), i("Int32"), i("Int64")],
            nats: [i("Nat8"), i("Nat16"), i("Nat32"), i("Nat64")],
            floats: [i("Float32"), i("Float64")],
            int: i("Int"),
            nat: i("Nat"),
            float: i("Float"),
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

        let widths = [IntWidth::I8, IntWidth::I16, IntWidth::I32, IntWidth::I64];
        for (idx, &width) in widths.iter().enumerate() {
            let _ = symbols
                .define(
                    self.ints[idx],
                    SymbolKind::Type,
                    TyRepr::int(width),
                    span,
                    false,
                )
                .expect("Int builtin already defined");
            let _ = symbols
                .define(
                    self.nats[idx],
                    SymbolKind::Type,
                    TyRepr::nat(width),
                    span,
                    false,
                )
                .expect("Nat builtin already defined");
        }

        let float_widths = [FloatWidth::F32, FloatWidth::F64];
        for (idx, &width) in float_widths.iter().enumerate() {
            let _ = symbols
                .define(
                    self.floats[idx],
                    SymbolKind::Type,
                    TyRepr::float(width),
                    span,
                    false,
                )
                .expect("Float builtin already defined");
        }

        let aliases = [
            (self.int, TyRepr::int(IntWidth::native())),
            (self.nat, TyRepr::nat(IntWidth::native())),
            (self.float, TyRepr::float(FloatWidth::native())),
            (self.bool, TyRepr::bool()),
            (self.rune, TyRepr::rune()),
            (self.string, TyRepr::string()),
            (self.unit, TyRepr::unit()),
            (self.never, TyRepr::never()),
            (self.any, TyRepr::any()),
        ];

        for (name, ty) in aliases {
            let _ = symbols
                .define(name, SymbolKind::Type, ty, span, false)
                .expect("builtin already defined");
        }
    }
}
