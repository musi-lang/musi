use crate::{Interner, Symbol};

#[derive(Debug, Clone, Copy)]
pub struct KnownSymbols {
    pub type_: Symbol,
    pub array: Symbol,
    pub any: Symbol,
    pub unknown: Symbol,
    pub syntax: Symbol,
    pub empty: Symbol,
    pub unit: Symbol,
    pub bool_: Symbol,
    pub nat: Symbol,
    pub int_: Symbol,
    pub float_: Symbol,
    pub string_: Symbol,
    pub bound: Symbol,
    pub range: Symbol,
    pub handler: Symbol,
    pub rangeable: Symbol,
    pub compare: Symbol,
    pub next: Symbol,
    pub prev: Symbol,
    pub cstring: Symbol,
    pub cptr: Symbol,
    pub abort: Symbol,
    pub abort_op: Symbol,
    pub musi: Symbol,
    pub lang: Symbol,
    pub name_key: Symbol,
    pub lang_option: Symbol,
    pub some: Symbol,
    pub none: Symbol,
}

impl KnownSymbols {
    #[must_use]
    pub fn new(interner: &mut Interner) -> Self {
        Self {
            type_: interner.intern("Type"),
            array: interner.intern("Array"),
            any: interner.intern("Any"),
            unknown: interner.intern("Unknown"),
            syntax: interner.intern("Syntax"),
            empty: interner.intern("Empty"),
            unit: interner.intern("Unit"),
            bool_: interner.intern("Bool"),
            nat: interner.intern("Nat"),
            int_: interner.intern("Int"),
            float_: interner.intern("Float"),
            string_: interner.intern("String"),
            bound: interner.intern("Bound"),
            range: interner.intern("Range"),
            handler: interner.intern("Handler"),
            rangeable: interner.intern("Rangeable"),
            compare: interner.intern("compare"),
            next: interner.intern("next"),
            prev: interner.intern("prev"),
            cstring: interner.intern("CString"),
            cptr: interner.intern("CPtr"),
            abort: interner.intern("Abort"),
            abort_op: interner.intern("abort"),
            musi: interner.intern("musi"),
            lang: interner.intern("lang"),
            name_key: interner.intern("name"),
            lang_option: interner.intern("Option"),
            some: interner.intern("Some"),
            none: interner.intern("None"),
        }
    }

    #[must_use]
    pub const fn compiler_prelude(self) -> [Symbol; 17] {
        [
            self.type_,
            self.any,
            self.unknown,
            self.syntax,
            self.empty,
            self.unit,
            self.bool_,
            self.nat,
            self.int_,
            self.float_,
            self.string_,
            self.bound,
            self.range,
            self.rangeable,
            self.cstring,
            self.cptr,
            self.abort,
        ]
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
