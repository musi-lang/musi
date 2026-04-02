use crate::{Interner, Symbol};

#[derive(Debug, Clone, Copy)]
pub struct KnownSymbols {
    pub type_: Symbol,
    pub any: Symbol,
    pub unknown: Symbol,
    pub syntax: Symbol,
    pub empty: Symbol,
    pub unit: Symbol,
    pub bool_: Symbol,
    pub int_: Symbol,
    pub float_: Symbol,
    pub string_: Symbol,
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
            any: interner.intern("Any"),
            unknown: interner.intern("Unknown"),
            syntax: interner.intern("Syntax"),
            empty: interner.intern("Empty"),
            unit: interner.intern("Unit"),
            bool_: interner.intern("Bool"),
            int_: interner.intern("Int"),
            float_: interner.intern("Float"),
            string_: interner.intern("String"),
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
    pub const fn compiler_prelude(self) -> [Symbol; 13] {
        [
            self.type_,
            self.any,
            self.unknown,
            self.syntax,
            self.empty,
            self.unit,
            self.bool_,
            self.int_,
            self.float_,
            self.string_,
            self.cstring,
            self.cptr,
            self.abort,
        ]
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
