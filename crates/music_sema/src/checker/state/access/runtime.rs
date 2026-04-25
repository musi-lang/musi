use music_names::{Interner, KnownSymbols, Symbol};

use crate::api::{SemaEnv, TargetInfo};

use crate::checker::state::{Builtins, PassBase};

impl PassBase<'_, '_, '_> {
    pub const fn builtins(&self) -> Builtins {
        self.runtime.builtins
    }

    pub const fn known(&self) -> KnownSymbols {
        self.runtime.known
    }

    pub const fn target(&self) -> Option<&TargetInfo> {
        self.runtime.target.as_ref()
    }

    pub const fn interner(&self) -> &Interner {
        self.runtime.interner
    }

    pub fn intern(&mut self, text: &str) -> Symbol {
        self.runtime.interner.intern(text)
    }

    pub fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.runtime.interner.resolve(symbol)
    }
}

impl<'env> PassBase<'_, '_, 'env> {
    pub const fn sema_env(&self) -> Option<&'env dyn SemaEnv> {
        self.runtime.env
    }
}
