use super::*;

impl ResolveDb {
    /// Populate the module scope with builtin types and prelude classes.
    pub fn seed_builtins(&mut self) {
        let scope = self.module_scope;
        for builtin in BuiltinType::ALL {
            let name = self.db.interner.intern(builtin.name());
            let _ = self.define_and_bind_with_module(
                name,
                Span::DUMMY,
                DefKind::Builtin(*builtin),
                Visibility::Exported,
                scope,
                Some(PRELUDE_MODULE_NAME.to_owned()),
            );
        }

        for class in PRELUDE_CLASSES {
            let name = self.db.interner.intern(class.name);
            let _ = self.define_and_bind_with_module(
                name,
                Span::DUMMY,
                DefKind::TypeClass,
                Visibility::Exported,
                scope,
                Some(PRELUDE_MODULE_NAME.to_owned()),
            );

            for method in class.methods {
                let op_name = self.db.interner.intern(method.op_name);
                let _ = self.define_and_bind_with_module(
                    op_name,
                    Span::DUMMY,
                    DefKind::Method,
                    Visibility::Exported,
                    scope,
                    Some(PRELUDE_MODULE_NAME.to_owned()),
                );
            }
        }
    }
}
