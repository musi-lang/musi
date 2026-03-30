use std::collections::HashMap;

use music_basic::SourceMap;
use music_check::AnalyzedModule;
use music_hir::{HirExprId, HirExprKind};
use music_il::{ConstantPool, Instruction, MethodEntry, MethodName, Opcode, TypeDescriptor};
use music_names::{Interner, NameBindingId, Symbol};

use crate::errors::{EmitError, EmitErrorKind, EmitResult};

use super::method::MethodEmitter;

pub(super) struct FunctionEmitter<'a> {
    interner: &'a Interner,
    sources: &'a SourceMap,
    analyzed: &'a AnalyzedModule,
    globals_by_binding: &'a HashMap<NameBindingId, u16>,
    import_globals_by_binding: &'a HashMap<NameBindingId, u16>,
    module_export_globals: &'a HashMap<(String, Symbol), u16>,
    constants: &'a mut ConstantPool,
    methods: &'a mut Vec<MethodEntry>,
    types: &'a mut Vec<TypeDescriptor>,
}

impl<'a> FunctionEmitter<'a> {
    pub(super) fn new(
        interner: &'a Interner,
        sources: &'a SourceMap,
        analyzed: &'a AnalyzedModule,
        globals_by_binding: &'a HashMap<NameBindingId, u16>,
        import_globals_by_binding: &'a HashMap<NameBindingId, u16>,
        module_export_globals: &'a HashMap<(String, Symbol), u16>,
        constants: &'a mut ConstantPool,
        methods: &'a mut Vec<MethodEntry>,
        types: &'a mut Vec<TypeDescriptor>,
    ) -> Self {
        Self {
            interner,
            sources,
            analyzed,
            globals_by_binding,
            import_globals_by_binding,
            module_export_globals,
            constants,
            methods,
            types,
        }
    }

    pub(super) fn emit_let_function_method(&mut self, expr_id: HirExprId) -> EmitResult<usize> {
        let expr = self.analyzed.module.store.exprs.get(expr_id).clone();
        let HirExprKind::Let { params, value, .. } = expr.kind else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };
        let Some(body) = value else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };

        let mut emitter = MethodEmitter::new(
            self.interner,
            self.sources,
            self.analyzed.module.source_id,
            &self.analyzed.module.store,
            &self.analyzed.names,
            &self.analyzed.ir,
            self.globals_by_binding,
            self.import_globals_by_binding,
            self.module_export_globals,
            self.constants,
            self.methods,
            self.types,
        );
        emitter.bind_params(&params);
        emitter.emit_expr(body)?;
        emitter.instructions.push(Instruction::basic(Opcode::Ret));

        let locals_count = u16::try_from(emitter.locals_count()).unwrap_or(u16::MAX);
        Ok(emitter.finish_method(MethodName::Anonymous, locals_count))
    }

    pub(super) fn emit_expr_into(
        &mut self,
        out: &mut Vec<Instruction>,
        expr_id: HirExprId,
    ) -> EmitResult<usize> {
        let mut emitter = MethodEmitter::new(
            self.interner,
            self.sources,
            self.analyzed.module.source_id,
            &self.analyzed.module.store,
            &self.analyzed.names,
            &self.analyzed.ir,
            self.globals_by_binding,
            self.import_globals_by_binding,
            self.module_export_globals,
            self.constants,
            self.methods,
            self.types,
        );
        emitter.emit_expr(expr_id)?;
        let locals_count = emitter.locals_count();
        out.extend(emitter.instructions.drain(..));
        Ok(locals_count)
    }
}
