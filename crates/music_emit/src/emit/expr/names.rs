use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_name(
        &mut self,
        binding: Option<NameBindingId>,
        name: &str,
        import_record_target: Option<&ModuleKey>,
        expr: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        if let Some(binding) = binding
            && let Some(slot) = self.locals.get(&binding).copied()
        {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            return;
        }
        if let Some(slot) = self.synthetic_locals.get(name).copied() {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            return;
        }
        if let Some(global) = self.resolve_global(binding, name, import_record_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdGlob,
                Operand::Global(global),
            )));
            return;
        }
        if let Some(procedure) = self.resolve_procedure(binding, name, import_record_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::NewFn,
                Operand::WideProcedureCaptures {
                    procedure,
                    captures: 0,
                },
            )));
            return;
        }
        if let Some(foreign) = self.resolve_foreign(binding, name, import_record_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdFfi,
                Operand::Foreign(foreign),
            )));
            return;
        }
        super::support::push_expr_diag_with(
            diags,
            self.module_key,
            &expr.origin,
            EmitDiagKind::UnsupportedNameRef,
            DiagContext::new().with("name", name),
        );
        emit_zero(self);
    }

    pub(super) fn resolve_procedure(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        import_record_target: Option<&ModuleKey>,
    ) -> Option<ProcedureId> {
        if import_record_target.is_none_or(|target| target == self.module_key) {
            if let Some(procedure) = self.layout.callables_by_name.get(name).copied() {
                return Some(procedure);
            }
        }
        resolve_named_binding(
            binding,
            name,
            import_record_target,
            &self.layout.callables,
            &self.tables.qualified.procedures,
            &self.tables.unique.procedures,
        )
    }

    pub(super) fn resolve_foreign(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        import_record_target: Option<&ModuleKey>,
    ) -> Option<ForeignId> {
        resolve_named_binding(
            binding,
            name,
            import_record_target,
            &self.layout.foreigns,
            &self.tables.qualified.foreigns,
            &self.tables.unique.foreigns,
        )
    }

    pub(super) fn resolve_global(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        import_record_target: Option<&ModuleKey>,
    ) -> Option<GlobalId> {
        resolve_named_binding(
            binding,
            name,
            import_record_target,
            &self.layout.globals,
            &self.tables.qualified.globals,
            &self.tables.unique.globals,
        )
    }
}

fn resolve_named_binding<T: Copy>(
    binding: Option<NameBindingId>,
    name: &str,
    import_record_target: Option<&ModuleKey>,
    locals: &HashMap<NameBindingId, T>,
    qualified: &HashMap<(ModuleKey, Box<str>), T>,
    unique: &HashMap<Box<str>, T>,
) -> Option<T> {
    if let Some(target) = binding.and_then(|binding| locals.get(&binding).copied()) {
        return Some(target);
    }
    if let Some(target) = import_record_target {
        return qualified.get(&(target.clone(), name.into())).copied();
    }
    unique.get(name).copied()
}
