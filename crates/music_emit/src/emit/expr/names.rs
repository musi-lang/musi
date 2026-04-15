use super::super::*;
use crate::EmitDiagKind;

impl MethodEmitter<'_, '_> {
    pub(super) fn compile_name(
        &mut self,
        binding: Option<NameBindingId>,
        name: &str,
        module_target: Option<&ModuleKey>,
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
        if let Some(global) = self.resolve_global(binding, name, module_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdGlob,
                Operand::Global(global),
            )));
            return;
        }
        if let Some(method) = self.resolve_method(binding, name, module_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::ClsNew,
                Operand::WideMethodCaptures {
                    method,
                    captures: 0,
                },
            )));
            return;
        }
        if let Some(foreign) = self.resolve_foreign(binding, name, module_target) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::FfiRef,
                Operand::Foreign(foreign),
            )));
            return;
        }
        super::support::push_expr_diag(
            diags,
            self.module_key,
            &expr.origin,
            EmitDiagKind::UnsupportedNameRef,
            format!("name ref `{name}` has no emitted form"),
        );
        emit_zero(self);
    }

    pub(super) fn resolve_method(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        module_target: Option<&ModuleKey>,
    ) -> Option<MethodId> {
        if module_target.is_none() || module_target.is_some_and(|target| target == self.module_key)
        {
            if let Some(method) = self.layout.callables_by_name.get(name).copied() {
                return Some(method);
            }
        }
        resolve_named_binding(
            binding,
            name,
            module_target,
            &self.layout.callables,
            &self.tables.qualified.methods,
            &self.tables.unique.methods,
        )
    }

    pub(super) fn resolve_foreign(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        module_target: Option<&ModuleKey>,
    ) -> Option<ForeignId> {
        resolve_named_binding(
            binding,
            name,
            module_target,
            &self.layout.foreigns,
            &self.tables.qualified.foreigns,
            &self.tables.unique.foreigns,
        )
    }

    pub(super) fn resolve_global(
        &self,
        binding: Option<NameBindingId>,
        name: &str,
        module_target: Option<&ModuleKey>,
    ) -> Option<GlobalId> {
        resolve_named_binding(
            binding,
            name,
            module_target,
            &self.layout.globals,
            &self.tables.qualified.globals,
            &self.tables.unique.globals,
        )
    }
}

fn resolve_named_binding<T: Copy>(
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
    locals: &HashMap<NameBindingId, T>,
    qualified: &HashMap<(ModuleKey, Box<str>), T>,
    unique: &HashMap<Box<str>, T>,
) -> Option<T> {
    binding
        .and_then(|binding| locals.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| qualified.get(&(target.clone(), name.into())).copied())
        })
        .or_else(|| unique.get(name).copied())
}
