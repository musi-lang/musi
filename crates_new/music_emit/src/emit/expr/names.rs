use super::super::*;

pub(super) fn compile_name(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) {
    if let Some(binding) = binding
        && let Some(slot) = emitter.locals.get(&binding).copied()
    {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
        return;
    }
    if let Some(global) = resolve_global(emitter, binding, name, module_target) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdGlob,
            Operand::Global(global),
        )));
        return;
    }
    if let Some(method) = resolve_method(emitter, binding, name, module_target) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::ClsNew,
            Operand::WideMethodCaptures {
                method,
                captures: 0,
            },
        )));
        return;
    }
    super::support::push_expr_diag(
        diags,
        emitter.module_key,
        &expr.origin,
        format!("unsupported emitted name reference `{name}`"),
    );
    emit_zero(emitter);
}

pub(super) fn resolve_method(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<MethodId> {
    if module_target.is_none() || module_target.is_some_and(|target| target == emitter.module_key) {
        if let Some(method) = emitter.layout.callables_by_name.get(name).copied() {
            return Some(method);
        }
    }
    binding
        .and_then(|binding| emitter.layout.callables.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_methods
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_methods.get(name).copied())
}

pub(super) fn resolve_foreign(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<ForeignId> {
    binding
        .and_then(|binding| emitter.layout.foreigns.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_foreigns
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_foreigns.get(name).copied())
}

pub(super) fn resolve_global(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<GlobalId> {
    binding
        .and_then(|binding| emitter.layout.globals.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_globals
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_globals.get(name).copied())
}

