use super::super::*;
use crate::EmitDiagKind;

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
        &EmitDiagKind::UnsupportedNameRef,
        format!("name ref `{name}` has no emitted form"),
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
    resolve_named_binding(
        binding,
        name,
        module_target,
        &emitter.layout.callables,
        &emitter.tables.qualified.methods,
        &emitter.tables.unique.methods,
    )
}

pub(super) fn resolve_foreign(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<ForeignId> {
    resolve_named_binding(
        binding,
        name,
        module_target,
        &emitter.layout.foreigns,
        &emitter.tables.qualified.foreigns,
        &emitter.tables.unique.foreigns,
    )
}

pub(super) fn resolve_global(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<GlobalId> {
    resolve_named_binding(
        binding,
        name,
        module_target,
        &emitter.layout.globals,
        &emitter.tables.qualified.globals,
        &emitter.tables.unique.globals,
    )
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
