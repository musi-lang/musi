use std::collections::{BTreeSet, HashMap};
use std::slice::from_ref;

use music_base::{SourceId, Span, diag::Diag};
use music_bc::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, EffectDescriptor,
    EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor,
    MetaDescriptor, MethodDescriptor, TypeDescriptor,
};
use music_bc::{
    Artifact, ClassId, CodeEntry, EffectId, ForeignId, GlobalId, Instruction, Label, MethodId,
    Opcode, Operand, StringId, TypeId,
};
use music_ir::{
    DefinitionKey, IrArg, IrAssignTarget, IrBinaryOp, IrCaseArm, IrCasePattern, IrEffectDef,
    IrExpr, IrExprKind, IrHandleOp, IrLit, IrModule, IrNameRef, IrOrigin, IrParam, IrRecordField,
    IrRecordLayoutField, IrSeqPart, IrTempId,
};
use music_module::ModuleKey;
use music_names::NameBindingId;

use crate::api::{EmitDiagList, EmitOptions, EmittedBinding, EmittedModule, EmittedProgram};

mod expr;
mod register;

type MethodTable = HashMap<NameBindingId, MethodId>;
type MethodNameTable = HashMap<Box<str>, MethodId>;
type ForeignTable = HashMap<NameBindingId, ForeignId>;
type GlobalTable = HashMap<NameBindingId, GlobalId>;
type TypeTable = HashMap<Box<str>, TypeId>;
type LocalSlots = HashMap<NameBindingId, u16>;
type CodeBuffer = Vec<CodeEntry>;
type QualifiedMethodTable = HashMap<(ModuleKey, Box<str>), MethodId>;
type QualifiedForeignTable = HashMap<(ModuleKey, Box<str>), ForeignId>;
type QualifiedGlobalTable = HashMap<(ModuleKey, Box<str>), GlobalId>;
type UniqueMethodTable = HashMap<Box<str>, MethodId>;
type UniqueForeignTable = HashMap<Box<str>, ForeignId>;
type UniqueGlobalTable = HashMap<Box<str>, GlobalId>;
type EffectTable = HashMap<DefinitionKey, EffectId>;

#[derive(Debug, Default)]
struct UniqueTables {
    methods: UniqueMethodTable,
    foreigns: UniqueForeignTable,
    globals: UniqueGlobalTable,
}

#[derive(Debug, Default)]
struct QualifiedTables {
    methods: QualifiedMethodTable,
    foreigns: QualifiedForeignTable,
    globals: QualifiedGlobalTable,
}

#[derive(Debug, Default)]
struct ModuleLayout {
    callables: MethodTable,
    callables_by_name: MethodNameTable,
    foreigns: ForeignTable,
    globals: GlobalTable,
    types: TypeTable,
    effects: EffectTable,
    classes: HashMap<DefinitionKey, ClassId>,
    init_methods: Vec<MethodId>,
}

#[derive(Debug, Default)]
struct ProgramState {
    artifact: Artifact,
    diags: EmitDiagList,
    types_by_name: HashMap<Box<str>, TypeId>,
    effects_by_key: HashMap<DefinitionKey, EffectId>,
    unique: UniqueTables,
    qualified: QualifiedTables,
}

#[derive(Debug)]
struct MethodEmitter<'artifact, 'module> {
    artifact: &'artifact mut Artifact,
    module_key: &'module ModuleKey,
    layout: &'module ModuleLayout,
    tables: EmitterTables<'module>,
    locals: LocalSlots,
    temps: HashMap<IrTempId, u16>,
    next_local: u16,
    labels: Vec<StringId>,
    code: CodeBuffer,
}

#[derive(Clone, Copy)]
struct RecordUpdateInput<'a> {
    ty_name: &'a str,
    field_count: u16,
    base: &'a IrExpr,
    base_fields: &'a [IrRecordLayoutField],
    result_fields: &'a [IrRecordLayoutField],
    updates: &'a [IrRecordField],
}

#[derive(Debug, Clone, Copy)]
struct EmitterTables<'a> {
    unique: &'a UniqueTables,
    qualified: &'a QualifiedTables,
}

/// Lowers one IR module into a standalone SEAM artifact.
///
/// # Errors
///
/// Returns emit diagnostics when the module contains unsupported lowered expressions or invalid
/// runtime-contract values.
pub fn lower_ir_module(
    module: &IrModule,
    options: EmitOptions,
) -> Result<EmittedModule, EmitDiagList> {
    let modules = [module];
    let mut state = ProgramState::default();
    let layout = register::register_module(&mut state, module, options);
    build_unique_maps(&mut state, &modules, from_ref(&layout));
    compile_module(&mut state, module, &layout);
    if !state.diags.is_empty() {
        return Err(state.diags);
    }

    let entry_method = build_module_entry(&mut state.artifact, &module.module_key, &layout);
    Ok(EmittedModule {
        module_key: module.module_key.clone(),
        artifact: state.artifact,
        entry_method,
        exports: collect_exports(module, &layout).into_boxed_slice(),
        static_imports: module.static_imports.clone(),
    })
}

/// Lowers a reachable IR module set into one merged SEAM artifact.
///
/// # Errors
///
/// Returns emit diagnostics when any module contains unsupported lowered expressions or invalid
/// runtime-contract values.
pub fn lower_ir_program(
    modules: &[IrModule],
    entry_module: &ModuleKey,
    options: EmitOptions,
) -> Result<EmittedProgram, EmitDiagList> {
    let mut state = ProgramState::default();
    let layouts = modules
        .iter()
        .map(|module| register::register_module(&mut state, module, options))
        .collect::<Vec<_>>();
    let module_refs = modules.iter().collect::<Vec<_>>();
    build_unique_maps(&mut state, &module_refs, &layouts);
    for (module, layout) in modules.iter().zip(layouts.iter()) {
        compile_module(&mut state, module, layout);
    }
    let entry_method = build_program_entry(&mut state.artifact, entry_module, &layouts);
    if !state.diags.is_empty() {
        return Err(state.diags);
    }
    Ok(EmittedProgram {
        entry_module: entry_module.clone(),
        artifact: state.artifact,
        entry_method,
        modules: modules
            .iter()
            .map(|module| module.module_key.clone())
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    })
}

fn build_unique_maps(state: &mut ProgramState, modules: &[&IrModule], layouts: &[ModuleLayout]) {
    let mut method_candidates = HashMap::<Box<str>, Vec<MethodId>>::new();
    let mut foreign_candidates = HashMap::<Box<str>, Vec<ForeignId>>::new();
    let mut global_candidates = HashMap::<Box<str>, Vec<GlobalId>>::new();
    for (module, layout) in modules.iter().zip(layouts) {
        for callable in &module.callables {
            if let Some(binding) = callable.binding
                && let Some(method) = layout.callables.get(&binding).copied()
            {
                let _ = state
                    .qualified
                    .methods
                    .insert((module.module_key.clone(), callable.name.clone()), method);
                method_candidates
                    .entry(callable.name.clone())
                    .or_default()
                    .push(method);
            }
        }
        for foreign in &module.foreigns {
            if let Some(binding) = foreign.binding
                && let Some(id) = layout.foreigns.get(&binding).copied()
            {
                let _ = state
                    .qualified
                    .foreigns
                    .insert((module.module_key.clone(), foreign.name.clone()), id);
                foreign_candidates
                    .entry(foreign.name.clone())
                    .or_default()
                    .push(id);
            }
        }
        for global in &module.globals {
            if let Some(binding) = global.binding
                && let Some(id) = layout.globals.get(&binding).copied()
            {
                let _ = state
                    .qualified
                    .globals
                    .insert((module.module_key.clone(), global.name.clone()), id);
                global_candidates
                    .entry(global.name.clone())
                    .or_default()
                    .push(id);
            }
        }
    }
    state.unique.methods = method_candidates
        .into_iter()
        .filter_map(unique_candidate)
        .collect();
    state.unique.foreigns = foreign_candidates
        .into_iter()
        .filter_map(unique_candidate)
        .collect();
    state.unique.globals = global_candidates
        .into_iter()
        .filter_map(unique_candidate)
        .collect();
}

fn unique_candidate<T: Copy>((name, ids): (Box<str>, Vec<T>)) -> Option<(Box<str>, T)> {
    ids.first()
        .copied()
        .filter(|_| ids.len() == 1)
        .map(|id| (name, id))
}

fn compile_module(state: &mut ProgramState, module: &IrModule, layout: &ModuleLayout) {
    compile_callables(state, module, layout);
    compile_globals(state, module, layout);
}

fn compile_callables(state: &mut ProgramState, module: &IrModule, layout: &ModuleLayout) {
    let ProgramState {
        artifact,
        diags,
        unique,
        qualified,
        ..
    } = state;
    let tables = EmitterTables { unique, qualified };
    for callable in &module.callables {
        let Some(method_id) = layout
            .callables_by_name
            .get(callable.name.as_ref())
            .copied()
        else {
            continue;
        };
        let locals = build_param_locals(&callable.params);
        let mut emitter = method_emitter(artifact, &module.module_key, layout, tables, locals);
        expr::compile_expr(&mut emitter, &callable.body, true, diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finish_emitter_method(emitter, method_id);
    }
}

fn compile_globals(state: &mut ProgramState, module: &IrModule, layout: &ModuleLayout) {
    let ProgramState {
        artifact,
        diags,
        unique,
        qualified,
        ..
    } = state;
    let tables = EmitterTables { unique, qualified };
    for global in &module.globals {
        let Some(binding) = global.binding else {
            continue;
        };
        let Some(global_id) = layout.globals.get(&binding).copied() else {
            continue;
        };
        let Some(init_method) = artifact.globals.get(global_id).initializer else {
            continue;
        };
        let mut emitter =
            method_emitter(artifact, &module.module_key, layout, tables, HashMap::new());
        expr::compile_expr(&mut emitter, &global.body, true, diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StGlob,
            Operand::Global(global_id),
        )));
        emit_zero(&mut emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finish_emitter_method(emitter, init_method);
    }
}

fn build_module_entry(
    artifact: &mut Artifact,
    module_key: &ModuleKey,
    layout: &ModuleLayout,
) -> Option<MethodId> {
    if layout.init_methods.is_empty() {
        return None;
    }
    Some(build_entry_method(
        artifact,
        &format!("{}::__module_init", module_key.as_str()),
        &layout.init_methods,
    ))
}

fn build_program_entry(
    artifact: &mut Artifact,
    entry_module: &ModuleKey,
    layouts: &[ModuleLayout],
) -> MethodId {
    build_entry_method(
        artifact,
        &format!("{}::__entry", entry_module.as_str()),
        &layouts
            .iter()
            .flat_map(|layout| layout.init_methods.iter().copied())
            .collect::<Vec<_>>(),
    )
}

fn entry_code(init_methods: &[MethodId]) -> CodeBuffer {
    let mut code = method_prologue();
    for init_method in init_methods {
        code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Call,
            Operand::Method(*init_method),
        )));
        code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(0),
        )));
    }
    code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::Ret,
        Operand::None,
    )));
    code
}

fn collect_exports(module: &IrModule, layout: &ModuleLayout) -> Vec<EmittedBinding> {
    module
        .exports
        .iter()
        .map(|export| EmittedBinding {
            name: export.name.clone(),
            method: binding_export(module, export.name.as_ref(), |binding| {
                layout.callables.get(&binding).copied()
            }),
            global: binding_export(module, export.name.as_ref(), |binding| {
                layout.globals.get(&binding).copied()
            }),
        })
        .collect()
}

fn binding_export<T, F>(module: &IrModule, export_name: &str, mut lower: F) -> Option<T>
where
    F: FnMut(NameBindingId) -> Option<T>,
{
    register::export_binding(module, export_name).and_then(&mut lower)
}

fn method_emitter<'artifact, 'module>(
    artifact: &'artifact mut Artifact,
    module_key: &'module ModuleKey,
    layout: &'module ModuleLayout,
    tables: EmitterTables<'module>,
    locals: LocalSlots,
) -> MethodEmitter<'artifact, 'module> {
    let next_local = u16::try_from(locals.len()).unwrap_or(u16::MAX);
    let labels = initial_labels(artifact);
    MethodEmitter {
        artifact,
        module_key,
        layout,
        tables,
        locals,
        temps: HashMap::new(),
        next_local,
        labels,
        code: method_prologue(),
    }
}

fn finish_emitter_method(emitter: MethodEmitter<'_, '_>, method_id: MethodId) {
    finalize_method(
        emitter.artifact,
        method_id,
        emitter.next_local.saturating_add(1),
        emitter.labels,
        emitter.code,
    );
}

fn build_entry_method(artifact: &mut Artifact, name: &str, init_methods: &[MethodId]) -> MethodId {
    let method_id = alloc_method(artifact, name, false, 0);
    let labels = initial_labels(artifact);
    finalize_method(artifact, method_id, 1, labels, entry_code(init_methods));
    method_id
}

fn build_param_locals(params: &[IrParam]) -> LocalSlots {
    params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| u16::try_from(index).ok().map(|slot| (param.binding, slot)))
        .collect()
}

fn emit_zero(emitter: &mut MethodEmitter<'_, '_>) {
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdSmi,
        Operand::I16(0),
    )));
}

fn method_prologue() -> CodeBuffer {
    vec![CodeEntry::Label(Label { id: 0 })]
}

fn initial_labels(artifact: &mut Artifact) -> Vec<StringId> {
    vec![artifact.intern_string("L0")]
}

fn alloc_method(artifact: &mut Artifact, name: &str, export: bool, params: u16) -> MethodId {
    let name_id = artifact.intern_string(name);
    artifact.methods.alloc(MethodDescriptor {
        name: name_id,
        params,
        locals: 0,
        export,
        labels: Box::new([]),
        code: Box::new([]),
    })
}

fn finalize_method(
    artifact: &mut Artifact,
    method_id: MethodId,
    locals: u16,
    labels: Vec<StringId>,
    code: CodeBuffer,
) {
    let method = artifact.methods.get_mut(method_id);
    method.locals = locals;
    method.labels = labels.into_boxed_slice();
    method.code = code.into_boxed_slice();
}

fn qualified_name(module: &ModuleKey, local: &str) -> Box<str> {
    format!("{}::{local}", module.as_str()).into()
}

fn handler_type_name(effect: &DefinitionKey) -> Box<str> {
    let base = qualified_name(&effect.module, &effect.name);
    format!("{base}::handler").into()
}
