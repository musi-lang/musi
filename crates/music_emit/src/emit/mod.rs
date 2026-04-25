use std::any::Any;
use std::collections::{BTreeSet, HashMap};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::slice::from_ref;

use music_base::{
    SourceId, Span,
    diag::{Diag, DiagContext},
};
use music_ir::{
    DefinitionKey, IrArg, IrAssignTarget, IrEffectDef, IrExpr, IrExprKind, IrHandleOp, IrLit,
    IrMatchArm, IrModule, IrModuleInitPart, IrNameRef, IrOrigin, IrParam, IrRecordField,
    IrRecordLayoutField, IrSeqPart, IrTempId,
};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_seam::descriptor::{
    ConstantDescriptor, ConstantValue, DataDescriptor, EffectDescriptor, EffectOpDescriptor,
    ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor, MetaDescriptor,
    ProcedureDescriptor, TypeDescriptor,
};
use music_seam::{
    Artifact, CodeEntry, EffectId, ForeignId, GlobalId, Instruction, Label, Opcode, Operand,
    ProcedureId, ShapeId, StringId, TypeId,
};

use crate::api::{EmitDiagList, EmitOptions, EmittedBinding, EmittedModule, EmittedProgram};
use crate::diag::EmitDiagKind;

mod expr;
mod register;

type ProcedureTable = HashMap<NameBindingId, ProcedureId>;
type ProcedureNameTable = HashMap<Box<str>, ProcedureId>;
type ForeignTable = HashMap<NameBindingId, ForeignId>;
type GlobalTable = HashMap<NameBindingId, GlobalId>;
type TypeTable = HashMap<Box<str>, TypeId>;
type LocalSlots = HashMap<NameBindingId, u16>;
type SyntheticLocalSlots = HashMap<Box<str>, u16>;
type CodeBuffer = Vec<CodeEntry>;
type QualifiedProcedureTable = HashMap<(ModuleKey, Box<str>), ProcedureId>;
type QualifiedForeignTable = HashMap<(ModuleKey, Box<str>), ForeignId>;
type QualifiedGlobalTable = HashMap<(ModuleKey, Box<str>), GlobalId>;
type UniqueProcedureTable = HashMap<Box<str>, ProcedureId>;
type UniqueForeignTable = HashMap<Box<str>, ForeignId>;
type UniqueGlobalTable = HashMap<Box<str>, GlobalId>;
type EffectTable = HashMap<DefinitionKey, EffectId>;
type ExprEmitter<'artifact, 'module> = ProcedureEmitter<'artifact, 'module>;
type ExprEmitterMut<'emitter, 'artifact, 'module> = &'emitter mut ExprEmitter<'artifact, 'module>;
type ExprEmitterRef<'emitter, 'artifact, 'module> = &'emitter ExprEmitter<'artifact, 'module>;

#[derive(Debug, Default)]
struct UniqueTables {
    procedures: UniqueProcedureTable,
    foreigns: UniqueForeignTable,
    globals: UniqueGlobalTable,
}

#[derive(Debug, Default)]
struct QualifiedTables {
    procedures: QualifiedProcedureTable,
    foreigns: QualifiedForeignTable,
    globals: QualifiedGlobalTable,
}

#[derive(Debug, Default)]
struct ModuleLayout {
    callables: ProcedureTable,
    callables_by_name: ProcedureNameTable,
    foreigns: ForeignTable,
    globals: GlobalTable,
    global_init_procedures: HashMap<Box<str>, ProcedureId>,
    types: TypeTable,
    effects: EffectTable,
    shapes: HashMap<DefinitionKey, ShapeId>,
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
struct ProcedureEmitter<'artifact, 'module> {
    artifact: &'artifact mut Artifact,
    module_key: &'module ModuleKey,
    layout: &'module ModuleLayout,
    tables: EmitterTables<'module>,
    locals: LocalSlots,
    synthetic_locals: SyntheticLocalSlots,
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

#[derive(Debug)]
struct EmitInvariant {
    description: Box<str>,
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
    with_emit_invariant_guard(|| lower_ir_module_impl(module, options))
}

fn lower_ir_module_impl(
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

    let entry_procedure = build_module_entry(&mut state, module, &layout);
    Ok(EmittedModule::new(
        module.module_key().clone(),
        state.artifact,
        entry_procedure,
        collect_exports(module, &layout).into_boxed_slice(),
        module.static_imports().to_vec().into_boxed_slice(),
    ))
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
    with_emit_invariant_guard(|| lower_ir_program_impl(modules, entry_module, options))
}

fn with_emit_invariant_guard<T>(
    emit: impl FnOnce() -> Result<T, EmitDiagList>,
) -> Result<T, EmitDiagList> {
    match catch_unwind(AssertUnwindSafe(emit)) {
        Ok(result) => result,
        Err(payload) => {
            let detail = payload.downcast_ref::<EmitInvariant>().map_or_else(
                || panic_payload_text(payload.as_ref()),
                |invariant| invariant.description.clone(),
            );
            Err(invariant_violation_diag(&detail))
        }
    }
}

fn invariant_violation_diag(detail: &str) -> EmitDiagList {
    vec![
        Diag::error(EmitDiagKind::EmitInvariantViolated.message())
            .with_code(EmitDiagKind::EmitInvariantViolated.code())
            .with_note(format!("detail `{detail}`")),
    ]
}

fn panic_payload_text(payload: &(dyn Any + Send)) -> Box<str> {
    if let Some(text) = payload.downcast_ref::<String>() {
        return text.clone().into_boxed_str();
    }
    if let Some(text) = payload.downcast_ref::<&'static str>() {
        return (*text).into();
    }
    "<non-string panic payload>".into()
}

fn lower_ir_program_impl(
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
    let module_entry_procedures = modules
        .iter()
        .zip(layouts.iter())
        .filter_map(|(module, layout)| build_module_entry(&mut state, module, layout))
        .collect::<Vec<_>>();
    let entry_procedure =
        build_program_entry(&mut state.artifact, entry_module, &module_entry_procedures);
    if !state.diags.is_empty() {
        return Err(state.diags);
    }
    Ok(EmittedProgram::new(
        entry_module.clone(),
        state.artifact,
        entry_procedure,
        modules
            .iter()
            .map(|module| module.module_key().clone())
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    ))
}

fn build_unique_maps(state: &mut ProgramState, modules: &[&IrModule], layouts: &[ModuleLayout]) {
    let mut procedure_candidates = HashMap::<Box<str>, Vec<ProcedureId>>::new();
    let mut foreign_candidates = HashMap::<Box<str>, Vec<ForeignId>>::new();
    let mut global_candidates = HashMap::<Box<str>, Vec<GlobalId>>::new();
    for (module, layout) in modules.iter().zip(layouts) {
        for callable in module.callables() {
            if let Some(procedure) = layout
                .callables_by_name
                .get(callable.name.as_ref())
                .copied()
            {
                let _ = state.qualified.procedures.insert(
                    (module.module_key().clone(), callable.name.clone()),
                    procedure,
                );
                procedure_candidates
                    .entry(callable.name.clone())
                    .or_default()
                    .push(procedure);
            }
        }
        for foreign in module.foreigns() {
            if let Some(binding) = foreign.binding
                && let Some(id) = layout.foreigns.get(&binding).copied()
            {
                let _ = state
                    .qualified
                    .foreigns
                    .insert((module.module_key().clone(), foreign.name.clone()), id);
                foreign_candidates
                    .entry(foreign.name.clone())
                    .or_default()
                    .push(id);
            }
        }
        for global in module.globals() {
            let id = global
                .binding
                .and_then(|binding| layout.globals.get(&binding).copied());
            if let Some(id) = id {
                let _ = state
                    .qualified
                    .globals
                    .insert((module.module_key().clone(), global.name.clone()), id);
                global_candidates
                    .entry(global.name.clone())
                    .or_default()
                    .push(id);
            }
        }
    }
    state.unique.procedures = procedure_candidates
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
    for callable in module.callables() {
        let Some(procedure_id) = layout
            .callables_by_name
            .get(callable.name.as_ref())
            .copied()
        else {
            continue;
        };
        let (locals, synthetic_locals) = build_param_locals(&callable.params);
        let mut emitter = procedure_emitter(
            artifact,
            module.module_key(),
            layout,
            tables,
            locals,
            synthetic_locals,
        );
        expr::compile_expr(&mut emitter, &callable.body, true, diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finish_emitter_procedure(emitter, procedure_id);
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
    for global in module.globals() {
        let Some(binding) = global.binding else {
            continue;
        };
        let Some(global_id) = layout.globals.get(&binding).copied() else {
            continue;
        };
        let Some(init_procedure) = artifact.globals.get(global_id).initializer else {
            continue;
        };
        let mut emitter = procedure_emitter(
            artifact,
            module.module_key(),
            layout,
            tables,
            HashMap::new(),
            HashMap::new(),
        );
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
        finish_emitter_procedure(emitter, init_procedure);
    }
}

fn build_module_entry(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &ModuleLayout,
) -> Option<ProcedureId> {
    if module.init_parts().is_empty() {
        return None;
    }
    Some(build_entry_procedure(
        state,
        &format!("{}::__module_init", module.module_key().as_str()),
        module.module_key(),
        layout,
        module.init_parts(),
    ))
}

fn build_program_entry(
    artifact: &mut Artifact,
    entry_module: &ModuleKey,
    init_procedures: &[ProcedureId],
) -> ProcedureId {
    build_call_only_entry_procedure(
        artifact,
        &format!("{}::__entry", entry_module.as_str()),
        init_procedures,
    )
}

fn entry_code(init_procedures: &[ProcedureId]) -> CodeBuffer {
    let mut code = procedure_prologue();
    for init_procedure in init_procedures {
        code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Call,
            Operand::Procedure(*init_procedure),
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
        .exports()
        .iter()
        .map(|export| {
            EmittedBinding::new(
                export.name.clone(),
                binding_export(module, export.name.as_ref(), |binding| {
                    layout.callables.get(&binding).copied()
                })
                .or_else(|| layout.callables_by_name.get(export.name.as_ref()).copied()),
                binding_export(module, export.name.as_ref(), |binding| {
                    layout.globals.get(&binding).copied()
                }),
            )
        })
        .collect()
}

fn binding_export<T, F>(module: &IrModule, export_name: &str, mut lower: F) -> Option<T>
where
    F: FnMut(NameBindingId) -> Option<T>,
{
    register::export_binding(module, export_name).and_then(&mut lower)
}

fn procedure_emitter<'artifact, 'module>(
    artifact: &'artifact mut Artifact,
    module_key: &'module ModuleKey,
    layout: &'module ModuleLayout,
    tables: EmitterTables<'module>,
    locals: LocalSlots,
    synthetic_locals: SyntheticLocalSlots,
) -> ProcedureEmitter<'artifact, 'module> {
    let next_local =
        u16::try_from(locals.len().saturating_add(synthetic_locals.len())).unwrap_or(u16::MAX);
    let labels = initial_labels(artifact);
    ProcedureEmitter {
        artifact,
        module_key,
        layout,
        tables,
        locals,
        synthetic_locals,
        temps: HashMap::new(),
        next_local,
        labels,
        code: procedure_prologue(),
    }
}

fn finish_emitter_procedure(emitter: ProcedureEmitter<'_, '_>, procedure_id: ProcedureId) {
    finalize_procedure(
        emitter.artifact,
        procedure_id,
        emitter.next_local.saturating_add(1),
        emitter.labels,
        emitter.code,
    );
}

fn build_call_only_entry_procedure(
    artifact: &mut Artifact,
    name: &str,
    init_procedures: &[ProcedureId],
) -> ProcedureId {
    let procedure_id = alloc_procedure(artifact, name, false, false, false, 0);
    let labels = initial_labels(artifact);
    finalize_procedure(
        artifact,
        procedure_id,
        1,
        labels,
        entry_code(init_procedures),
    );
    procedure_id
}

fn build_entry_procedure(
    state: &mut ProgramState,
    name: &str,
    module_key: &ModuleKey,
    layout: &ModuleLayout,
    init_parts: &[IrModuleInitPart],
) -> ProcedureId {
    let procedure_id = alloc_procedure(&mut state.artifact, name, false, false, false, 0);
    let tables = EmitterTables {
        unique: &state.unique,
        qualified: &state.qualified,
    };
    let mut emitter = procedure_emitter(
        &mut state.artifact,
        module_key,
        layout,
        tables,
        HashMap::new(),
        HashMap::new(),
    );
    emit_init_parts(&mut emitter, init_parts, &mut state.diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::Ret,
        Operand::None,
    )));
    finish_emitter_procedure(emitter, procedure_id);
    procedure_id
}

fn emit_init_parts(
    emitter: &mut ProcedureEmitter<'_, '_>,
    init_parts: &[IrModuleInitPart],
    diags: &mut EmitDiagList,
) {
    for init_part in init_parts {
        match init_part {
            IrModuleInitPart::Global { name } => {
                let Some(init_procedure) = emitter.layout.global_init_procedures.get(name).copied()
                else {
                    continue;
                };
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Call,
                    Operand::Procedure(init_procedure),
                )));
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StLoc,
                    Operand::Local(0),
                )));
            }
            IrModuleInitPart::Expr(expr) => emitter.compile_expr(expr, false, diags),
        }
    }
}

fn build_param_locals(params: &[IrParam]) -> (LocalSlots, SyntheticLocalSlots) {
    let mut locals = HashMap::new();
    let mut synthetic = HashMap::new();
    for (index, param) in params.iter().enumerate() {
        let Ok(slot) = u16::try_from(index) else {
            continue;
        };
        if let Some(binding) = param.binding {
            let _ = locals.insert(binding, slot);
        } else {
            let _ = synthetic.insert(param.name.clone(), slot);
        }
    }
    (locals, synthetic)
}

fn emit_zero(emitter: &mut ProcedureEmitter<'_, '_>) {
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdCI4,
        Operand::I16(0),
    )));
}

fn procedure_prologue() -> CodeBuffer {
    vec![CodeEntry::Label(Label { id: 0 })]
}

fn initial_labels(artifact: &mut Artifact) -> Vec<StringId> {
    vec![artifact.intern_string("L0")]
}

fn alloc_procedure(
    artifact: &mut Artifact,
    name: &str,
    export: bool,
    hot: bool,
    cold: bool,
    params: u16,
) -> ProcedureId {
    let name_id = artifact.intern_string(name);
    artifact.procedures.alloc(
        ProcedureDescriptor::new(name_id, params, 0, Box::new([]))
            .with_export(export)
            .with_hot(hot)
            .with_cold(cold),
    )
}

fn finalize_procedure(
    artifact: &mut Artifact,
    procedure_id: ProcedureId,
    locals: u16,
    labels: Vec<StringId>,
    code: CodeBuffer,
) {
    let procedure = artifact.procedures.get_mut(procedure_id);
    procedure.locals = locals;
    procedure.labels = labels.into_boxed_slice();
    procedure.code = code.into_boxed_slice();
}

fn qualified_name(module: &ModuleKey, local: &str) -> Box<str> {
    format!("{}::{local}", module.as_str()).into()
}

fn answer_type_name(effect: &DefinitionKey) -> Box<str> {
    let base = qualified_name(&effect.module, &effect.name);
    format!("{base}::answer").into()
}
