use std::collections::{BTreeSet, HashMap};
use std::slice::from_ref;

use music_base::{SourceId, Span, diag::Diag};
use music_bc::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, EffectDescriptor, EffectOpDescriptor,
    ForeignDescriptor, GlobalDescriptor, MethodDescriptor, TypeDescriptor,
};
use music_bc::{
    Artifact, CodeEntry, ForeignId, GlobalId, Instruction, Label, MethodId, Opcode, Operand,
    StringId, TypeId,
};
use music_ir::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCaseArm, IrCasePattern, IrExpr, IrExprKind, IrLit,
    IrModule, IrNameRef, IrOrigin, IrParam,
};
use music_module::ModuleKey;
use music_names::NameBindingId;

use crate::api::{EmitDiagList, EmitOptions, EmittedBinding, EmittedModule, EmittedProgram};

type MethodTable = HashMap<NameBindingId, MethodId>;
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

#[derive(Debug, Default)]
struct ModuleLayout {
    callables: MethodTable,
    foreigns: ForeignTable,
    globals: GlobalTable,
    types: TypeTable,
    init_methods: Vec<MethodId>,
}

#[derive(Debug, Default)]
struct ProgramState {
    artifact: Artifact,
    diags: EmitDiagList,
    unique_methods: UniqueMethodTable,
    unique_foreigns: UniqueForeignTable,
    unique_globals: UniqueGlobalTable,
    qualified_methods: QualifiedMethodTable,
    qualified_foreigns: QualifiedForeignTable,
    qualified_globals: QualifiedGlobalTable,
}

#[derive(Debug)]
struct MethodEmitter<'artifact, 'module> {
    artifact: &'artifact mut Artifact,
    module_key: &'module ModuleKey,
    layout: &'module ModuleLayout,
    unique_methods: &'module UniqueMethodTable,
    unique_foreigns: &'module UniqueForeignTable,
    unique_globals: &'module UniqueGlobalTable,
    qualified_methods: &'module QualifiedMethodTable,
    qualified_foreigns: &'module QualifiedForeignTable,
    qualified_globals: &'module QualifiedGlobalTable,
    locals: LocalSlots,
    next_local: u16,
    labels: Vec<StringId>,
    code: CodeBuffer,
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
    let layout = register_module(&mut state, module, options);
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
        .map(|module| register_module(&mut state, module, options))
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

fn register_module(
    state: &mut ProgramState,
    module: &IrModule,
    _options: EmitOptions,
) -> ModuleLayout {
    let mut layout = ModuleLayout::default();
    register_types(state, module);
    register_data_defs(state, module);
    register_effects(state, module);
    register_classes(state, module);
    register_foreigns(state, module, &mut layout);
    register_callables(state, module, &mut layout);
    register_globals(state, module, &mut layout);
    register_expr_types(state, module, &mut layout);
    layout
}

fn register_types(state: &mut ProgramState, module: &IrModule) {
    let mut seen = BTreeSet::<Box<str>>::new();
    for index in 0..module.types.len() {
        let name: Box<str> = format!("{}::type::{index}", module.module_key.as_str()).into();
        if seen.insert(name.clone()) {
            let name_id = state.artifact.intern_string(name.as_ref());
            let _ = state.artifact.types.alloc(TypeDescriptor { name: name_id });
        }
    }
}

fn register_data_defs(state: &mut ProgramState, module: &IrModule) {
    for data in &module.data_defs {
        let name = qualified_name(&module.module_key, &data.name);
        let name_id = state.artifact.intern_string(name.as_ref());
        let _ = state.artifact.types.alloc(TypeDescriptor { name: name_id });
    }
}

fn register_effects(state: &mut ProgramState, module: &IrModule) {
    for effect in &module.effects {
        let name = qualified_name(&effect.key.module, &effect.key.name);
        let name_id = state.artifact.intern_string(name.as_ref());
        let ops = effect
            .ops
            .iter()
            .map(|op| EffectOpDescriptor {
                name: state.artifact.intern_string(op),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let _ = state
            .artifact
            .effects
            .alloc(EffectDescriptor { name: name_id, ops });
    }
}

fn register_classes(state: &mut ProgramState, module: &IrModule) {
    for class in &module.classes {
        let name = qualified_name(&class.key.module, &class.key.name);
        let name_id = state.artifact.intern_string(name.as_ref());
        let _ = state
            .artifact
            .classes
            .alloc(ClassDescriptor { name: name_id });
    }
}

fn register_foreigns(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for foreign in &module.foreigns {
        let qualified = qualified_name(&module.module_key, &foreign.name);
        let name_id = state.artifact.intern_string(qualified.as_ref());
        let abi_id = state.artifact.intern_string(&foreign.abi);
        let symbol_id = state.artifact.intern_string(&foreign.name);
        let foreign_id = state.artifact.foreigns.alloc(ForeignDescriptor {
            name: name_id,
            abi: abi_id,
            symbol: symbol_id,
        });
        if let Some(binding) = foreign.binding {
            let _ = layout.foreigns.insert(binding, foreign_id);
        }
    }
}

fn register_callables(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for callable in &module.callables {
        let name = qualified_name(&module.module_key, &callable.name);
        let method_id = alloc_method(&mut state.artifact, name.as_ref(), callable.exported);
        if let Some(binding) = callable.binding {
            let _ = layout.callables.insert(binding, method_id);
        }
    }
}

fn register_globals(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for global in &module.globals {
        let name = qualified_name(&module.module_key, &global.name);
        let init_name = format!("{name}::init");
        let init_method = alloc_method(&mut state.artifact, &init_name, false);
        let name_id = state.artifact.intern_string(name.as_ref());
        let global_id = state.artifact.globals.alloc(GlobalDescriptor {
            name: name_id,
            export: global.exported,
            initializer: Some(init_method),
        });
        layout.init_methods.push(init_method);
        if let Some(binding) = global.binding {
            let _ = layout.globals.insert(binding, global_id);
        }
    }
}

fn register_expr_types(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for callable in &module.callables {
        collect_expr_types(state, layout, &callable.body);
    }
    for global in &module.globals {
        collect_expr_types(state, layout, &global.body);
    }
}

fn collect_expr_types(state: &mut ProgramState, layout: &mut ModuleLayout, expr: &IrExpr) {
    match &expr.kind {
        IrExprKind::Unit
        | IrExprKind::Name { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::Unsupported { .. } => {}
        IrExprKind::Sequence { exprs } => {
            for expr in exprs {
                collect_expr_types(state, layout, expr);
            }
        }
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            let _ = ensure_type(state, layout, ty_name);
            for item in items {
                collect_expr_types(state, layout, item);
            }
        }
        IrExprKind::Let { value, .. } => collect_expr_types(state, layout, value),
        IrExprKind::Assign { target, value } => {
            collect_assign_target_types(state, layout, target);
            collect_expr_types(state, layout, value);
        }
        IrExprKind::Index { base, index } => {
            collect_expr_types(state, layout, base);
            collect_expr_types(state, layout, index);
        }
        IrExprKind::ClosureNew { captures, .. } => {
            for cap in captures {
                collect_expr_types(state, layout, cap);
            }
        }
        IrExprKind::Binary { left, right, .. } => {
            collect_expr_types(state, layout, left);
            collect_expr_types(state, layout, right);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_expr_types(state, layout, scrutinee);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_types(state, layout, guard);
                }
                collect_expr_types(state, layout, &arm.expr);
            }
        }
        IrExprKind::Call { callee, args } => {
            collect_expr_types(state, layout, callee);
            for arg in args {
                collect_expr_types(state, layout, &arg.expr);
            }
        }
    }
}

fn collect_assign_target_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    target: &IrAssignTarget,
) {
    if let IrAssignTarget::Index { base, index } = target {
        collect_expr_types(state, layout, base);
        collect_expr_types(state, layout, index);
    }
}

fn ensure_type(state: &mut ProgramState, layout: &mut ModuleLayout, ty_name: &str) -> TypeId {
    if let Some(id) = layout.types.get(ty_name).copied() {
        return id;
    }
    let name_id = state.artifact.intern_string(ty_name);
    let type_id = state.artifact.types.alloc(TypeDescriptor { name: name_id });
    let _ = layout.types.insert(ty_name.into(), type_id);
    type_id
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
                    .qualified_methods
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
                    .qualified_foreigns
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
                    .qualified_globals
                    .insert((module.module_key.clone(), global.name.clone()), id);
                global_candidates
                    .entry(global.name.clone())
                    .or_default()
                    .push(id);
            }
        }
    }
    state.unique_methods = method_candidates
        .into_iter()
        .filter_map(unique_candidate)
        .collect();
    state.unique_foreigns = foreign_candidates
        .into_iter()
        .filter_map(unique_candidate)
        .collect();
    state.unique_globals = global_candidates
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
    for callable in &module.callables {
        let Some(binding) = callable.binding else {
            continue;
        };
        let Some(method_id) = layout.callables.get(&binding).copied() else {
            continue;
        };
        let locals = build_param_locals(&callable.params);
        let next_local = u16::try_from(locals.len()).unwrap_or(u16::MAX);
        let labels = initial_labels(&mut state.artifact);
        let mut emitter = MethodEmitter {
            artifact: &mut state.artifact,
            module_key: &module.module_key,
            layout,
            unique_methods: &state.unique_methods,
            unique_foreigns: &state.unique_foreigns,
            unique_globals: &state.unique_globals,
            qualified_methods: &state.qualified_methods,
            qualified_foreigns: &state.qualified_foreigns,
            qualified_globals: &state.qualified_globals,
            locals,
            next_local,
            labels,
            code: method_prologue(),
        };
        compile_expr(&mut emitter, &callable.body, true, &mut state.diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finalize_method(
            emitter.artifact,
            method_id,
            emitter.next_local.saturating_add(1),
            emitter.labels,
            emitter.code,
        );
    }
}

fn compile_globals(state: &mut ProgramState, module: &IrModule, layout: &ModuleLayout) {
    for global in &module.globals {
        let Some(binding) = global.binding else {
            continue;
        };
        let Some(global_id) = layout.globals.get(&binding).copied() else {
            continue;
        };
        let Some(init_method) = state.artifact.globals.get(global_id).initializer else {
            continue;
        };
        let labels = initial_labels(&mut state.artifact);
        let mut emitter = MethodEmitter {
            artifact: &mut state.artifact,
            module_key: &module.module_key,
            layout,
            unique_methods: &state.unique_methods,
            unique_foreigns: &state.unique_foreigns,
            unique_globals: &state.unique_globals,
            qualified_methods: &state.qualified_methods,
            qualified_foreigns: &state.qualified_foreigns,
            qualified_globals: &state.qualified_globals,
            locals: HashMap::new(),
            next_local: 0,
            labels,
            code: method_prologue(),
        };
        compile_expr(&mut emitter, &global.body, true, &mut state.diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StGlob,
            Operand::Global(global_id),
        )));
        emit_zero(&mut emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finalize_method(
            emitter.artifact,
            init_method,
            emitter.next_local.saturating_add(1),
            emitter.labels,
            emitter.code,
        );
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
    let entry_name = format!("{}::__module_init", module_key.as_str());
    let method_id = alloc_method(artifact, &entry_name, false);
    let labels = initial_labels(artifact);
    let code = entry_code(&layout.init_methods);
    finalize_method(artifact, method_id, 1, labels, code);
    Some(method_id)
}

fn build_program_entry(
    artifact: &mut Artifact,
    entry_module: &ModuleKey,
    layouts: &[ModuleLayout],
) -> MethodId {
    let entry_name = format!("{}::__entry", entry_module.as_str());
    let method_id = alloc_method(artifact, &entry_name, false);
    let init_methods = layouts
        .iter()
        .flat_map(|layout| layout.init_methods.iter().copied())
        .collect::<Vec<_>>();
    let labels = initial_labels(artifact);
    let code = entry_code(&init_methods);
    finalize_method(artifact, method_id, 1, labels, code);
    method_id
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
            method: module
                .callables
                .iter()
                .find(|callable| callable.name.as_ref() == export.name.as_ref())
                .and_then(|callable| callable.binding)
                .and_then(|binding| layout.callables.get(&binding).copied()),
            global: module
                .globals
                .iter()
                .find(|global| global.name.as_ref() == export.name.as_ref())
                .and_then(|global| global.binding)
                .and_then(|binding| layout.globals.get(&binding).copied()),
        })
        .collect()
}

fn build_param_locals(params: &[IrParam]) -> LocalSlots {
    params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| u16::try_from(index).ok().map(|slot| (param.binding, slot)))
        .collect()
}

fn compile_expr(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    keep_result: bool,
    diags: &mut EmitDiagList,
) {
    match &expr.kind {
        IrExprKind::Unit => emit_zero(emitter),
        IrExprKind::Name {
            binding,
            name,
            module_target,
            ..
        } => compile_name(emitter, *binding, name, module_target.as_ref(), expr, diags),
        IrExprKind::Lit(lit) => compile_lit(emitter, lit, &expr.origin, diags),
        IrExprKind::Sequence { exprs } => compile_sequence(emitter, exprs, diags),
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            compile_sequence_literal(emitter, ty_name, items, diags);
        }
        IrExprKind::Let {
            binding,
            name,
            value,
            ..
        } => compile_let(emitter, *binding, name, value, diags),
        IrExprKind::Assign { target, value } => compile_assign(emitter, target, value, diags),
        IrExprKind::Index { base, index } => compile_index(emitter, base, index, diags),
        IrExprKind::ClosureNew { callee, captures } => {
            compile_closure_new(emitter, callee, captures, diags);
        }
        IrExprKind::Binary { op, left, right } => {
            compile_binary(emitter, op, left, right, diags);
        }
        IrExprKind::Case { scrutinee, arms } => compile_case(emitter, scrutinee, arms, diags),
        IrExprKind::Call { callee, args } => compile_call(emitter, callee, args, diags),
        IrExprKind::Unsupported { description } => {
            push_expr_diag(
                diags,
                emitter.module_key,
                &expr.origin,
                format!("unsupported emitted expression `{description}`"),
            );
            emit_zero(emitter);
        }
    }
    if !keep_result {
        let slot = scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
}

fn compile_lit(
    emitter: &mut MethodEmitter<'_, '_>,
    lit: &IrLit,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    match lit {
        IrLit::Int { raw } => compile_int_literal(emitter, raw, origin, diags),
        IrLit::Float { raw } => compile_float_literal(emitter, raw, origin, diags),
        IrLit::String { value } => compile_string_literal(emitter, value),
        IrLit::Rune { value } => compile_i64(emitter, i64::from(*value)),
    }
}

fn compile_int_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    raw: &str,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    if let Some(value) = parse_int_literal(raw) {
        compile_i64(emitter, value);
    } else {
        push_expr_diag(
            diags,
            emitter.module_key,
            origin,
            format!("invalid integer literal `{raw}`"),
        );
        emit_zero(emitter);
    }
}

fn compile_string_literal(emitter: &mut MethodEmitter<'_, '_>, value: &str) {
    let string_id = emitter.artifact.intern_string(value);
    let const_name = format!("const:string:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::String(string_id),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_float_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    raw: &str,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    let compact = raw.replace('_', "");
    let Ok(value) = compact.parse::<f64>() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            origin,
            format!("invalid float literal `{raw}`"),
        );
        emit_zero(emitter);
        return;
    };
    let const_name = format!("const:float:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::Float(value),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_i64(emitter: &mut MethodEmitter<'_, '_>, value: i64) {
    if let Ok(short) = i16::try_from(value) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdSmi,
            Operand::I16(short),
        )));
        return;
    }
    let const_name = format!("const:int:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::Int(value),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_name(
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
    push_expr_diag(
        diags,
        emitter.module_key,
        &expr.origin,
        format!("unsupported emitted name reference `{name}`"),
    );
    emit_zero(emitter);
}

fn compile_sequence(
    emitter: &mut MethodEmitter<'_, '_>,
    exprs: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for (index, expr) in exprs.iter().enumerate() {
        let keep_result = index + 1 == exprs.len();
        compile_expr(emitter, expr, keep_result, diags);
    }
}

fn compile_sequence_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    ty_name: &str,
    items: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for item in items {
        compile_expr(emitter, item, true, diags);
    }
    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        let missing_origin = items.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |expr| expr.origin,
        );
        push_expr_diag(
            diags,
            emitter.module_key,
            &missing_origin,
            format!("unknown emitted sequence type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqNew,
        Operand::TypeLen {
            ty,
            len: u16::try_from(items.len()).unwrap_or(u16::MAX),
        },
    )));
}

fn compile_let(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    _name: &str,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, value, true, diags);
    if let Some(binding) = binding {
        let slot = ensure_local_slot(emitter, binding);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    } else {
        let slot = scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
    emit_zero(emitter);
}

fn compile_assign(
    emitter: &mut MethodEmitter<'_, '_>,
    target: &IrAssignTarget,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    match target {
        IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        } => {
            compile_expr(emitter, value, true, diags);
            if let Some(binding) = binding
                && let Some(slot) = emitter.locals.get(binding).copied()
            {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StLoc,
                    Operand::Local(slot),
                )));
                emit_zero(emitter);
                return;
            }
            if let Some(global) = resolve_global(emitter, *binding, name, module_target.as_ref()) {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StGlob,
                    Operand::Global(global),
                )));
                emit_zero(emitter);
                return;
            }
            push_expr_diag(
                diags,
                emitter.module_key,
                &value.origin,
                format!("unsupported emitted assignment target `{name}`"),
            );
            emit_zero(emitter);
        }
        IrAssignTarget::Index { base, index } => {
            compile_expr(emitter, base, true, diags);
            compile_expr(emitter, index, true, diags);
            compile_expr(emitter, value, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::SeqSet,
                Operand::None,
            )));
            emit_zero(emitter);
        }
    }
}

fn compile_index(
    emitter: &mut MethodEmitter<'_, '_>,
    base: &IrExpr,
    index: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, base, true, diags);
    compile_expr(emitter, index, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqGet,
        Operand::None,
    )));
}

fn compile_binary(
    emitter: &mut MethodEmitter<'_, '_>,
    op: &IrBinaryOp,
    left: &IrExpr,
    right: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, left, true, diags);
    compile_expr(emitter, right, true, diags);
    let opcode = match op {
        IrBinaryOp::Add => Opcode::IAdd,
        IrBinaryOp::Sub => Opcode::ISub,
        IrBinaryOp::Mul => Opcode::IMul,
        IrBinaryOp::Div => Opcode::IDiv,
        IrBinaryOp::Rem => Opcode::IRem,
        IrBinaryOp::Eq => Opcode::CmpEq,
        IrBinaryOp::Ne => Opcode::CmpNe,
        IrBinaryOp::Lt => Opcode::CmpLt,
        IrBinaryOp::Gt => Opcode::CmpGt,
        IrBinaryOp::Le => Opcode::CmpLe,
        IrBinaryOp::Ge => Opcode::CmpGe,
        IrBinaryOp::Other(name) => {
            push_expr_diag(
                diags,
                emitter.module_key,
                &left.origin,
                format!("unsupported emitted binary operator `{name}`"),
            );
            Opcode::CmpEq
        }
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        opcode,
        Operand::None,
    )));
}

fn compile_case(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee: &IrExpr,
    arms: &[IrCaseArm],
    diags: &mut EmitDiagList,
) {
    let scrutinee_slot = reserve_temp_slot(emitter);
    let end_label = alloc_label(emitter);
    compile_expr(emitter, scrutinee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(scrutinee_slot),
    )));

    for arm in arms {
        let next_label = alloc_label(emitter);
        if !compile_case_pattern(emitter, &arm.pattern, scrutinee_slot, next_label, diags) {
            continue;
        }
        if let Some(guard) = &arm.guard {
            compile_expr(emitter, guard, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
        }
        compile_expr(emitter, &arm.expr, true, diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Br,
            Operand::Label(end_label),
        )));
        emitter
            .code
            .push(CodeEntry::Label(Label { id: next_label }));
    }

    emit_zero(emitter);
    emitter.code.push(CodeEntry::Label(Label { id: end_label }));
}

fn compile_case_pattern(
    emitter: &mut MethodEmitter<'_, '_>,
    pattern: &IrCasePattern,
    scrutinee_slot: u16,
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    match pattern {
        IrCasePattern::Wildcard => true,
        IrCasePattern::Bind { binding, .. } => {
            let slot = ensure_local_slot(emitter, *binding);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(scrutinee_slot),
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
            true
        }
        IrCasePattern::Lit(lit) => {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(scrutinee_slot),
            )));
            compile_lit(
                emitter,
                lit,
                &IrOrigin {
                    source_id: SourceId::from_raw(0),
                    span: Span::new(0, 0),
                },
                diags,
            );
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CmpEq,
                Operand::None,
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
            true
        }
    }
}

fn compile_call(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrExpr,
    args: &[IrArg],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        if arg.spread {
            push_expr_diag(
                diags,
                emitter.module_key,
                &arg.expr.origin,
                "spread call arguments are not yet emitted".into(),
            );
        }
        compile_expr(emitter, &arg.expr, true, diags);
    }
    if let IrExprKind::Name {
        binding,
        name,
        module_target,
        ..
    } = &callee.kind
    {
        if let Some(binding) = binding
            && let Some(slot) = emitter.locals.get(binding).copied()
        {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CallCls,
                Operand::None,
            )));
            return;
        }
        if let Some(method) = resolve_method(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Call,
                Operand::Method(method),
            )));
            return;
        }
        if let Some(foreign) = resolve_foreign(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::FfiCall,
                Operand::Foreign(foreign),
            )));
            return;
        }
    }

    compile_expr(emitter, callee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CallCls,
        Operand::None,
    )));
}

fn compile_closure_new(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrNameRef,
    captures: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for cap in captures {
        compile_expr(emitter, cap, true, diags);
    }
    let Some(method) = resolve_method(
        emitter,
        callee.binding,
        callee.name.as_ref(),
        callee.module_target.as_ref(),
    ) else {
        let origin = captures.first().map_or_else(|| IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        }, |expr| expr.origin);
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            format!("unknown emitted closure target `{}`", callee.name),
        );
        emit_zero(emitter);
        return;
    };
    let captures = u8::try_from(captures.len()).unwrap_or(u8::MAX);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::ClsNew,
        Operand::WideMethodCaptures { method, captures },
    )));
}

fn resolve_method(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<MethodId> {
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

fn resolve_foreign(
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

fn resolve_global(
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

fn alloc_method(artifact: &mut Artifact, name: &str, export: bool) -> MethodId {
    let name_id = artifact.intern_string(name);
    artifact.methods.alloc(MethodDescriptor {
        name: name_id,
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

fn ensure_local_slot(emitter: &mut MethodEmitter<'_, '_>, binding: NameBindingId) -> u16 {
    if let Some(slot) = emitter.locals.get(&binding).copied() {
        return slot;
    }
    let slot = reserve_temp_slot(emitter);
    let _ = emitter.locals.insert(binding, slot);
    slot
}

const fn reserve_temp_slot(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let slot = emitter.next_local;
    emitter.next_local = emitter.next_local.saturating_add(1);
    slot
}

const fn scratch_slot(emitter: &MethodEmitter<'_, '_>) -> u16 {
    emitter.next_local
}

fn alloc_label(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let id = u16::try_from(emitter.labels.len()).unwrap_or(u16::MAX);
    let name = format!("L{id}");
    emitter.labels.push(emitter.artifact.intern_string(&name));
    id
}

fn qualified_name(module: &ModuleKey, local: &str) -> Box<str> {
    format!("{}::{local}", module.as_str()).into()
}

fn parse_int_literal(raw: &str) -> Option<i64> {
    let compact = raw.replace('_', "");
    let (sign, digits) = compact
        .strip_prefix('-')
        .map_or((1_i64, compact.as_str()), |rest| (-1_i64, rest));
    let (radix, digits) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
        .map_or_else(
            || {
                digits
                    .strip_prefix("0o")
                    .or_else(|| digits.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            digits
                                .strip_prefix("0b")
                                .or_else(|| digits.strip_prefix("0B"))
                                .map_or((10, digits), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    i64::from_str_radix(digits, radix)
        .ok()
        .map(|value| value * sign)
}

fn push_expr_diag(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    origin: &IrOrigin,
    message: String,
) {
    push_span_diag(diags, module_key, origin.source_id, origin.span, message);
}

fn push_span_diag(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    source_id: SourceId,
    span: Span,
    message: String,
) {
    diags.push(Diag::error(message).with_label(span, source_id, module_key.as_str()));
}
