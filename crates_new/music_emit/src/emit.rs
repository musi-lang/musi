use std::collections::{BTreeSet, HashMap};
use std::slice::from_ref;

use music_arena::SliceRange;
use music_base::{SourceId, Span, diag::Diag};
use music_bc::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, EffectDescriptor, EffectOpDescriptor,
    ForeignDescriptor, GlobalDescriptor, MethodDescriptor, TypeDescriptor,
};
use music_bc::{
    Artifact, CodeEntry, ForeignId, GlobalId, Instruction, Label, MethodId, Opcode, Operand,
};
use music_hir::{
    HirArg, HirBinaryOp, HirExprId, HirExprKind, HirLitId, HirLitKind, HirParam, HirPatKind,
};
use music_ir::IrModule;
use music_module::ModuleKey;
use music_names::Symbol;

use crate::api::{EmitDiagList, EmitOptions, EmittedBinding, EmittedModule, EmittedProgram};

#[derive(Debug, Clone, PartialEq, Eq)]
struct TopLevelLet {
    name: Symbol,
    params: SliceRange<HirParam>,
    value: HirExprId,
    exported: bool,
}

#[derive(Debug, Default)]
struct ModuleLayout {
    callables: HashMap<Symbol, MethodId>,
    foreigns: HashMap<Symbol, ForeignId>,
    globals: HashMap<Symbol, GlobalId>,
    init_methods: Vec<MethodId>,
}

#[derive(Debug, Default)]
struct ProgramState {
    artifact: Artifact,
    diags: EmitDiagList,
    unique_methods: HashMap<Symbol, MethodId>,
    unique_foreigns: HashMap<Symbol, ForeignId>,
}

#[derive(Debug)]
struct MethodEmitter<'artifact, 'module> {
    artifact: &'artifact mut Artifact,
    ir: &'module IrModule,
    layout: &'module ModuleLayout,
    unique_methods: &'module HashMap<Symbol, MethodId>,
    unique_foreigns: &'module HashMap<Symbol, ForeignId>,
    locals: HashMap<Symbol, u16>,
    scratch_slot: u16,
    code: Vec<CodeEntry>,
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
    let top_levels = scan_top_level_lets(module);
    let layout = register_module(&mut state, module, &top_levels, options);
    build_unique_maps(&mut state, &modules, from_ref(&layout));
    compile_module(&mut state, module, &layout, &top_levels);
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
    let top_levels = modules.iter().map(scan_top_level_lets).collect::<Vec<_>>();
    let layouts = modules
        .iter()
        .zip(top_levels.iter())
        .map(|(module, lets)| register_module(&mut state, module, lets, options))
        .collect::<Vec<_>>();
    let module_refs = modules.iter().collect::<Vec<_>>();
    build_unique_maps(&mut state, &module_refs, &layouts);
    for ((module, layout), lets) in modules.iter().zip(layouts.iter()).zip(top_levels.iter()) {
        compile_module(&mut state, module, layout, lets);
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
    top_levels: &HashMap<Symbol, TopLevelLet>,
    _options: EmitOptions,
) -> ModuleLayout {
    let mut layout = ModuleLayout::default();
    register_types(state, module);
    register_data_defs(state, module);
    register_effects(state, module);
    register_classes(state, module);
    register_foreigns(state, module, &mut layout);
    register_callables(state, module, &mut layout);
    register_globals(state, module, top_levels, &mut layout);
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
        let _ = layout.foreigns.insert(foreign.symbol, foreign_id);
    }
}

fn register_callables(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for callable in &module.callables {
        let name = qualified_name(&module.module_key, &callable.name);
        let export = module.exported_value(&callable.name).is_some();
        let method_id = alloc_method(&mut state.artifact, name.as_ref(), export);
        let _ = layout.callables.insert(callable.symbol, method_id);
    }
}

fn register_globals(
    state: &mut ProgramState,
    module: &IrModule,
    top_levels: &HashMap<Symbol, TopLevelLet>,
    layout: &mut ModuleLayout,
) {
    for global in &module.globals {
        let name = qualified_name(&module.module_key, &global.name);
        let init_name = format!("{name}::init");
        let init_method = alloc_method(&mut state.artifact, &init_name, false);
        let export = module.exported_value(&global.name).is_some()
            || top_levels
                .get(&global.symbol)
                .is_some_and(|binding| binding.exported);
        let name_id = state.artifact.intern_string(name.as_ref());
        let global_id = state.artifact.globals.alloc(GlobalDescriptor {
            name: name_id,
            export,
            initializer: Some(init_method),
        });
        layout.init_methods.push(init_method);
        let _ = layout.globals.insert(global.symbol, global_id);
    }
}

fn build_unique_maps(state: &mut ProgramState, modules: &[&IrModule], layouts: &[ModuleLayout]) {
    let mut method_candidates = HashMap::<Symbol, Vec<MethodId>>::new();
    let mut foreign_candidates = HashMap::<Symbol, Vec<ForeignId>>::new();
    for (module, layout) in modules.iter().zip(layouts) {
        for callable in &module.callables {
            if let Some(method) = layout.callables.get(&callable.symbol).copied() {
                method_candidates
                    .entry(callable.symbol)
                    .or_default()
                    .push(method);
            }
        }
        for foreign in &module.foreigns {
            if let Some(id) = layout.foreigns.get(&foreign.symbol).copied() {
                foreign_candidates
                    .entry(foreign.symbol)
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
}

fn unique_candidate<T: Copy>((symbol, ids): (Symbol, Vec<T>)) -> Option<(Symbol, T)> {
    ids.first()
        .copied()
        .filter(|_| ids.len() == 1)
        .map(|id| (symbol, id))
}

fn compile_module(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &ModuleLayout,
    top_levels: &HashMap<Symbol, TopLevelLet>,
) {
    compile_callables(state, module, layout, top_levels);
    compile_globals(state, module, layout);
}

fn compile_callables(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &ModuleLayout,
    top_levels: &HashMap<Symbol, TopLevelLet>,
) {
    for callable in &module.callables {
        let Some(method_id) = layout.callables.get(&callable.symbol).copied() else {
            continue;
        };
        let Some(binding) = top_levels.get(&callable.symbol).cloned() else {
            continue;
        };
        let locals = build_param_locals(module, binding.params);
        let scratch_slot = u16::try_from(locals.len()).unwrap_or(u16::MAX);
        let mut emitter = MethodEmitter {
            artifact: &mut state.artifact,
            ir: module,
            layout,
            unique_methods: &state.unique_methods,
            unique_foreigns: &state.unique_foreigns,
            locals,
            scratch_slot,
            code: method_prologue(),
        };
        compile_expr(&mut emitter, binding.value, true, &mut state.diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finalize_method(
            emitter.artifact,
            method_id,
            scratch_slot.saturating_add(1),
            emitter.code,
        );
    }
}

fn compile_globals(state: &mut ProgramState, module: &IrModule, layout: &ModuleLayout) {
    for global in &module.globals {
        let Some(global_id) = layout.globals.get(&global.symbol).copied() else {
            continue;
        };
        let Some(init_method) = state.artifact.globals.get(global_id).initializer else {
            continue;
        };
        let mut emitter = MethodEmitter {
            artifact: &mut state.artifact,
            ir: module,
            layout,
            unique_methods: &state.unique_methods,
            unique_foreigns: &state.unique_foreigns,
            locals: HashMap::new(),
            scratch_slot: 0,
            code: method_prologue(),
        };
        compile_expr(&mut emitter, global.expr, true, &mut state.diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        )));
        finalize_method(emitter.artifact, init_method, 1, emitter.code);
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
    let code = entry_code(&layout.init_methods);
    finalize_method(artifact, method_id, 1, code);
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
    let code = entry_code(&init_methods);
    finalize_method(artifact, method_id, 1, code);
    method_id
}

fn entry_code(init_methods: &[MethodId]) -> Vec<CodeEntry> {
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
                .and_then(|callable| layout.callables.get(&callable.symbol).copied()),
            global: module
                .globals
                .iter()
                .find(|global| global.name.as_ref() == export.name.as_ref())
                .and_then(|global| layout.globals.get(&global.symbol).copied()),
        })
        .collect()
}

fn scan_top_level_lets(module: &IrModule) -> HashMap<Symbol, TopLevelLet> {
    let mut lets = HashMap::new();
    collect_top_level_lets(module, module.root, false, &mut lets);
    lets
}

fn collect_top_level_lets(
    module: &IrModule,
    expr_id: HirExprId,
    exported: bool,
    lets: &mut HashMap<Symbol, TopLevelLet>,
) {
    match module.hir.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in module.hir.store.expr_ids.get(exprs).iter().copied() {
                collect_top_level_lets(module, expr, false, lets);
            }
        }
        HirExprKind::Export { expr, .. } => collect_top_level_lets(module, expr, true, lets),
        HirExprKind::Let {
            pat, params, value, ..
        } => {
            let HirPatKind::Bind { name } = module.hir.store.pats.get(pat).kind else {
                return;
            };
            let _ = lets.insert(
                name.name,
                TopLevelLet {
                    name: name.name,
                    params,
                    value,
                    exported,
                },
            );
        }
        _ => {}
    }
}

fn build_param_locals(module: &IrModule, params: SliceRange<HirParam>) -> HashMap<Symbol, u16> {
    module
        .hir
        .store
        .params
        .get(params)
        .iter()
        .enumerate()
        .filter_map(|(index, param)| {
            u16::try_from(index)
                .ok()
                .map(|slot| (param.name.name, slot))
        })
        .collect()
}

fn compile_expr(
    emitter: &mut MethodEmitter<'_, '_>,
    expr_id: HirExprId,
    keep_result: bool,
    diags: &mut EmitDiagList,
) {
    match emitter.ir.hir.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => compile_lit(emitter, lit, diags),
        HirExprKind::Name { name } => compile_name(emitter, name.name, expr_id, diags),
        HirExprKind::Sequence { exprs } => compile_sequence(emitter, exprs, diags),
        HirExprKind::Binary { op, left, right } => {
            compile_binary(emitter, &op, left, right, diags);
        }
        HirExprKind::Call { callee, args } => compile_call(emitter, callee, args, diags),
        other => {
            push_expr_diag(
                diags,
                emitter.ir,
                expr_id,
                format!("unsupported emitted expression `{other:?}`"),
            );
            emit_zero(emitter);
        }
    }
    if !keep_result {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(emitter.scratch_slot),
        )));
    }
}

fn compile_lit(emitter: &mut MethodEmitter<'_, '_>, lit: HirLitId, diags: &mut EmitDiagList) {
    match &emitter.ir.hir.store.lits.get(lit).kind {
        HirLitKind::Int { raw } => compile_int_literal(emitter, raw, lit, diags),
        HirLitKind::String { value } => compile_string_literal(emitter, value),
        HirLitKind::Rune { value } => compile_i64(emitter, i64::from(*value)),
        HirLitKind::Float { .. } => {
            let origin = emitter.ir.hir.store.lits.get(lit).origin;
            push_span_diag(
                diags,
                &emitter.ir.module_key,
                origin.source_id,
                origin.span,
                "float literals are not yet emitted".into(),
            );
            emit_zero(emitter);
        }
    }
}

fn compile_int_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    raw: &str,
    lit: HirLitId,
    diags: &mut EmitDiagList,
) {
    if let Some(value) = parse_int_literal(raw) {
        compile_i64(emitter, value);
    } else {
        let origin = emitter.ir.hir.store.lits.get(lit).origin;
        push_span_diag(
            diags,
            &emitter.ir.module_key,
            origin.source_id,
            origin.span,
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
    name: Symbol,
    expr_id: HirExprId,
    diags: &mut EmitDiagList,
) {
    if let Some(slot) = emitter.locals.get(&name).copied() {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
        return;
    }
    push_expr_diag(
        diags,
        emitter.ir,
        expr_id,
        format!("unsupported emitted name reference `{name}`"),
    );
    emit_zero(emitter);
}

fn compile_sequence(
    emitter: &mut MethodEmitter<'_, '_>,
    exprs: SliceRange<HirExprId>,
    diags: &mut EmitDiagList,
) {
    let items = emitter.ir.hir.store.expr_ids.get(exprs);
    for (index, expr) in items.iter().copied().enumerate() {
        let keep_result = index + 1 == items.len();
        compile_expr(emitter, expr, keep_result, diags);
    }
}

fn compile_binary(
    emitter: &mut MethodEmitter<'_, '_>,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, left, true, diags);
    compile_expr(emitter, right, true, diags);
    let opcode = match op {
        HirBinaryOp::Add => Opcode::IAdd,
        HirBinaryOp::Eq => Opcode::CmpEq,
        _ => {
            push_expr_diag(
                diags,
                emitter.ir,
                left,
                format!("unsupported emitted binary operator `{op:?}`"),
            );
            Opcode::CmpEq
        }
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        opcode,
        Operand::None,
    )));
}

fn compile_call(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: HirExprId,
    args: SliceRange<HirArg>,
    diags: &mut EmitDiagList,
) {
    for arg in emitter.ir.hir.store.args.get(args) {
        if arg.spread {
            push_expr_diag(
                diags,
                emitter.ir,
                arg.expr,
                "spread call arguments are not yet emitted".into(),
            );
        }
        compile_expr(emitter, arg.expr, true, diags);
    }
    match emitter.ir.hir.store.exprs.get(callee).kind.clone() {
        HirExprKind::Name { name } => compile_named_call(emitter, name.name, callee, diags),
        other => {
            push_expr_diag(
                diags,
                emitter.ir,
                callee,
                format!("unsupported emitted call target `{other:?}`"),
            );
            emit_zero(emitter);
        }
    }
}

fn compile_named_call(
    emitter: &mut MethodEmitter<'_, '_>,
    name: Symbol,
    callee: HirExprId,
    diags: &mut EmitDiagList,
) {
    if let Some(method) = emitter
        .layout
        .callables
        .get(&name)
        .copied()
        .or_else(|| emitter.unique_methods.get(&name).copied())
    {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Call,
            Operand::Method(method),
        )));
        return;
    }
    if let Some(foreign) = emitter
        .layout
        .foreigns
        .get(&name)
        .copied()
        .or_else(|| emitter.unique_foreigns.get(&name).copied())
    {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::FfiCall,
            Operand::Foreign(foreign),
        )));
        return;
    }
    push_expr_diag(
        diags,
        emitter.ir,
        callee,
        format!("unknown emitted call target `{name}`"),
    );
    emit_zero(emitter);
}

fn emit_zero(emitter: &mut MethodEmitter<'_, '_>) {
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdSmi,
        Operand::I16(0),
    )));
}

fn method_prologue() -> Vec<CodeEntry> {
    vec![CodeEntry::Label(Label { id: 0 })]
}

fn alloc_method(artifact: &mut Artifact, name: &str, export: bool) -> MethodId {
    let name_id = artifact.intern_string(name);
    let label_id = artifact.intern_string("L0");
    artifact.methods.alloc(MethodDescriptor {
        name: name_id,
        locals: 0,
        export,
        labels: Box::new([label_id]),
        code: Box::new([]),
    })
}

fn finalize_method(
    artifact: &mut Artifact,
    method_id: MethodId,
    locals: u16,
    code: Vec<CodeEntry>,
) {
    let method = artifact.methods.get_mut(method_id);
    method.locals = locals;
    method.code = code.into_boxed_slice();
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
    module: &IrModule,
    expr_id: HirExprId,
    message: String,
) {
    let origin = module.hir.store.exprs.get(expr_id).origin;
    push_span_diag(
        diags,
        &module.module_key,
        origin.source_id,
        origin.span,
        message,
    );
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
