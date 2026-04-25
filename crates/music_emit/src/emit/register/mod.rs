use super::*;

mod descriptors;
mod exports;
mod expr_types;
mod symbols;

use descriptors::{
    register_callables, register_data_defs, register_effects, register_foreigns, register_globals,
    register_meta, register_shapes, register_types,
};
use exports::register_exports;
use expr_types::register_expr_types;
use symbols::{ensure_effect, ensure_type};

pub(super) use exports::export_binding;

pub(super) fn register_module(
    state: &mut ProgramState,
    module: &IrModule,
    _options: EmitOptions,
) -> ModuleLayout {
    let mut layout = ModuleLayout::default();
    register_types(state, module);
    register_data_defs(state, module, &mut layout);
    register_effects(state, module, &mut layout);
    register_shapes(state, module, &mut layout);
    register_foreigns(state, module, &mut layout);
    register_callables(state, module, &mut layout);
    register_globals(state, module, &mut layout);
    register_exports(state, module, &mut layout);
    register_meta(state, module);
    register_expr_types(state, module, &mut layout);
    layout
}
