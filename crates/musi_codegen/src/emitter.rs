#![allow(clippy::too_many_lines)]

use std::collections::{HashMap, HashSet};

use musi_ast::ParsedModule;
use musi_shared::Interner;

use crate::Module;
use crate::error::CodegenError;

mod assemble;
mod call;
mod control;
mod expr;
mod field;
mod pattern;
mod register;
mod state;

use state::EmitState;

/// Compile a set of parsed modules into a bytecode `Module`.
///
/// # Errors
///
/// Returns `CodegenError` if any expression cannot be compiled.
pub fn emit(
    prelude: &ParsedModule,
    deps: &[&ParsedModule],
    user: &ParsedModule,
    interner: &Interner,
) -> Result<Module, CodegenError> {
    let mut module = Module::new();
    let mut state = EmitState {
        fn_map: HashMap::new(),
        fn_return_types: HashMap::new(),
        type_map: HashMap::new(),
        variant_map: HashMap::new(),
        lambda_counter: 0,
        pending_lambdas: Vec::new(),
        class_method_names: HashSet::new(),
        type_tag_map: HashMap::new(),
        next_type_tag: 10,
    };

    register::register_module_decls(prelude, interner, &mut module, &mut state)?;
    for &dep in deps {
        register::register_module_decls(dep, interner, &mut module, &mut state)?;
    }
    register::register_module_decls(user, interner, &mut module, &mut state)?;

    assemble::emit_module_fn_bodies(prelude, interner, &mut state, &mut module)?;
    for &dep in deps {
        assemble::emit_module_fn_bodies(dep, interner, &mut state, &mut module)?;
    }
    assemble::emit_module_fn_bodies(user, interner, &mut state, &mut module)?;

    assemble::emit_main_body(user, interner, &mut state, &mut module)?;

    Ok(module)
}
