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

pub fn emit(
    prelude: &ParsedModule,
    deps: &[&ParsedModule],
    dep_paths: &[&str],
    user: &ParsedModule,
    interner: &Interner,
) -> Result<Module, CodegenError> {
    use musi_ast::{Expr, ImportClause};

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
        pkg_map: HashMap::new(),
        dep_fn_maps: Vec::new(),
    };

    register::register_module_decls(prelude, interner, &mut module, &mut state)?;

    // Register each dep and capture the fn_map snapshot right after each registration.
    // This ensures that each dep's function indices are captured before a later dep
    // can override same-named entries in fn_map.
    for &dep in deps {
        let fn_map_before = state.fn_map.clone();
        register::register_module_decls(dep, interner, &mut module, &mut state)?;
        // Capture all entries that are new or changed (covers both new fns and overrides).
        let mut dep_map: HashMap<String, u16> = HashMap::new();
        for (name, &idx) in &state.fn_map {
            if fn_map_before.get(name.as_str()) != Some(&idx) {
                let _prev = dep_map.insert(name.clone(), idx);
            }
        }
        state.dep_fn_maps.push(dep_map);
    }

    register::register_module_decls(user, interner, &mut module, &mut state)?;

    // Build pkg_map: alias name → dep index, from the user module's GlobAs imports.
    for &item_idx in user.ctx.expr_lists.get_slice(user.items) {
        if let Expr::Import { items: ImportClause::GlobAs(alias), path, .. } =
            user.ctx.exprs.get(item_idx)
        {
            let import_path = interner.resolve(*path).trim_matches('"').to_owned();
            if let Some(dep_idx) = dep_paths.iter().position(|p| *p == import_path) {
                let alias_str = interner.resolve(*alias).to_owned();
                let _prev = state.pkg_map.insert(alias_str, dep_idx);
            }
        }
    }

    assemble::emit_module_fn_bodies(prelude, interner, &mut state, &mut module, None)?;
    for (dep_i, &dep) in deps.iter().enumerate() {
        // Clone the snapshot to avoid a simultaneous &/&mut borrow on `state`.
        let snapshot = state.dep_fn_maps.get(dep_i).cloned();
        assemble::emit_module_fn_bodies(dep, interner, &mut state, &mut module, snapshot.as_ref())?;
    }
    assemble::emit_module_fn_bodies(user, interner, &mut state, &mut module, None)?;

    assemble::emit_main_body(user, interner, &mut state, &mut module)?;

    Ok(module)
}
