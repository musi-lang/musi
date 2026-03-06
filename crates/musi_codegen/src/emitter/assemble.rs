#![allow(clippy::too_many_lines)]

use std::collections::HashMap;
use std::mem;

use musi_ast::{ClassMember, Expr, Modifier, ParsedModule, Ty};
use musi_shared::{Idx, Interner};

use crate::error::CodegenError;
use crate::{FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};
use crate::intrinsics;

use super::state::{EmitArenas, EmitState, FnEmitter, TypeTag, param_list_with_types, ty_name_str};
use super::expr::emit_expr;

pub(super) fn emit_fn_body(
    fn_idx: u16,
    params: &[(String, Option<String>)],
    body_idx: Idx<Expr>,
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    emit_fn_body_with_ret(fn_idx, params, body_idx, arenas, state, module, None)
}

pub(super) fn emit_fn_body_with_ret(
    fn_idx: u16,
    params: &[(String, Option<String>)],
    body_idx: Idx<Expr>,
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    ret_ty: Option<&str>,
) -> Result<(), CodegenError> {
    let mut out = FnEmitter::new();
    out.push_scope();
    for (name, type_name_opt) in params {
        let slot = out.define_local(name)?;
        if let Some(type_name) = type_name_opt {
            if state.type_map.contains_key(type_name) {
                let _prev = out.local_types.insert(slot, type_name.clone());
            }
        }
    }
    let body = arenas.exprs.get(body_idx).clone();

    // If the body is an AnonRec and we know the return type, use the named type
    if let (Some(ty_name), Expr::AnonRec { fields, .. }) = (ret_ty, &body) {
        if let (Some(type_info), Some(&type_tag)) = (
            state.type_map.get(ty_name),
            state.type_tag_map.get(ty_name),
        ) {
            let declared_fields = type_info.field_names.clone();
            let explicit: HashMap<String, Idx<Expr>> = fields.iter()
                .filter_map(|fi| if let musi_ast::FieldInit::Named { name, value, .. } = fi {
                    Some((arenas.interner.resolve(*name).to_owned(), *value))
                } else { None })
                .collect();
            for field_name in &declared_fields {
                if let Some(&val_idx) = explicit.get(field_name) {
                    let val = arenas.exprs.get(val_idx).clone();
                    emit_expr(arenas, state, &val, module, &mut out)?;
                } else {
                    out.push(&Opcode::LdImmUnit);
                }
            }
            let field_count = u16::try_from(declared_fields.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
            out.push(&Opcode::NewObj { type_tag, field_count });
            out.push(&Opcode::Ret);
            out.pop_scope();
            return finalize_fn_body(fn_idx, out, module);
        }
    }

    emit_expr(arenas, state, &body, module, &mut out)?;
    out.push(&Opcode::Ret);
    out.pop_scope();
    finalize_fn_body(fn_idx, out, module)
}

fn finalize_fn_body(fn_idx: u16, out: FnEmitter, module: &mut Module) -> Result<(), CodegenError> {
    let local_count = out.local_count();
    let code_offset = u32::try_from(module.code.len()).map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = out.byte_len();
    out.flush_into(&mut module.code);
    let code_length = u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;
    let entry = module.function_table.get_mut(usize::from(fn_idx)).ok_or(CodegenError::UnsupportedExpr)?;
    entry.local_count = local_count;
    entry.code_offset = code_offset;
    entry.code_length = code_length;
    Ok(())
}

pub(super) fn emit_pending_lambdas(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    loop {
        let pending = mem::take(&mut state.pending_lambdas);
        if pending.is_empty() { break; }
        for lam in pending {
            emit_fn_body(lam.fn_idx, &lam.params, lam.body, arenas, state, module)?;
        }
    }
    Ok(())
}

pub(super) fn emit_module_fn_bodies(
    parsed: &ParsedModule,
    interner: &Interner,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let arenas = EmitArenas { exprs: &parsed.ctx.exprs, interner };
    let mut pending: Vec<(u16, Vec<(String, Option<String>)>, Idx<Expr>, Option<String>)> = Vec::new();

    for &item_idx in &parsed.items {
        match parsed.ctx.exprs.get(item_idx) {
            Expr::FnDef { name, params, ret_ty, body: Some(body_idx), modifiers, .. } => {
                if modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_))) { continue; }
                let fn_name = interner.resolve(*name).to_owned();
                let fn_idx = *state.fn_map.get(&fn_name)
                    .ok_or_else(|| CodegenError::UnknownFunction(fn_name.clone().into_boxed_str()))?;
                let param_info = param_list_with_types(params, interner);
                let ret_name = ret_ty.as_ref().and_then(|t| ty_name_str(t, interner));
                pending.push((fn_idx, param_info, *body_idx, ret_name));
            }
            Expr::GivenDef { class_app, members, .. } => {
                let type_tag_opt: Option<u16> = match class_app {
                    Ty::Named { args, .. } if !args.is_empty() => {
                        if let Ty::Named { name: arg_name, .. } = &args[0] {
                            let arg_str = interner.resolve(*arg_name);
                            if let Some(prim) = TypeTag::from_type_name(arg_str) {
                                Some(prim as u16)
                            } else {
                                state.type_tag_map.get(arg_str).copied()
                            }
                        } else if matches!(&args[0], Ty::Arr { .. }) {
                            Some(TypeTag::Array as u16)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                let Some(type_tag) = type_tag_opt else { continue; };
                let members = members.clone();
                for member in &members {
                    let ClassMember::Method(method_idx) = member else { continue; };
                    let fn_expr = parsed.ctx.exprs.get(*method_idx);
                    let Expr::FnDef { name, params, body: Some(body_idx), modifiers, .. } = fn_expr else { continue; };
                    if modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_))) { continue; }
                    let fn_name = interner.resolve(*name).to_owned();
                    let fn_idx = match module.method_table.iter().find(|e| {
                        e.name.as_ref() == fn_name.as_str() && e.type_tag == type_tag
                    }) {
                        Some(entry) => entry.fn_idx,
                        None => continue,
                    };
                    let param_info = param_list_with_types(params, interner);
                    pending.push((fn_idx, param_info, *body_idx, None));
                }
            }
            _ => {}
        }
    }

    for (fn_idx, param_names, body_idx, ret_name) in pending {
        emit_fn_body_with_ret(fn_idx, &param_names, body_idx, &arenas, state, module, ret_name.as_deref())?;
    }
    emit_pending_lambdas(&arenas, state, module)
}

pub(super) fn emit_main_body(
    user: &ParsedModule,
    interner: &Interner,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<(), CodegenError> {
    let arenas = EmitArenas { exprs: &user.ctx.exprs, interner };
    let mut out = FnEmitter::new();
    out.push_scope();
    for &item_idx in &user.items {
        match user.ctx.exprs.get(item_idx) {
            Expr::FnDef { .. }
            | Expr::Record { .. }
            | Expr::Choice { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::ClassDef { .. }
            | Expr::GivenDef { .. } => {}
            expr => {
                let e = expr.clone();
                emit_expr(&arenas, state, &e, module, &mut out)?;
                out.push(&Opcode::Drop);
            }
        }
    }
    emit_pending_lambdas(&arenas, state, module)?;
    out.pop_scope();
    out.push(&Opcode::Halt);

    let local_count = out.local_count();
    let code_offset = u32::try_from(module.code.len()).map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = out.byte_len();
    out.flush_into(&mut module.code);
    let code_length = u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let sym_idx = module.push_symbol(SymbolEntry {
        name: "main".into(),
        flags: SymbolFlags::new(0),
        intrinsic_id: intrinsics::NONE_ID,
        abi: Box::from(""),
        link_lib: None,
        link_name: None,
    })?;
    let _main = module.push_function(FunctionEntry {
        symbol_idx: sym_idx,
        param_count: 0,
        local_count,
        code_offset,
        code_length,
    })?;
    Ok(())
}
