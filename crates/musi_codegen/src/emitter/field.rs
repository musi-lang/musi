use musi_ast::{Expr, FieldInit, PostfixOp};
use musi_shared::{Idx, Symbol};
use std::collections::HashMap;

use crate::error::CodegenError;
use crate::Module;
use crate::Opcode;

use super::state::{EmitArenas, EmitState, FnEmitter};
use super::call::emit_call;
use super::expr::emit_expr;

pub(super) fn emit_postfix(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    op: &PostfixOp,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match op {
        PostfixOp::Call { args, .. } => emit_call(arenas, state, base, args, module, out),
        PostfixOp::Field { name, .. } => emit_field_access(arenas, state, base, *name, module, out),
        PostfixOp::RecDot { fields, .. } => emit_rec_dot(arenas, state, base, fields, module, out),
        PostfixOp::Index { args, .. } => {
            let base_expr = arenas.exprs.get(base).clone();
            emit_expr(arenas, state, &base_expr, module, out)?;
            if let Some(&idx) = args.first() {
                let idx_expr = arenas.exprs.get(idx).clone();
                emit_expr(arenas, state, &idx_expr, module, out)?;
            }
            out.push(&Opcode::ArrGet);
            Ok(())
        }
        PostfixOp::OptField { name, .. } => emit_opt_field_access(arenas, state, base, *name, module, out),
        PostfixOp::As { .. } => {
            let base_expr = arenas.exprs.get(base).clone();
            emit_expr(arenas, state, &base_expr, module, out)
        }
    }
}

pub(super) fn emit_field_access(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    field_sym: Symbol,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let field_name = arenas.interner.resolve(field_sym);
    let base_expr = arenas.exprs.get(base);

    if let Expr::Ident { name: base_name, .. } = base_expr {
        let base_name_str = arenas.interner.resolve(*base_name);
        let slot = out.lookup_local(base_name_str)
            .ok_or_else(|| CodegenError::UndefinedVariable(base_name_str.into()))?;

        if let Some(type_name) = out.local_types.get(&slot).cloned() {
            if let Some(type_info) = state.type_map.get(&type_name) {
                let field_idx = type_info.field_names.iter().position(|f| f == field_name)
                    .ok_or_else(|| CodegenError::UnknownField(field_name.into()))?;
                let field_idx_u16 = u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
                let base_cloned = base_expr.clone();
                emit_expr(arenas, state, &base_cloned, module, out)?;
                out.push(&Opcode::LdFld(field_idx_u16));
                return Ok(());
            }
        }

        if let Some(layout) = out.anon_layouts.get(&slot).cloned() {
            let field_idx = layout.iter().position(|f| f == field_name)
                .ok_or_else(|| CodegenError::UnknownField(field_name.into()))?;
            let field_idx_u16 = u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
            let base_cloned = base_expr.clone();
            emit_expr(arenas, state, &base_cloned, module, out)?;
            out.push(&Opcode::LdFld(field_idx_u16));
            return Ok(());
        }
    }

    Err(CodegenError::UnsupportedExpr)
}

pub(super) fn emit_opt_field_access(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    field_sym: Symbol,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let field_name = arenas.interner.resolve(field_sym);
    let base_expr = arenas.exprs.get(base);

    if let Expr::Ident { name: base_name, .. } = base_expr {
        let base_name_str = arenas.interner.resolve(*base_name);
        let slot = out.lookup_local(base_name_str)
            .ok_or_else(|| CodegenError::UndefinedVariable(base_name_str.into()))?;

        let field_idx_u16 = if let Some(type_name) = out.local_types.get(&slot).cloned() {
            if let Some(type_info) = state.type_map.get(&type_name) {
                let idx = type_info.field_names.iter().position(|f| f == field_name)
                    .ok_or_else(|| CodegenError::UnknownField(field_name.into()))?;
                u16::try_from(idx).map_err(|_| CodegenError::UnsupportedExpr)?
            } else {
                return Err(CodegenError::UnsupportedExpr);
            }
        } else {
            0
        };

        let base_cloned = base_expr.clone();
        emit_expr(arenas, state, &base_cloned, module, out)?;
        out.push(&Opcode::OptField(field_idx_u16));
        return Ok(());
    }

    Err(CodegenError::UnsupportedExpr)
}

pub(super) fn emit_rec_dot(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    fields: &[FieldInit],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let base_expr = arenas.exprs.get(base);
    let Expr::Ident { name: type_name_sym, .. } = base_expr else {
        return Err(CodegenError::UnsupportedExpr);
    };
    let type_name = arenas.interner.resolve(*type_name_sym).to_owned();

    let type_info = state.type_map.get(&type_name)
        .ok_or_else(|| CodegenError::UnknownType(type_name.clone().into_boxed_str()))?;
    let declared_fields = type_info.field_names.clone();
    let field_count = u16::try_from(declared_fields.len()).map_err(|_| CodegenError::UnsupportedExpr)?;

    let spread_expr: Option<Idx<Expr>> = fields.iter().find_map(|fi| {
        if let FieldInit::Spread { expr, .. } = fi { Some(*expr) } else { None }
    });

    let explicit: HashMap<String, Idx<Expr>> = fields.iter()
        .filter_map(|fi| if let FieldInit::Named { name, value, .. } = fi {
            Some((arenas.interner.resolve(*name).to_owned(), *value))
        } else { None })
        .collect();

    let spread_slot: Option<u16> = if let Some(spread_idx) = spread_expr {
        let temp_name = format!("$spread_{}", out.next_slot);
        let slot = out.define_local(&temp_name)?;
        let spread = arenas.exprs.get(spread_idx).clone();
        emit_expr(arenas, state, &spread, module, out)?;
        out.push(&Opcode::StLoc(slot));
        if let Expr::Ident { name: spread_name, .. } = arenas.exprs.get(spread_idx) {
            let spread_name_str = arenas.interner.resolve(*spread_name);
            if let Some(src_slot) = out.lookup_local(spread_name_str)
                && let Some(t) = out.local_types.get(&src_slot).cloned()
            {
                let _prev = out.local_types.insert(slot, t);
            }
        }
        Some(slot)
    } else {
        None
    };

    for (field_idx, field_name) in declared_fields.iter().enumerate() {
        if let Some(&val_idx) = explicit.get(field_name) {
            let val = arenas.exprs.get(val_idx).clone();
            emit_expr(arenas, state, &val, module, out)?;
        } else if let Some(spread_slot_id) = spread_slot {
            let fidx = u16::try_from(field_idx).map_err(|_| CodegenError::UnsupportedExpr)?;
            out.push(&Opcode::LdLoc(spread_slot_id));
            out.push(&Opcode::LdFld(fidx));
        } else {
            return Err(CodegenError::UnknownField(field_name.clone().into_boxed_str()));
        }
    }
    let type_tag = state.type_tag_map.get(&type_name).copied().unwrap_or(0);
    out.push(&Opcode::NewObj { type_tag, field_count });
    Ok(())
}
