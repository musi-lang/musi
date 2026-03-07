#![allow(clippy::too_many_lines)]

use musi_ast::{AttrArg, Expr, FieldInit, LitValue, Modifier, Pat, PostfixOp, PrefixOp};
use musi_shared::Idx;

use crate::error::CodegenError;
use crate::{ConstEntry, Module, Opcode};

use super::call::{emit_static_call, emit_variant_construct};
use super::control::{ForData, IfData, emit_binary, emit_for, emit_if, emit_loop, emit_while};
use super::field::emit_postfix;
use super::pattern::emit_match;
use super::state::{EmitArenas, EmitState, FnEmitter, param_list_with_types, register_fn_closure};

pub(super) fn emit_expr_or_unit(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    opt: Option<Idx<Expr>>,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    if let Some(idx) = opt {
        let expr = arenas.exprs.get(idx).clone();
        emit_expr(arenas, state, &expr, module, out)
    } else {
        out.push(&Opcode::LdImmUnit);
        Ok(())
    }
}

pub(super) fn emit_args(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    args: &[Idx<Expr>],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    for &arg_idx in args {
        let arg = arenas.exprs.get(arg_idx).clone();
        emit_expr(arenas, state, &arg, module, out)?;
    }
    Ok(())
}

pub(super) fn emit_expr(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    expr: &Expr,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match expr {
        Expr::Lit { value, .. } => super::control::emit_lit(value, arenas, module, out),

        Expr::Unit { .. } => {
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Paren { inner, .. } => {
            let inner_expr = arenas.exprs.get(*inner).clone();
            emit_expr(arenas, state, &inner_expr, module, out)
        }

        Expr::Ident { name, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if state.variant_map.contains_key(name_str) {
                return Err(CodegenError::VariantConstructRequiresDot(name_str.into()));
            }
            let slot = out
                .lookup_local(name_str)
                .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
            out.push(&Opcode::LdLoc(slot));
            Ok(())
        }

        Expr::Bind { pat, init, .. } => {
            if let Pat::Ident {
                name, suffix: None, ..
            } = pat
            {
                // Fast path: simple name binding (existing behavior preserved)
                let name_str = arenas.interner.resolve(*name);
                let slot = out.define_local(name_str)?;
                if let Some(init_idx) = init {
                    let init_expr = arenas.exprs.get(*init_idx);
                    super::pattern::track_type_of_binding(init_expr, slot, arenas, state, out);
                    if let Expr::AnonRec { fields, .. } = init_expr {
                        let field_names: Vec<String> = fields
                            .iter()
                            .filter_map(|fi| {
                                if let FieldInit::Named { name, .. } = fi {
                                    Some(arenas.interner.resolve(*name).to_owned())
                                } else {
                                    None
                                }
                            })
                            .collect();
                        let _prev = out.anon_layouts.insert(slot, field_names);
                    }
                    let init_cloned = init_expr.clone();
                    emit_expr(arenas, state, &init_cloned, module, out)?;
                } else {
                    out.push(&Opcode::LdImmUnit);
                }
                out.push(&Opcode::StLoc(slot));
            } else {
                // Destructuring: emit init, spill to temp, then pattern-bind
                if let Some(init_idx) = init {
                    let init_expr = arenas.exprs.get(*init_idx).clone();
                    emit_expr(arenas, state, &init_expr, module, out)?;
                } else {
                    out.push(&Opcode::LdImmUnit);
                }
                let tmp_name = format!("$bind_{}", out.next_slot);
                let tmp_slot = out.define_local(&tmp_name)?;
                out.push(&Opcode::StLoc(tmp_slot));
                super::pattern::emit_pattern_bindings(arenas, state, pat, tmp_slot, out)?;
            }
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Assign { target, value, .. } => {
            let target_expr = arenas.exprs.get(*target);
            match target_expr {
                Expr::Ident { name, .. } => {
                    let name_str = arenas.interner.resolve(*name);
                    let slot = out
                        .lookup_local(name_str)
                        .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
                    let val = arenas.exprs.get(*value).clone();
                    emit_expr(arenas, state, &val, module, out)?;
                    out.push(&Opcode::StLoc(slot));
                    out.push(&Opcode::LdImmUnit);
                    Ok(())
                }
                Expr::Postfix {
                    base: arr_base,
                    op: PostfixOp::Index { args, .. },
                    ..
                } => {
                    let arr_base = *arr_base;
                    let idx_idx = arenas.expr_lists.get_slice(*args).first().copied();
                    let value_idx = *value;
                    let base_expr = arenas.exprs.get(arr_base).clone();
                    emit_expr(arenas, state, &base_expr, module, out)?;
                    if let Some(idx) = idx_idx {
                        let idx_expr = arenas.exprs.get(idx).clone();
                        emit_expr(arenas, state, &idx_expr, module, out)?;
                    }
                    let val = arenas.exprs.get(value_idx).clone();
                    emit_expr(arenas, state, &val, module, out)?;
                    out.push(&Opcode::ArrSet);
                    Ok(())
                }
                _ => Err(CodegenError::UnsupportedExpr),
            }
        }

        Expr::Prefix { op, operand, .. } => {
            let opcode = match op {
                PrefixOp::Neg => Opcode::NegI64,
                PrefixOp::Not => Opcode::Not,
                PrefixOp::BitNot => Opcode::BitNot,
                PrefixOp::Deref | PrefixOp::AddrOf => return Err(CodegenError::UnsupportedExpr),
            };
            let operand_expr = arenas.exprs.get(*operand).clone();
            emit_expr(arenas, state, &operand_expr, module, out)?;
            out.push(&opcode);
            Ok(())
        }

        Expr::Binary { op, lhs, rhs, .. } => {
            emit_binary(arenas, state, *op, *lhs, *rhs, module, out)
        }

        Expr::Block { stmts, tail, .. } => {
            out.push_scope();
            for &stmt_idx in arenas.expr_lists.get_slice(*stmts) {
                let stmt = arenas.exprs.get(stmt_idx).clone();
                emit_expr(arenas, state, &stmt, module, out)?;
                out.push(&Opcode::Drop);
            }
            emit_expr_or_unit(arenas, state, *tail, module, out)?;
            out.pop_scope();
            Ok(())
        }

        Expr::Postfix { base, op, .. } => emit_postfix(arenas, state, *base, op, module, out),

        Expr::Lambda { params, body, .. } => {
            let lambda_name = format!("__lambda_{}", state.lambda_counter);
            state.lambda_counter = state
                .lambda_counter
                .checked_add(1)
                .ok_or(CodegenError::TooManyFunctions)?;
            let params_with_types = param_list_with_types(params, arenas.interner);
            let fn_idx =
                register_fn_closure(&lambda_name, params_with_types, *body, state, module)?;
            out.push(&Opcode::LdFnIdx(fn_idx));
            Ok(())
        }

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            ..
        } => emit_if(
            arenas,
            state,
            IfData {
                cond,
                then_body: *then_body,
                elif_chains,
                else_body: *else_body,
            },
            module,
            out,
        ),

        Expr::While {
            cond, guard, body, ..
        } => emit_while(
            arenas,
            state,
            cond,
            guard.as_ref().copied(),
            *body,
            module,
            out,
        ),

        Expr::Loop {
            body, post_cond, ..
        } => emit_loop(arenas, state, *body, post_cond.as_deref(), module, out),

        Expr::For {
            pat,
            iter,
            guard,
            body,
            ..
        } => emit_for(
            arenas,
            state,
            ForData {
                pat,
                iter: *iter,
                guard: guard.as_ref().copied(),
                body: *body,
            },
            module,
            out,
        ),

        Expr::Break { value, .. } => {
            emit_expr_or_unit(arenas, state, *value, module, out)?;
            let fixup = out.emit_jump_placeholder(FnEmitter::BR);
            out.push_break_fixup(fixup);
            Ok(())
        }

        Expr::Cycle { guard, .. } => {
            let start_pos = out
                .current_loop_start()
                .ok_or(CodegenError::UnsupportedExpr)?;
            if let Some(guard_idx) = guard {
                let guard_expr = arenas.exprs.get(*guard_idx).clone();
                emit_expr(arenas, state, &guard_expr, module, out)?;
                let skip_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
                out.emit_br_back(start_pos)?;
                out.patch_jump_to_here(skip_fixup)?;
                // Guard was false: fall through, push Unit so caller can Drop
                out.push(&Opcode::LdImmUnit);
            } else {
                out.emit_br_back(start_pos)?;
            }
            Ok(())
        }

        Expr::Return { value, .. } => {
            emit_expr_or_unit(arenas, state, *value, module, out)?;
            out.push(&Opcode::Ret);
            Ok(())
        }

        Expr::Match {
            scrutinee, arms, ..
        } => emit_match(arenas, state, *scrutinee, arms, module, out),

        Expr::DotPrefix { name, args, .. } => {
            let name_str = arenas.interner.resolve(*name);
            let args = arenas.expr_lists.get_slice(*args);
            if let Some(vinfo) = state.variant_map.get(name_str).cloned() {
                return emit_variant_construct(&vinfo, args, arenas, state, module, out);
            }
            if let Some(&fn_idx) = state.fn_map.get(name_str) {
                return emit_static_call(fn_idx, args, arenas, state, module, out);
            }
            Err(CodegenError::UnknownFunction(name_str.into()))
        }

        Expr::Array { items, .. } => {
            let has_spread = items.iter().any(|i| matches!(i, musi_ast::ArrayItem::Spread(_)));
            if has_spread {
                // Spread path: build incrementally starting from an empty array.
                out.push(&Opcode::NewArr(0));
                for item in items {
                    match item {
                        musi_ast::ArrayItem::Single(e) => {
                            let expr = arenas.exprs.get(*e).clone();
                            emit_expr(arenas, state, &expr, module, out)?;
                            out.push(&Opcode::ArrPush1);
                        }
                        musi_ast::ArrayItem::Spread(e) => {
                            let expr = arenas.exprs.get(*e).clone();
                            emit_expr(arenas, state, &expr, module, out)?;
                            out.push(&Opcode::ArrConcat);
                        }
                    }
                }
            } else {
                // Fast path: push all elements then create array in one shot.
                let mut count = 0u16;
                for item in items {
                    if let musi_ast::ArrayItem::Single(e) = item {
                        let expr = arenas.exprs.get(*e).clone();
                        emit_expr(arenas, state, &expr, module, out)?;
                        count += 1;
                    }
                }
                out.push(&Opcode::NewArr(count));
            }
            Ok(())
        }

        Expr::AnonRec { fields, .. } => {
            let named: Vec<(String, Idx<Expr>)> = fields
                .iter()
                .filter_map(|fi| {
                    if let FieldInit::Named { name, value, .. } = fi {
                        Some((arenas.interner.resolve(*name).to_owned(), *value))
                    } else {
                        None
                    }
                })
                .collect();
            for (_, val_idx) in &named {
                let val = arenas.exprs.get(*val_idx).clone();
                emit_expr(arenas, state, &val, module, out)?;
            }
            let field_count =
                u16::try_from(named.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
            out.push(&Opcode::NewObj {
                type_tag: 0,
                field_count,
            });
            Ok(())
        }

        Expr::FnDef {
            attrs,
            name,
            params,
            body: Some(body_idx),
            modifiers,
            ..
        } => {
            if modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_))) {
                return Ok(());
            }
            let fn_name = arenas.interner.resolve(*name).to_owned();
            let params_with_types = param_list_with_types(params, arenas.interner);
            let fn_idx =
                register_fn_closure(&fn_name, params_with_types, *body_idx, state, module)?;
            let slot = out.define_local(&fn_name)?;
            out.push(&Opcode::LdFnIdx(fn_idx));
            out.push(&Opcode::StLoc(slot));

            // Emit test registration for #[test("label")] functions
            for attr in attrs {
                if arenas.interner.resolve(attr.name) == "test"
                    && let Some(AttrArg::Value {
                        value: LitValue::Str(label_sym),
                        ..
                    }) = attr.args.first()
                {
                    let raw = arenas.interner.resolve(*label_sym);
                    let label: Box<str> = raw.trim_matches('"').into();
                    let label_const = module.push_const(ConstEntry::String(label))?;
                    if let Some(&test_fn_idx) = state.fn_map.get("test") {
                        out.push(&Opcode::LdConst(label_const));
                        out.push(&Opcode::LdFnIdx(fn_idx));
                        out.push(&Opcode::Call(test_fn_idx));
                        out.push(&Opcode::Drop);
                    }
                }
            }

            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Tuple { elements, .. } => {
            let elems: Vec<Idx<Expr>> = arenas.expr_lists.get_slice(*elements).to_vec();
            let count = u16::try_from(elems.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
            for e in elems {
                let elem_expr = arenas.exprs.get(e).clone();
                emit_expr(arenas, state, &elem_expr, module, out)?;
            }
            out.push(&Opcode::NewObj {
                type_tag: 0,
                field_count: count,
            });
            Ok(())
        }

        Expr::Defer { .. }
        | Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Using { .. }
        | Expr::Record { .. }
        | Expr::Choice { .. }
        | Expr::FnDef { .. }
        | Expr::ClassDef { .. }
        | Expr::GivenDef { .. }
        | Expr::Error { .. } => Err(CodegenError::UnsupportedExpr),
    }
}
