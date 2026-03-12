//! Expression emission: tree-walks AST expressions, leaving results on the stack.

use musi_bc::Opcode;
use music_ast::Pat;
use music_ast::expr::{Arg, ArrayElem, BinOp, Expr, FieldKey, RecField, UnaryOp};
use music_ast::lit::Lit;
use music_sema::def::DefKind;
use music_sema::types::Type;
use music_shared::Symbol;

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use music_ast::ExprIdx;

use super::super::emitter::{Emitter, FnBytecode, FnEntry};
use super::FnCtx;
use super::desugar;

/// Type family used to select the right arithmetic/comparison opcodes.
#[derive(Clone, Copy)]
pub(crate) enum TypeFamily {
    Signed,
    Unsigned,
    Float,
    Bool,
}

/// Emit bytecode for `expr_idx`. Returns `true` if a value was pushed onto the stack.
pub(crate) fn emit_expr(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    expr_idx: ExprIdx,
) -> Result<bool, EmitError> {
    let expr = em.ast.exprs[expr_idx].clone();
    match expr {
        Expr::Lit { lit, .. } => emit_lit(em, fc, &lit),

        Expr::Name { name, span } => {
            let name_str = em.interner.resolve(name);
            if name_str == "true" {
                let cv = ConstValue::Bool(true);
                if let Some(i) = em.cp.intern(&cv, em.interner)? {
                    fc.fe.emit_ld_cst(i);
                }
                return Ok(true);
            }
            if name_str == "false" {
                let cv = ConstValue::Bool(false);
                if let Some(i) = em.cp.intern(&cv, em.interner)? {
                    fc.fe.emit_ld_cst(i);
                }
                return Ok(true);
            }

            let Some(&def_id) = em.sema.resolution.expr_defs.get(&expr_idx) else {
                return Err(EmitError::UnsupportedFeature {
                    desc: format!("unresolved name `{}`", em.interner.resolve(name)).into(),
                });
            };
            let _ = span;

            if let Some(&fn_id) = em.fn_map.get(&def_id) {
                let cv = ConstValue::FnRef(fn_id);
                if let Some(i) = em.cp.intern(&cv, em.interner)? {
                    fc.fe.emit_ld_cst(i);
                }
                return Ok(true);
            }

            if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
                let cv = ConstValue::FnRef(ffi_idx);
                if let Some(i) = em.cp.intern(&cv, em.interner)? {
                    fc.fe.emit_ld_cst(i);
                }
                return Ok(true);
            }

            if let Some(&slot) = fc.local_map.get(&def_id) {
                fc.fe.emit_ld_loc(slot);
                return Ok(true);
            }

            Err(EmitError::UnsupportedFeature {
                desc: format!("unresolved local `{}`", em.interner.resolve(name)).into(),
            })
        }

        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => emit_expr(em, fc, inner),

        Expr::Quantified { body, .. } => emit_expr(em, fc, body),

        Expr::Block { stmts, tail, .. } => {
            for &stmt_idx in &stmts {
                let produced = emit_expr(em, fc, stmt_idx)?;
                if produced {
                    fc.fe.emit_pop();
                }
            }
            if let Some(tail_idx) = tail {
                emit_expr(em, fc, tail_idx)
            } else {
                Ok(false)
            }
        }

        Expr::Let { fields, body, .. } => {
            if let Some(val_idx) = fields.value {
                let produced = emit_expr(em, fc, val_idx)?;
                if produced {
                    bind_pat(em, fc, fields.pat)?;
                }
            }
            if let Some(body_idx) = body {
                emit_expr(em, fc, body_idx)
            } else {
                Ok(false)
            }
        }

        Expr::Binding { fields, .. } => {
            // Top-level bindings for functions were handled during scan; non-fn bindings
            // (like local `let x := 5`) are emitted here if they have a value.
            let Some(val_idx) = fields.value else {
                return Ok(false);
            };
            let is_fn = matches!(
                em.ast.exprs[val_idx],
                Expr::Fn { .. } | Expr::Quantified { .. }
            );
            if is_fn {
                return Ok(false);
            }
            let produced = emit_expr(em, fc, val_idx)?;
            if produced {
                bind_pat(em, fc, fields.pat)?;
            }
            Ok(false)
        }

        Expr::BinOp {
            op, left, right, ..
        } => match op {
            BinOp::And => {
                desugar::emit_and(em, fc, left, right)?;
                Ok(true)
            }
            BinOp::Or => {
                desugar::emit_or(em, fc, left, right)?;
                Ok(true)
            }
            BinOp::Assign => {
                desugar::emit_assign(em, fc, left, right)?;
                Ok(false)
            }
            BinOp::Pipe => {
                desugar::emit_pipe(em, fc, expr_idx, left, right)?;
                Ok(true)
            }
            BinOp::In | BinOp::Cons | BinOp::NilCoal | BinOp::RangeInc | BinOp::RangeExc => {
                Err(EmitError::UnsupportedFeature {
                    desc: format!("binary operator `{op:?}`").into(),
                })
            }
            _ => {
                let produced_left = emit_expr(em, fc, left)?;
                if !produced_left {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "binop left operand produced no value".into(),
                    });
                }
                let produced_right = emit_expr(em, fc, right)?;
                if !produced_right {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "binop right operand produced no value".into(),
                    });
                }
                let family = classify_type_family(em, left);
                let opcode = map_binop(op, family)?;
                fc.fe.emit_binop(opcode);
                Ok(true)
            }
        },

        Expr::UnaryOp { op, operand, span } => {
            let produced = emit_expr(em, fc, operand)?;
            if !produced {
                return Err(EmitError::UnsupportedFeature {
                    desc: "unary operand produced no value".into(),
                });
            }
            let _ = span;
            match op {
                UnaryOp::Neg => {
                    let family = classify_type_family(em, operand);
                    let opcode = match family {
                        Some(TypeFamily::Float) => Opcode::F_NEG,
                        _ => Opcode::I_NEG,
                    };
                    fc.fe.emit_unop(opcode);
                    Ok(true)
                }
                UnaryOp::Not => {
                    fc.fe.emit_unop(Opcode::B_NOT);
                    Ok(true)
                }
                _ => Err(EmitError::UnsupportedFeature {
                    desc: format!("unary operator `{op:?}`").into(),
                }),
            }
        }

        Expr::Call { callee, args, span } => {
            let _ = span;
            let arg_count = emit_call_args(em, fc, &args)?;

            let callee_expr = em.ast.exprs[callee].clone();
            match callee_expr {
                Expr::Name { .. } => {
                    if let Some(&def_id) = em.sema.resolution.expr_defs.get(&callee) {
                        if let Some(&ffi_idx) = em.foreign_map.get(&def_id) {
                            let ac = i32::try_from(arg_count).map_err(|_| {
                                EmitError::UnresolvableType {
                                    desc: "arg count".into(),
                                }
                            })?;
                            fc.fe.emit_inv_ffi(ffi_idx, ac);
                            return Ok(true);
                        }
                        if let Some(&fn_id) = em.fn_map.get(&def_id) {
                            let ac = i32::try_from(arg_count).map_err(|_| {
                                EmitError::UnresolvableType {
                                    desc: "arg count".into(),
                                }
                            })?;
                            fc.fe.emit_inv(fn_id, false, ac);
                            return Ok(true);
                        }
                        if let Some(&slot) = fc.local_map.get(&def_id) {
                            fc.fe.emit_ld_loc(slot);
                            let ac_i = i32::try_from(arg_count).map_err(|_| {
                                EmitError::UnresolvableType {
                                    desc: "arg count".into(),
                                }
                            })?;
                            fc.fe.emit_inv_dyn(ac_i)?;
                            return Ok(true);
                        }
                    }
                    Err(EmitError::UnsupportedFeature {
                        desc: "unresolved callee".into(),
                    })
                }
                _ => {
                    let produced = emit_expr(em, fc, callee)?;
                    if !produced {
                        return Err(EmitError::UnsupportedFeature {
                            desc: "callee produced no value".into(),
                        });
                    }
                    let ac_i =
                        i32::try_from(arg_count).map_err(|_| EmitError::UnresolvableType {
                            desc: "arg count".into(),
                        })?;
                    fc.fe.emit_inv_dyn(ac_i)?;
                    Ok(true)
                }
            }
        }

        Expr::Tuple { elems, .. } => {
            let n = elems.len();
            for &e in &elems {
                let produced = emit_expr(em, fc, e)?;
                if !produced {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "tuple element produced no value".into(),
                    });
                }
            }
            let field_count = u32::try_from(n).map_err(|_| EmitError::OperandOverflow {
                desc: "tuple element count".into(),
            })?;
            let stack_pop = i32::try_from(n).map_err(|_| EmitError::OperandOverflow {
                desc: "tuple element count".into(),
            })?;
            fc.fe.emit_mk_prd(field_count, stack_pop)?;
            Ok(true)
        }

        Expr::Record { fields, .. } => {
            let named_fields: Vec<_> = fields
                .iter()
                .filter_map(|f| match f {
                    RecField::Named { value, .. } => Some(*value),
                    RecField::Spread { .. } => None,
                })
                .collect();
            let n = named_fields.len();
            for val_opt in named_fields {
                if let Some(val_idx) = val_opt {
                    let produced = emit_expr(em, fc, val_idx)?;
                    if !produced {
                        return Err(EmitError::UnsupportedFeature {
                            desc: "record field produced no value".into(),
                        });
                    }
                } else {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "record field with no value".into(),
                    });
                }
            }
            let field_count = u32::try_from(n).map_err(|_| EmitError::OperandOverflow {
                desc: "record field count".into(),
            })?;
            let stack_pop = i32::try_from(n).map_err(|_| EmitError::OperandOverflow {
                desc: "record field count".into(),
            })?;
            fc.fe.emit_mk_prd(field_count, stack_pop)?;
            Ok(true)
        }

        Expr::Array { elems, .. } => {
            emit_array(em, fc, &elems)?;
            Ok(true)
        }

        Expr::Variant { name, args, .. } => {
            for &arg_idx in &args {
                let produced = emit_expr(em, fc, arg_idx)?;
                if !produced {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "variant arg produced no value".into(),
                    });
                }
            }
            let tag = resolve_variant_tag(em, name);
            fc.fe.emit_mk_var(tag)?;
            Ok(true)
        }

        Expr::Field { object, field, .. } => {
            let produced = emit_expr(em, fc, object)?;
            if !produced {
                return Err(EmitError::UnsupportedFeature {
                    desc: "field object produced no value".into(),
                });
            }
            let index = match field {
                FieldKey::Pos { index, .. } => index,
                FieldKey::Name { name, .. } => resolve_field_name(em, object, name),
            };
            fc.fe.emit_ld_fld(index)?;
            Ok(true)
        }

        Expr::Index { object, index, .. } => {
            let produced_obj = emit_expr(em, fc, object)?;
            if !produced_obj {
                return Err(EmitError::UnsupportedFeature {
                    desc: "index object produced no value".into(),
                });
            }
            let produced_idx = emit_expr(em, fc, index)?;
            if !produced_idx {
                return Err(EmitError::UnsupportedFeature {
                    desc: "index value produced no value".into(),
                });
            }
            fc.fe.emit_ld_idx();
            Ok(true)
        }

        Expr::Return { value, .. } => {
            if let Some(val_idx) = value {
                let produced = emit_expr(em, fc, val_idx)?;
                if produced {
                    fc.fe.emit_ret();
                } else {
                    fc.fe.emit_ret_u();
                }
            } else {
                fc.fe.emit_ret_u();
            }
            Ok(false)
        }

        Expr::Piecewise { arms, .. } => {
            super::control::emit_piecewise(em, fc, &arms)?;
            Ok(true)
        }

        Expr::Match {
            scrutinee, arms, ..
        } => {
            super::control::emit_match(em, fc, scrutinee, &arms)?;
            Ok(true)
        }

        Expr::Fn { params, body, .. } => {
            let nested_fn_id = em.alloc_fn_id();
            let nested_param_count =
                u16::try_from(params.len()).map_err(|_| EmitError::UnresolvableType {
                    desc: "nested fn param count".into(),
                })?;
            let nested_entry = FnEntry {
                fn_id: nested_fn_id,
                name: format!("<closure@{}>", nested_fn_id),
                params: params.clone(),
                body,
                effect_mask: 0,
            };

            // Emit params mapping for nested function by registering param defs
            let mut nested_fc = FnCtx::new(nested_param_count);
            for (i, param) in params.iter().enumerate() {
                let slot = u32::try_from(i).map_err(|_| EmitError::UnresolvableType {
                    desc: "nested fn param index".into(),
                })?;
                if let Some(&did) = em.sema.resolution.pat_defs.get(&param.span) {
                    let _ = nested_fc.local_map.insert(did, slot);
                } else {
                    for def in &em.sema.defs {
                        if def.kind == music_sema::def::DefKind::Param
                            && def.name == param.name
                            && def.span == param.span
                        {
                            let _ = nested_fc.local_map.insert(def.id, slot);
                            break;
                        }
                    }
                }
            }
            let had_value = emit_expr(em, &mut nested_fc, body)?;
            if had_value {
                nested_fc.fe.emit_ret();
            } else {
                nested_fc.fe.emit_ret_u();
            }
            nested_fc.fe.resolve_fixups(&nested_entry.name)?;
            let _code_len = nested_fc.fe.validate_code_len()?;
            let type_id = em
                .tp
                .lower_well_known_def(em.sema.well_known.unit, &em.sema.well_known)
                .ok_or_else(|| EmitError::UnresolvableType {
                    desc: "Unit type".into(),
                })?;
            let nested_bc = FnBytecode {
                fn_id: nested_fn_id,
                type_id,
                local_count: nested_fc.fe.local_count,
                param_count: nested_fc.fe.param_count,
                max_stack: nested_fc.fe.max_stack,
                effect_mask: 0,
                code: nested_fc.fe.code,
                handlers: nested_fc.fe.handlers,
            };
            em.nested_fns.push(nested_bc);

            let cv = ConstValue::FnRef(nested_fn_id);
            if let Some(i) = em.cp.intern(&cv, em.interner)? {
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }

        // These produce no bytecode value.
        Expr::Import { .. }
        | Expr::Export { .. }
        | Expr::Choice { .. }
        | Expr::RecordDef { .. }
        | Expr::Class { .. }
        | Expr::Given { .. }
        | Expr::Effect { .. }
        | Expr::Foreign { .. }
        | Expr::Update { .. }
        | Expr::Error { .. } => Ok(false),
    }
}

fn emit_lit(em: &mut Emitter<'_>, fc: &mut FnCtx, lit: &Lit) -> Result<bool, EmitError> {
    match lit {
        Lit::Unit { .. } => Ok(false),
        Lit::Int { value, .. } => {
            let cv = ConstValue::Int(*value);
            if let Some(i) = em.cp.intern(&cv, em.interner)? {
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }
        Lit::Float { value, .. } => {
            let cv = ConstValue::Float(*value);
            if let Some(i) = em.cp.intern(&cv, em.interner)? {
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }
        Lit::Rune { codepoint, .. } => {
            let cv = ConstValue::Rune(*codepoint);
            if let Some(i) = em.cp.intern(&cv, em.interner)? {
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }
        Lit::Str { value, .. } => {
            let cv = ConstValue::Str(*value);
            if let Some(i) = em.cp.intern(&cv, em.interner)? {
                fc.fe.emit_ld_cst(i);
            }
            Ok(true)
        }
        Lit::FStr { parts, .. } => {
            desugar::emit_fstr(em, fc, parts)?;
            Ok(true)
        }
    }
}

/// Consume the top-of-stack value and bind it to `pat_idx`.
pub(crate) fn bind_pat(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    pat_idx: music_ast::PatIdx,
) -> Result<(), EmitError> {
    let pat = em.ast.pats[pat_idx].clone();
    match pat {
        Pat::Wild { .. } => {
            fc.fe.emit_pop();
            Ok(())
        }
        Pat::Bind { span, .. } => {
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            if let Some(&did) = em.sema.resolution.pat_defs.get(&span) {
                let _ = fc.local_map.insert(did, slot);
            }
            Ok(())
        }
        Pat::Tuple { elems, .. } => {
            // Store product into a temp slot, then extract each field.
            let tmp = fc.alloc_local();
            fc.fe.emit_st_loc(tmp);
            for (i, &elem) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| EmitError::OperandOverflow {
                    desc: "tuple destructure index".into(),
                })?;
                fc.fe.emit_ld_loc(tmp);
                fc.fe.emit_ld_fld(idx)?;
                bind_pat(em, fc, elem)?;
            }
            Ok(())
        }
        _ => {
            // For other patterns (Lit, Variant, etc.) in binding position,
            // just store to a scratch slot and discard.
            let slot = fc.alloc_local();
            fc.fe.emit_st_loc(slot);
            Ok(())
        }
    }
}

fn emit_call_args(em: &mut Emitter<'_>, fc: &mut FnCtx, args: &[Arg]) -> Result<usize, EmitError> {
    let mut count = 0usize;
    for &arg in args {
        match arg {
            Arg::Pos { expr, .. } => {
                let produced = emit_expr(em, fc, expr)?;
                if !produced {
                    return Err(EmitError::UnsupportedFeature {
                        desc: "call arg produced no value".into(),
                    });
                }
                count += 1;
            }
            Arg::Hole { .. } => {
                return Err(EmitError::UnsupportedFeature {
                    desc: "partial application (hole argument)".into(),
                });
            }
        }
    }
    Ok(count)
}

fn emit_array(em: &mut Emitter<'_>, fc: &mut FnCtx, elems: &[ArrayElem]) -> Result<(), EmitError> {
    let count = u32::try_from(elems.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "array element count".into(),
    })?;
    fc.fe.emit_mk_arr(count);

    for (i, &elem) in elems.iter().enumerate() {
        let expr_idx = match elem {
            ArrayElem::Elem { expr, .. } => expr,
            ArrayElem::Spread { .. } => {
                return Err(EmitError::UnsupportedFeature {
                    desc: "spread array element".into(),
                });
            }
        };
        let idx_u32 = u32::try_from(i).map_err(|_| EmitError::OperandOverflow {
            desc: "array index".into(),
        })?;
        // dup array ref, push index, push value, ST_IDX
        fc.fe.emit_dup();
        let idx_cv = ConstValue::Int(i64::from(idx_u32));
        if let Some(ci) = em.cp.intern(&idx_cv, em.interner)? {
            fc.fe.emit_ld_cst(ci);
        }
        let produced = emit_expr(em, fc, expr_idx)?;
        if !produced {
            return Err(EmitError::UnsupportedFeature {
                desc: "array element produced no value".into(),
            });
        }
        fc.fe.emit_st_idx();
    }
    Ok(())
}

/// Resolve a variant's tag by scanning the def table for sibling variants.
pub(crate) fn resolve_variant_tag_by_name(em: &Emitter<'_>, name: Symbol) -> u32 {
    resolve_variant_tag(em, name)
}

fn resolve_variant_tag(em: &Emitter<'_>, name: Symbol) -> u32 {
    let name_str = em.interner.resolve(name);
    for def in &em.sema.defs {
        if def.kind == DefKind::Variant && em.interner.resolve(def.name) == name_str {
            if let Some(parent_id) = def.parent {
                let mut siblings: Vec<u32> = em
                    .sema
                    .defs
                    .iter()
                    .filter(|d| d.kind == DefKind::Variant && d.parent == Some(parent_id))
                    .map(|d| d.id.0)
                    .collect();
                siblings.sort_unstable();
                if let Some(pos) = siblings.iter().position(|&x| x == def.id.0) {
                    return u32::try_from(pos).unwrap_or(0);
                }
            }
            return 0;
        }
    }
    0
}

/// Resolve a named field to its positional index by inspecting the object's type.
fn resolve_field_name(em: &Emitter<'_>, object_expr: ExprIdx, name: Symbol) -> u32 {
    let Some(&ty_idx) = em.sema.expr_types.get(&object_expr) else {
        return 0;
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { fields, .. } => {
            for (i, f) in fields.iter().enumerate() {
                if f.name == name {
                    return u32::try_from(i).unwrap_or(0);
                }
            }
            0
        }
        Type::Tuple { elems } => {
            let name_str = em.interner.resolve(name);
            if let Ok(n) = name_str.parse::<usize>() {
                return u32::try_from(n).unwrap_or(0);
            }
            let _ = elems;
            0
        }
        _ => 0,
    }
}

/// Classify the type family of an expression for opcode selection.
pub(crate) fn classify_type_family(em: &Emitter<'_>, expr_idx: ExprIdx) -> Option<TypeFamily> {
    let ty_idx = em.sema.expr_types.get(&expr_idx).copied()?;
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    let ty = &em.sema.types[resolved];
    let wk = &em.sema.well_known;
    match ty {
        Type::Named { def, .. } => {
            let d = *def;
            if d == wk.ints.int
                || d == wk.ints.int8
                || d == wk.ints.int16
                || d == wk.ints.int32
                || d == wk.ints.int64
            {
                return Some(TypeFamily::Signed);
            }
            if d == wk.uints.uint8
                || d == wk.uints.uint16
                || d == wk.uints.uint32
                || d == wk.uints.uint64
            {
                return Some(TypeFamily::Unsigned);
            }
            if d == wk.floats.float32 || d == wk.floats.float64 {
                return Some(TypeFamily::Float);
            }
            if d == wk.bool {
                return Some(TypeFamily::Bool);
            }
            None
        }
        _ => None,
    }
}

/// Map an AST `BinOp` + `TypeFamily` to the corresponding bytecode `Opcode`.
pub(crate) fn map_binop(op: BinOp, family: Option<TypeFamily>) -> Result<Opcode, EmitError> {
    let opcode = match (op, family) {
        (BinOp::Add, Some(TypeFamily::Float)) => Opcode::F_ADD,
        (BinOp::Add, _) => Opcode::I_ADD,
        (BinOp::Sub, Some(TypeFamily::Float)) => Opcode::F_SUB,
        (BinOp::Sub, _) => Opcode::I_SUB,
        (BinOp::Mul, Some(TypeFamily::Float)) => Opcode::F_MUL,
        (BinOp::Mul, _) => Opcode::I_MUL,
        (BinOp::Div, Some(TypeFamily::Float)) => Opcode::F_DIV,
        (BinOp::Div, Some(TypeFamily::Unsigned)) => Opcode::I_DIV_UN,
        (BinOp::Div, _) => Opcode::I_DIV,
        (BinOp::Rem, Some(TypeFamily::Float)) => Opcode::F_REM,
        (BinOp::Rem, Some(TypeFamily::Unsigned)) => Opcode::I_REM_UN,
        (BinOp::Rem, _) => Opcode::I_REM,
        (BinOp::Eq, Some(TypeFamily::Float)) => Opcode::CMP_F_EQ,
        (BinOp::Eq, _) => Opcode::CMP_EQ,
        (BinOp::Ne, Some(TypeFamily::Float)) => Opcode::CMP_F_NE,
        (BinOp::Ne, _) => Opcode::CMP_NE,
        (BinOp::Lt, Some(TypeFamily::Float)) => Opcode::CMP_F_LT,
        (BinOp::Lt, Some(TypeFamily::Unsigned)) => Opcode::CMP_LT_UN,
        (BinOp::Lt, _) => Opcode::CMP_LT,
        (BinOp::Le, Some(TypeFamily::Float)) => Opcode::CMP_F_LE,
        (BinOp::Le, Some(TypeFamily::Unsigned)) => Opcode::CMP_LE_UN,
        (BinOp::Le, _) => Opcode::CMP_LE,
        (BinOp::Gt, Some(TypeFamily::Float)) => Opcode::CMP_F_GT,
        (BinOp::Gt, Some(TypeFamily::Unsigned)) => Opcode::CMP_GT_UN,
        (BinOp::Gt, _) => Opcode::CMP_GT,
        (BinOp::Ge, Some(TypeFamily::Float)) => Opcode::CMP_F_GE,
        (BinOp::Ge, Some(TypeFamily::Unsigned)) => Opcode::CMP_GE_UN,
        (BinOp::Ge, _) => Opcode::CMP_GE,
        (BinOp::Xor, _) => Opcode::B_XOR,
        (BinOp::Shl, _) => Opcode::B_SHL,
        (BinOp::Shr, _) => Opcode::B_SHR,
        (BinOp::ShrUn, _) => Opcode::B_SHR_UN,
        (op, _) => {
            return Err(EmitError::UnsupportedFeature {
                desc: format!("binary operator `{op:?}`").into(),
            });
        }
    };
    Ok(opcode)
}
