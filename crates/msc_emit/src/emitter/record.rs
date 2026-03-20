//! Record literal, variant, and update emission.

use msc_ast::ExprIdx;
use msc_ast::expr::RecField;
use msc_sema::types::Type;
use msc_shared::Symbol;

use crate::error::EmitError;
use crate::error::EmitResult;

use super::Emitter;
use super::FnCtx;
use super::expr::emit_require;
use super::type_query::resolve_variant_tag;

pub(super) fn emit_record_lit(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
    _record_type_id: Option<u32>,
) -> EmitResult<bool> {
    let has_spread = fields.iter().any(|f| matches!(f, RecField::Spread { .. }));

    if has_spread {
        emit_record_lit_with_spread(em, fc, fields)
    } else {
        emit_record_lit_fixed(em, fc, fields)
    }
}

fn emit_record_lit_fixed(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
) -> EmitResult<bool> {
    // Collect (name, value) pairs and sort by name for canonical field ordering.
    // This ordering must match `resolve_field_in_type` and the type checker's
    // canonical sort so that field indices are consistent across modules.
    let mut named_fields: Vec<(Symbol, Option<ExprIdx>)> = fields
        .iter()
        .filter_map(|f| match f {
            RecField::Named { name, value, .. } => Some((*name, *value)),
            RecField::Spread { .. } => None,
        })
        .collect();
    named_fields.sort_by(|(a, _), (b, _)| em.interner.resolve(*a).cmp(em.interner.resolve(*b)));
    let n = named_fields.len();
    for (_, val_opt) in named_fields {
        if let Some(val_idx) = val_opt {
            emit_require(em, fc, val_idx, "record field")?;
        } else {
            return Err(EmitError::UnsupportedFeature {
                desc: "record field with no value".into(),
            });
        }
    }
    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
}

/// Emit a record literal that contains a spread (`{...base, name: val}`).
///
/// Records are heap-allocated products. The spread provides the base object;
/// named fields override specific positions in that object via `ST_FLD`.
/// Only a single leading spread followed by named overrides is supported
/// (the common `{...base, key: val}` pattern). Multiple spreads or spreads
/// mixed with overrides in arbitrary order would require full type-layout
/// knowledge that isn't available here.
fn emit_record_lit_with_spread(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    fields: &[RecField],
) -> EmitResult<bool> {
    let mut spread_expr: Option<ExprIdx> = None;
    for f in fields {
        if let RecField::Spread { expr, .. } = f {
            if spread_expr.is_some() {
                return Err(EmitError::UnsupportedFeature {
                    desc: "multiple spread elements in record literal".into(),
                });
            }
            spread_expr = Some(*expr);
        }
    }
    let base_expr = spread_expr.ok_or_else(|| EmitError::UnsupportedFeature {
        desc: "record spread without base expression".into(),
    })?;

    emit_require(em, fc, base_expr, "record spread base")?;
    let rec_slot = fc.alloc_local();
    fc.fe.emit_st_loc(rec_slot);

    for f in fields {
        if let RecField::Named { name, value, .. } = f {
            let val_idx = value.ok_or_else(|| EmitError::UnsupportedFeature {
                desc: "record spread override field has no value".into(),
            })?;

            let field_idx = resolve_field_name_by_symbol(em, base_expr, *name)?;

            fc.fe.emit_ld_loc(rec_slot);
            emit_require(em, fc, val_idx, "record spread override field")?;
            fc.fe.emit_st_fld(field_idx)?;
        }
    }

    fc.fe.emit_ld_loc(rec_slot);
    Ok(true)
}

fn resolve_field_name_by_symbol(
    em: &Emitter<'_>,
    object_expr: ExprIdx,
    name: Symbol,
) -> EmitResult<u32> {
    let Some(&ty_idx) = em.expr_types().get(&object_expr) else {
        return Err(EmitError::NoTypeInfo {
            desc: "record spread base".into(),
        });
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { fields, .. } => {
            let mut sorted: Vec<_> = fields.clone();
            sorted.sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            for (i, f) in sorted.iter().enumerate() {
                if f.name == name {
                    return u32::try_from(i).map_err(|_| {
                        EmitError::overflow(format!(
                            "record field index for `{}`",
                            em.interner.resolve(name)
                        ))
                    });
                }
            }
            Err(EmitError::FieldNotFound {
                desc: format!("record field `{}`", em.interner.resolve(name)).into(),
            })
        }
        _ => Err(EmitError::FieldNotFound {
            desc: format!(
                "spread base is not a record type (field `{}`)",
                em.interner.resolve(name)
            )
            .into(),
        }),
    }
}

pub(super) fn emit_variant(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    name: Symbol,
    args: &[ExprIdx],
) -> EmitResult<bool> {
    let name_str = em.interner.resolve(name);
    if args.is_empty() {
        if name_str == "True" {
            fc.fe.emit_ld_true();
            return Ok(true);
        }
        if name_str == "False" {
            fc.fe.emit_ld_false();
            return Ok(true);
        }
        if name_str == "None" {
            fc.fe.emit_ld_none();
            return Ok(true);
        }
    }

    for &arg_idx in args {
        emit_require(em, fc, arg_idx, "variant arg")?;
    }
    let tag = resolve_variant_tag(em, name)?;
    let arity = u8::try_from(args.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "variant arity exceeds 255".into(),
    })?;
    fc.fe.emit_mk_var(tag, arity);
    Ok(true)
}

/// Emit `{ base | field = val, ... }` (functional record update).
/// Copies unmodified fields from base, substitutes updated fields, then builds
/// a new product.
pub(super) fn emit_update(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    base: ExprIdx,
    fields: &[RecField],
) -> EmitResult<bool> {
    emit_require(em, fc, base, "update base")?;
    let base_slot = fc.alloc_local();
    fc.fe.emit_st_loc(base_slot);

    // Look up the record type to determine field layout.
    let Some(&ty_idx) = em.expr_types().get(&base) else {
        return Err(EmitError::NoTypeInfo {
            desc: "update base".into(),
        });
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    // Clone needed: the borrow of em.sema.types must end before emit_expr borrows em mutably.
    let type_fields = match &em.sema.types[resolved] {
        Type::Record { fields, .. } => fields.clone(),
        _ => {
            return Err(EmitError::UnsupportedFeature {
                desc: "update on non-record type".into(),
            });
        }
    };

    let n = type_fields.len();

    for (i, type_field) in type_fields.iter().enumerate() {
        // Check if this field is being updated.
        let update_val = fields.iter().find_map(|f| {
            if let RecField::Named {
                name,
                value: Some(val_idx),
                ..
            } = f
                && *name == type_field.name
            {
                return Some(*val_idx);
            }
            None
        });

        if let Some(val_idx) = update_val {
            emit_require(em, fc, val_idx, "update field value")?;
        } else {
            let idx = u32::try_from(i).map_err(|_| EmitError::overflow("record field index"))?;
            fc.fe.emit_ld_loc(base_slot);
            fc.fe.emit_ld_fld(idx)?;
        }
    }

    let field_count = u32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    let stack_pop = i32::try_from(n).map_err(|_| EmitError::overflow("record field count"))?;
    fc.fe.emit_mk_prd(field_count, stack_pop)?;
    Ok(true)
}
