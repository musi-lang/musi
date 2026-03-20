//! Type classification and field/variant resolution helpers.

use msc_ast::ExprIdx;
use msc_ast::expr::{BinOp, Expr};
use msc_bc::Opcode;
use msc_sema::TypeIdx;
use msc_sema::def::DefKind;
use msc_sema::types::{RecordField, Type};
use msc_shared::Symbol;

use crate::error::{EmitError, EmitResult};

use super::super::emitter::Emitter;

/// Bit-width of a numeric type, used for narrow-type truncation after arithmetic.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Width {
    W8,
    W16,
    W32,
    W64,
}

/// Type family used to select the right arithmetic/comparison opcodes.
#[derive(Clone, Copy)]
pub(super) enum TypeFamily {
    Signed(Width),
    Unsigned(Width),
    Float,
    Bool,
}

/// Returns `true` when the object expression's sema type resolves to `Type::Tuple`.
pub(super) fn object_type_is_tuple(em: &Emitter<'_>, object: ExprIdx) -> bool {
    let Some(&ty_idx) = em.expr_types().get(&object) else {
        return false;
    };
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    matches!(&em.sema.types[resolved], Type::Tuple { .. })
}

/// Resolve an AST type annotation (`ExprIdx`) to a bytecode `type_id`.
///
/// First checks well-known types by name. If not found, walks sema defs to
/// find a matching named type and lowers its sema type. Returns `None` if
/// the type cannot be resolved (e.g. type variable, unresolved name).
pub(super) fn lower_ast_ty_to_type_id(em: &mut Emitter<'_>, ty: ExprIdx) -> Option<u32> {
    let name = match &em.ast.exprs[ty] {
        Expr::Name { name_ref, .. } => em.ast.name_refs[*name_ref].name,
        Expr::TypeApp { callee, .. } => match &em.ast.exprs[*callee] {
            Expr::Name { name_ref, .. } => em.ast.name_refs[*name_ref].name,
            _ => return None,
        },
        _ => return None,
    };
    let wk = &em.sema.well_known;
    let name_str = em.interner.resolve(name);
    // Well-known primitive types - match by name so they work even without sema ty_info.
    match name_str {
        "Bool" => return em.tp.lower_well_known_def(wk.bool, wk),
        "Int" | "Int64" => return em.tp.lower_well_known_def(wk.ints.int, wk),
        "Int8" => return em.tp.lower_well_known_def(wk.ints.int8, wk),
        "Int16" => return em.tp.lower_well_known_def(wk.ints.int16, wk),
        "Int32" => return em.tp.lower_well_known_def(wk.ints.int32, wk),
        "Nat" | "Nat64" => return em.tp.lower_well_known_def(wk.nats.nat, wk),
        "Nat8" => return em.tp.lower_well_known_def(wk.nats.nat8, wk),
        "Nat16" => return em.tp.lower_well_known_def(wk.nats.nat16, wk),
        "Nat32" => return em.tp.lower_well_known_def(wk.nats.nat32, wk),
        "Float" => return em.tp.lower_well_known_def(wk.float, wk),
        "Float32" => return em.tp.lower_well_known_def(wk.floats.float32, wk),
        "Float64" => return em.tp.lower_well_known_def(wk.floats.float64, wk),
        "Rune" => return em.tp.lower_well_known_def(wk.rune, wk),
        "String" => return em.tp.lower_well_known_def(wk.string, wk),
        "Unit" => return em.tp.lower_well_known_def(wk.unit, wk),
        _ => {}
    }
    // User-defined named type: find by name in sema defs.
    let def = em
        .sema
        .defs
        .iter()
        .find(|d| d.name == name && matches!(d.kind, DefKind::Type | DefKind::Effect))?;
    let ty_idx = def.ty_info.ty?;
    em.tp
        .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
        .ok()
}

/// Resolve a variant's tag by scanning the def table for sibling variants.
pub fn resolve_variant_tag(em: &Emitter<'_>, name: Symbol) -> EmitResult<u32> {
    let name_str = em.interner.resolve(name);
    for def in &em.sema.defs {
        if def.kind == DefKind::Variant && em.interner.resolve(def.name) == name_str {
            let Some(parent_id) = def.parent else {
                return Err(EmitError::FieldNotFound {
                    desc: format!("variant `{name_str}` has no parent choice type").into(),
                });
            };
            let mut siblings: Vec<u32> = em
                .sema
                .defs
                .iter()
                .filter(|d| d.kind == DefKind::Variant && d.parent == Some(parent_id))
                .map(|d| d.id.0)
                .collect();
            siblings.sort_unstable();
            let pos = siblings
                .iter()
                .position(|&x| x == def.id.0)
                .ok_or_else(|| EmitError::FieldNotFound {
                    desc: format!("variant `{name_str}` not found among its siblings").into(),
                })?;
            return u32::try_from(pos)
                .map_err(|_| EmitError::overflow(format!("variant tag index for `{name_str}`")));
        }
    }
    // Fall back to well-known tags for open variants not in the def table.
    let name_str_owned = em.interner.resolve(name).to_owned();
    if name_str_owned == "Some" {
        return Ok(em.some_tag);
    }
    if name_str_owned == "None" {
        return Ok(u32::from(em.some_tag == 0));
    }
    if name_str_owned == "Ok" {
        return Ok(em.ok_tag);
    }
    if name_str_owned == "Err" {
        return Ok(u32::from(em.ok_tag == 0));
    }
    // Ordering variants: Less + Equal + Greater (defined in @std/cmp/ordering)
    if name_str_owned == "Less" {
        return Ok(0);
    }
    if name_str_owned == "Equal" {
        return Ok(1);
    }
    if name_str_owned == "Greater" {
        return Ok(2);
    }
    Err(EmitError::FieldNotFound {
        desc: format!("variant `{name_str}` not found in any choice type").into(),
    })
}

/// Resolve a named field to its positional index by inspecting the object's type.
pub(super) fn resolve_field_name(
    em: &Emitter<'_>,
    object_expr: ExprIdx,
    name: Symbol,
) -> EmitResult<u32> {
    let Some(&ty_idx) = em.expr_types().get(&object_expr) else {
        return Err(EmitError::NoTypeInfo {
            desc: "field access object".into(),
        });
    };
    // Try the expression type first, then fall back to resolving through the
    // object's definition type if the field isn't found (handles partially
    // unified row types in cross-module emission).
    if let Ok(idx) = resolve_field_in_type(em, ty_idx, name) {
        return Ok(idx);
    }
    // Fallback: look up the object's definition type (the declared type of
    // the variable, which may be fully concrete).
    if let Some(&def_id) = em.expr_defs().get(&object_expr)
        && let Some(def_info) = em.sema.defs.iter().find(|d| d.id == def_id)
        && let Some(def_ty) = def_info.ty_info.ty
        && let Ok(idx) = resolve_field_in_type(em, def_ty, name)
    {
        return Ok(idx);
    }
    Err(EmitError::FieldNotFound {
        desc: format!("record field `{}`", em.interner.resolve(name)).into(),
    })
}

fn collect_record_fields(em: &Emitter<'_>, ty_idx: TypeIdx, out: &mut Vec<RecordField>) -> bool {
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    if let Type::Record { fields, rest } = &em.sema.types[resolved] {
        for f in fields {
            if !out.iter().any(|ef| ef.name == f.name) {
                out.push(f.clone());
            }
        }
        rest.is_none_or(|rest_idx| collect_record_fields(em, rest_idx, out))
    } else {
        false // rest resolved to non-record (unbound var) - incomplete
    }
}

/// Search for a closed record type definition whose fields are a superset
/// of `partial_fields`. Returns the full field list if found.
fn find_complete_record_type(
    em: &Emitter<'_>,
    partial_fields: &[RecordField],
) -> Option<Vec<RecordField>> {
    for def in &em.sema.defs {
        // Look at type defs and let bindings that define record types.
        if !matches!(def.kind, DefKind::Type | DefKind::Let) {
            continue;
        }
        let Some(ty_idx) = def.ty_info.ty else {
            continue;
        };
        let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
        let ty = &em.sema.types[resolved];

        // Unwrap Type::Named to its underlying type (e.g., Deque['a] → Record).
        let record_resolved = match ty {
            Type::Record { .. } => resolved,
            Type::Named { def: named_def, .. } => {
                let inner_def = em.sema.defs.iter().find(|d| d.id == *named_def);
                if let Some(d) = inner_def {
                    if let Some(inner_ty) = d.ty_info.ty {
                        em.sema.unify.resolve(inner_ty, &em.sema.types)
                    } else {
                        continue;
                    }
                } else {
                    continue;
                }
            }
            _ => continue,
        };

        if let Type::Record { rest, .. } = &em.sema.types[record_resolved] {
            if rest.is_some() {
                continue;
            }
            let mut full_fields = vec![];
            let _ = collect_record_fields(em, record_resolved, &mut full_fields);
            let is_superset = partial_fields
                .iter()
                .all(|pf| full_fields.iter().any(|ff| ff.name == pf.name));
            if is_superset && full_fields.len() >= partial_fields.len() {
                return Some(full_fields);
            }
        }
    }
    None
}

fn resolve_field_in_type(em: &Emitter<'_>, ty_idx: TypeIdx, name: Symbol) -> EmitResult<u32> {
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    match &em.sema.types[resolved] {
        Type::Record { .. } => {
            let mut all_fields: Vec<RecordField> = vec![];
            let complete = collect_record_fields(em, resolved, &mut all_fields);
            if !complete {
                all_fields
                    .sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            }
            // If the record is open (incomplete from dep module inference),
            // find the canonical closed record type definition.
            if !complete && let Some(full) = find_complete_record_type(em, &all_fields) {
                all_fields = full;
            }
            // Sort by name string for canonical ordering - matches emit_record_lit_fixed.
            all_fields.sort_by(|a, b| em.interner.resolve(a.name).cmp(em.interner.resolve(b.name)));
            for (i, f) in all_fields.iter().enumerate() {
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
        Type::Tuple { elems } => {
            let name_str = em.interner.resolve(name);
            let n = name_str
                .parse::<usize>()
                .map_err(|_| EmitError::FieldNotFound {
                    desc: format!("tuple index `{name_str}`").into(),
                })?;
            let _ = elems;
            u32::try_from(n)
                .map_err(|_| EmitError::overflow(format!("tuple field index `{name_str}`")))
        }
        _ => Err(EmitError::FieldNotFound {
            desc: format!("field `{}` on non-record type", em.interner.resolve(name)).into(),
        }),
    }
}

/// Returns true if the family indicates logical (short-circuit) semantics.
/// Bool → logical; known numeric → bitwise; None → logical (safe default).
pub(super) const fn is_logical_family(family: Option<TypeFamily>) -> bool {
    match family {
        Some(TypeFamily::Bool) | None => true,
        Some(_) => false,
    }
}

/// Classify the type family of an expression for opcode selection.
pub fn classify_type_family(em: &Emitter<'_>, expr_idx: ExprIdx) -> Option<TypeFamily> {
    let ty_idx = em.expr_types().get(&expr_idx).copied()?;
    let resolved = em.sema.unify.resolve(ty_idx, &em.sema.types);
    let ty = &em.sema.types[resolved];
    let wk = &em.sema.well_known;
    match ty {
        Type::Named { def, .. } => {
            let d = *def;
            if d == wk.ints.int || d == wk.ints.int64 {
                return Some(TypeFamily::Signed(Width::W64));
            }
            if d == wk.ints.int32 {
                return Some(TypeFamily::Signed(Width::W32));
            }
            if d == wk.ints.int16 {
                return Some(TypeFamily::Signed(Width::W16));
            }
            if d == wk.ints.int8 {
                return Some(TypeFamily::Signed(Width::W8));
            }
            if d == wk.nats.nat || d == wk.nats.nat64 {
                return Some(TypeFamily::Unsigned(Width::W64));
            }
            if d == wk.nats.nat32 {
                return Some(TypeFamily::Unsigned(Width::W32));
            }
            if d == wk.nats.nat16 {
                return Some(TypeFamily::Unsigned(Width::W16));
            }
            if d == wk.nats.nat8 {
                return Some(TypeFamily::Unsigned(Width::W8));
            }
            if d == wk.float || d == wk.floats.float64 {
                return Some(TypeFamily::Float);
            }
            if d == wk.floats.float32 {
                return Some(TypeFamily::Float);
            }
            if d == wk.bool {
                return Some(TypeFamily::Bool);
            }
            // Any-typed expressions: treat as integer for opcode selection.
            // At runtime, bitwise ops on ints give correct results, and on bools
            // (1/0) they match boolean semantics, so this is safe.
            if d == wk.any {
                return Some(TypeFamily::Signed(Width::W64));
            }
            None
        }
        _ => None,
    }
}

/// Map an AST `BinOp` to the corresponding SEAM `Opcode`.
///
/// NaN-boxing handles runtime type dispatch; no type-specific variants exist.
pub fn map_binop(op: BinOp, _family: Option<TypeFamily>) -> EmitResult<Opcode> {
    let opcode = match op {
        BinOp::Add => Opcode::ADD,
        BinOp::Sub => Opcode::SUB,
        BinOp::Mul => Opcode::MUL,
        BinOp::Div => Opcode::DIV,
        BinOp::Rem => Opcode::REM,
        BinOp::Eq => Opcode::CMP_EQ,
        BinOp::Ne => Opcode::CMP_NE,
        BinOp::Lt => Opcode::CMP_LT,
        BinOp::Le => Opcode::CMP_LE,
        BinOp::Gt => Opcode::CMP_GT,
        BinOp::Ge => Opcode::CMP_GE,
        BinOp::And => Opcode::BAND,
        BinOp::Or => Opcode::BOR,
        BinOp::Xor => Opcode::BXOR,
        op => {
            return Err(EmitError::UnsupportedFeature {
                desc: format!("binary operator `{op:?}`").into(),
            });
        }
    };
    Ok(opcode)
}
