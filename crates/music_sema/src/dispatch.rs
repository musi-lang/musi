use music_ast::expr::BinOp;
use music_il::opcode::Opcode;
use music_owned::types::BuiltinType;

use crate::env::{DispatchInfo, TypeEnv};
use crate::types::{SemaTypeId, Ty};

/// Result of resolving a binary operator's dispatch.
pub struct BinOpResolution {
    pub result_ty: SemaTypeId,
    pub dispatch: Option<DispatchInfo>,
    /// For `Dictionary` dispatch, the class name string the caller must
    /// resolve to a `Symbol`. `None` for static/dynamic dispatch.
    pub needs_class: Option<&'static str>,
}

/// Maps a binary operator to (class name, int opcode, float opcode, method index).
const fn binop_class_method(op: BinOp) -> Option<(&'static str, Opcode, Option<Opcode>, usize)> {
    match op {
        BinOp::Add => Some(("Num", Opcode::IAdd, Some(Opcode::FAdd), 0)),
        BinOp::Sub => Some(("Num", Opcode::ISub, Some(Opcode::FSub), 1)),
        BinOp::Mul => Some(("Num", Opcode::IMul, Some(Opcode::FMul), 2)),
        BinOp::Div => Some(("Num", Opcode::IDiv, Some(Opcode::FDiv), 3)),
        BinOp::Rem => Some(("Num", Opcode::IRem, None, 4)),
        BinOp::Shl => Some(("Bits", Opcode::Shl, None, 0)),
        BinOp::Shr => Some(("Bits", Opcode::Shr, None, 1)),
        BinOp::Eq => Some(("Eq", Opcode::CmpEq, None, 0)),
        BinOp::NotEq => Some(("Eq", Opcode::CmpNeq, None, 1)),
        BinOp::Lt => Some(("Ord", Opcode::CmpLt, None, 0)),
        BinOp::Gt => Some(("Ord", Opcode::CmpGt, None, 1)),
        BinOp::LtEq => Some(("Ord", Opcode::CmpLeq, None, 2)),
        BinOp::GtEq => Some(("Ord", Opcode::CmpGeq, None, 3)),
        BinOp::And
        | BinOp::Or
        | BinOp::Xor
        | BinOp::Cons
        | BinOp::Range
        | BinOp::RangeExcl
        | BinOp::NilCoalesce
        | BinOp::PipeRight => None,
    }
}

/// Returns `true` if the builtin type has an instance for the given class name.
pub(crate) fn builtin_has_instance(bt: BuiltinType, class: &str) -> bool {
    match class {
        "Num" => matches!(
            bt,
            BuiltinType::Int
                | BuiltinType::Nat
                | BuiltinType::Float
                | BuiltinType::Int8
                | BuiltinType::Int16
                | BuiltinType::Int32
                | BuiltinType::Int64
                | BuiltinType::Nat8
                | BuiltinType::Nat16
                | BuiltinType::Nat32
                | BuiltinType::Nat64
                | BuiltinType::Float32
                | BuiltinType::Float64
        ),
        "Eq" => matches!(
            bt,
            BuiltinType::Int
                | BuiltinType::Nat
                | BuiltinType::Float
                | BuiltinType::Bool
                | BuiltinType::Rune
                | BuiltinType::String
                | BuiltinType::Unit
                | BuiltinType::Int8
                | BuiltinType::Int16
                | BuiltinType::Int32
                | BuiltinType::Int64
                | BuiltinType::Nat8
                | BuiltinType::Nat16
                | BuiltinType::Nat32
                | BuiltinType::Nat64
                | BuiltinType::Float32
                | BuiltinType::Float64
        ),
        "Ord" => matches!(
            bt,
            BuiltinType::Int
                | BuiltinType::Nat
                | BuiltinType::Float
                | BuiltinType::Rune
                | BuiltinType::String
                | BuiltinType::Int8
                | BuiltinType::Int16
                | BuiltinType::Int32
                | BuiltinType::Int64
                | BuiltinType::Nat8
                | BuiltinType::Nat16
                | BuiltinType::Nat32
                | BuiltinType::Nat64
                | BuiltinType::Float32
                | BuiltinType::Float64
        ),
        "Bits" => matches!(
            bt,
            BuiltinType::Int
                | BuiltinType::Nat
                | BuiltinType::Int8
                | BuiltinType::Int16
                | BuiltinType::Int32
                | BuiltinType::Int64
                | BuiltinType::Nat8
                | BuiltinType::Nat16
                | BuiltinType::Nat32
                | BuiltinType::Nat64
        ),
        _ => false,
    }
}

/// Returns `true` if the operator produces a `Bool` result (comparisons).
const fn is_comparison(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq
    )
}

/// Resolves the result type and dispatch info for a logical/bitwise operator.
fn resolve_logical(env: &TypeEnv, op: BinOp, lhs_ty: SemaTypeId) -> BinOpResolution {
    let resolved = env.resolve_var(lhs_ty);
    let ty = env.types.get(resolved).clone();
    match ty {
        Ty::Builtin(
            BuiltinType::Bool
            | BuiltinType::Int
            | BuiltinType::Nat
            | BuiltinType::Int8
            | BuiltinType::Int16
            | BuiltinType::Int32
            | BuiltinType::Int64
            | BuiltinType::Nat8
            | BuiltinType::Nat16
            | BuiltinType::Nat32
            | BuiltinType::Nat64,
        ) => {
            let opcode = match op {
                BinOp::And => Opcode::And,
                BinOp::Or => Opcode::Or,
                BinOp::Xor => Opcode::Xor,
                _ => {
                    return BinOpResolution {
                        result_ty: resolved,
                        dispatch: None,
                        needs_class: None,
                    };
                }
            };
            BinOpResolution {
                result_ty: resolved,
                dispatch: Some(DispatchInfo::Static { opcode }),
                needs_class: None,
            }
        }
        Ty::Any => BinOpResolution {
            result_ty: resolved,
            dispatch: Some(DispatchInfo::Dynamic),
            needs_class: None,
        },
        _ => BinOpResolution {
            result_ty: resolved,
            dispatch: None,
            needs_class: None,
        },
    }
}

/// Resolves the result type and dispatch info for a binary operator.
///
/// For concrete builtin types with known class instances, produces
/// `DispatchInfo::Static`. For type parameters, sets `needs_class` so the
/// caller can resolve the class symbol and build `DispatchInfo::Dictionary`.
/// For `Any`, produces `DispatchInfo::Dynamic`.
pub fn resolve_binop(
    env: &mut TypeEnv,
    op: BinOp,
    lhs_ty: SemaTypeId,
    _rhs_ty: SemaTypeId,
) -> BinOpResolution {
    if matches!(op, BinOp::And | BinOp::Or | BinOp::Xor) {
        return resolve_logical(env, op, lhs_ty);
    }

    if op == BinOp::Cons {
        let list_ty = env.intern(Ty::List(lhs_ty));
        return BinOpResolution {
            result_ty: list_ty,
            dispatch: Some(DispatchInfo::Static {
                opcode: Opcode::ArrCaten,
            }),
            needs_class: None,
        };
    }

    if matches!(op, BinOp::Range | BinOp::RangeExcl) {
        let range_ty = env.intern(Ty::Tuple(vec![lhs_ty, lhs_ty]));
        return BinOpResolution {
            result_ty: range_ty,
            dispatch: Some(DispatchInfo::Static {
                opcode: Opcode::ArrNew,
            }),
            needs_class: None,
        };
    }

    if op == BinOp::NilCoalesce {
        let resolved = env.resolve_var(lhs_ty);
        return BinOpResolution {
            result_ty: resolved,
            dispatch: Some(DispatchInfo::Static {
                opcode: Opcode::Nop,
            }),
            needs_class: None,
        };
    }

    // `PipeRight` should be lowered away before sema
    if op == BinOp::PipeRight {
        return BinOpResolution {
            result_ty: lhs_ty,
            dispatch: None,
            needs_class: None,
        };
    }

    let Some((class_name, int_opcode, float_opcode, _)) = binop_class_method(op) else {
        return BinOpResolution {
            result_ty: lhs_ty,
            dispatch: None,
            needs_class: None,
        };
    };

    let resolved = env.resolve_var(lhs_ty);
    let ty = env.types.get(resolved).clone();

    match ty {
        Ty::Builtin(bt) if builtin_has_instance(bt, class_name) => {
            let result_ty = if is_comparison(op) {
                env.builtin(BuiltinType::Bool)
            } else {
                resolved
            };
            let effective = if is_float_type(bt) {
                float_opcode.unwrap_or(int_opcode)
            } else {
                int_opcode
            };
            BinOpResolution {
                result_ty,
                dispatch: Some(DispatchInfo::Static { opcode: effective }),
                needs_class: None,
            }
        }
        Ty::Param(_) => {
            let result_ty = if is_comparison(op) {
                env.builtin(BuiltinType::Bool)
            } else {
                resolved
            };
            BinOpResolution {
                result_ty,
                dispatch: None,
                needs_class: Some(class_name),
            }
        }
        Ty::Any => {
            let any = env.intern(Ty::Any);
            BinOpResolution {
                result_ty: any,
                dispatch: Some(DispatchInfo::Dynamic),
                needs_class: None,
            }
        }
        _ => BinOpResolution {
            result_ty: resolved,
            dispatch: None,
            needs_class: None,
        },
    }
}

const fn is_float_type(bt: BuiltinType) -> bool {
    matches!(
        bt,
        BuiltinType::Float | BuiltinType::Float32 | BuiltinType::Float64
    )
}

/// Returns the method index within a class for dictionary dispatch.
#[must_use]
pub const fn method_index_for_op(op: BinOp) -> usize {
    match binop_class_method(op) {
        Some((_, _, _, idx)) => idx,
        None => 0,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
