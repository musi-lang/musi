use music_ast::expr::BinOp;
use music_builtins::types::BuiltinType;

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

/// Maps a binary operator to (class name, intrinsic, method index).
const fn binop_class_method(op: BinOp) -> Option<(&'static str, &'static str, usize)> {
    match op {
        BinOp::Add => Some(("Num", "i.add", 0)),
        BinOp::Sub => Some(("Num", "i.sub", 1)),
        BinOp::Mul => Some(("Num", "i.mul", 2)),
        BinOp::Div => Some(("Num", "i.div", 3)),
        BinOp::Rem => Some(("Num", "i.mod", 4)),
        BinOp::Eq => Some(("Eq", "cmp.eq", 0)),
        BinOp::NotEq => Some(("Eq", "cmp.neq", 1)),
        BinOp::Lt => Some(("Ord", "cmp.lt", 0)),
        BinOp::Gt => Some(("Ord", "cmp.gt", 1)),
        BinOp::LtEq => Some(("Ord", "cmp.leq", 2)),
        BinOp::GtEq => Some(("Ord", "cmp.geq", 3)),
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
        Ty::Builtin(BuiltinType::Bool) => {
            let intrinsic = match op {
                BinOp::And => "and",
                BinOp::Or => "or",
                BinOp::Xor => "xor",
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
                dispatch: Some(DispatchInfo::Static { intrinsic }),
                needs_class: None,
            }
        }
        Ty::Builtin(
            BuiltinType::Int
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
            let intrinsic = match op {
                BinOp::And => "i.and",
                BinOp::Or => "i.or",
                BinOp::Xor => "i.xor",
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
                dispatch: Some(DispatchInfo::Static { intrinsic }),
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
                intrinsic: "list.cons",
            }),
            needs_class: None,
        };
    }

    if matches!(op, BinOp::Range | BinOp::RangeExcl) {
        let range_ty = env.intern(Ty::Tuple(vec![lhs_ty, lhs_ty]));
        return BinOpResolution {
            result_ty: range_ty,
            dispatch: Some(DispatchInfo::Static {
                intrinsic: "range.new",
            }),
            needs_class: None,
        };
    }

    if op == BinOp::NilCoalesce {
        let resolved = env.resolve_var(lhs_ty);
        return BinOpResolution {
            result_ty: resolved,
            dispatch: Some(DispatchInfo::Static {
                intrinsic: "option.unwrap_or",
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

    let Some((class_name, intrinsic, _)) = binop_class_method(op) else {
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
            BinOpResolution {
                result_ty,
                dispatch: Some(DispatchInfo::Static { intrinsic }),
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

/// Returns the method index within a class for dictionary dispatch.
#[must_use]
pub const fn method_index_for_op(op: BinOp) -> usize {
    match binop_class_method(op) {
        Some((_, _, idx)) => idx,
        None => 0,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
