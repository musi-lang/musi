use super::*;

pub(super) fn lower_binary_op(
    ctx: &LowerCtx<'_>,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
    interner: &Interner,
) -> IrBinaryOp {
    let sema = ctx.sema;
    let left_ty = sema.ty(sema
        .try_expr_ty(left)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for binary left")));
    let right_ty = sema.ty(sema
        .try_expr_ty(right)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for binary right")));
    let wants_float = matches!(
        left_ty.kind,
        HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
    ) || matches!(
        right_ty.kind,
        HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
    );
    let wants_string =
        matches!(left_ty.kind, HirTyKind::String) || matches!(right_ty.kind, HirTyKind::String);
    if let Some(op) = lower_arithmetic_binary_op(op, wants_float, wants_string) {
        return op;
    }
    if let Some(op) = lower_comparison_binary_op(op) {
        return op;
    }
    if let Some(op) = lower_logical_binary_op(op, &left_ty.kind, interner) {
        return op;
    }
    match op {
        HirBinaryOp::UserOp(ident) => IrBinaryOp::Other(interner.resolve(ident.name).into()),
        other => IrBinaryOp::Other(format!("{other:?}").into()),
    }
}

const fn lower_arithmetic_binary_op(
    op: &HirBinaryOp,
    wants_float: bool,
    wants_string: bool,
) -> Option<IrBinaryOp> {
    Some(match op {
        HirBinaryOp::Add => {
            if wants_string {
                IrBinaryOp::StrCat
            } else if wants_float {
                IrBinaryOp::FAdd
            } else {
                IrBinaryOp::IAdd
            }
        }
        HirBinaryOp::Sub => {
            if wants_float {
                IrBinaryOp::FSub
            } else {
                IrBinaryOp::ISub
            }
        }
        HirBinaryOp::Mul => {
            if wants_float {
                IrBinaryOp::FMul
            } else {
                IrBinaryOp::IMul
            }
        }
        HirBinaryOp::Div => {
            if wants_float {
                IrBinaryOp::FDiv
            } else {
                IrBinaryOp::IDiv
            }
        }
        HirBinaryOp::Rem => {
            if wants_float {
                IrBinaryOp::FRem
            } else {
                IrBinaryOp::IRem
            }
        }
        _ => return None,
    })
}

const fn lower_comparison_binary_op(op: &HirBinaryOp) -> Option<IrBinaryOp> {
    Some(match op {
        HirBinaryOp::Eq => IrBinaryOp::Eq,
        HirBinaryOp::Ne => IrBinaryOp::Ne,
        HirBinaryOp::Lt => IrBinaryOp::Lt,
        HirBinaryOp::Gt => IrBinaryOp::Gt,
        HirBinaryOp::Le => IrBinaryOp::Le,
        HirBinaryOp::Ge => IrBinaryOp::Ge,
        _ => return None,
    })
}

fn lower_logical_binary_op(
    op: &HirBinaryOp,
    left_ty: &HirTyKind,
    interner: &Interner,
) -> Option<IrBinaryOp> {
    Some(match (op, left_ty) {
        (HirBinaryOp::Xor, HirTyKind::Bool) => IrBinaryOp::LogicalXor,
        (HirBinaryOp::And, _) if is_bits_ty(left_ty, interner) => IrBinaryOp::BitsAnd,
        (HirBinaryOp::Or, _) if is_bits_ty(left_ty, interner) => IrBinaryOp::BitsOr,
        (HirBinaryOp::Xor, _) if is_bits_ty(left_ty, interner) => IrBinaryOp::BitsXor,
        _ => return None,
    })
}

fn is_bits_ty(ty: &HirTyKind, interner: &Interner) -> bool {
    match ty {
        HirTyKind::Bits { .. } => true,
        HirTyKind::Named { name, .. } => interner.resolve(*name) == "Bits",
        _ => false,
    }
}
