//! Type lowering: sema `Type` -> `IrType`.

use music_sema::SemaResult;
use music_sema::types::Type;
use music_shared::{Arena, Idx};

use crate::error::IrError;
use crate::types::{IrEffectMask, IrType};

/// Lowers a sema type to an IR type, interning it into `ir_types`.
///
/// Resolves unification variables before matching, so callers do not need
/// to pre-resolve.
///
/// # Errors
///
/// Returns [`IrError::UnresolvedTypeVariable`] if the type contains an
/// unsolved unification variable, or [`IrError::UnsupportedExpr`] if the
/// type is not representable in the IR MVP subset.
pub fn lower_ty(
    ty: Idx<Type>,
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<Idx<IrType>, IrError> {
    // Resolve through unification variable chains first.
    let ty = sema.unify.resolve(ty, &sema.types);
    // Clone to avoid holding a borrow on `sema` during recursive calls.
    let ty_data = sema.types[ty].clone();
    match ty_data {
        Type::Named { def, .. } => lower_named(def, sema, ir_types),
        Type::Tuple { elems } => lower_tuple(&elems, sema, ir_types),
        Type::Fn { params, ret, .. } => lower_fn(&params, ret, sema, ir_types),
        Type::Var(_) | Type::Rigid(_) => Err(IrError::UnresolvedTypeVariable),
        _ => Err(IrError::UnsupportedExpr),
    }
}

fn lower_named(
    def: music_sema::DefId,
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<Idx<IrType>, IrError> {
    let wk = &sema.well_known;
    let ir_ty = if def == wk.ints.int || def == wk.ints.int64 {
        IrType::Int64
    } else if def == wk.ints.int32 {
        IrType::Int32
    } else if def == wk.ints.int16 {
        IrType::Int16
    } else if def == wk.ints.int8 {
        IrType::Int8
    } else if def == wk.uints.uint64 {
        IrType::UInt64
    } else if def == wk.uints.uint32 {
        IrType::UInt32
    } else if def == wk.uints.uint16 {
        IrType::UInt16
    } else if def == wk.uints.uint8 {
        IrType::UInt8
    } else if def == wk.floats.float64 {
        IrType::Float64
    } else if def == wk.floats.float32 {
        IrType::Float32
    } else if def == wk.bool {
        IrType::Bool
    } else if def == wk.unit {
        IrType::Unit
    } else if def == wk.rune {
        IrType::Rune
    } else if def == wk.string {
        // Strings are NaN-boxed opaque handles in the VM.
        IrType::UInt64
    } else if def == wk.never {
        // Never is unreachable; use Unit as a placeholder.
        IrType::Unit
    } else {
        return Err(IrError::UnsupportedExpr);
    };
    Ok(ir_types.alloc(ir_ty))
}

fn lower_tuple(
    elems: &[Idx<Type>],
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<Idx<IrType>, IrError> {
    let mut fields = Vec::with_capacity(elems.len());
    for &elem in elems {
        fields.push(lower_ty(elem, sema, ir_types)?);
    }
    Ok(ir_types.alloc(IrType::Product { fields }))
}

fn lower_fn(
    params: &[Idx<Type>],
    ret: Idx<Type>,
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<Idx<IrType>, IrError> {
    let mut ir_params = Vec::with_capacity(params.len());
    for &p in params {
        ir_params.push(lower_ty(p, sema, ir_types)?);
    }
    let ir_ret = lower_ty(ret, sema, ir_types)?;
    Ok(ir_types.alloc(IrType::Fn {
        params: ir_params,
        ret: ir_ret,
        effect_mask: IrEffectMask::PURE,
    }))
}
