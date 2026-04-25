use super::*;

pub(super) fn verify_instruction(
    instruction: &HilInstruction,
    capabilities: &[HilShape],
    type_map: &mut HilTypeMap,
) -> HilVerifyResult {
    match instruction {
        HilInstruction::ConstInt { out, ty, .. } => define_value(type_map, *out, ty.clone()),
        HilInstruction::Binary {
            out,
            ty,
            left,
            right,
            ..
        } => {
            expect_type(type_map, *left, ty)?;
            expect_type(type_map, *right, ty)?;
            define_value(type_map, *out, ty.clone())
        }
        HilInstruction::Call {
            out,
            result_ty,
            args,
            ..
        } => {
            expect_defined_many(type_map, args)?;
            define_optional_value(type_map, *out, result_ty.clone())
        }
        HilInstruction::NewObj {
            out, ty, fields, ..
        } => {
            expect_defined_many(type_map, fields)?;
            define_value(type_map, *out, ty.clone())
        }
        HilInstruction::EffectCall {
            out,
            result_ty,
            args,
            ..
        } => {
            require_capability(capabilities, HilShape::Effect)?;
            expect_defined_many(type_map, args)?;
            define_value(type_map, *out, result_ty.clone())
        }
        HilInstruction::ForeignCall {
            out,
            result_ty,
            args,
            ..
        } => {
            require_capability(capabilities, HilShape::Native)?;
            expect_defined_many(type_map, args)?;
            define_optional_value(type_map, *out, result_ty.clone())
        }
    }
}

pub(super) fn verify_terminator(
    terminator: &HilTerminator,
    result_ty: Option<&HilType>,
    block_names: &HilBlockSet,
    type_map: &HilTypeMap,
) -> HilVerifyResult {
    match terminator {
        HilTerminator::Return(Some(id)) => {
            let found = type_of(type_map, *id)?;
            let Some(expected) = result_ty else {
                return Err(HilVerifyError::ReturnValueUnexpected);
            };
            if found == *expected {
                Ok(())
            } else {
                Err(HilVerifyError::ReturnTypeMismatch {
                    expected: expected.clone(),
                    found,
                })
            }
        }
        HilTerminator::Return(None) => result_ty.map_or(Ok(()), |expected| {
            Err(HilVerifyError::ReturnValueMissing {
                expected: expected.clone(),
            })
        }),
        HilTerminator::Branch { target } => verify_target(block_names, target),
        HilTerminator::CondBranch {
            condition,
            then_target,
            else_target,
        } => {
            let _ = type_of(type_map, *condition)?;
            verify_target(block_names, then_target)?;
            verify_target(block_names, else_target)
        }
        HilTerminator::TailCall { args, .. } => expect_defined_many(type_map, args),
    }
}

fn define_optional_value(
    type_map: &mut HilTypeMap,
    id: Option<HilValueId>,
    ty: Option<HilType>,
) -> HilVerifyResult {
    match (id, ty) {
        (Some(id), Some(ty)) => define_value(type_map, id, ty),
        _ => Ok(()),
    }
}

fn define_value(type_map: &mut HilTypeMap, id: HilValueId, ty: HilType) -> HilVerifyResult {
    if type_map.insert(id, ty).is_some() {
        Err(HilVerifyError::DuplicateValue { value: id })
    } else {
        Ok(())
    }
}

fn expect_defined_many(type_map: &HilTypeMap, ids: &[HilValueId]) -> HilVerifyResult {
    for id in ids {
        let _ = type_of(type_map, *id)?;
    }
    Ok(())
}

fn expect_type(type_map: &HilTypeMap, id: HilValueId, expected: &HilType) -> HilVerifyResult {
    let found = type_of(type_map, id)?;
    if &found == expected {
        Ok(())
    } else {
        Err(HilVerifyError::TypeMismatch {
            value: id,
            expected: expected.clone(),
            found,
        })
    }
}

fn type_of(type_map: &HilTypeMap, id: HilValueId) -> Result<HilType, HilVerifyError> {
    type_map
        .get(&id)
        .cloned()
        .ok_or(HilVerifyError::UndefinedValue { value: id })
}

fn require_capability(capabilities: &[HilShape], required: HilShape) -> HilVerifyResult {
    if capabilities.contains(&required) {
        Ok(())
    } else {
        Err(HilVerifyError::ShapeRequired {
            capability: required,
        })
    }
}

fn verify_target(block_names: &HilBlockSet, target: &str) -> HilVerifyResult {
    if block_names.contains(target) {
        Ok(())
    } else {
        Err(HilVerifyError::MissingBlockTarget {
            target: target.into(),
        })
    }
}
