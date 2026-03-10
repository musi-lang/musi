//! Type pool: maps `IrTypeIdx` to a bytecode `type_id` (u32).
//!
//! Primitive types have fixed IDs matching §11.3 of the spec.
//! Compound types are interned by structure.

use std::collections::HashMap;

use music_ir::{IrSumVariant, IrType, IrTypeIdx};
use music_shared::Arena;

use crate::error::EmitError;

// §11.3 type tags
const TAG_UNIT: u8 = 0x01;
const TAG_BOOL: u8 = 0x02;
const TAG_I8: u8 = 0x03;
const TAG_I16: u8 = 0x04;
const TAG_I32: u8 = 0x05;
const TAG_I64: u8 = 0x06;
const TAG_U8: u8 = 0x07;
const TAG_U16: u8 = 0x08;
const TAG_U32: u8 = 0x09;
const TAG_U64: u8 = 0x0A;
const TAG_F32: u8 = 0x0B;
const TAG_F64: u8 = 0x0C;
const TAG_RUNE: u8 = 0x0D;
const TAG_PTR: u8 = 0x0E;
const TAG_ARR: u8 = 0x0F;
const TAG_PRODUCT: u8 = 0x10;
const TAG_SUM: u8 = 0x11;
const TAG_FN: u8 = 0x12;
const TAG_REF: u8 = 0x13;
const TAG_ANY: u8 = 0x14;

/// A serialized type pool entry.
struct TypeEntry {
    tag: u8,
    data: Vec<u8>,
}

/// Type pool builder.
///
/// Maps arena `IrTypeIdx` to bytecode `type_id` (u32).
/// Primitive types are resolved directly from their variant; compound types are
/// interned to avoid duplication.
pub struct TypePool {
    entries: Vec<TypeEntry>,
    /// Cache: arena index → `type_id`
    cache: HashMap<u32, u32>,
}

impl TypePool {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            cache: HashMap::new(),
        }
    }

    /// Lower an IR type index to a bytecode `type_id`.
    pub fn lower_ir_type(
        &mut self,
        idx: IrTypeIdx,
        arena: &Arena<IrType>,
    ) -> Result<u32, EmitError> {
        if let Some(&cached) = self.cache.get(&idx.raw()) {
            return Ok(cached);
        }
        let ty = arena[idx].clone();
        let type_id = self.intern_type(&ty, arena)?;
        let _ = self.cache.insert(idx.raw(), type_id);
        Ok(type_id)
    }

    fn intern_type(&mut self, ty: &IrType, arena: &Arena<IrType>) -> Result<u32, EmitError> {
        match ty {
            IrType::Unit => Ok(self.push_tag_only(TAG_UNIT)),
            IrType::Bool => Ok(self.push_tag_only(TAG_BOOL)),
            IrType::Int8 => Ok(self.push_tag_only(TAG_I8)),
            IrType::Int16 => Ok(self.push_tag_only(TAG_I16)),
            IrType::Int32 => Ok(self.push_tag_only(TAG_I32)),
            IrType::Int64 => Ok(self.push_tag_only(TAG_I64)),
            IrType::UInt8 => Ok(self.push_tag_only(TAG_U8)),
            IrType::UInt16 => Ok(self.push_tag_only(TAG_U16)),
            IrType::UInt32 => Ok(self.push_tag_only(TAG_U32)),
            IrType::UInt64 => Ok(self.push_tag_only(TAG_U64)),
            IrType::Float32 => Ok(self.push_tag_only(TAG_F32)),
            IrType::Float64 => Ok(self.push_tag_only(TAG_F64)),
            IrType::Rune => Ok(self.push_tag_only(TAG_RUNE)),
            IrType::Any => Ok(self.push_tag_only(TAG_ANY)),
            IrType::Ptr { inner } => {
                let inner_id = self.lower_ir_type(*inner, arena)?;
                Ok(self.push_entry(TAG_PTR, inner_id.to_le_bytes().to_vec()))
            }
            IrType::Ref { inner } => {
                let inner_id = self.lower_ir_type(*inner, arena)?;
                Ok(self.push_entry(TAG_REF, inner_id.to_le_bytes().to_vec()))
            }
            IrType::Array { elem } => {
                let elem_id = self.lower_ir_type(*elem, arena)?;
                Ok(self.push_entry(TAG_ARR, elem_id.to_le_bytes().to_vec()))
            }
            IrType::Product { fields } => self.encode_product(fields, arena),
            IrType::Sum { variants } => self.encode_sum(variants, arena),
            IrType::Fn {
                params,
                ret,
                effect_mask,
            } => self.encode_fn_type(params, *ret, effect_mask.0, arena),
            // Closures are lowered as product of (fn_ty, env_ty)
            IrType::Closure { fn_ty, env_ty } => {
                let fn_id = self.lower_ir_type(*fn_ty, arena)?;
                let env_id = self.lower_ir_type(*env_ty, arena)?;
                let mut data = Vec::with_capacity(12);
                data.extend_from_slice(&2u32.to_le_bytes());
                data.extend_from_slice(&fn_id.to_le_bytes());
                data.extend_from_slice(&env_id.to_le_bytes());
                Ok(self.push_entry(TAG_PRODUCT, data))
            }
            // Opaque foreign types are pointers at runtime — emit as *Unit
            IrType::Opaque { .. } => {
                let unit_id = self.push_tag_only(TAG_UNIT);
                Ok(self.push_entry(TAG_PTR, unit_id.to_le_bytes().to_vec()))
            }
            IrType::TypeParam { index } => Err(EmitError::UnresolvableType {
                desc: format!("opaque type param {index} in monomorphized IR").into_boxed_str(),
            }),
            IrType::WitnessTable { .. } => {
                // Witness tables are pointers at runtime — emit as *Unit
                let unit_id = self.push_tag_only(TAG_UNIT);
                Ok(self.push_entry(TAG_PTR, unit_id.to_le_bytes().to_vec()))
            }
        }
    }

    fn encode_product(
        &mut self,
        fields: &[IrTypeIdx],
        arena: &Arena<IrType>,
    ) -> Result<u32, EmitError> {
        let field_ids: Vec<u32> = fields
            .iter()
            .map(|f| self.lower_ir_type(*f, arena))
            .collect::<Result<_, _>>()?;
        let field_count =
            u32::try_from(field_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "product field count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * field_ids.len());
        data.extend_from_slice(&field_count.to_le_bytes());
        for id in &field_ids {
            data.extend_from_slice(&id.to_le_bytes());
        }
        Ok(self.push_entry(TAG_PRODUCT, data))
    }

    fn encode_fn_type(
        &mut self,
        params: &[IrTypeIdx],
        ret: IrTypeIdx,
        effect_mask: u16,
        arena: &Arena<IrType>,
    ) -> Result<u32, EmitError> {
        let param_ids: Vec<u32> = params
            .iter()
            .map(|p| self.lower_ir_type(*p, arena))
            .collect::<Result<_, _>>()?;
        let ret_id = self.lower_ir_type(ret, arena)?;
        let param_count =
            u32::try_from(param_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "fn param count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * param_ids.len() + 4 + 2);
        data.extend_from_slice(&param_count.to_le_bytes());
        for id in &param_ids {
            data.extend_from_slice(&id.to_le_bytes());
        }
        data.extend_from_slice(&ret_id.to_le_bytes());
        data.extend_from_slice(&effect_mask.to_le_bytes());
        Ok(self.push_entry(TAG_FN, data))
    }

    fn encode_sum(
        &mut self,
        variants: &[IrSumVariant],
        arena: &Arena<IrType>,
    ) -> Result<u32, EmitError> {
        // Allocate a slot first to handle recursive types.
        let slot = u32::try_from(self.entries.len()).map_err(|_| EmitError::TooManyTypes)?;
        self.entries.push(TypeEntry {
            tag: TAG_SUM,
            data: vec![],
        });

        let variant_count =
            u32::try_from(variants.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "sum variant count overflow".into(),
            })?;

        let mut data = vec![];
        data.extend_from_slice(&variant_count.to_le_bytes());

        for (tag_value, variant) in variants.iter().enumerate() {
            let tag_u32 = u32::try_from(tag_value).map_err(|_| EmitError::UnresolvableType {
                desc: "variant tag overflow".into(),
            })?;
            data.extend_from_slice(&tag_u32.to_le_bytes());
            let payload_id = self.encode_variant_payload(variant, arena)?;
            data.extend_from_slice(&payload_id.to_le_bytes());
        }

        let idx = usize::try_from(slot).map_err(|_| EmitError::TooManyTypes)?;
        self.entries[idx].data = data;
        Ok(slot)
    }

    fn encode_variant_payload(
        &mut self,
        variant: &IrSumVariant,
        arena: &Arena<IrType>,
    ) -> Result<u32, EmitError> {
        match variant.fields.as_slice() {
            [] => Ok(0xFFFF_FFFFu32),
            [single] => self.lower_ir_type(*single, arena),
            _ => self.encode_product(&variant.fields, arena),
        }
    }

    fn push_tag_only(&mut self, tag: u8) -> u32 {
        self.push_entry(tag, vec![])
    }

    fn push_entry(&mut self, tag: u8, data: Vec<u8>) -> u32 {
        let id = u32::try_from(self.entries.len()).expect("type pool fits in u32");
        self.entries.push(TypeEntry { tag, data });
        id
    }

    /// Serialize the type pool into `buf`.
    pub fn write_into(&self, buf: &mut Vec<u8>) -> Result<(), EmitError> {
        let count = u32::try_from(self.entries.len()).map_err(|_| EmitError::TooManyTypes)?;
        buf.extend_from_slice(&count.to_le_bytes());
        for entry in &self.entries {
            buf.push(entry.tag);
            buf.extend_from_slice(&entry.data);
        }
        Ok(())
    }
}
