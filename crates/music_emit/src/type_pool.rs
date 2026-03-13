//! Type pool: maps sema `TypeIdx` to a bytecode `type_id` (u32).
//!
//! Primitive types have fixed IDs matching §11.3 of the spec.
//! Compound types are interned by structure.

use std::collections::HashMap;

use music_sema::types::{EffectRow, SumVariant, Type};
use music_sema::well_known::WellKnown;
use music_sema::{DefId, TypeIdx, UnifyTable};
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
/// Maps sema `TypeIdx` to bytecode `type_id` (u32).
pub struct TypePool {
    entries: Vec<TypeEntry>,
    cache: HashMap<u32, u32>,
}

impl TypePool {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            cache: HashMap::new(),
        }
    }

    /// Lower a sema type index to a bytecode `type_id`.
    pub fn lower_sema_type(
        &mut self,
        idx: TypeIdx,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> Result<u32, EmitError> {
        let resolved = unify.resolve(idx, types);
        if let Some(&cached) = self.cache.get(&resolved.raw()) {
            return Ok(cached);
        }
        let ty = types[resolved].clone();
        let type_id = self.intern_sema_type(&ty, types, unify, wk)?;
        let _ = self.cache.insert(resolved.raw(), type_id);
        Ok(type_id)
    }

    /// Lower a well-known `DefId` directly to a bytecode `type_id`.
    ///
    /// Returns `None` if `def` is not a recognized well-known type.
    pub fn lower_well_known_def(&mut self, def: DefId, wk: &WellKnown) -> Option<u32> {
        self.lower_named_def(def, wk)
    }

    fn lower_named_def(&mut self, def: DefId, wk: &WellKnown) -> Option<u32> {
        if def == wk.unit {
            return Some(self.push_tag_only(TAG_UNIT));
        }
        if def == wk.bool {
            return Some(self.push_tag_only(TAG_BOOL));
        }
        if def == wk.ints.int8 {
            return Some(self.push_tag_only(TAG_I8));
        }
        if def == wk.ints.int16 {
            return Some(self.push_tag_only(TAG_I16));
        }
        if def == wk.ints.int32 {
            return Some(self.push_tag_only(TAG_I32));
        }
        if def == wk.ints.int || def == wk.ints.int64 {
            return Some(self.push_tag_only(TAG_I64));
        }
        if def == wk.uints.uint8 {
            return Some(self.push_tag_only(TAG_U8));
        }
        if def == wk.uints.uint16 {
            return Some(self.push_tag_only(TAG_U16));
        }
        if def == wk.uints.uint32 {
            return Some(self.push_tag_only(TAG_U32));
        }
        if def == wk.uints.uint64 {
            return Some(self.push_tag_only(TAG_U64));
        }
        if def == wk.floats.float32 {
            return Some(self.push_tag_only(TAG_F32));
        }
        if def == wk.floats.float64 {
            return Some(self.push_tag_only(TAG_F64));
        }
        if def == wk.rune {
            return Some(self.push_tag_only(TAG_RUNE));
        }
        if def == wk.any || def == wk.never {
            return Some(self.push_tag_only(TAG_ANY));
        }
        if def == wk.string {
            return Some(self.push_tag_only(TAG_U64));
        }
        if def == wk.ffi.ptr {
            let unit_id = self.push_tag_only(TAG_UNIT);
            return Some(self.push_entry(TAG_PTR, unit_id.to_le_bytes().to_vec()));
        }
        None
    }

    fn intern_sema_type(
        &mut self,
        ty: &Type,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> Result<u32, EmitError> {
        match ty {
            Type::Named { def, args } => {
                if let Some(id) = self.lower_named_def(*def, wk) {
                    if args.is_empty() {
                        return Ok(id);
                    }
                    let mut payload = id.to_le_bytes().to_vec();
                    let arg_count =
                        u32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
                            desc: "generic arg count overflow".into(),
                        })?;
                    payload.extend_from_slice(&arg_count.to_le_bytes());
                    for &arg in args {
                        let arg_id = self.lower_sema_type(arg, types, unify, wk)?;
                        payload.extend_from_slice(&arg_id.to_le_bytes());
                    }
                    Ok(self.push_entry(TAG_ANY, payload))
                } else {
                    Ok(self.push_tag_only(TAG_ANY))
                }
            }
            Type::Tuple { elems } => {
                let field_ids: Vec<u32> = elems
                    .iter()
                    .map(|&e| self.lower_sema_type(e, types, unify, wk))
                    .collect::<Result<_, _>>()?;
                self.encode_product_from_ids(&field_ids)
            }
            Type::Record { fields, .. } => {
                let field_ids: Vec<u32> = fields
                    .iter()
                    .map(|f| self.lower_sema_type(f.ty, types, unify, wk))
                    .collect::<Result<_, _>>()?;
                self.encode_product_from_ids(&field_ids)
            }
            Type::Fn {
                params,
                ret,
                effects,
            } => {
                let param_ids: Vec<u32> = params
                    .iter()
                    .map(|&p| self.lower_sema_type(p, types, unify, wk))
                    .collect::<Result<_, _>>()?;
                let ret_id = self.lower_sema_type(*ret, types, unify, wk)?;
                let effect_mask = lower_effect_row_mask(effects, wk);
                self.encode_fn_type_from_ids(&param_ids, ret_id, effect_mask)
            }
            Type::Array { elem, .. } => {
                let elem_id = self.lower_sema_type(*elem, types, unify, wk)?;
                Ok(self.push_entry(TAG_ARR, elem_id.to_le_bytes().to_vec()))
            }
            Type::Ref { inner } => {
                let inner_id = self.lower_sema_type(*inner, types, unify, wk)?;
                Ok(self.push_entry(TAG_REF, inner_id.to_le_bytes().to_vec()))
            }
            Type::Sum { variants } => self.encode_sum_from_sema(variants, types, unify, wk),
            Type::AnonSum { variants } => {
                let sema_variants: Vec<SumVariant> = variants
                    .iter()
                    .enumerate()
                    .map(|(i, &v)| -> Result<SumVariant, EmitError> {
                        Ok(SumVariant {
                            name: music_shared::Symbol(u32::try_from(i).map_err(|_| {
                                EmitError::UnresolvableType {
                                    desc: "anonymous sum variant index overflow".into(),
                                }
                            })?),
                            fields: vec![v],
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.encode_sum_from_sema(&sema_variants, types, unify, wk)
            }
            Type::Quantified { body, .. } => self.lower_sema_type(*body, types, unify, wk),
            Type::Var(_) | Type::Rigid(_) => Err(EmitError::UnresolvableType {
                desc: "unresolved type variable reached emit".into(),
            }),
            Type::Error => Err(EmitError::UnresolvableType {
                desc: "error type propagated to emit".into(),
            }),
        }
    }

    fn encode_product_from_ids(&mut self, field_ids: &[u32]) -> Result<u32, EmitError> {
        let field_count =
            u32::try_from(field_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "product field count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * field_ids.len());
        data.extend_from_slice(&field_count.to_le_bytes());
        for id in field_ids {
            data.extend_from_slice(&id.to_le_bytes());
        }
        Ok(self.push_entry(TAG_PRODUCT, data))
    }

    fn encode_fn_type_from_ids(
        &mut self,
        param_ids: &[u32],
        ret_id: u32,
        effect_mask: u16,
    ) -> Result<u32, EmitError> {
        let param_count =
            u32::try_from(param_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "fn param count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * param_ids.len() + 4 + 2);
        data.extend_from_slice(&param_count.to_le_bytes());
        for id in param_ids {
            data.extend_from_slice(&id.to_le_bytes());
        }
        data.extend_from_slice(&ret_id.to_le_bytes());
        data.extend_from_slice(&effect_mask.to_le_bytes());
        Ok(self.push_entry(TAG_FN, data))
    }

    fn encode_sum_from_sema(
        &mut self,
        variants: &[SumVariant],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> Result<u32, EmitError> {
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
            let payload_id = match variant.fields.as_slice() {
                [] => 0xFFFF_FFFFu32,
                [single] => self.lower_sema_type(*single, types, unify, wk)?,
                _ => {
                    let field_ids: Vec<u32> = variant
                        .fields
                        .iter()
                        .map(|&f| self.lower_sema_type(f, types, unify, wk))
                        .collect::<Result<_, _>>()?;
                    self.encode_product_from_ids(&field_ids)?
                }
            };
            data.extend_from_slice(&payload_id.to_le_bytes());
        }

        let idx = usize::try_from(slot).map_err(|_| EmitError::TooManyTypes)?;
        self.entries[idx].data = data;
        Ok(slot)
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

fn lower_effect_row_mask(row: &EffectRow, wk: &WellKnown) -> u16 {
    let mut mask: u16 = 0;
    for entry in &row.effects {
        if entry.def == wk.effects.io {
            mask |= 1;
        } else if entry.def == wk.effects.async_eff {
            mask |= 2;
        } else if entry.def == wk.effects.state {
            mask |= 4;
        } else if entry.def == wk.effects.throw {
            mask |= 8;
        }
    }
    mask
}
