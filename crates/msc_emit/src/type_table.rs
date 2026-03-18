//! Type table: maps sema `TypeIdx` to a bytecode `type_id` (u32).
//!
//! Primitive types have fixed IDs matching §11.3 of the spec.
//! Compound types are interned by structure.

use std::collections::{HashMap, HashSet};

use msc_sema::types::{EffectRow, RecordField, SumVariant, Type};
use msc_sema::well_known::WellKnown;
use msc_sema::{DefId, TypeIdx, UnifyTable};
use msc_shared::{Arena, Symbol};

use crate::error::{EmitError, EmitResult};

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
const TAG_CSTRUCT: u8 = 0x15;

/// A serialized type pool entry.
struct TypeEntry {
    tag: u8,
    data: Vec<u8>,
}

/// Type pool builder.
///
/// Maps sema `TypeIdx` to bytecode `type_id` (u32).
pub struct TypeTable {
    entries: Vec<TypeEntry>,
    cache: HashMap<u32, u32>,
    /// Sema `TypeIdx`s (raw u32) that carry `#[repr("C")]` - encoded as
    /// `TAG_CSTRUCT` instead of `TAG_PRODUCT`.
    repr_c_types: HashSet<u32>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            cache: HashMap::new(),
            repr_c_types: HashSet::new(),
        }
    }

    /// Mark a sema type index as `repr(C)`. Must be called before lowering.
    pub fn mark_repr_c(&mut self, idx: TypeIdx) {
        _ = self.repr_c_types.insert(idx.raw());
    }

    /// Lower a sema type index to a bytecode `type_id`.
    pub fn lower_sema_type(
        &mut self,
        idx: TypeIdx,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let resolved = unify.resolve(idx, types);
        if let Some(&cached) = self.cache.get(&resolved.raw()) {
            return Ok(cached);
        }
        let ty = types[resolved].clone();
        let type_id = self.intern_sema_type(resolved, &ty, types, unify, wk)?;
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
        match def {
            _ if def == wk.unit => return Some(self.push_tag_only(TAG_UNIT)),
            _ if def == wk.bool => return Some(self.push_tag_only(TAG_BOOL)),
            _ if def == wk.ints.int8 => return Some(self.push_tag_only(TAG_I8)),
            _ if def == wk.ints.int16 => return Some(self.push_tag_only(TAG_I16)),
            _ if def == wk.ints.int32 => return Some(self.push_tag_only(TAG_I32)),
            _ if def == wk.ints.int || def == wk.ints.int64 => {
                return Some(self.push_tag_only(TAG_I64));
            }
            _ if def == wk.nats.nat || def == wk.nats.nat64 => {
                return Some(self.push_tag_only(TAG_U64));
            }
            _ if def == wk.nats.nat8 => return Some(self.push_tag_only(TAG_U8)),
            _ if def == wk.nats.nat16 => return Some(self.push_tag_only(TAG_U16)),
            _ if def == wk.nats.nat32 => return Some(self.push_tag_only(TAG_U32)),
            _ if def == wk.floats.float32 => return Some(self.push_tag_only(TAG_F32)),
            _ if def == wk.float || def == wk.floats.float64 => {
                return Some(self.push_tag_only(TAG_F64));
            }
            _ if def == wk.rune => return Some(self.push_tag_only(TAG_RUNE)),
            _ if def == wk.any || def == wk.never => return Some(self.push_tag_only(TAG_ANY)),
            _ if def == wk.string => return Some(self.push_tag_only(TAG_U64)),
            _ if def == wk.ffi.ptr => {
                let unit_id = self.push_tag_only(TAG_UNIT);
                return Some(self.push_entry(TAG_PTR, unit_id.to_be_bytes().to_vec()));
            }
            _ => {}
        }
        None
    }

    fn intern_sema_type(
        &mut self,
        resolved: TypeIdx,
        ty: &Type,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        match ty {
            Type::Named { def, args } => self.intern_named_type(*def, args, types, unify, wk),
            Type::Tuple { elems } => self.intern_tuple_type(elems, types, unify, wk),
            Type::Record { fields, .. } => {
                self.intern_record_type(resolved, fields, types, unify, wk)
            }
            Type::Fn {
                params,
                ret,
                effects,
            } => self.intern_fn_type(params, *ret, effects, types, unify, wk),
            Type::Array { elem, .. } => self.intern_array_type(*elem, types, unify, wk),
            Type::Ref { inner } => self.intern_ref_type(*inner, types, unify, wk),
            Type::Sum { variants } => self.encode_sum_from_sema(variants, types, unify, wk),
            Type::AnonSum { variants } => self.intern_anon_sum_type(variants, types, unify, wk),
            Type::Pi { param_ty, body, .. } => {
                self.intern_pi_type(*param_ty, *body, types, unify, wk)
            }
            Type::Quantified { body, .. } => self.lower_sema_type(*body, types, unify, wk),
            Type::Universe { .. } => Ok(self.push_tag_only(TAG_ANY)),
            Type::Var(_) | Type::Rigid(_) => Err(EmitError::UnresolvableType {
                desc: "unresolved type variable reached emit".into(),
            }),
            Type::Error => Err(EmitError::UnresolvableType {
                desc: "error type propagated to emit".into(),
            }),
        }
    }

    fn intern_named_type(
        &mut self,
        def: DefId,
        args: &[TypeIdx],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        // Ptr[T]: encode as TAG_PTR with inner = lowered T
        if def == wk.ffi.ptr && args.len() == 1 {
            let inner_id = self.lower_sema_type(args[0], types, unify, wk)?;
            return Ok(self.push_entry(TAG_PTR, inner_id.to_be_bytes().to_vec()));
        }
        if let Some(id) = self.lower_named_def(def, wk) {
            if args.is_empty() {
                return Ok(id);
            }
            let mut payload = id.to_be_bytes().to_vec();
            let arg_count = u32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "generic arg count overflow".into(),
            })?;
            payload.extend_from_slice(&arg_count.to_be_bytes());
            for &arg in args {
                let arg_id = self.lower_sema_type(arg, types, unify, wk)?;
                payload.extend_from_slice(&arg_id.to_be_bytes());
            }
            Ok(self.push_entry(TAG_ANY, payload))
        } else {
            Ok(self.push_tag_only(TAG_ANY))
        }
    }

    fn intern_tuple_type(
        &mut self,
        elems: &[TypeIdx],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let field_ids: Vec<u32> = elems
            .iter()
            .map(|&e| self.lower_sema_type(e, types, unify, wk))
            .collect::<Result<_, _>>()?;
        self.encode_product_from_ids(&field_ids)
    }

    fn intern_record_type(
        &mut self,
        resolved: TypeIdx,
        fields: &[RecordField],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let field_ids: Vec<u32> = fields
            .iter()
            .map(|f| self.lower_sema_type(f.ty, types, unify, wk))
            .collect::<Result<_, _>>()?;
        if self.repr_c_types.contains(&resolved.raw()) {
            self.encode_cstruct(&field_ids)
        } else {
            self.encode_product_from_ids(&field_ids)
        }
    }

    fn intern_fn_type(
        &mut self,
        params: &[TypeIdx],
        ret: TypeIdx,
        effects: &EffectRow,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let param_ids: Vec<u32> = params
            .iter()
            .map(|&p| self.lower_sema_type(p, types, unify, wk))
            .collect::<Result<_, _>>()?;
        let ret_id = self.lower_sema_type(ret, types, unify, wk)?;
        let effect_mask = lower_effect_row_mask(effects, wk);
        self.encode_fn_type_from_ids(&param_ids, ret_id, effect_mask)
    }

    fn intern_array_type(
        &mut self,
        elem: TypeIdx,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let elem_id = self.lower_sema_type(elem, types, unify, wk)?;
        Ok(self.push_entry(TAG_ARR, elem_id.to_be_bytes().to_vec()))
    }

    fn intern_ref_type(
        &mut self,
        inner: TypeIdx,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let inner_id = self.lower_sema_type(inner, types, unify, wk)?;
        Ok(self.push_entry(TAG_REF, inner_id.to_be_bytes().to_vec()))
    }

    fn intern_anon_sum_type(
        &mut self,
        variants: &[TypeIdx],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let sema_variants: Vec<SumVariant> = variants
            .iter()
            .enumerate()
            .map(|(i, &v)| -> EmitResult<SumVariant> {
                Ok(SumVariant {
                    name: Symbol(u32::try_from(i).map_err(|_| EmitError::UnresolvableType {
                        desc: "anonymous sum variant index overflow".into(),
                    })?),
                    fields: vec![v],
                })
            })
            .collect::<EmitResult<Vec<_>>>()?;
        self.encode_sum_from_sema(&sema_variants, types, unify, wk)
    }

    fn intern_pi_type(
        &mut self,
        param_ty: TypeIdx,
        body: TypeIdx,
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
        let param_id = self.lower_sema_type(param_ty, types, unify, wk)?;
        let body_id = self.lower_sema_type(body, types, unify, wk)?;
        self.encode_fn_type_from_ids(&[param_id], body_id, 0)
    }

    /// Encode a C-layout struct type. Computes field offsets, sizes, and
    /// alignment from the field type tags, then emits `TAG_CSTRUCT`.
    fn encode_cstruct(&mut self, field_ids: &[u32]) -> EmitResult<u32> {
        let field_count =
            u16::try_from(field_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "cstruct field count overflow".into(),
            })?;

        let field_layouts = self.compute_cstruct_layout(field_ids);
        let (total_size, struct_align) = Self::measure_cstruct(&field_layouts);

        // Encoding: field_count:u16 + per-field(type_id:u32, size:u16, offset:u16) + total_size:u32 + alignment:u16
        let mut data = Vec::with_capacity(2 + field_ids.len() * 8 + 6);
        data.extend_from_slice(&field_count.to_be_bytes());
        for &(type_id, size, off) in &field_layouts {
            data.extend_from_slice(&type_id.to_be_bytes());
            data.extend_from_slice(&size.to_be_bytes());
            data.extend_from_slice(&off.to_be_bytes());
        }
        data.extend_from_slice(&total_size.to_be_bytes());
        data.extend_from_slice(&struct_align.to_be_bytes());

        Ok(self.push_entry(TAG_CSTRUCT, data))
    }

    fn compute_cstruct_layout(&self, field_ids: &[u32]) -> Vec<(u32, u16, u16)> {
        let mut struct_align = 1;
        let mut offset: u16 = 0;
        let mut field_layouts = Vec::with_capacity(field_ids.len());

        for &fid in field_ids {
            let tag = self
                .entries
                .get(usize::try_from(fid).unwrap_or(usize::MAX))
                .map_or(0, |e| e.tag);
            let size = c_sizeof(tag, fid, &self.entries);
            let align = c_alignof(tag, fid, &self.entries);
            // pad to alignment
            let misalign = offset % align;
            if misalign != 0 {
                offset += align - misalign;
            }
            field_layouts.push((fid, size, offset));
            offset += size;
            if align > struct_align {
                struct_align = align;
            }
        }

        // final padding to struct alignment
        let tail_misalign = offset % struct_align;
        if tail_misalign != 0 {
            offset += struct_align - tail_misalign;
        }

        _ = (offset, struct_align);
        field_layouts
    }

    fn measure_cstruct(field_layouts: &[(u32, u16, u16)]) -> (u32, u16) {
        let struct_align = 1;
        let mut offset = 0;

        for &(_fid, size, _off) in field_layouts {
            offset += size;
        }

        // final padding to struct alignment
        let tail_misalign = offset % struct_align;
        if tail_misalign != 0 {
            offset += struct_align - tail_misalign;
        }

        let total_size = u32::from(offset);
        (total_size, struct_align)
    }

    fn encode_product_from_ids(&mut self, field_ids: &[u32]) -> EmitResult<u32> {
        let field_count =
            u32::try_from(field_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "product field count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * field_ids.len());
        data.extend_from_slice(&field_count.to_be_bytes());
        for id in field_ids {
            data.extend_from_slice(&id.to_be_bytes());
        }
        Ok(self.push_entry(TAG_PRODUCT, data))
    }

    fn encode_fn_type_from_ids(
        &mut self,
        param_ids: &[u32],
        ret_id: u32,
        effect_mask: u16,
    ) -> EmitResult<u32> {
        let param_count =
            u32::try_from(param_ids.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "fn param count overflow".into(),
            })?;
        let mut data = Vec::with_capacity(4 + 4 * param_ids.len() + 4 + 2);
        data.extend_from_slice(&param_count.to_be_bytes());
        for id in param_ids {
            data.extend_from_slice(&id.to_be_bytes());
        }
        data.extend_from_slice(&ret_id.to_be_bytes());
        data.extend_from_slice(&effect_mask.to_be_bytes());
        Ok(self.push_entry(TAG_FN, data))
    }

    fn encode_sum_from_sema(
        &mut self,
        variants: &[SumVariant],
        types: &Arena<Type>,
        unify: &UnifyTable,
        wk: &WellKnown,
    ) -> EmitResult<u32> {
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
        data.extend_from_slice(&variant_count.to_be_bytes());

        for (tag_value, variant) in variants.iter().enumerate() {
            let tag_u32 = u32::try_from(tag_value).map_err(|_| EmitError::UnresolvableType {
                desc: "variant tag overflow".into(),
            })?;
            data.extend_from_slice(&tag_u32.to_be_bytes());
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
            data.extend_from_slice(&payload_id.to_be_bytes());
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

    /// Serialize the type table into `buf`.
    pub fn write_into(&self, buf: &mut Vec<u8>) -> EmitResult {
        let count = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyTypes)?;
        buf.extend_from_slice(&count.to_be_bytes());
        for entry in &self.entries {
            buf.push(entry.tag);
            buf.extend_from_slice(&entry.data);
        }
        Ok(())
    }
}

/// C `sizeof` for a type tag. Recurses into CSTRUCT entries.
fn c_sizeof(tag: u8, type_id: u32, entries: &[TypeEntry]) -> u16 {
    match tag {
        TAG_BOOL | TAG_I8 | TAG_U8 => 1,
        TAG_I16 | TAG_U16 => 2,
        TAG_I32 | TAG_U32 | TAG_F32 | TAG_RUNE => 4,
        TAG_CSTRUCT => {
            // total_size is stored at the end of the entry data
            let entry = &entries[usize::try_from(type_id).unwrap_or(0)];
            let d = &entry.data;
            if d.len() >= 6 {
                let off = d.len() - 6;
                u16::try_from(u32::from_be_bytes([
                    d[off],
                    d[off + 1],
                    d[off + 2],
                    d[off + 3],
                ]))
                .unwrap_or(u16::MAX)
            } else {
                0
            }
        }
        _ => 8,
    }
}

/// C `alignof` for a type tag. Recurses into CSTRUCT entries.
fn c_alignof(tag: u8, type_id: u32, entries: &[TypeEntry]) -> u16 {
    match tag {
        TAG_BOOL | TAG_I8 | TAG_U8 => 1,
        TAG_I16 | TAG_U16 => 2,
        TAG_I32 | TAG_U32 | TAG_F32 | TAG_RUNE => 4,
        TAG_CSTRUCT => {
            let entry = &entries[usize::try_from(type_id).unwrap_or(0)];
            let d = &entry.data;
            if d.len() >= 2 {
                let off = d.len() - 2;
                u16::from_be_bytes([d[off], d[off + 1]])
            } else {
                1
            }
        }
        _ => 8,
    }
}

fn lower_effect_row_mask(row: &EffectRow, wk: &WellKnown) -> u16 {
    let mut mask: u16 = 0;
    for entry in &row.effects {
        match entry.def {
            def if def == wk.effects.io => mask |= 1,
            def if def == wk.effects.async_eff => mask |= 2,
            def if def == wk.effects.state => mask |= 4,
            def if def == wk.effects.throw => mask |= 8,
            _ => {}
        }
    }
    mask
}
