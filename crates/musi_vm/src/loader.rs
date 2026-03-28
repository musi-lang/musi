#![allow(clippy::arithmetic_side_effects)]

use std::collections::HashMap;

use music_il::format::{
    self, ClassDescriptor, ClassInstance, ClassMethod, EffectDescriptor, EffectOpDescriptor,
    FfiType, ForeignAbi, ForeignDescriptor, HEADER_SIZE, TypeDescriptor, TypeKind,
};
use music_il::opcode::Opcode;

use crate::errors::LoadError;
use crate::module::{ConstantEntry, GlobalDef, Method, Module};
use crate::value::Value;

/// Decode a `.seam` binary slice into a [`Module`].
///
/// # Errors
/// Returns a [`LoadError`] if the binary is malformed, has an unsupported
/// version, contains an invalid opcode, or is truncated.
pub fn load(data: &[u8]) -> Result<Module, LoadError> {
    if data.len() < HEADER_SIZE {
        return Err(LoadError::TruncatedHeader);
    }

    let magic = read_bytes::<4>(data, 0).ok_or(LoadError::TruncatedHeader)?;
    if magic != format::MAGIC {
        return Err(LoadError::InvalidMagic);
    }

    let major = *data.get(4).ok_or(LoadError::TruncatedHeader)?;
    let minor = *data.get(5).ok_or(LoadError::TruncatedHeader)?;
    if major != format::VERSION_MAJOR || minor != format::VERSION_MINOR {
        return Err(LoadError::UnsupportedVersion { major, minor });
    }

    let section_count = read_u32_le(data, 8).ok_or(LoadError::TruncatedHeader)?;

    let mut strings: Vec<String> = Vec::new();
    let mut offset_map: HashMap<u16, u16> = HashMap::new();
    let mut constants: Vec<ConstantEntry> = Vec::new();
    let mut methods: Vec<Method> = Vec::new();
    let mut globals: Vec<GlobalDef> = Vec::new();
    let mut types: Vec<TypeDescriptor> = Vec::new();
    let mut effects: Vec<EffectDescriptor> = Vec::new();
    let mut classes: Vec<ClassDescriptor> = Vec::new();
    let mut foreigns: Vec<ForeignDescriptor> = Vec::new();

    let mut pos = HEADER_SIZE;
    for _ in 0..section_count {
        let tag = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(4);

        let length = usize::try_from(read_u32_le(data, pos).ok_or(LoadError::TruncatedSection)?)
            .map_err(|_| LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(4);

        let section_data = data
            .get(pos..pos.wrapping_add(length))
            .ok_or(LoadError::TruncatedSection)?;

        match tag {
            format::section::STRT => {
                let (s, m) = decode_strt(section_data);
                strings = s;
                offset_map = m;
            }
            format::section::TYPE => {
                types = decode_type(section_data)?;
            }
            format::section::CNST => {
                constants = decode_cnst(section_data, &offset_map)?;
            }
            format::section::METH => {
                methods = decode_meth(section_data)?;
            }
            format::section::GLOB => {
                globals = decode_glob(section_data)?;
            }
            format::section::CLSS => {
                classes = decode_clss(section_data)?;
            }
            format::section::EFCT => {
                effects = decode_efct(section_data)?;
            }
            format::section::FRGN => {
                foreigns = decode_frgn(section_data)?;
            }
            _ => {
                // Unknown section: skip for forward-compatibility
            }
        }

        pos = pos.wrapping_add(length);
    }

    Ok(Module {
        constants,
        strings,
        methods,
        globals,
        types,
        effects,
        classes,
        foreigns,
    })
}

/// Decode the STRT section into strings and a byte-offset→index map.
///
/// STRT is a sequence of null-terminated UTF-8 strings. The map allows
/// `decode_cnst` to resolve the cumulative byte offsets written by the
/// emitter back to sequential string indices.
fn decode_strt(data: &[u8]) -> (Vec<String>, HashMap<u16, u16>) {
    let mut strings = Vec::new();
    let mut offset_map = HashMap::new();
    let mut pos = 0usize;
    while pos < data.len() {
        let idx = u16::try_from(strings.len()).unwrap_or(u16::MAX);
        let offset = u16::try_from(pos).unwrap_or(u16::MAX);
        let _ = offset_map.insert(offset, idx);
        let end = data[pos..]
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(data.len() - pos);
        strings.push(String::from_utf8_lossy(&data[pos..pos + end]).into_owned());
        pos += end + 1;
    }
    (strings, offset_map)
}

fn decode_type(data: &[u8]) -> Result<Vec<TypeDescriptor>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let id_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let id = u16::from_le_bytes(id_bytes);
        pos = pos.wrapping_add(2);

        let kind_byte = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        let kind = match kind_byte {
            0 => TypeKind::Builtin,
            1 => TypeKind::Record,
            2 => TypeKind::Choice,
            other => return Err(LoadError::InvalidConstantTag { tag: other }),
        };
        pos = pos.wrapping_add(1);

        let mc_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let member_count = u16::from_le_bytes(mc_bytes);
        pos = pos.wrapping_add(2);

        out.push(TypeDescriptor {
            id,
            kind,
            member_count,
        });
    }

    Ok(out)
}

fn decode_cnst(
    data: &[u8],
    offset_map: &HashMap<u16, u16>,
) -> Result<Vec<ConstantEntry>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let tag = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        match tag {
            0x01 => {
                // Int: 8-byte LE i64
                let bytes = read_bytes::<8>(data, pos).ok_or(LoadError::TruncatedSection)?;
                out.push(ConstantEntry::Value(Value::from_int(i64::from_le_bytes(
                    bytes,
                ))));
                pos = pos.wrapping_add(8);
            }
            0x02 => {
                // Float: 8-byte LE u64 bits
                let bytes = read_bytes::<8>(data, pos).ok_or(LoadError::TruncatedSection)?;
                out.push(ConstantEntry::Value(Value::from_float(f64::from_bits(
                    u64::from_le_bytes(bytes),
                ))));
                pos = pos.wrapping_add(8);
            }
            0x03 => {
                // Str: 2-byte LE byte-offset into STRT → resolve to string index
                let bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
                let byte_offset = u16::from_le_bytes(bytes);
                let str_idx = offset_map.get(&byte_offset).copied().ok_or(
                    LoadError::InvalidStringOffset {
                        offset: byte_offset,
                    },
                )?;
                out.push(ConstantEntry::StringRef(str_idx));
                pos = pos.wrapping_add(2);
            }
            0x04 => {
                // Tag: 2-byte LE variant tag id
                let bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
                out.push(ConstantEntry::Value(Value::from_tag(u16::from_le_bytes(
                    bytes,
                ))));
                pos = pos.wrapping_add(2);
            }
            other => return Err(LoadError::InvalidConstantTag { tag: other }),
        }
    }

    Ok(out)
}

fn decode_meth(data: &[u8]) -> Result<Vec<Method>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let name_bytes = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let name = u32::from_le_bytes(name_bytes);
        pos = pos.wrapping_add(4);

        let locals_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let locals_count = u16::from_le_bytes(locals_bytes);
        pos = pos.wrapping_add(2);

        let instr_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let instr_count = u16::from_le_bytes(instr_bytes);
        pos = pos.wrapping_add(2);

        let code = scan_method_bytes(data, &mut pos, instr_count)?;
        out.push(Method {
            name,
            locals_count,
            code,
        });
    }

    Ok(out)
}

fn decode_glob(data: &[u8]) -> Result<Vec<GlobalDef>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let name_bytes = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let name = u32::from_le_bytes(name_bytes);
        pos = pos.wrapping_add(4);

        let flags = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        out.push(GlobalDef {
            name,
            exported: (flags & 0x01) != 0,
            opaque: (flags & 0x02) != 0,
        });
    }

    Ok(out)
}

/// Scan `instr_count` encoded instructions starting at `pos`, advancing `pos`
/// and returning the raw bytes. The wire format stores instruction count, not
/// byte count, so we must decode each opcode to know how many bytes to skip.
fn scan_method_bytes(data: &[u8], pos: &mut usize, instr_count: u16) -> Result<Vec<u8>, LoadError> {
    let start = *pos;
    for _ in 0..instr_count {
        let byte_offset = *pos;
        let opcode_byte = *data.get(*pos).ok_or(LoadError::TruncatedSection)?;
        *pos = pos.wrapping_add(1);

        let op = Opcode::from_byte(opcode_byte).ok_or(LoadError::InvalidOpcode {
            byte: opcode_byte,
            offset: byte_offset,
        })?;

        let extra = operand_extra_bytes(op, data, *pos)?;
        if data.get(*pos..*pos + extra).is_none() {
            return Err(LoadError::TruncatedSection);
        }
        *pos = pos.wrapping_add(extra);
    }
    data.get(start..*pos)
        .map(<[u8]>::to_vec)
        .ok_or(LoadError::TruncatedSection)
}

/// Returns the number of operand bytes following the opcode byte.
fn operand_extra_bytes(op: Opcode, data: &[u8], pos: usize) -> Result<usize, LoadError> {
    match op {
        // U8 operand (1 byte)
        Opcode::LdLoc
        | Opcode::StLoc
        | Opcode::Call
        | Opcode::CallTail
        | Opcode::Res
        | Opcode::TyclCall
        | Opcode::ArrGetI
        | Opcode::ArrSetI => Ok(1),

        // U16 or I16 operand (2 bytes)
        Opcode::LdConst
        | Opcode::LdGlob
        | Opcode::LdUpv
        | Opcode::StGlob
        | Opcode::StUpv
        | Opcode::LdLocW
        | Opcode::StLocW
        | Opcode::LdSmi
        | Opcode::BrTrue
        | Opcode::BrFalse
        | Opcode::BrJmp
        | Opcode::BrBack
        | Opcode::ArrNew
        | Opcode::TyclDict
        | Opcode::FfiCall
        | Opcode::TyChk
        | Opcode::TyCast => Ok(2),

        Opcode::Perf => Ok(4),

        // Wide (u16 + u8) and Tagged (u8 + u16) operands are both 3 bytes
        Opcode::ClsNew | Opcode::ArrNewT => Ok(3),

        // EffectJump (u16 + u16 + i16) = 6 bytes
        Opcode::HndlPush => Ok(6),

        // Variable: u16 count + count * i16
        Opcode::BrTbl => {
            let count_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
            let count = usize::from(u16::from_le_bytes(count_bytes));
            let offsets = count.checked_mul(2).ok_or(LoadError::TruncatedSection)?;
            offsets.checked_add(2).ok_or(LoadError::TruncatedSection)
        }

        // No operand
        _ => Ok(0),
    }
}

fn decode_efct(data: &[u8]) -> Result<Vec<EffectDescriptor>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let id = u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(2);
        let module_name = read_inline_string(data, &mut pos)?;
        let name = read_inline_string(data, &mut pos)?;
        let op_count = usize::from(u16::from_le_bytes(
            read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?,
        ));
        pos = pos.wrapping_add(2);
        let mut operations = Vec::with_capacity(op_count);
        for _ in 0..op_count {
            let op_id =
                u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
            pos = pos.wrapping_add(2);
            let op_name = read_inline_string(data, &mut pos)?;
            operations.push(EffectOpDescriptor {
                id: op_id,
                name: op_name,
            });
        }
        out.push(EffectDescriptor {
            id,
            module_name,
            name,
            operations,
        });
    }

    Ok(out)
}

fn read_inline_string(data: &[u8], pos: &mut usize) -> Result<String, LoadError> {
    let len = usize::from(u16::from_le_bytes(
        read_bytes::<2>(data, *pos).ok_or(LoadError::TruncatedSection)?,
    ));
    *pos = pos.wrapping_add(2);
    let bytes = data
        .get(*pos..pos.wrapping_add(len))
        .ok_or(LoadError::TruncatedSection)?;
    *pos = pos.wrapping_add(len);
    Ok(String::from_utf8_lossy(bytes).into_owned())
}

fn decode_clss(data: &[u8]) -> Result<Vec<ClassDescriptor>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let id = u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(2);

        let name_idx =
            u32::from_le_bytes(read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(4);

        let method_count =
            u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(2);

        let mut method_names = Vec::with_capacity(usize::from(method_count));
        for _ in 0..method_count {
            let mn =
                u32::from_le_bytes(read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?);
            pos = pos.wrapping_add(4);
            method_names.push(mn);
        }

        let inst_count =
            u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(2);

        let mut instances = Vec::with_capacity(usize::from(inst_count));
        for _ in 0..inst_count {
            let type_id =
                u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
            pos = pos.wrapping_add(2);

            let m_count =
                u16::from_le_bytes(read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?);
            pos = pos.wrapping_add(2);

            let mut methods = Vec::with_capacity(usize::from(m_count));
            for _ in 0..m_count {
                let mn_idx = u32::from_le_bytes(
                    read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?,
                );
                pos = pos.wrapping_add(4);
                let mi = u16::from_le_bytes(
                    read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?,
                );
                pos = pos.wrapping_add(2);
                methods.push(ClassMethod {
                    name_idx: mn_idx,
                    method_idx: mi,
                });
            }
            instances.push(ClassInstance { type_id, methods });
        }

        out.push(ClassDescriptor {
            id,
            name_idx,
            method_count,
            method_names,
            instances,
        });
    }

    Ok(out)
}

fn decode_frgn(data: &[u8]) -> Result<Vec<ForeignDescriptor>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let name_idx =
            u32::from_le_bytes(read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(4);

        let symbol_idx =
            u32::from_le_bytes(read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(4);

        let lib_idx =
            u32::from_le_bytes(read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?);
        pos = pos.wrapping_add(4);

        let abi_byte = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        let arity = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        let flags = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        let return_type_byte = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        let mut param_types = Vec::with_capacity(usize::from(arity));
        for _ in 0..arity {
            let pt = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
            pos = pos.wrapping_add(1);
            param_types.push(FfiType::from_byte(pt));
        }

        out.push(ForeignDescriptor {
            name_idx,
            symbol_idx,
            lib_idx,
            abi: ForeignAbi::from_byte(abi_byte),
            arity,
            exported: (flags & 0x01) != 0,
            param_types,
            return_type: FfiType::from_byte(return_type_byte),
        });
    }

    Ok(out)
}

/// Read exactly `N` bytes from `data` at `pos`, returning them as a fixed-size array.
fn read_bytes<const N: usize>(data: &[u8], pos: usize) -> Option<[u8; N]> {
    data.get(pos..pos.wrapping_add(N))?.try_into().ok()
}

fn read_u32_le(data: &[u8], pos: usize) -> Option<u32> {
    Some(u32::from_le_bytes(read_bytes::<4>(data, pos)?))
}

#[cfg(test)]
mod tests;
