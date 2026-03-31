use music_il::{ConstantEntry, MethodName, SeamArtifact};

use super::*;

pub(super) struct StringIndex {
    pub(super) bytes: Vec<u8>,
    offsets_by_text: TextOffsets,
    pub(super) constant_offsets: ConstantOffsets,
}

impl StringIndex {
    pub(super) fn build(artifact: &SeamArtifact) -> CodecResult<Self> {
        let mut table = Self {
            bytes: Vec::new(),
            offsets_by_text: TextOffsets::new(),
            constant_offsets: ConstantOffsets::new(),
        };

        for (index, entry) in artifact.constants.entries().iter().enumerate() {
            if let ConstantEntry::Str(text) = entry {
                let offset = table.intern(text)?;
                let index = u16::try_from(index).map_err(|_| CodecError::ModuleTooLarge)?;
                let offset = u16::try_from(offset).map_err(|_| CodecError::ModuleTooLarge)?;
                let _ = table.constant_offsets.insert(index, offset);
            }
        }

        for descriptor in &artifact.types {
            let _ = table.intern(&descriptor.key)?;
        }

        for method in &artifact.methods {
            if let MethodName::Named(name) = &method.name {
                let _ = table.intern(name)?;
            }
        }

        for global in &artifact.globals {
            let _ = table.intern(&global.name)?;
        }

        for effect in &artifact.effects {
            let _ = table.intern(&effect.module_name)?;
            let _ = table.intern(&effect.name)?;
            for operation in &effect.operations {
                let _ = table.intern(&operation.name)?;
            }
        }

        for class in &artifact.classes {
            let _ = table.intern(&class.name)?;
            for method_name in &class.method_names {
                let _ = table.intern(method_name)?;
            }
            for instance in &class.instances {
                for method in &instance.methods {
                    let _ = table.intern(&method.name)?;
                }
            }
        }

        for foreign in &artifact.foreigns {
            let _ = table.intern(&foreign.name)?;
            if let Some(symbol) = &foreign.symbol {
                let _ = table.intern(symbol)?;
            }
            if let Some(library) = &foreign.library {
                let _ = table.intern(library)?;
            }
        }

        Ok(table)
    }

    pub(super) fn intern(&mut self, text: &str) -> CodecResult<u32> {
        if let Some(&offset) = self.offsets_by_text.get(text) {
            return Ok(offset);
        }

        let offset = u32::try_from(self.bytes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        self.bytes.extend_from_slice(text.as_bytes());
        self.bytes.push(0);
        let _ = self.offsets_by_text.insert(text.to_owned(), offset);
        Ok(offset)
    }

    pub(super) fn offset(&self, text: &str) -> CodecResult<u32> {
        self.offsets_by_text
            .get(text)
            .copied()
            .ok_or(CodecError::InvalidStringOffset { offset: u32::MAX })
    }
}

pub(super) fn decode_string_pool(data: &[u8]) -> (DecodedStrings, StringOffsets) {
    let mut strings = DecodedStrings::new();
    let mut offsets = StringOffsets::new();
    let mut position = 0_usize;

    while position < data.len() {
        let offset = u32::try_from(position).unwrap_or(u32::MAX);
        let index = u32::try_from(strings.len()).unwrap_or(u32::MAX);
        let _ = offsets.insert(offset, index);
        let length = data[position..]
            .iter()
            .position(|&byte| byte == 0)
            .unwrap_or(data.len() - position);
        strings.push(String::from_utf8_lossy(&data[position..position + length]).into_owned());
        position += length + 1;
    }

    (strings, offsets)
}

pub(super) fn read_named_string(
    offset: u32,
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    resolve_string(offset, strings, offsets)
        .map_err(|_| CodecError::InvalidMethodName { reference: offset })
}

pub(super) fn read_string_ref(
    data: &[u8],
    position: &mut usize,
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    let offset = read_u32(data, *position).ok_or(CodecError::TruncatedSection)?;
    *position += 4;
    resolve_string(offset, strings, offsets)
}

pub(super) fn read_optional_string_ref(
    data: &[u8],
    position: &mut usize,
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<Option<String>> {
    let offset = read_u32(data, *position).ok_or(CodecError::TruncatedSection)?;
    *position += 4;
    if offset == u32::MAX {
        return Ok(None);
    }
    Ok(Some(resolve_string(offset, strings, offsets)?))
}

pub(super) fn resolve_string(
    offset: u32,
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    let string_index = offsets
        .get(&offset)
        .copied()
        .ok_or(CodecError::InvalidStringOffset { offset })?;
    strings
        .get(usize::try_from(string_index).unwrap_or(usize::MAX))
        .cloned()
        .ok_or(CodecError::InvalidStringOffset { offset })
}
