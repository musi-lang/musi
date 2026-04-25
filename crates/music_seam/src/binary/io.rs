use super::*;

pub(super) fn push_section_tag(out: &mut Vec<u8>, tag: SectionTag) {
    out.push(section_tag_byte(tag));
}

pub(super) fn require_section(cursor: &mut Cursor<'_>, tag: SectionTag) -> AssemblyResult {
    let found = cursor.read_u8()?;
    if found == section_tag_byte(tag) {
        Ok(())
    } else {
        Err(AssemblyError::UnknownSectionTag(found))
    }
}

pub(super) const fn section_tag_byte(tag: SectionTag) -> u8 {
    match tag {
        SectionTag::Strings => 1,
        SectionTag::Types => 2,
        SectionTag::Constants => 3,
        SectionTag::Globals => 4,
        SectionTag::Procedures => 5,
        SectionTag::Effects => 6,
        SectionTag::Shapes => 7,
        SectionTag::Foreigns => 8,
        SectionTag::Exports => 9,
        SectionTag::Data => 10,
        SectionTag::Meta => 11,
    }
}

pub(super) fn push_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_i16(out: &mut Vec<u8>, value: i16) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_i64(out: &mut Vec<u8>, value: i64) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_u64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn push_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    push_u32(out, u32::try_from(bytes.len()).expect("payload overflow"));
    out.extend_from_slice(bytes);
}

pub(super) struct Cursor<'bytes> {
    bytes: &'bytes [u8],
    offset: usize,
}

impl<'bytes> Cursor<'bytes> {
    pub(super) const fn new(bytes: &'bytes [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    pub(super) const fn is_eof(&self) -> bool {
        self.offset >= self.bytes.len()
    }

    pub(super) fn peek_u8(&self) -> Option<u8> {
        self.bytes.get(self.offset).copied()
    }

    pub(super) fn read_exact(&mut self, len: usize) -> AssemblyResult<[u8; 4]> {
        if len != 4 {
            return Err(AssemblyError::BinaryPayloadTruncated);
        }
        let end = self.offset.saturating_add(4);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        let mut out = [0_u8; 4];
        out.copy_from_slice(slice);
        Ok(out)
    }

    pub(super) fn read_u8(&mut self) -> AssemblyResult<u8> {
        let byte = *self
            .bytes
            .get(self.offset)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = self.offset.saturating_add(1);
        Ok(byte)
    }

    pub(super) fn read_u16(&mut self) -> AssemblyResult<u16> {
        let bytes = self.read_array::<2>()?;
        Ok(u16::from_le_bytes(bytes))
    }

    pub(super) fn read_i16(&mut self) -> AssemblyResult<i16> {
        let bytes = self.read_array::<2>()?;
        Ok(i16::from_le_bytes(bytes))
    }

    pub(super) fn read_u32(&mut self) -> AssemblyResult<u32> {
        let bytes = self.read_array::<4>()?;
        Ok(u32::from_le_bytes(bytes))
    }

    pub(super) fn read_i64(&mut self) -> AssemblyResult<i64> {
        let bytes = self.read_array::<8>()?;
        Ok(i64::from_le_bytes(bytes))
    }

    pub(super) fn read_u64(&mut self) -> AssemblyResult<u64> {
        let bytes = self.read_array::<8>()?;
        Ok(u64::from_le_bytes(bytes))
    }

    pub(super) fn read_idx<T>(&mut self) -> AssemblyResult<Idx<T>> {
        Ok(Idx::from_raw(self.read_u32()?))
    }

    pub(super) fn read_bytes(&mut self) -> AssemblyResult<Vec<u8>> {
        let len = read_len(self, "byte payload length")?;
        let end = self.offset.saturating_add(len);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        Ok(slice.to_vec())
    }

    fn read_array<const N: usize>(&mut self) -> AssemblyResult<[u8; N]> {
        let end = self.offset.saturating_add(N);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        let mut out = [0_u8; N];
        out.copy_from_slice(slice);
        Ok(out)
    }
}

pub(super) fn read_len(cursor: &mut Cursor<'_>, what: &'static str) -> AssemblyResult<usize> {
    usize::try_from(cursor.read_u32()?)
        .map_err(|_| AssemblyError::text_parse_source(format!("{what} does not fit in usize")))
}
