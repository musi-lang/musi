use std::collections::HashMap;
use std::io::{Read, Seek, SeekFrom};

use crate::errors::{MusiError, MusiResult};

const MAGIC_BYTES: &[u8] = b"MUSI";
const SUPPORT_VERSION: u32 = 1;
const MAX_HEADER_SIZE: usize = 32;

#[derive(Debug, Clone)]
pub struct BytecodeHeader {
    pub version: u32,
    pub imports_offset: u32,
    pub consts_offset: u32,
    pub procs_offset: u32,
    pub code_offset: u32,
    pub code_size: u32,
}

impl BytecodeHeader {
    pub fn check(&self) -> MusiResult<()> {
        if self.version != SUPPORT_VERSION {
            return Err(MusiError::InvalidBytecode(format!(
                "unsupported version: {} (expected {SUPPORT_VERSION})",
                self.version,
            )));
        }

        if self.imports_offset < MAX_HEADER_SIZE as u32
            || self.consts_offset < self.imports_offset
            || self.procs_offset < self.consts_offset
            || self.code_offset < self.procs_offset
        {
            return Err(MusiError::InvalidBytecode(
                "invalid section offsets".to_string(),
            ));
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ConstPool {
    pub ints: Vec<i64>,
    pub nats: Vec<u64>,
    pub bins: Vec<f64>,
    pub strs: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ProcInfo {
    pub name: String,
    pub param_count: u16,
    pub local_count: u16,
    pub code_offset: u32,
    pub code_size: u32,
}

#[derive(Debug, Clone)]
pub struct BytecodeModule {
    pub header: BytecodeHeader,
    pub imports: Vec<String>,
    pub consts: ConstPool,
    pub procs: HashMap<String, ProcInfo>,
    pub code: Vec<u8>,
}

fn read_u16<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<u16> {
    let mut buf = [0u8; 2];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    Ok(u16::from_le_bytes(buf))
}

fn read_u32<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<u32> {
    let mut buf = [0u8; 4];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    Ok(u32::from_le_bytes(buf))
}

fn read_i64<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<i64> {
    let mut buf = [0u8; 8];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    Ok(i64::from_le_bytes(buf))
}

fn read_u64<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<u64> {
    let mut buf = [0u8; 8];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    Ok(u64::from_le_bytes(buf))
}

fn read_f64<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<f64> {
    let mut buf = [0u8; 8];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    Ok(f64::from_le_bytes(buf))
}

fn read_string<R: Read>(reader: &mut R, ctx: &str) -> MusiResult<String> {
    let len = read_u32(reader, &format!("{ctx} length"))? as usize;
    let mut buf = vec![0u8; len];
    reader
        .read_exact(&mut buf)
        .map_err(|why| MusiError::IoError(format!("{ctx}: {why}")))?;
    String::from_utf8(buf).map_err(|why| MusiError::InvalidBytecode(format!("{ctx} UTF-8: {why}")))
}

impl BytecodeModule {
    pub fn read<R: Read + Seek>(reader: &mut R) -> MusiResult<Self> {
        let header = Self::read_header(reader)?;
        header.check()?;

        reader.seek(SeekFrom::Start(header.imports_offset as u64))?;
        let imports = Self::read_imports(reader)?;

        reader.seek(SeekFrom::Start(header.consts_offset as u64))?;
        let consts = Self::read_consts(reader)?;

        reader.seek(SeekFrom::Start(header.procs_offset as u64))?;
        let procs = Self::read_procs(reader)?;

        reader.seek(SeekFrom::Start(header.code_offset as u64))?;
        let mut code = vec![0u8; header.code_size as usize];
        reader
            .read_exact(&mut code)
            .map_err(|why| MusiError::IoError(format!("code section: {why}")))?;

        Ok(Self {
            header,
            imports,
            consts,
            procs,
            code,
        })
    }

    fn read_header<R: Read>(reader: &mut R) -> MusiResult<BytecodeHeader> {
        let mut magic = [0u8; 4];
        reader
            .read_exact(&mut magic)
            .map_err(|why| MusiError::IoError(format!("magic bytes: {why}")))?;

        if &magic != MAGIC_BYTES {
            return Err(MusiError::InvalidBytecode("invalid magic".to_string()));
        }

        Ok(BytecodeHeader {
            version: read_u32(reader, "version")?,
            imports_offset: read_u32(reader, "imports offset")?,
            consts_offset: read_u32(reader, "consts offset")?,
            procs_offset: read_u32(reader, "procs offset")?,
            code_offset: read_u32(reader, "code offset")?,
            code_size: read_u32(reader, "code size")?,
        })
    }

    fn read_imports<R: Read>(reader: &mut R) -> MusiResult<Vec<String>> {
        let count = read_u32(reader, "import count")? as usize;
        (0..count).map(|_| read_string(reader, "import")).collect()
    }

    fn read_consts<R: Read>(reader: &mut R) -> MusiResult<ConstPool> {
        let int_count = read_u32(reader, "int count")? as usize;
        let nat_count = read_u32(reader, "nat count")? as usize;
        let bin_count = read_u32(reader, "bin count")? as usize;
        let str_count = read_u32(reader, "str count")? as usize;

        let ints = (0..int_count)
            .map(|_| read_i64(reader, "int const"))
            .collect::<MusiResult<_>>()?;
        let nats = (0..nat_count)
            .map(|_| read_u64(reader, "nat const"))
            .collect::<MusiResult<_>>()?;
        let bins = (0..bin_count)
            .map(|_| read_f64(reader, "bin const"))
            .collect::<MusiResult<_>>()?;
        let strs = (0..str_count)
            .map(|_| read_string(reader, "str const"))
            .collect::<MusiResult<_>>()?;
        Ok(ConstPool {
            ints,
            nats,
            bins,
            strs,
        })
    }

    fn read_procs<R: Read>(reader: &mut R) -> MusiResult<HashMap<String, ProcInfo>> {
        let count = read_u32(reader, "proc count")? as usize;
        let mut procs = HashMap::with_capacity(count);

        for _ in 0..count {
            let name = read_string(reader, "proc name")?;
            let param_count = read_u16(reader, "param count")?;
            let local_count = read_u16(reader, "local count")?;
            let code_offset = read_u32(reader, "code offset")?;
            let code_size = read_u32(reader, "code size")?;

            procs.insert(
                name.clone(),
                ProcInfo {
                    name,
                    param_count,
                    local_count,
                    code_offset,
                    code_size,
                },
            );
        }

        Ok(procs)
    }

    pub fn get_proc(&self, name: &str) -> Option<&ProcInfo> {
        self.procs.get(name)
    }

    pub fn get_proc_code(&self, proc: &ProcInfo) -> Option<&[u8]> {
        let start = (proc.code_offset - self.header.code_offset) as usize;
        let end = start + proc.code_size as usize;
        self.code.get(start..end)
    }

    pub fn get_int_const(&self, idx: usize) -> MusiResult<i64> {
        self.consts
            .ints
            .get(idx)
            .copied()
            .ok_or(MusiError::IndexOutOfBounds {
                idx,
                len: self.consts.ints.len(),
            })
    }

    pub fn get_nat_const(&self, idx: usize) -> MusiResult<u64> {
        self.consts
            .nats
            .get(idx)
            .copied()
            .ok_or(MusiError::IndexOutOfBounds {
                idx,
                len: self.consts.nats.len(),
            })
    }

    pub fn get_bin_const(&self, idx: usize) -> MusiResult<f64> {
        self.consts
            .bins
            .get(idx)
            .copied()
            .ok_or(MusiError::IndexOutOfBounds {
                idx,
                len: self.consts.bins.len(),
            })
    }

    pub fn get_str_const(&self, idx: usize) -> MusiResult<&str> {
        self.consts
            .strs
            .get(idx)
            .map(|s| s.as_str())
            .ok_or(MusiError::IndexOutOfBounds {
                idx,
                len: self.consts.strs.len(),
            })
    }

    pub fn check_instr(&self) -> MusiResult<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_header_validation() {
        let mut header = BytecodeHeader {
            version: SUPPORT_VERSION,
            imports_offset: 32,
            consts_offset: 64,
            procs_offset: 128,
            code_offset: 256,
            code_size: 1024,
        };

        assert!(header.check().is_ok());

        header.version = 999;
        assert!(header.check().is_err());

        header.version = SUPPORT_VERSION;
        header.consts_offset = 16;
        assert!(header.check().is_err());
    }
}
