//! `.seam` binary module serializer.
//!
//! Writes the 16-byte BE "SEAM" header followed by 10 tagged sections.

use msc_shared::{Interner, Symbol};

use msc_bc::crc32_slice;

use crate::const_pool::ConstPool;
use crate::emitter::{FnBytecode, write_function_pool};
use crate::error::{EmitError, EmitResult};
use crate::global_table::GlobalTable;
use crate::section::write_section;
use crate::string_table::StringTable;
use crate::type_table::TypeTable;

/// A method definition within a class declaration.
pub struct ClassMethodDef {
    pub name: Symbol,
    /// Type ref index into the TYPE table (method signature).
    pub signature_type_id: u16,
    /// Whether the method has a default implementation (bit0 of flags).
    pub has_default: bool,
}

/// A class (typeclass) declaration.
pub struct ClassDef {
    pub name: Symbol,
    /// Names of the class's type parameters (e.g. `['T]`).
    pub type_params: Vec<Symbol>,
    pub methods: Vec<ClassMethodDef>,
    /// Law entries: (law name symbol, method ref string index).
    pub laws: Vec<(Symbol, u16)>,
    /// Superclass constraint class refs (indices into the class table).
    pub superclasses: Vec<u16>,
}

/// An instance record for the CLSS section.
pub struct ClassInstanceDef {
    /// Index into the class declarations (within this section).
    pub class_ref: u16,
    /// Index into the TYPE table for the target type.
    pub type_ref: u16,
    /// 0x00=manual, 0x01=via delegation, 0x02=derives.
    pub kind: u8,
    /// Method refs for manual instances (one per method, kind=0x00 only).
    pub method_refs: Vec<u16>,
    /// Delegate type ref (kind=0x01 only).
    pub delegate_type_ref: Option<u16>,
}

// Header flag bits
const FLAG_LIBRARY: u8 = 1 << 1;
const FLAG_SCRIPT: u8 = 1 << 2;

/// A foreign (FFI) function declaration for the foreign pool.
pub struct ForeignFn {
    /// Musi-side name (how the function is known in Musi code).
    pub musi_name: Symbol,
    /// External symbol name (C-side linker symbol).
    pub ext_name: Symbol,
    /// Library name, or None for builtin.
    pub library: Option<Symbol>,
    /// ABI: 0x00=C, 0x01=stdcall, 0x02=system.
    pub abi: u8,
    /// Link kind: 0x00=dynamic, 0x01=static, 0x02=framework.
    pub link_kind_tag: u8,
    /// Type signature index into the TYPE table.
    pub signature_type_id: u16,
    /// Flags: bit0=exported.
    pub ffi_flags: u8,
    pub param_type_ids: Vec<u32>,
    pub ret_type_id: u32,
    pub variadic: bool,
}

/// An effect definition for the effect pool.
pub struct EffectDef {
    pub name: Symbol,
    /// Names of the effect's type parameters (e.g. `['E]` in `effect Foo['E]`).
    pub type_param_names: Vec<Symbol>,
    pub ops: Vec<EffectOpDef>,
    /// Law entries: (law name symbol, method ref string index).
    pub laws: Vec<(Symbol, u16)>,
}

/// An effect operation definition.
pub struct EffectOpDef {
    /// Type table reference index - used as the operation's type signature.
    pub signature: u32,
    pub name: Symbol,
    pub fatal: bool,
}

/// Parameters for assembling a `.seam` binary.
pub struct AssembleParams<'a> {
    pub cp: &'a mut ConstPool,
    pub tp: &'a mut TypeTable,
    pub st: &'a mut StringTable,
    pub gt: &'a GlobalTable,
    pub functions: &'a [FnBytecode],
    pub effects: &'a [EffectDef],
    pub classes: &'a [ClassDef],
    pub class_instances: &'a [ClassInstanceDef],
    pub foreign_fns: &'a [ForeignFn],
    pub interner: &'a Interner,
    pub entry_fn_id: Option<u32>,
    pub module_name: &'a str,
    pub source_path: &'a str,
    /// Names of direct module dependencies (used to populate the DEPS section).
    /// Each entry is serialized as `u16 name_stridx + u8 major + u8 minor + u8 patch`
    /// with version `0.0.0` (merged binary already contains all code).
    pub dep_names: &'a [&'a str],
}

/// Assembles the complete `.seam` binary.
pub fn assemble(params: AssembleParams<'_>) -> EmitResult<Vec<u8>> {
    let AssembleParams {
        cp,
        tp,
        st,
        gt,
        functions,
        effects,
        classes,
        class_instances,
        foreign_fns,
        interner,
        entry_fn_id,
        module_name,
        source_path,
        dep_names,
    } = params;

    // Intern strings for effect/foreign pools into StringTable.
    // These must happen before we serialize st.
    let mut effect_payload = vec![];
    write_effect_pool(&mut effect_payload, effects, st, interner)?;

    let mut foreign_payload = vec![];
    write_foreign_pool(&mut foreign_payload, foreign_fns, st, interner)?;

    // ConstPool serialization (cp was mutated by callers; we serialize as-is).
    let mut const_payload = vec![];
    cp.write_into(&mut const_payload)?;

    // TypeTable serialization.
    let mut type_payload = vec![];
    tp.write_into(&mut type_payload)?;

    // DEPS section.
    let mut deps_payload = vec![];
    write_deps_section(&mut deps_payload, dep_names, st)?;

    // GLOB section.
    let mut glob_payload: Vec<u8> = vec![];
    gt.write_into(&mut glob_payload);

    // METH section (function pool).
    let mut meth_payload = vec![];
    write_function_pool(&mut meth_payload, functions)?;

    // EFCT section (effect pool).
    // Already built above in effect_payload.

    // CLSS section.
    let mut clss_payload = vec![];
    write_class_pool(&mut clss_payload, classes, class_instances, st, interner)?;

    // FRGN section (foreign pool).
    // Already built above in foreign_payload.

    // DBUG section.
    let mut dbug_payload = vec![];
    write_dbug_section(&mut dbug_payload, functions)?;

    // Intern module_name and source_path into StringTable before final STRT build.
    let name_stridx = st.intern_str(module_name)?;
    let path_stridx = st.intern_str(source_path)?;

    // Final STRT payload including module_name and source_path.
    let mut strt_final = vec![];
    st.write_into(&mut strt_final);

    // Build the 10 sections in order.
    let mut sections = vec![];
    write_section(&mut sections, *b"STRT", &strt_final);
    write_section(&mut sections, *b"TYPE", &type_payload);
    write_section(&mut sections, *b"CNST", &const_payload);
    write_section(&mut sections, *b"DEPS", &deps_payload);
    write_section(&mut sections, *b"GLOB", &glob_payload);
    write_section(&mut sections, *b"METH", &meth_payload);
    write_section(&mut sections, *b"EFCT", &effect_payload);
    write_section(&mut sections, *b"CLSS", &clss_payload);
    write_section(&mut sections, *b"FRGN", &foreign_payload);
    write_section(&mut sections, *b"DBUG", &dbug_payload);

    // CRC-32 of all section bytes.
    let crc2 = crc32_slice(&sections);

    // 16-byte header (BE).
    let flags: u8 = if entry_fn_id.is_none() {
        FLAG_LIBRARY
    } else {
        FLAG_SCRIPT
    };

    let mut header = Vec::with_capacity(16);
    header.extend_from_slice(b"SEAM");
    header.push(1u8); // major
    header.push(0u8); // minor
    header.push(0u8); // patch
    header.push(flags);
    header.extend_from_slice(&name_stridx.to_be_bytes());
    header.extend_from_slice(&path_stridx.to_be_bytes());
    header.extend_from_slice(&crc2.to_be_bytes());

    debug_assert_eq!(header.len(), 16, "header must be exactly 16 bytes");

    let mut out = Vec::with_capacity(16 + sections.len());
    out.extend_from_slice(&header);
    out.extend_from_slice(&sections);
    Ok(out)
}

/// Serialize the DEPS section into `buf`.
///
/// Format: `u16 count`, then per dep: `u16 name_stridx + u8 major + u8 minor + u8 patch`.
/// Version is always `0.0.0` - the merged binary already contains all dependency code.
fn write_deps_section(buf: &mut Vec<u8>, dep_names: &[&str], st: &mut StringTable) -> EmitResult {
    let count = u16::try_from(dep_names.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many module dependencies".into(),
    })?;
    buf.extend_from_slice(&count.to_be_bytes());
    for &name in dep_names {
        let name_stridx = st.intern_str(name)?;
        buf.extend_from_slice(&name_stridx.to_be_bytes());
        // version 0.0.0 - merged binary carries all code, version is informational only
        buf.push(0u8); // major
        buf.push(0u8); // minor
        buf.push(0u8); // patch
    }
    Ok(())
}

/// Serialize the class pool (declarations + instances) into `buf`, interning names into `st`.
fn write_class_pool(
    buf: &mut Vec<u8>,
    classes: &[ClassDef],
    instances: &[ClassInstanceDef],
    st: &mut StringTable,
    interner: &Interner,
) -> EmitResult {
    // Class declarations: u16 class_count, then per-class data.
    let class_count = u16::try_from(classes.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many classes".into(),
    })?;
    buf.extend_from_slice(&class_count.to_be_bytes());
    for cls in classes {
        // u16 name + u8 type_param_count
        let name_stridx = st.intern(cls.name, interner)?;
        buf.extend_from_slice(&name_stridx.to_be_bytes());
        let type_param_count =
            u8::try_from(cls.type_params.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many class type params".into(),
            })?;
        buf.push(type_param_count);
        // u16 method_count, per method: u16 name + u16 signature + u8 flags
        let method_count =
            u16::try_from(cls.methods.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many class methods".into(),
            })?;
        buf.extend_from_slice(&method_count.to_be_bytes());
        for method in &cls.methods {
            let method_name_stridx = st.intern(method.name, interner)?;
            buf.extend_from_slice(&method_name_stridx.to_be_bytes());
            buf.extend_from_slice(&method.signature_type_id.to_be_bytes());
            buf.push(u8::from(method.has_default));
        }
        // u16 law_count, per law: u16 name + u16 method_ref
        let law_count = u16::try_from(cls.laws.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many class laws".into(),
        })?;
        buf.extend_from_slice(&law_count.to_be_bytes());
        for &(law_name, method_ref) in &cls.laws {
            let law_name_stridx = st.intern(law_name, interner)?;
            buf.extend_from_slice(&law_name_stridx.to_be_bytes());
            buf.extend_from_slice(&method_ref.to_be_bytes());
        }
        // u16 superclass_count, per superclass: u16 class_ref
        let superclass_count =
            u16::try_from(cls.superclasses.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many superclasses".into(),
            })?;
        buf.extend_from_slice(&superclass_count.to_be_bytes());
        for &sc_ref in &cls.superclasses {
            buf.extend_from_slice(&sc_ref.to_be_bytes());
        }
    }
    // Instance records: u16 instance_count, then per-instance data.
    let instance_count =
        u16::try_from(instances.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many class instances".into(),
        })?;
    buf.extend_from_slice(&instance_count.to_be_bytes());
    for inst in instances {
        buf.extend_from_slice(&inst.class_ref.to_be_bytes());
        buf.extend_from_slice(&inst.type_ref.to_be_bytes());
        buf.push(inst.kind);
        match inst.kind {
            0x00 => {
                // Manual: per-method u16 method_ref (one per method in the class).
                for &method_ref in &inst.method_refs {
                    buf.extend_from_slice(&method_ref.to_be_bytes());
                }
            }
            0x01 => {
                // Via delegation: u16 delegate_type_ref.
                let delegate = inst.delegate_type_ref.unwrap_or(0xFFFF);
                buf.extend_from_slice(&delegate.to_be_bytes());
            }
            0x02 => {
                // Derives: no additional data.
            }
            _ => {
                return Err(EmitError::UnsupportedFeature {
                    desc: format!("unknown class instance kind 0x{:02X}", inst.kind).into(),
                });
            }
        }
    }
    Ok(())
}

/// Serialize the effect pool into `buf`, interning names into `st`.
fn write_effect_pool(
    buf: &mut Vec<u8>,
    effects: &[EffectDef],
    st: &mut StringTable,
    interner: &Interner,
) -> EmitResult {
    let count = u16::try_from(effects.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many effects".into(),
    })?;
    buf.extend_from_slice(&count.to_be_bytes());
    for eff in effects {
        // u16 name (string index) - spec replaces old u16 id + u16 name pair
        let name_stridx = st.intern(eff.name, interner)?;
        buf.extend_from_slice(&name_stridx.to_be_bytes());
        // u8 type_param_count + per-param u16 name (string index)
        let type_param_count =
            u8::try_from(eff.type_param_names.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many effect type params".into(),
            })?;
        buf.push(type_param_count);
        for &tp_name in &eff.type_param_names {
            let tp_stridx = st.intern(tp_name, interner)?;
            buf.extend_from_slice(&tp_stridx.to_be_bytes());
        }
        // u16 op_count, per op: u16 name + u16 signature + u8 flags (bit0=fatal)
        let op_count = u16::try_from(eff.ops.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many effect operations".into(),
        })?;
        buf.extend_from_slice(&op_count.to_be_bytes());
        for op in &eff.ops {
            let op_name_stridx = st.intern(op.name, interner)?;
            buf.extend_from_slice(&op_name_stridx.to_be_bytes());
            // u16 signature: type ref index into the TYPE table
            buf.extend_from_slice(
                &u16::try_from(op.signature)
                    .unwrap_or(u16::MAX)
                    .to_be_bytes(),
            );
            // u8 flags: bit0=fatal
            buf.push(u8::from(op.fatal));
        }
        // u16 law_count + per-law: u16 name + u16 method_ref
        let law_count = u16::try_from(eff.laws.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many effect laws".into(),
        })?;
        buf.extend_from_slice(&law_count.to_be_bytes());
        for &(law_name, method_ref) in &eff.laws {
            let law_name_stridx = st.intern(law_name, interner)?;
            buf.extend_from_slice(&law_name_stridx.to_be_bytes());
            buf.extend_from_slice(&method_ref.to_be_bytes());
        }
    }
    Ok(())
}

/// Serialize the foreign function pool into `buf`, interning names into `st`.
fn write_foreign_pool(
    buf: &mut Vec<u8>,
    foreign_fns: &[ForeignFn],
    st: &mut StringTable,
    interner: &Interner,
) -> EmitResult {
    let count = u16::try_from(foreign_fns.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many foreign functions".into(),
    })?;
    buf.extend_from_slice(&count.to_be_bytes());
    for ff in foreign_fns {
        // Spec: u16 musi_name + u16 symbol_name + u16 library_name
        let musi_stridx = st.intern(ff.musi_name, interner)?;
        buf.extend_from_slice(&musi_stridx.to_be_bytes());
        let ext_name_stridx = st.intern(ff.ext_name, interner)?;
        buf.extend_from_slice(&ext_name_stridx.to_be_bytes());

        if let Some(lib) = ff.library {
            let lib_stridx = st.intern(lib, interner)?;
            buf.extend_from_slice(&lib_stridx.to_be_bytes());
        } else {
            buf.extend_from_slice(&0xFFFFu16.to_be_bytes());
        }

        // Spec: u8 abi + u8 link_kind + u16 signature + u8 flags
        buf.push(ff.abi);
        buf.push(ff.link_kind_tag);
        buf.extend_from_slice(&ff.signature_type_id.to_be_bytes());
        buf.push(ff.ffi_flags);

        let param_count =
            u16::try_from(ff.param_type_ids.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many foreign fn params".into(),
            })?;
        buf.extend_from_slice(&param_count.to_be_bytes());
        for &type_id in &ff.param_type_ids {
            buf.extend_from_slice(&type_id.to_be_bytes());
        }
        buf.extend_from_slice(&ff.ret_type_id.to_be_bytes());
        buf.push(u8::from(ff.variadic));
    }
    Ok(())
}

/// Serialize the DBUG section into `buf`.
///
/// Format:
///   Source map: u16 entry count, per entry: u32 `bytecode_offset` + u32 line + u16 column.
///   Local names: u16 method count, per method: u16 local count,
///                per local: u16 `name_stridx` + u32 `scope_start` + u32 `scope_end`.
fn write_dbug_section(buf: &mut Vec<u8>, functions: &[FnBytecode]) -> EmitResult {
    // Source map: collect all entries from all functions.
    let total_entries: usize = functions.iter().map(|f| f.source_map.len()).sum();
    let entry_count = u16::try_from(total_entries).map_err(|_| EmitError::OperandOverflow {
        desc: "too many source map entries".into(),
    })?;
    buf.extend_from_slice(&entry_count.to_be_bytes());
    for fn_bc in functions {
        for &(offset, line, column) in &fn_bc.source_map {
            buf.extend_from_slice(&offset.to_be_bytes());
            buf.extend_from_slice(&line.to_be_bytes());
            buf.extend_from_slice(&column.to_be_bytes());
        }
    }

    // Local names: u16 method count, then per method.
    let method_count = u16::try_from(functions.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many functions in local names table".into(),
    })?;
    buf.extend_from_slice(&method_count.to_be_bytes());
    for fn_bc in functions {
        let local_count =
            u16::try_from(fn_bc.local_names.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many locals in debug info".into(),
            })?;
        buf.extend_from_slice(&local_count.to_be_bytes());
        for &(name_stridx, scope_start, scope_end) in &fn_bc.local_names {
            buf.extend_from_slice(&name_stridx.to_be_bytes());
            buf.extend_from_slice(&scope_start.to_be_bytes());
            buf.extend_from_slice(&scope_end.to_be_bytes());
        }
    }
    Ok(())
}
