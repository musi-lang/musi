use std::collections::HashMap;

use music_basic::{SourceId, SourceMap};
use music_hir::HirStore;
use music_il::{ConstantPool, MethodEntry, TypeDescriptor};
use music_ir::IrModuleInfo;
use music_names::{Interner, NameBindingId, NameResolution, Symbol};

pub(super) type ModuleExportKey = (String, Symbol);

#[derive(Clone, Copy)]
pub(super) struct EmitMaps<'a> {
    pub(super) globals_by_binding: &'a HashMap<NameBindingId, u16>,
    pub(super) import_globals_by_binding: &'a HashMap<NameBindingId, u16>,
    pub(super) module_export_globals: &'a HashMap<ModuleExportKey, u16>,
}

pub(super) struct EmitPools<'a> {
    pub(super) constants: &'a mut ConstantPool,
    pub(super) methods: &'a mut Vec<MethodEntry>,
    pub(super) types: &'a mut Vec<TypeDescriptor>,
}

#[derive(Clone, Copy)]
pub(super) struct EmitModuleCx<'a> {
    pub(super) interner: &'a Interner,
    pub(super) sources: &'a SourceMap,
    pub(super) source_id: SourceId,
    pub(super) store: &'a HirStore,
    pub(super) names: &'a NameResolution,
    pub(super) ir: &'a IrModuleInfo,
    pub(super) maps: EmitMaps<'a>,
}
