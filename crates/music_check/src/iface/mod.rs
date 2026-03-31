use std::collections::BTreeMap;

use music_basic::SourceId;

/// Portable semantic interface model for cross-module checking.
///
/// This is intentionally string-based so `music_session::SessionImportEnv` can store summaries
/// without borrowing an interner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportTy {
    Error,
    Unknown,
    Any,

    Named {
        name: String,
        args: Box<[ExportTy]>,
    },
    Tuple {
        items: Box<[ExportTy]>,
    },
    Array {
        dims: Box<[ExportDim]>,
        elem: Box<ExportTy>,
    },
    Arrow {
        flavor: ExportArrowFlavor,
        input: Box<ExportTy>,
        output: Box<ExportTy>,
    },
    Binary {
        op: ExportTyBinOp,
        left: Box<ExportTy>,
        right: Box<ExportTy>,
    },
    Mut {
        base: Box<ExportTy>,
    },
    Record {
        /// Field order is stable and sorted.
        fields: Box<[(String, ExportTy)]>,
    },
    Generic(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportArrowFlavor {
    Pure,
    Effectful,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportTyBinOp {
    Sum,
    Product,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportDim {
    Inferred,
    Int(u64),
    Name(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEffectKey {
    pub name: String,
    pub arg: Option<ExportTy>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEffectRow {
    pub items: Box<[ExportEffectKey]>,
    pub is_open: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportTyNamed {
    pub name: String,
    pub args: Box<[ExportTy]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportConstraint {
    Subtype {
        name: String,
        idx: u32,
        bound: ExportTyNamed,
    },
    Implements {
        name: String,
        idx: u32,
        class: ExportTyNamed,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportValueScheme {
    pub generic_count: u32,
    pub ty: ExportTy,
    pub declared_effects: Option<ExportEffectRow>,
    pub constraints: Box<[ExportConstraint]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEffectOpSig {
    pub params: Box<[ExportTy]>,
    pub ret: ExportTy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportClassOpSig {
    pub params: Box<[ExportTy]>,
    pub ret: ExportTy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEffectFamily {
    pub generic_count: u32,
    pub ops: BTreeMap<String, ExportEffectOpSig>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportClassFamily {
    pub generic_count: u32,
    pub ops: BTreeMap<String, ExportClassOpSig>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDataDef {
    pub generic_count: u32,
    pub variants: Option<BTreeMap<String, Option<ExportTy>>>,
    pub fields: Option<BTreeMap<String, ExportTy>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportItemKind {
    Value,
    Data { def: ExportDataDef },
    Effect { family: ExportEffectFamily },
    Class { family: ExportClassFamily },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportItem {
    pub scheme: ExportValueScheme,
    pub kind: ExportItemKind,
    pub opaque: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedName {
    pub name: String,
    pub opaque: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportInstance {
    pub target: ExportTyNamed,
    pub generic_count: u32,
    pub constraints: Box<[ExportConstraint]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExportSummary {
    /// Exported binding names, in stable order.
    pub exports: Box<[ExportedName]>,
    /// Exported bindings by name.
    pub items: BTreeMap<String, ExportItem>,
    /// Exported instances.
    pub instances: Box<[ExportInstance]>,
}

impl ModuleExportSummary {
    #[must_use]
    pub fn is_export_opaque(&self, name: &str) -> bool {
        self.items.get(name).is_some_and(|it| it.opaque)
    }
}

/// Sema-facing import environment.
///
/// `music_resolve::ImportEnv` owns syntax-only decisions like which names exist in a module.
/// `SemaImportEnv` owns semantic facts about exported items: schemes, defs, families, instances.
pub trait SemaImportEnv {
    fn module_summary(&self, from: SourceId, path: &str) -> Option<&ModuleExportSummary>;
}
