use music_base::Span;
use music_hir::HirAttr;
use music_names::NameBindingId;

#[derive(Debug, Clone)]
pub(in crate::checker::surface) struct ExportBinding {
    pub(super) binding: NameBindingId,
    pub(super) name: Box<str>,
    pub(super) opaque: bool,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Clone)]
pub(in crate::checker::surface) struct ExportGiven {
    pub(super) span: Span,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Default)]
pub(in crate::checker::surface) struct ModuleExports {
    pub(super) bindings: Vec<ExportBinding>,
    pub(super) givens: Vec<ExportGiven>,
}
