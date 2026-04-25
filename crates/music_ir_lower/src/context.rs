use super::{
    BTreeMap, ComptimeValue, ConstraintKey, ExportedValue, HashMap, HashSet, HirExprId, HirParam,
    HirPatId, HirRecordItem, Interner, IrCallable, IrDataDef, IrExprKind, IrForeignDef, IrGlobal,
    IrLoweredMatchArm, IrModuleInitPart, IrRecordLayoutField, ModuleKey, NameBindingId, SemaModule,
    SliceRange,
};

#[derive(Default)]
pub(crate) struct TopLevelItems {
    pub(crate) exports: Vec<ExportedValue>,
    pub(crate) callables: Vec<IrCallable>,
    pub(crate) globals: Vec<IrGlobal>,
    pub(crate) init_parts: Vec<IrModuleInitPart>,
    pub(crate) data_defs: Vec<IrDataDef>,
    pub(crate) foreigns: Vec<IrForeignDef>,
}

pub(crate) struct LetItemInput {
    pub(crate) expr_id: HirExprId,
    pub(crate) pat: HirPatId,
    pub(crate) params: HirParamRange,
    pub(crate) value: HirExprId,
    pub(crate) is_callable: bool,
    pub(crate) exported: bool,
}

pub(crate) type RecordLayout = (BTreeMap<Box<str>, u16>, Box<[IrRecordLayoutField]>, u16);
pub(crate) type HirParamRange = SliceRange<HirParam>;
pub(crate) type HirRecordItemRange = SliceRange<HirRecordItem>;
pub(crate) type BoundNameSet = HashSet<NameBindingId>;
pub(crate) type LoweredMatchArmList = Box<[IrLoweredMatchArm]>;
pub(crate) type ConstraintAnswerBindingMap = HashMap<ConstraintKey, Box<str>>;
pub(crate) type ConstraintAnswerBindingStack = Vec<ConstraintAnswerBindingMap>;

pub(crate) fn qualified_name(module: &ModuleKey, name: &str) -> Box<str> {
    format!("{}::{name}", module.as_str()).into_boxed_str()
}

pub(crate) struct LowerCtx<'a> {
    pub(crate) sema: &'a SemaModule,
    pub(crate) interner: &'a Interner,
    pub(crate) module_key: ModuleKey,
    pub(crate) module_level_bindings: BoundNameSet,
    pub(crate) next_lambda_id: u32,
    pub(crate) next_temp_id: u32,
    pub(crate) extra_callables: Vec<IrCallable>,
    pub(crate) constraint_answer_bindings: ConstraintAnswerBindingStack,
    pub(crate) comptime_bindings: HashMap<NameBindingId, ComptimeValue>,
    pub(crate) specialized_callables: HashSet<Box<str>>,
}

pub(crate) type LoweringResult<T = IrExprKind> = Result<T, Box<str>>;
