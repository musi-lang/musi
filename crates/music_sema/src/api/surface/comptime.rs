use music_module::ModuleKey;
use music_term::{SyntaxTerm, TypeTerm};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComptimeValue {
    Unit,
    Int(i64),
    Nat(u64),
    Float(Box<str>),
    String(Box<str>),
    Rune(u32),
    CPtr(usize),
    Syntax(SyntaxTerm),
    Seq(ComptimeSeqValue),
    Data(ComptimeDataValue),
    Closure(ComptimeClosureValue),
    Continuation(ComptimeContinuationValue),
    Type(ComptimeTypeValue),
    ImportRecord(ComptimeImportRecordValue),
    Foreign(ComptimeForeignValue),
    Effect(ComptimeEffectValue),
    Shape(ComptimeShapeValue),
}

pub type ComptimeValueList = Box<[ComptimeValue]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeSeqValue {
    pub ty: TypeTerm,
    pub items: ComptimeValueList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeDataValue {
    pub ty: TypeTerm,
    pub tag: i64,
    pub variant: Box<str>,
    pub fields: ComptimeValueList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeClosureValue {
    pub module: ModuleKey,
    pub name: Box<str>,
    pub captures: ComptimeValueList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeContinuationValue {
    pub frames: ComptimeValueList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeTypeValue {
    pub term: TypeTerm,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeImportRecordValue {
    pub key: ModuleKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeForeignValue {
    pub module: ModuleKey,
    pub name: Box<str>,
    pub type_args: Box<[TypeTerm]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeEffectValue {
    pub module: ModuleKey,
    pub name: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComptimeShapeValue {
    pub module: ModuleKey,
    pub name: Box<str>,
}
