#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub path: Box<[Box<str>]>,
    pub args: Box<[AttrArg]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrArg {
    pub name: Option<Box<str>>,
    pub value: AttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrRecordField {
    pub name: Box<str>,
    pub value: AttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrValue {
    String(Box<str>),
    Int(Box<str>),
    Rune(u32),
    Variant { tag: Box<str>, args: Box<[Self]> },
    Array { items: Box<[Self]> },
    Record { fields: Box<[AttrRecordField]> },
}
