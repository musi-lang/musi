#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SurfaceTyId(u32);

impl SurfaceTyId {
    #[must_use]
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceTy {
    pub kind: SurfaceTyKind,
}

impl SurfaceTy {
    #[must_use]
    pub const fn new(kind: SurfaceTyKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceTyKind {
    Error,
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Float,
    Float32,
    Float64,
    String,
    Rune,
    CString,
    CPtr,
    Bits {
        width: u32,
    },
    NatLit(u64),
    Named {
        name: Box<str>,
        args: Box<[SurfaceTyId]>,
    },
    Pi {
        binder: Box<str>,
        binder_ty: SurfaceTyId,
        body: SurfaceTyId,
        is_effectful: bool,
    },
    Arrow {
        params: Box<[SurfaceTyId]>,
        ret: SurfaceTyId,
        is_effectful: bool,
    },
    Sum {
        left: SurfaceTyId,
        right: SurfaceTyId,
    },
    Tuple {
        items: Box<[SurfaceTyId]>,
    },
    Seq {
        item: SurfaceTyId,
    },
    Array {
        dims: Box<[SurfaceDim]>,
        item: SurfaceTyId,
    },
    Range {
        bound: SurfaceTyId,
    },
    Handler {
        effect: SurfaceTyId,
        input: SurfaceTyId,
        output: SurfaceTyId,
    },
    Mut {
        inner: SurfaceTyId,
    },
    AnyShape {
        capability: SurfaceTyId,
    },
    SomeShape {
        capability: SurfaceTyId,
    },
    Record {
        fields: Box<[SurfaceTyField]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceDim {
    Unknown,
    Name(Box<str>),
    Int(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceTyField {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
}

impl SurfaceTyField {
    #[must_use]
    pub fn new<Name>(name: Name, ty: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            ty,
        }
    }
}
