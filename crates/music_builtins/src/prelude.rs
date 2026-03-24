/// A type class that is part of the Musi prelude.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PreludeClass {
    pub name: &'static str,
    pub methods: &'static [IntrinsicMethod],
}

/// A method within a prelude class that maps to a compiler intrinsic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntrinsicMethod {
    pub name: &'static str,
    pub op_name: &'static str,
    pub intrinsic: &'static str,
}

/// All prelude type classes and their intrinsic method mappings.
pub const PRELUDE_CLASSES: &[PreludeClass] = &[
    PreludeClass {
        name: "Eq",
        methods: &[
            IntrinsicMethod {
                name: "eq",
                op_name: "(=)",
                intrinsic: "cmp.eq",
            },
            IntrinsicMethod {
                name: "ne",
                op_name: "(/=)",
                intrinsic: "cmp.neq",
            },
        ],
    },
    PreludeClass {
        name: "Ord",
        methods: &[
            IntrinsicMethod {
                name: "lt",
                op_name: "(<)",
                intrinsic: "cmp.lt",
            },
            IntrinsicMethod {
                name: "gt",
                op_name: "(>)",
                intrinsic: "cmp.gt",
            },
            IntrinsicMethod {
                name: "le",
                op_name: "(<=)",
                intrinsic: "cmp.leq",
            },
            IntrinsicMethod {
                name: "ge",
                op_name: "(>=)",
                intrinsic: "cmp.geq",
            },
        ],
    },
    PreludeClass {
        name: "Num",
        methods: &[
            IntrinsicMethod {
                name: "add",
                op_name: "(+)",
                intrinsic: "i.add",
            },
            IntrinsicMethod {
                name: "sub",
                op_name: "(-)",
                intrinsic: "i.sub",
            },
            IntrinsicMethod {
                name: "mul",
                op_name: "(*)",
                intrinsic: "i.mul",
            },
            IntrinsicMethod {
                name: "div",
                op_name: "(/)",
                intrinsic: "i.div",
            },
            IntrinsicMethod {
                name: "rem",
                op_name: "(%)",
                intrinsic: "i.mod",
            },
            IntrinsicMethod {
                name: "neg",
                op_name: "neg",
                intrinsic: "i.neg",
            },
        ],
    },
    PreludeClass {
        name: "Bits",
        methods: &[
            IntrinsicMethod {
                name: "shl",
                op_name: "shl",
                intrinsic: "i.shl",
            },
            IntrinsicMethod {
                name: "shr",
                op_name: "shr",
                intrinsic: "i.shr",
            },
        ],
    },
    PreludeClass {
        name: "Show",
        methods: &[],
    },
];

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
