/// A type class that is part of the Musi prelude.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PreludeClass {
    pub name: &'static str,
    pub methods: &'static [PreludeMethod],
}

/// Canonical Musi source for the compiler-owned injected prelude.
pub const PRELUDE_SOURCE: &str = include_str!("../prelude.ms");

/// A method within a prelude class, used for name resolution seeding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PreludeMethod {
    pub name: &'static str,
    pub op_name: &'static str,
}

/// All prelude type classes and their method names.
pub const PRELUDE_CLASSES: &[PreludeClass] = &[
    PreludeClass {
        name: "Eq",
        methods: &[
            PreludeMethod {
                name: "eq",
                op_name: "(=)",
            },
            PreludeMethod {
                name: "ne",
                op_name: "(/=)",
            },
        ],
    },
    PreludeClass {
        name: "Ord",
        methods: &[
            PreludeMethod {
                name: "lt",
                op_name: "(<)",
            },
            PreludeMethod {
                name: "gt",
                op_name: "(>)",
            },
            PreludeMethod {
                name: "le",
                op_name: "(<=)",
            },
            PreludeMethod {
                name: "ge",
                op_name: "(>=)",
            },
        ],
    },
    PreludeClass {
        name: "Num",
        methods: &[
            PreludeMethod {
                name: "add",
                op_name: "(+)",
            },
            PreludeMethod {
                name: "sub",
                op_name: "(-)",
            },
            PreludeMethod {
                name: "mul",
                op_name: "(*)",
            },
            PreludeMethod {
                name: "div",
                op_name: "(/)",
            },
            PreludeMethod {
                name: "rem",
                op_name: "(%)",
            },
            PreludeMethod {
                name: "neg",
                op_name: "neg",
            },
        ],
    },
    PreludeClass {
        name: "Bits",
        methods: &[
            PreludeMethod {
                name: "shl",
                op_name: "shl",
            },
            PreludeMethod {
                name: "shr",
                op_name: "shr",
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
